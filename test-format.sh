#!/usr/bin/env bash

# Script to test Rust format implementation against Lua format implementation using .alc test cases
# Usage: ./test-format.sh [--generate-expected] [--verbose] [--binary-search] [--test <test-name>]

set -e

GENERATE_EXPECTED=false
VERBOSE=false
BINARY_SEARCH=false
TEST_FILTER=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --generate-expected) GENERATE_EXPECTED=true; shift ;;
        --verbose) VERBOSE=true; shift ;;
        --binary-search) BINARY_SEARCH=true; shift ;;
        --test) TEST_FILTER="$2"; shift 2 ;;
        --help) echo "Usage: $0 [--generate-expected] [--verbose] [--binary-search] [--test <test-name>]"; exit 0 ;;
        *) echo "Unknown option $1"; exit 1 ;;
    esac
done

# Build Rust harness if needed
if [ ! -f target/debug/format-harness ]; then
    echo "Building Rust format harness..."
    cargo build --bin format-harness --quiet && cd ..
fi

# Binary search function to find the failing line
binary_search_failure() {
    local test_file="$1"
    local total_lines=$(wc -l < "$test_file")
    local low=1
    local high=$total_lines
    local last_working=0
    local first_failing=$total_lines
    
    echo "  Starting binary search on $total_lines lines..."
    
    while [ $low -le $high ]; do
        local mid=$(( (low + high) / 2 ))
        local temp_file=$(mktemp)
        local temp_lua_output=$(mktemp)
        local temp_lua_normalized=$(mktemp)
        local temp_rust_output=$(mktemp)
        
        # Create test file with first 'mid' lines
        head -n $mid "$test_file" > "$temp_file"
        
        # Run Lua format and normalize
        local lua_success=false
        cd Lua-Alicorn0
        if luajit ../parse-harness.lua -f "$temp_file" > "$temp_lua_output" 2>/dev/null; then
            cd ..
            if luajit normalize-output.lua "$temp_lua_output" > "$temp_lua_normalized" 2>/dev/null; then
                lua_success=true
            fi
        else
            cd ..
        fi
        
        # Run Rust format
        local rust_success=false
        if target/debug/format-harness -f "$temp_file" > "$temp_rust_output" 2>/dev/null; then
            rust_success=true
        fi
        
        # Compare results
        if [ "$lua_success" = true ] && [ "$rust_success" = true ]; then
            # Both succeeded, compare outputs
            if diff -q "$temp_lua_normalized" "$temp_rust_output" >/dev/null 2>&1; then
                echo "    Lines 1-$mid: SUCCESS"
                last_working=$mid
                low=$((mid + 1))
            else
                echo "    Lines 1-$mid: FAIL (outputs differ)"
                first_failing=$mid
                high=$((mid - 1))
            fi
        elif [ "$lua_success" = false ] && [ "$rust_success" = false ]; then
            # Both failed - this is actually success (consistent behavior)
            echo "    Lines 1-$mid: SUCCESS (both parsers failed)"
            last_working=$mid
            low=$((mid + 1))
        else
            # One succeeded, one failed - this is a failure
            if [ "$lua_success" = true ]; then
                echo "    Lines 1-$mid: FAIL (Rust failed, Lua succeeded)"
            else
                echo "    Lines 1-$mid: FAIL (Lua failed, Rust succeeded)"
            fi
            first_failing=$mid
            high=$((mid - 1))
        fi
        
        rm -f "$temp_file" "$temp_lua_output" "$temp_lua_normalized" "$temp_rust_output"
    done
    
    echo "  Binary search complete!"
    echo "  Last working: line $last_working"
    echo "  First failing: line $first_failing"
    
    if [ $first_failing -le $total_lines ]; then
        echo "  Failing line $first_failing:"
        sed -n "${first_failing}p" "$test_file" | sed 's/^/    /'
        
        # Show context
        if [ $first_failing -gt 1 ]; then
            echo "  Context (lines $((first_failing-2))-$((first_failing+2))):"
            sed -n "$((first_failing-2)),$((first_failing+2))p" "$test_file" | nl -v $((first_failing-2)) | sed 's/^/    /'
        fi
        
        # Find first differing character for the last working configuration
        if [ $first_failing -gt 0 ]; then
            echo ""
            echo "  Finding first differing character (using first $first_failing lines)..."
            
            local temp_file=$(mktemp)
            local temp_lua_output=$(mktemp)
            local temp_lua_normalized=$(mktemp)
            local temp_rust_output=$(mktemp)
            
            # Create test file with first 'last_working' lines
            head -n $first_failing "$test_file" > "$temp_file"
            
            # Run Lua format and normalize
            cd Lua-Alicorn0
            if luajit ../parse-harness.lua -f "$temp_file" > "$temp_lua_output" 2>/dev/null; then
                cd ..
                if luajit normalize-output.lua "$temp_lua_output" > "$temp_lua_normalized" 2>/dev/null; then
                    # Run Rust format
                    if target/debug/format-harness -f "$temp_file" > "$temp_rust_output" 2>/dev/null; then
                        # Find first differing character
                        local lua_content=$(cat "$temp_lua_normalized")
                        local rust_content=$(cat "$temp_rust_output")
                        local min_len=${#lua_content}
                        [ ${#rust_content} -lt $min_len ] && min_len=${#rust_content}
                        
                        local first_diff=$(cmp -l <(printf '%s' "$lua_content") <(printf '%s' "$rust_content") 2>/dev/null | head -1 | awk '{print $1-1}')
                        [ -z "$first_diff" ] && first_diff=-1
                        
                        if [ $first_diff -eq -1 ] && [ ${#lua_content} -ne ${#rust_content} ]; then
                            first_diff=$min_len
                        fi
                        
                        if [ $first_diff -ne -1 ]; then
                            echo "  First difference at character position: $first_diff"
                            local start=$((first_diff > 20 ? first_diff - 20 : 0))
                            local end=$((first_diff + 20))
                            echo "  Lua output  (chars $start-$end): '${lua_content:start:40}'"
                            echo "  Rust output (chars $start-$end): '${rust_content:start:40}'"
                            echo "  Difference marker: $(printf '%*s' $((first_diff - start + 13)) '')^"
                        else
                            echo "  No character differences found (outputs are identical)"
                        fi
                    fi
                else
                    cd ..
                fi
            else
                cd ..
            fi
            
            rm -f "$temp_file" "$temp_lua_output" "$temp_lua_normalized" "$temp_rust_output"
        fi
    fi
}

PASSED=0
FAILED=0
LUA_FAILS=0
PASSED_TESTS=()
FAILED_TESTS=()
LUA_FAIL_TESTS=()

for test_file in testcases/*.alc; do
    [ ! -f "$test_file" ] && continue
    
    test_name=$(basename "$test_file" .alc)
    expected_file="testcases/$test_name.expected"
    
    # Skip if test filter is specified and doesn't match
    if [ -n "$TEST_FILTER" ] && [ "$test_name" != "$TEST_FILTER" ]; then
        continue
    fi
    
    echo "Testing: $test_name"
    
    # Generate expected output if requested
    if [ "$GENERATE_EXPECTED" = true ]; then
        cd Lua-Alicorn0
        if lua_output=$(luajit ../parse-harness.lua -f "../$test_file" 2>/dev/null); then
            cd ..
            # Normalize the Lua output before storing
            lua_temp=$(mktemp)
            echo "$lua_output" > "$lua_temp"
            if luajit normalize-output.lua "$lua_temp" > "$expected_file" 2>/dev/null; then
                echo "  Generated normalized expected output"
            else
                echo "$lua_output" > "$expected_file"
                echo "  Generated raw expected output (normalization failed)"
            fi
            rm -f "$lua_temp"
        else
            cd ..
            echo "  SKIP: Lua parser failed to generate expected output"
            continue
        fi
    fi
    
    # Skip if no expected file
    if [ ! -f "$expected_file" ]; then
        echo "  SKIP: No expected file (use --generate-expected)"
        continue
    fi
    
    # Run Rust format harness
    if rust_output=$(target/debug/format-harness -f "$test_file" 2>&1); then
        # Check if expected file is empty (Lua failed but Rust succeeded)
        if [ ! -s "$expected_file" ]; then
            echo "  LUA_FAILS: Rust succeeded but Lua failed"
            if [ "$VERBOSE" = true ]; then
                echo "Rust output: $rust_output"
            fi
            LUA_FAILS=$((LUA_FAILS + 1))
            LUA_FAIL_TESTS+=("$test_name")
        else
            # Direct comparison (assume Rust output is already normalized)
            expected_output=$(cat "$expected_file")
            
            if [ "$rust_output" = "$expected_output" ]; then
                echo "  PASS"
                PASSED=$((PASSED + 1))
                PASSED_TESTS+=("$test_name")
            else
                echo "  FAIL: Output differs"
                if [ "$VERBOSE" = true ]; then
                    echo "Expected: $expected_output"
                    echo "Got:      $rust_output"
                fi
                FAILED=$((FAILED + 1))
                FAILED_TESTS+=("$test_name")
                
                # Run binary search if requested
                if [ "$BINARY_SEARCH" = true ]; then
                    binary_search_failure "$test_file"
                    exit 1
                fi
            fi
        fi
    else
        # Check if expected file is empty (meaning Lua also crashed)
        if [ -f "$expected_file" ] && [ ! -s "$expected_file" ]; then
            echo "  PASS: Both parsers failed consistently"
            PASSED=$((PASSED + 1))
            PASSED_TESTS+=("$test_name")
        else
            echo "  FAIL: Rust format harness crashed"
            if [ "$VERBOSE" = true ]; then
                echo "Error output:"
                echo "$rust_output" | sed 's/^/    /'
            fi
            FAILED=$((FAILED + 1))
            FAILED_TESTS+=("$test_name")
            
            # Run binary search if requested
            if [ "$BINARY_SEARCH" = true ]; then
                binary_search_failure "$test_file"
                exit 1
            fi
        fi
    fi
done

echo ""
echo "Results: $PASSED passed, $FAILED failed, $LUA_FAILS lua_fails"
echo ""

if [ ${#PASSED_TESTS[@]} -gt 0 ]; then
    echo "PASSED tests:"
    for test in $(printf "%s\n" "${PASSED_TESTS[@]}" | sort); do
        if [ "$test" = "prelude" ]; then
            echo "  $test 🐱"
        else
            echo "  $test"
        fi
    done
    echo ""
fi

if [ ${#FAILED_TESTS[@]} -gt 0 ]; then
    echo "FAILED tests:"
    printf "  %s\n" "${FAILED_TESTS[@]}" | sort
    echo ""
fi

if [ ${#LUA_FAIL_TESTS[@]} -gt 0 ]; then
    echo "LUA_FAILS (Rust succeeds, Lua fails):"
    printf "  %s\n" "${LUA_FAIL_TESTS[@]}" | sort
    echo ""
fi

[ $FAILED -gt 0 ] && exit 1 || exit 0
