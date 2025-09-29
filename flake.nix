{
  description = "An experimental language for high performance safe convenient metaprogramming";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      pre-commit-hooks,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        luajit_lua5_2 = pkgs.luajit.override {
          enable52Compat = true;
          self = luajit_lua5_2;
        };
      in
      {
        packages = rec {
          inherit (pkgs) hello;
          default = hello;
        };
        apps = rec {
          hello = flake-utils.lib.mkApp { drv = self.packages.${system}.hello; };
          default = hello;
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              statix.enable = true;
              nixpkgs-fmt.enable = true;
              deadnix.enable = true;
              rustfmt.enable = true;
            };
          };
        };
        # nix fmt's docs say this should only be for *.nix files but we're ignoring that as that's inconvenient
        # See https://github.com/NixOS/nix/issues/9132#issuecomment-1754999829
        formatter = pkgs.writeShellApplication {
          name = "run-formatters";
          runtimeInputs = [ pkgs.rustfmt ];
          text = ''
            set -xeu
            rustfmt "$@"
          '';
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [
            (luajit_lua5_2.withPackages (ps: [
              ps.inspect
              ps.lpeg
              #(ps.callPackage lqc { })
              ps.luasocket
              ps.luaunit
              ps.tl
            ]))
            pkgs.stylua
            pkgs.inferno
            pkgs.lua-language-server
            pkgs.cargo-modules
            pkgs.cargo-nextest
            pkgs.cargo-expand
            (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
          ];

          # Set strict miri flags for memory safety validation
          MIRIFLAGS = "-Zmiri-strict-provenance -Zmiri-symbolic-alignment-check -Zmiri-tree-borrows";
        };
      }
    );
}
