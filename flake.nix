{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        nativePkgConfigPath = pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" [ ];

        oxcamlInit = pkgs.writeShellApplication {
          name = "qmljs-oxcaml-init";
          runtimeInputs = [
            pkgs.autoconf
            pkgs.git
            pkgs.gnumake
            pkgs.m4
            pkgs.opam
            pkgs.patch
            pkgs.pkg-config
            pkgs.python3
            pkgs.quickjs-ng.dev
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.gmp
            pkgs.libev
            pkgs.libffi
            pkgs.openssl
            pkgs.zlib
          ];
          text = ''
            set -euo pipefail

            export OPAMROOT="''${OPAMROOT:-$HOME/.cache/opam}"
            export PKG_CONFIG_PATH="${nativePkgConfigPath}:''${PKG_CONFIG_PATH:-}"

            if [ ! -d "$OPAMROOT" ]; then
              opam init --bare --disable-sandboxing --no-setup --yes
            fi

            if ! opam switch list --short | grep -qx '5.2.0+ox'; then
              opam switch create 5.2.0+ox \
                --repos ox=git+https://github.com/oxcaml/opam-repository.git,default \
                --assume-depexts --yes
            fi

            opam switch set 5.2.0+ox
            opam install . --deps-only --with-test --assume-depexts --yes
          '';
        };
      in
      {
        packages.default = oxcamlInit;

        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.dune_3
            pkgs.ocaml
            pkgs.opam
            pkgs.pkg-config
            pkgs.quickjs-ng.dev
          ];
        };

        devShells.oxcaml = pkgs.mkShell {
          packages = [
            oxcamlInit
            pkgs.opam
            pkgs.pkg-config
            pkgs.quickjs-ng.dev
          ];

          shellHook = ''
            export OPAMROOT="''${OPAMROOT:-$HOME/.cache/opam}"
            export PKG_CONFIG_PATH="${nativePkgConfigPath}:''${PKG_CONFIG_PATH:-}"

            if [ -d "$OPAMROOT/5.2.0+ox" ]; then
              eval "$(opam env --switch=5.2.0+ox --set-switch)"
            fi
          '';
        };
      }
    );
}
