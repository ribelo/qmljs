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

        # quickjs-ng installs CMake metadata, but no pkg-config file.
        quickjsPkgConfig = pkgs.writeTextDir "lib/pkgconfig/quickjs.pc" ''
          Name: quickjs
          Description: QuickJS-ng JavaScript engine
          Version: ${pkgs.quickjs-ng.version}
          Libs: -L${pkgs.lib.getLib pkgs.quickjs-ng}/lib -lqjs
          Libs.private: -ldl -lm -lpthread
          Cflags: -I${pkgs.lib.getDev pkgs.quickjs-ng}/include -DUSING_QJS_SHARED
        '';
        nativePkgConfigPath = pkgs.lib.makeSearchPath "lib/pkgconfig" (
          [ quickjsPkgConfig ]
          ++ map pkgs.lib.getDev (pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.gmp
            pkgs.libev
            pkgs.libffi
            pkgs.openssl
            pkgs.zlib
          ])
        );

        oxcamlInit = pkgs.writeShellApplication {
          name = "qmljs-oxcaml-init";
          runtimeInputs = [
            pkgs.autoconf
            pkgs.stdenv.cc
            pkgs.which
            pkgs.bzip2
            pkgs.gnutar
            pkgs.git
            pkgs.gnumake
            pkgs.m4
            pkgs.opam
            pkgs.patch
            pkgs.pkg-config
            pkgs.python3
            pkgs.quickjs-ng.dev
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.bubblewrap
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

            if [ ! -f "$OPAMROOT/config" ]; then
              opam init --bare --no-setup --yes
            fi

            # Use a separate repository name: an inherited ox repository may
            # contain only newer compilers, and must not be changed globally.
            opam repository add qmljs-ox \
              git+https://github.com/oxcaml/opam-repository.git#f1bd228dda31430bf6271f0f9adb2e604c6957ca \
              --dont-select --yes
            if ! opam switch list --short | grep -qx '5.2.0+ox'; then
              opam switch create 5.2.0+ox \
                --packages=ocaml.5.2.0,ocaml-variants.5.2.0+ox,oxcaml-compiler.5.2.0minus40 \
                --repos=qmljs-ox,default --no-switch --assume-depexts --yes
            fi

            if [ "$(opam list --switch=5.2.0+ox --installed --short --columns=version ocaml)" != 5.2.0 ] ||
               [ "$(opam list --switch=5.2.0+ox --installed --short --columns=version ocaml-variants)" != 5.2.0+ox ]; then
              echo "qmljs: switch 5.2.0+ox must contain ocaml.5.2.0 and ocaml-variants.5.2.0+ox" >&2
              exit 1
            fi
            opam install . --switch=5.2.0+ox --deps-only --with-test --assume-depexts --yes
          '';
        };
        oxcamlShell = pkgs.mkShell {
          packages = [
            oxcamlInit
            pkgs.opam
            pkgs.pkg-config
            pkgs.quickjs-ng.dev
          ];

          shellHook = ''
            export OPAMROOT="''${OPAMROOT:-$HOME/.cache/opam}"
            export PKG_CONFIG_PATH="${nativePkgConfigPath}:''${PKG_CONFIG_PATH:-}"

            if [ ! -x "$OPAMROOT/5.2.0+ox/bin/ocamlc" ]; then
              echo "qmljs: mandatory OxCaml switch 5.2.0+ox is missing or incomplete in $OPAMROOT" >&2
              echo "Run 'nix run .' from the project root to install the pinned toolchain and test dependencies." >&2
              exit 1
            fi
            if ! qmljs_opam_env=$(opam env --switch=5.2.0+ox --set-switch); then
              echo "qmljs: cannot activate mandatory OxCaml switch 5.2.0+ox" >&2
              exit 1
            fi
            eval "$qmljs_opam_env"
            unset qmljs_opam_env
            if [ "$(ocamlc -version)" != 5.2.0+ox ] ||
               [ "$(opam list --installed --short --columns=version ocaml)" != 5.2.0 ] ||
               [ "$(opam list --installed --short --columns=version ocaml-variants)" != 5.2.0+ox ]; then
              echo "qmljs: expected ocaml.5.2.0, ocaml-variants.5.2.0+ox and compiler 5.2.0+ox; refusing an inherited compiler" >&2
              exit 1
            fi
          '';
        };

      in
      {
        packages.default = oxcamlInit;
        checks.quickjs-pkg-config = pkgs.runCommand "qmljs-quickjs-pkg-config-check" {
          nativeBuildInputs = [ pkgs.stdenv.cc pkgs.pkg-config ];
          PKG_CONFIG_PATH = nativePkgConfigPath;
        } ''
          pkg-config --exists quickjs
          # Test the metadata against the actual engine, not just its text.
          printf '%s\n' \
            '#include <quickjs.h>' \
            'int main(void) {' \
            '  JSRuntime *rt = JS_NewRuntime();' \
            '  if (!rt) return 1;' \
            '  JS_FreeRuntime(rt);' \
            '  return 0;' \
            '}' > probe.c
          $CC probe.c $(pkg-config --cflags --libs quickjs) -o probe
          ./probe
          touch "$out"
        '';
        devShells.default = oxcamlShell;
        devShells.oxcaml = oxcamlShell;
      }
    );
}
