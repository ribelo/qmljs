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

        # quickjs-ng 0.14.0 frees a JSContext that it already linked into
        # rt->gc_obj_list when JS_NewContextRaw() cannot allocate class_proto,
        # leaving a dangling node for the next JS_RunGC()/JS_FreeRuntime().
        # Upstream fix quickjs-ng/quickjs@8e14a39 is not in any release yet, so
        # patch the pinned source and use this derivation everywhere below.
        quickjsNg = pkgs.quickjs-ng.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [
            ./nix/patches/quickjs-ng-newcontextraw-gc-unlink.patch
          ];
        });

        # quickjs-ng installs CMake metadata, but no pkg-config file.
        quickjsPkgConfig = pkgs.writeTextDir "lib/pkgconfig/quickjs.pc" ''
          Name: quickjs
          Description: QuickJS-ng JavaScript engine
          Version: ${quickjsNg.version}
          Libs: -L${pkgs.lib.getLib quickjsNg}/lib -lqjs
          Libs.private: -ldl -lm -lpthread
          Cflags: -I${pkgs.lib.getDev quickjsNg}/include -DUSING_QJS_SHARED
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

        # Validate the existing shared switch; never mutate it from this flake.
        oxcamlInit = pkgs.writeShellApplication {
          name = "qmljs-oxcaml-init";
          runtimeInputs = [ pkgs.opam ];
          text = ''
            set -euo pipefail
            export OPAMROOT="$HOME/.cache/opam"

            if [ ! -x "$OPAMROOT/5.4.0+ox/bin/ocamlc" ]; then
              echo "qmljs: shared OxCaml switch 5.4.0+ox is missing or incomplete in $OPAMROOT" >&2
              exit 1
            fi
            if [ "$(opam exec --switch=5.4.0+ox -- ocamlc -version)" != 5.4.0+ox ] ||
               [ "$(opam list --switch=5.4.0+ox --installed --short --columns=version ocaml)" != 5.4.0 ] ||
               [ "$(opam list --switch=5.4.0+ox --installed --short --columns=version ocaml-variants)" != 5.4.0+ox ]; then
              echo "qmljs: expected ocaml.5.4.0, ocaml-variants.5.4.0+ox and compiler 5.4.0+ox" >&2
              exit 1
            fi
            for package in dune base core async_kernel async base_quickcheck; do
              if [ "$(opam list --switch=5.4.0+ox --installed --short --columns=name "$package")" != "$package" ]; then
                echo "qmljs: missing package $package in shared switch 5.4.0+ox; no packages were installed" >&2
                exit 1
              fi
            done
          '';
        };
        oxcamlShell = pkgs.mkShell {
          packages = [
            oxcamlInit
            pkgs.opam
            pkgs.pkg-config
            quickjsNg.dev
          ];

          shellHook = ''
            export OPAMROOT="$HOME/.cache/opam"
            export PKG_CONFIG_PATH="${nativePkgConfigPath}:''${PKG_CONFIG_PATH:-}"
            qmljs-oxcaml-init || exit $?
            if ! qmljs_opam_env=$(opam env --switch=5.4.0+ox --set-switch); then
              echo "qmljs: cannot activate mandatory OxCaml switch 5.4.0+ox" >&2
              exit 1
            fi
            eval "$qmljs_opam_env"
            unset qmljs_opam_env
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
