{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = {nixpkgs, ...}: let
    inherit (nixpkgs) lib;
    systems = ["aarch64-darwin" "x86_64-linux"];
    forEachSystem = f:
      lib.genAttrs systems (system:
        f {
          pkgs = import nixpkgs {
            inherit system;
            overlays = [overlay];
          };
        });
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = hfinal: hprev: {
          trie-match = hfinal.callCabal2nix "trie-match" ./. {};
        };
      };
      trie-match = final.haskell.lib.justStaticExecutables final.haskellPackages.trie-match;
    };
  in {
    packages = forEachSystem ({pkgs}: {
      default = pkgs.trie-match;
    });
    devShells = forEachSystem ({pkgs}: let
      haskellPackages = pkgs.haskellPackages;
    in {
      default = haskellPackages.shellFor {
        packages = hp: [
          hp.trie-match
        ];
        nativeBuildInputs = with haskellPackages; [
          cabal-install
          ghcid
          haskell-language-server
          cabal-fmt
          cabal-plan-bounds
          # used to test bash completions in direnv; see
          # https://github.com/nix-community/nix-direnv/issues/384
          # https://discourse.nixos.org/t/interactive-bash-with-nix-develop-flake/15486
          pkgs.bashInteractive
        ];
      };
      docs = pkgs.mkShell {
        meta.description = "Tools needed to build the diagrams in /doc.";
        packages = [pkgs.m4 pkgs.graphviz-nox];
      };
      logo = pkgs.mkShell {
        meta.description = "Tools needed to run the logo.hs Cabal script and its output.";
        packages = with haskellPackages; [
          (ghcWithPackages (p: with p; [random mtl]))
          cabal-install
          haskell-language-server
          pkgs.graphviz-nox
        ];
      };
    });
  };
}
