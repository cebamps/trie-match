{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = {nixpkgs, ...}: let
    inherit (nixpkgs) lib;
    systems = ["aarch64-darwin" "x86_64-linux"];
    forEachSystem = f: lib.genAttrs systems (system: f {pkgs = nixpkgs.legacyPackages.${system};});
  in {
    packages = forEachSystem ({pkgs}: {
      default = pkgs.haskellPackages.callPackage ./default.nix {};
    });
    devShells = forEachSystem ({pkgs}: let
      haskellPackages = pkgs.haskellPackages;
    in {
      default = haskellPackages.shellFor {
        packages = hp: [
          (hp.callPackage ./default.nix {})
        ];
        nativeBuildInputs = with haskellPackages; [
          cabal-install
          ghcid
          haskell-language-server
        ];
      };
      docs = pkgs.mkShell {
        meta.description = "Tools needed to build the diagrams in /doc.";
        packages = [pkgs.m4 pkgs.graphviz-nox];
      };
    });
  };
}
