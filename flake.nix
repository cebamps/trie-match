{
  inputs.nixpkgs.url = "nixpkgs";

  outputs = {nixpkgs, ...}: let
    inherit (nixpkgs) lib;
    systems = ["aarch64-darwin" "x86_64-linux"];
    forEachSystem = f: lib.genAttrs systems (system: f {pkgs = nixpkgs.legacyPackages.${system};});
  in {
    devShells = forEachSystem ({pkgs}: let
      haskellPackages = pkgs.haskellPackages;
    in {
      default = haskellPackages.shellFor {
        packages = hp: [
          (hp.callPackage ./default.nix {})
        ];
        nativeBuildInputs = with haskellPackages; [
          cabal-install
          fourmolu
          ghcid
          haskell-language-server
        ];
      };
    });
  };
}
