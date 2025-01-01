{pkgs,...}:
pkgs.haskellPackages.callCabal2nix "trie-match" ./. {}
