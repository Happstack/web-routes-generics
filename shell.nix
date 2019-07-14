with (import <nixpkgs> {});
  haskellPackages.developPackage { root = ./.; modifier = drv: haskell.lib.addBuildDepend drv cabal-install; }