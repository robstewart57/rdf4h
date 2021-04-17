# Nixpkgs from 2021/05/11
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/72d6fa9bf20d7e6b8c6538c09e4608916b3462c2.tar.gz") {} }:

let
  inherit (pkgs) haskellPackages;
in
pkgs.mkShell {
  buildInputs = [
    haskellPackages.cabal-install
    haskellPackages.ghc
    haskellPackages.stack
    pkgs.zlib
  ];
}
