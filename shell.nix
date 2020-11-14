{ pkgs ? import <nixpkgs> {} }:
(import ./. { inherit pkgs; }).env.overrideAttrs(o: rec {
  buildInputs = o.buildInputs or [] ++ [ pkgs.haskellPackages.cabal-install ];
})
