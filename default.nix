{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "stl-viewer" ./. { }
