{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let this = import ./. { stdenv = pkgs.stdenv; haskellPackages = pkgs.haskellPackages; };

in
{ chatter = this; }