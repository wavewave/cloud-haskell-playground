{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let chatter = import ./chatter { stdenv = pkgs.stdenv; haskellPackages = pkgs.haskellPackages; };
    tcptest = import ./network-transport-tcp { stdenv = pkgs.stdenv; haskellPackages = pkgs.haskellPackages; };
    zmqtest = import ./network-transport-zeromq { stdenv = pkgs.stdenv; haskellPackages = pkgs.haskellPackages; };
    

in
{ chatter = chatter;
  tcptest = tcptest;
  zmqtest = zmqtest;
}
