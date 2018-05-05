{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "network-transport-tcp" =
         haskell.lib.overrideCabal
           (haskell.lib.addBuildDepend  super.network-transport-tcp (self.uuid))
           (drv: {
              src = fetchgit {
                url = "git://github.com/haskell-distributed/network-transport-tcp";
                rev = "6a67f49717c2269e4b17995155c7a908c6f363ee";
                sha256 = "0919j3rrwiq4l9hk0p8a346jglshg14dl2ffrp5r486xwvl6ijs6";
              };
            });
    };
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    distributed-process
    network-transport-zeromq
    network-transport-tcp
  ]);

in

stdenv.mkDerivation {
  name = "cloud-haskell-env";
  buildInputs = [
    hsenv
  ];

}