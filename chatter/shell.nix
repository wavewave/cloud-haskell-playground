{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    newhaskellPackages = haskellPackages; # .override { overrides = hsconfig; };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
	      network-transport-zeromq
	      distributed-process
            ]);


in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv ];
     shellHook = ''
     '';
   }
