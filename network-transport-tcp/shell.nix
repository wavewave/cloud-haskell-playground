{ pkgs ? (import <nixpkgs>{}) }:

with pkgs;

let 
    newhaskellPackages = haskellPackages; 
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
	      distributed-process
            ]);


in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv ];
     shellHook = ''
     '';
   }
