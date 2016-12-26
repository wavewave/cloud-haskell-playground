{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let 
  hsconfig = import /home/wavewave/repo/srcp/nlp-prototype/nix/haskell-modules/configuration-ghc-8.0.x.nix { inherit pkgs; };
  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            distributed-process
	    network-transport-uphere
          ]);
in

stdenv.mkDerivation {
  name = "playground";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
