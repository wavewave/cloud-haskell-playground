{ stdenv, haskellPackages }:

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              cabal-install
	      network-transport-zeromq
	      distributed-process
            ]);


in stdenv.mkDerivation rec {
     name = "chatter-${version}";
     version = "0.0";
     src = ./.;
     buildInputs = [ hsenv ];
     buildPhase = ''
       ghc chatter-server-zmq.hs
       ghc chatter-client-zmq.hs
     '';
     installPhase = ''
       mkdir -p $out/bin
       cp chatter-server-zmq $out/bin
       cp chatter-client-zmq $out/bin
     '';

   }
