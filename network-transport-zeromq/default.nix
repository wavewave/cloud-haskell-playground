{ stdenv, haskellPackages }:

let 
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              network-transport-zeromq
	      distributed-process
            ]);


in stdenv.mkDerivation rec {
     name = "zmqtest-${version}";
     version = "0.0";
     src = ./.;
     buildInputs = [ hsenv ];
     buildPhase = ''
       ghc server.hs
       ghc client.hs
     '';
     installPhase = ''
       mkdir -p $out/bin
       cp server $out/bin
       cp client $out/bin
     '';

   }
