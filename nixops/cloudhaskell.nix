let

  hsconfig = self: super: {
    "network-transport-uphere" = self.callPackage /home/wavewave/repo/src/network-transport-uphere {};
  };
  hsenv = { pkgs }:
    let newHaskellPackages = pkgs.haskellPackages.override { overrides = hsconfig; };
    in newHaskellPackages.ghcWithPackages (p: with p;
         [ cabal-install network-transport-uphere ] );

  mach = 
    { config, pkgs, nodes, ... }:
    { networking.firewall.enable = false;
      environment.systemPackages = [
        pkgs.telnet (hsenv { inherit pkgs; })
      ];
    };

in

{
  nodebill = mach;
  nodemark = mach;
}

