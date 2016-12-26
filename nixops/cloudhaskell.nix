let

  hsconfig = self: super: {
    "network-transport-uphere" = self.callPackage /home/wavewave/repo/src/network-transport-uphere {};
  };
  hsenv = { pkgs }:
    let newHaskellPackages = pkgs.haskellPackages.override { overrides = hsconfig; };
    in newHaskellPackages.ghcWithPackages (p: with p;
         [ cabal-install network-transport-uphere ] );

  mach =
    { hostg, portg, hostl, portl }:
    { config, pkgs, nodes, ... }:
    { networking.firewall.enable = false;
      environment.systemPackages = [
        pkgs.telnet (hsenv { inherit pkgs; })
      ];
      environment.variables = {
        HOSTG = "${hostg}";
	PORTG = "${portg}";
	HOSTL = "${hostl}";
	PORTL = "${portl}";
	MASTER = "pid://192.168.1.102:16006:10.233.2.2:16006:0:10";
      };
     
      users.extraUsers.wavewave = {
        isNormalUser = true;
        uid = 1000;
        home = "/home/wavewave";
        description = "Ian-Woo Kim";
        extraGroups = [ "wheel" "uphere" ];
        openssh.authorizedKeys.keyFiles = [ "/home/wavewave/.ssh/id_rsa.pub" ];
      };
    };

in

{
  nodebill = mach { hostg = "192.168.1.102";
                    portg = "16006";
		    hostl = "10.233.2.2";
		    portl = "16006";
                  };
  nodemark = mach { hostg = "192.168.1.106";
                    portg = "17007";
		    hostl = "10.233.1.2";
		    portl = "17007";
		  };
}

