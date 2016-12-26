{
  nodebill = {
    deployment.targetEnv = "container";
    deployment.container.host = "localhost";
    deployment.container.forwardPorts = [
      { hostPort = 16006; }
      { hostPort = 16007; }
    ];
    deployment.container.bindMounts = {
      "/home/wavewave" = {
        hostPath = "/home/wavewave";
	isReadOnly = true;
      };
    };
    
  };
  nodemark = {
    deployment.targetEnv = "container";
    deployment.container.host = "mark.uphere.he";
    deployment.container.forwardPorts = [
      { hostPort = 17007; }
      { hostPort = 17008; }
    ];
    deployment.container.bindMounts = {
      "/home/wavewave" = {
        hostPath = "/home/wavewave";
	isReadOnly = true;
      };
    };
    
  };

}