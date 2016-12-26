{
  nodebill = {
    deployment.targetEnv = "container";
    deployment.container.host = "localhost";
    deployment.container.forwardPorts = [
      { hostPort = 16006; }
      { hostPort = 16007; }
    ];
  };
  nodemark = {
    deployment.targetEnv = "container";
    deployment.container.host = "mark.uphere.he";
    deployment.container.forwardPorts = [
      { hostPort = 17007; }
      { hostPort = 17008; }
    ];
  };

}