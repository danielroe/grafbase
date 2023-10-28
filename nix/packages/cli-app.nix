{ dream2nix, config, lib, ... }: {
  imports = [
    dream2nix.modules.dream2nix.nodejs-package-lock-v3
    dream2nix.modules.dream2nix.nodejs-granular-v3
  ];

  name = "cli-app";
  version = "0.0.1";

  nodejs-package-lock-v3.packageLockFile = "${config.mkDerivation.src}/package-lock.json";

  mkDerivation = {
    src = ../../packages/cli-app;
    buildPhase = "npm run cli-app:build";
  };
}
