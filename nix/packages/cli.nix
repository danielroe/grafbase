{ dream2nix, config, lib, self, ... }: {
  imports = [
    dream2nix.modules.dream2nix.rust-cargo-lock
    dream2nix.modules.dream2nix.rust-crane
  ];

  name = "grafbase";
  version = "0.41.2"; # TODO read the actual version.

  deps = { nixpkgs, ... }: {
    GRAFBASE_CLI_ASSETS_GZIP_PATH = nixpkgs.hello;
  };

  mkDerivation = {
    src = ../../.;
  };

  rust-cargo-lock.source = ../../.;
  rust-crane.runTests = false;
}
