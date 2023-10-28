{ dream2nix, config, lib, ... }: {
  imports = [
    dream2nix.modules.dream2nix.rust-cargo-lock
    dream2nix.modules.dream2nix.rust-crane
  ];

  name = "grafbase";
  version = "0.41.2"; # TODO read the actual version.

  deps = { nixpkgs, ... }: {
    inherit (nixpkgs) hello;
  };

  mkDerivation = {
    src = ../../.;
    preBuild = ''
      mkdir -p assets/assets
      mkdir -p assets/static/assets
      tar czf assets.tgz assets

      mkdir -p pathfinder_bundle/assets

      export GRAFBASE_CLI_ASSETS_GZIP_PATH=`pwd`/assets.tgz
      export GRAFBASE_CLI_PATHFINDER_BUNDLE_PATH=`pwd`/pathfinder_bundle
    '';
  };

  rust-cargo-lock.source = ../../.;
  rust-crane.runTests = false;
}
