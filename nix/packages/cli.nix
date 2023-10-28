{ dream2nix, config, lib, cli-app, ... }:

let src = ../../.; in
{
  imports = [
    dream2nix.modules.dream2nix.rust-cargo-lock
    dream2nix.modules.dream2nix.rust-crane
  ];

  name = "grafbase";
  version =
    let cargoToml = lib.trivial.importTOML (src + "/Cargo.toml"); in
    cargoToml.workspace.package.version;

  mkDerivation = {
    inherit src;

    preBuild = ''
      mkdir -p assets/static/assets
      tar czf assets.tgz assets

      export GRAFBASE_CLI_ASSETS_GZIP_PATH=`pwd`/assets.tgz
      export GRAFBASE_CLI_PATHFINDER_BUNDLE_PATH=${cli-app}/lib/node_modules/cli-app/dist
    '';
  };

  rust-cargo-lock.source = src;
  rust-crane.runTests = false;
}
