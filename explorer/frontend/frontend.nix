let
  localLib = import ../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, gitrev ? localLib.commitIdFromGitRepo ../../.git
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

with pkgs.lib;

let
  bowerComponents = pkgs.buildBowerComponents {
    name = "cardano-sl-explorer-frontend-deps";
    generated = ./bower-generated.nix;
    src = builtins.filterSource (name: path: baseNameOf (toString name) == "bower.json") ./.;
  };

  nodePackages = import ./composition.nix {
    inherit pkgs system;
  };

  cardanoPackages = import ../../default.nix {
    inherit system config gitrev pkgs;
  };
  cardanoPackages' = cardanoPackages.override {
    overrides = self: super: {
      cardano-sl-networking = pkgs.haskell.lib.dontCheck super.cardano-sl-networking;
    };
  };

  # p-d-l does not build with our main version of nixpkgs.
  # Needs to use something off 17.03 branch.
  oldHaskellPackages = (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/cb90e6a0361554d01b7a576af6c6fae4c28d7513.tar.gz) {}).pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      purescript-derive-lenses = oldHaskellPackages.callPackage ./purescript-derive-lenses.nix {};
    };
  };

  frontendBuildInputs = [
    pkgs.which
    pkgs.purescript
    oldHaskellPackages.purescript-derive-lenses
    (pkgs.haskell.lib.dontCheck cardanoPackages.cardano-sl-explorer-static)
    # pkgs.git  # required for git-revision-webpack-plugin which doesn't work anyway
  ];

in
  nodePackages // {
    inherit bowerComponents;

    package = nodePackages.package.override (oldAttrs: {
      dontNpmInstall = true; # handled by nix
      postInstall = ''
        # purescript code generation
        cardano-explorer-hs2purs --bridge-path src/Generated/
        ./scripts/generate-explorer-lenses.sh

        # frontend dependencies
        ln -s ${bowerComponents}/bower_components .

        # webpack build
        patchShebangs node_modules
        mkdir dist
        NODE_ENV=production COMMIT_HASH=${gitrev} ./node_modules/.bin/webpack --config webpack.config.babel.js || true
      '';
      buildInputs = oldAttrs.buildInputs ++ frontendBuildInputs;
    });
    shell = null;
    # # fixme: this override doesn't work
    # shell = nodePackages.shell.override (oldAttrs: {
    #   dontNpmInstall = true;
    #   buildInputs = oldAttrs.buildInputs ++ frontendBuildInputs;
    # });
  }
