#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz -i bash -p nodePackages.bower2nix nodePackages.node2nix

bower2nix bower.json bower-generated.nix

node2nix -6 -c composition.nix -d
