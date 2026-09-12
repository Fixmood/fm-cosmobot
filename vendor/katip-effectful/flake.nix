{
  description = "Development flake of katip-effectful Haskell package";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.haskell-flake.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: 
        let haskellProjectConfig = {
          }; 
        in 
        {
        treefmt.programs = {
          cabal-fmt.enable = true;
          formolu = {
            enable = true;
            package = pkgs.haskellPackages.fourmolu;
          };
        };
        treefmt.projectRootFile = "flake.nix";
        pre-commit.settings = {
          hooks = {
            treefmt.enable = true;
            typos.enable = true;
          };
          settings.typos = {
            ignored-words = ["wheres"];
          };
        };
        haskellProjects.default = haskellProjectConfig // {
          basePackages = pkgs.haskell.packages.ghc910;
          defaults.devShell.tools = p: {inherit (p) cabal-install haskell-language-server;};
          otherOverlays = [
            (hself: hsuper: { katip = pkgs.haskell.lib.dontCheck hsuper.katip;})
         ];
        };
        haskellProjects.ghc912 = haskellProjectConfig // {
          basePackages = pkgs.haskell.packages.ghc912;
          devShell.hoogle = true;
          defaults.devShell.tools = p: {inherit (p) hpc cabal-install;};
        };
      };
      flake = {
      };
    };
}
