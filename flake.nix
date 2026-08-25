{
  description = "TBD";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/26.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    feedback.url = "github:NorfairKing/feedback";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    nix-filter,
    pre-commit-hooks,
    feedback,
    ...
  }: let
    pkgsFor = system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default];
      };
    filteredSrc = nix-filter.lib {
      root = ./.;
      include = ["src/" "test/" "test-resources/" "package.yaml" "LICENSE"];
    };
  in
    {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides =
            final.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (self: super: {
              lemon-squeezy = (self.callCabal2nix "lemon-squeezy" filteredSrc {})
                .overrideAttrs (
                old: {
                  checkPhase = ''
                    export LEMON_SQUEEZY_API_KEY=${builtins.getEnv "LEMON_SQUEEZY_API_KEY"}
                    ${old.checkPhase}
                  '';
                }
              );
            });
        });
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = pkgsFor system;
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ormolu.enable = true;
            statix.enable = true;
          };
        };
      in rec {
        packages.lemon-squeezy = pkgs.haskellPackages.lemon-squeezy;
        packages.default = packages.lemon-squeezy;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [packages.lemon-squeezy];
          buildInputs = with pkgs;
          with pkgs.haskellPackages; [
            actionlint
            alejandra
            cabal-install
            feedback.packages.${system}.default
            ghcid
            haskell-language-server
            hlint
            nil
            ormolu
            statix
          ];
          inherit (precommitCheck) shellHook;
        };

        checks = {pre-commit-check = precommitCheck;};
      }
    );
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}
