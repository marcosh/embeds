{
  description = "embeds";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = {self, nixpkgs, flake-utils, nix-filter}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        packageName = "embeds";

        # folders to look at to know when recompiling
        src = nix-filter.lib {
          root = ./.;
          include =
            [
              "spec"
              "src"
              "package.yaml"
              "cabal.project"
            ];
        };
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName src rec {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
            fourmolu
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      }
    );
}
