{
  description = "WebSockets development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskell.compiler.ghc9124
            pkgs.zlib
            # pkgs.pkg-config
          ];

          shellHook = ''
            echo "WebSockets development environment loaded"
            echo "GHC version: $(ghc --version)"
          '';
        };
      }
    );
}
