{
  description = "alloy";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs =
            self.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8105";
              shell = {
                tools = {
                  cabal = { };
                  ghcid = { };
                  haskell-language-server = { };
                  hlint = { };
                  ormolu = { };
                };
              };
            };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            overlay
          ];
        };
        flake = pkgs.hsPkgs.flake { };
        alloyc = flake.packages."alloy:exe:alloy-exe";
      in
      flake // {
        defaultPackage = alloyc;
        packages.watcher = pkgs.writeShellScriptBin "alloy-watch" ''
          echo $1 | entr -c ${alloyc}/bin/alloy-exe -f $1
        '';
      }
    );
}
