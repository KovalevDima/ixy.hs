{
  description = "A flake for ixy.hs development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = {self, flake-parts, nixpkgs, ...} @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {self', pkgs, config, lib, ...}:
      {
        haskellProjects.default = {
          autoWire = ["packages" "apps"];
          settings = {
            ixy = {
              libraryProfiling = true;
              executableProfiling = true;
            };
            unix-memory = { broken = false; check = false; };
          };
        };
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = with pkgs; [  ];
            packages = with pkgs; [ zlib ghc cabal ];
          };
        };
      };
    };
}
