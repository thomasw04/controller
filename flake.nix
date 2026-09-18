{
  description = "Controller";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { nixpkgs, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      pkgsFor = system: import nixpkgs { inherit system; };
      controllerFor = pkgs:
        pkgs.haskellPackages.callCabal2nix "controller" (pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [ ./controller.cabal ./app ];
        }) { };
    in
    {
      packages = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.haskell.lib.justStaticExecutables (controllerFor pkgs);
        });

      devShells = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.haskellPackages.shellFor {
            packages = _: [ (controllerFor pkgs) ];
            nativeBuildInputs = [ pkgs.cabal-install ];
          };
        });
    };
}
