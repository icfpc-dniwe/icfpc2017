{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages_ = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};
  lib = pkgs.haskell.lib;

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
      fgl = self.callPackage ./fgl.nix { };
      #fgl = self.callPackage ./fgl { };
    };
  };

  drv_ = haskellPackages.callPackage ./default.nix {};
  drv = lib.addBuildDepends drv_ (with pkgs.python3.pkgs; [ networkx numpy Theano Lasagne ]);

in

  if pkgs.lib.inNixShell then drv.env else drv
