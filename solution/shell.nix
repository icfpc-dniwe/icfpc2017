{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages_ = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
      fgl = self.callPackage ./fgl.nix { };
    };
  };

  drv = haskellPackages.callPackage ./default.nix {};

in

  if pkgs.lib.inNixShell then drv.env else drv
