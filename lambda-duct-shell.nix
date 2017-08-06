{ pkgs ? import <nixpkgs> {} }:

with pkgs;

ocamlPackages_latest.callPackage ({ stdenv, ocaml, ocamlbuild, findlib, lwt2, yojson }:
  stdenv.mkDerivation rec {
    name = "lambda-duct";

    buildInputs = [ ocaml ocamlbuild findlib lwt2 yojson ];
}) { }
