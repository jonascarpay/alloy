let
  pkgs = import ./pkgs.nix;
  alloy-demo =
    let
      entr = "${pkgs.entr}/bin/entr";
      comp = "${pkgs.hsPkgs.alloy.components.exes.alloy-exe}/bin/alloy-exe";
    in
    pkgs.writeShellScriptBin "alloy-demo" ''
      ls *.ayy | ${entr} -c ${comp} -f syntax.ayy
    '';
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildPhase = "true";
  src = "./.";
  buildInputs = [ alloy-demo ];
}
