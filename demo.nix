let
  pkgs = import ./pkgs.nix;
  monitor =
    let
      entr = "${pkgs.entr}/bin/entr";
      comp = "${pkgs.hsPkgs.alloy.components.exes.alloy-exe}/bin/alloy-exe";
    in
    pkgs.writeShellScriptBin "monitor" ''
      ls *.ayy | ${entr} -c ${comp} -f syntax.ayy
    '';
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildPhase = "true";
  src = "./.";
  buildInputs = [ monitor ];
}
