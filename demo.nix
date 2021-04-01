let
  pkgs = import ./pkgs.nix;
  alloy-demo =
    let
      entr = "${pkgs.entr}/bin/entr";
      comp = "${pkgs.hsPkgs.alloy.components.exes.alloy-exe}/bin/alloy-exe";
    in
    pkgs.writeShellScriptBin "alloy-demo" ''
      ls *.ay | ${entr} -c ${comp} -f main.ay
    '';
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildPhase = "true";
  src = "./.";
  buildInputs = [ alloy-demo ];
}
