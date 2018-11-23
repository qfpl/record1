{ mkDerivation, base, lens, record1, stdenv }:
mkDerivation {
  pname = "record1-lens";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens record1 ];
  license = stdenv.lib.licenses.bsd3;
}
