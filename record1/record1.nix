{ mkDerivation, base, fixplate, stdenv, vector }:
mkDerivation {
  pname = "record1";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base fixplate vector ];
  testHaskellDepends = [ base vector ];
  license = stdenv.lib.licenses.bsd3;
}
