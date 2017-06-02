{ mkDerivation, aeson, base, bytestring, ldif, optparse-applicative
, stdenv, text
}:
mkDerivation {
  pname = "ldif-creator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring ldif optparse-applicative text
  ];
  license = stdenv.lib.licenses.bsd3;
}
