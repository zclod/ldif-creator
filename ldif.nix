{ mkDerivation, base, bytestring, cmdargs, containers, directory
, filepath, HUnit, parsec, rosezipper, stdenv
}:
mkDerivation {
  pname = "ldif";
  version = "0.0.13";
  sha256 = "0mf0cvjzgimrqf80g6hg75azv18930g5mrvjjxw70116mdpp718a";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cmdargs containers directory filepath parsec
    rosezipper
  ];
  executableHaskellDepends = [
    base bytestring cmdargs containers filepath
  ];
  testHaskellDepends = [ base HUnit ];
  doCheck = false;
  homepage = "http://rampa.sk/static/ldif.html";
  description = "The LDAP Data Interchange Format (LDIF) tools";
  license = stdenv.lib.licenses.bsd3;
}
