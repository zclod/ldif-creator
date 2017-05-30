{ mkDerivation, base, bytestring, ldif, stdenv }:
mkDerivation {
  pname = "ldap-profiles";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring ldif ];
  license = stdenv.lib.licenses.bsd3;
}
