let config = {
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: rec {

        ldap-profiles = self.callPackage ./default.nix {};
        ldif = self.callPackage ./ldif.nix {};

      };
    };
  };
};

pkgs = import <nixpkgs> {inherit config;};

in {
  ldap-profiles = pkgs.haskellPackages.callPackage ./default.nix {};
}
