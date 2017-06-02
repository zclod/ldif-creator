let config = {
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: rec {

        ldif-creator = self.callPackage ./default.nix {};
        ldif = self.callPackage ./ldif.nix {};

      };
    };
  };
};

pkgs = import <nixpkgs> {inherit config;};

in {
  ldif-creator = pkgs.haskellPackages.callPackage ./default.nix {};
}
