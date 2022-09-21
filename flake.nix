{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);
      ldap = haskellPackages.callCabal2nix "LDAP" ./. { };
    in
    {
      packages = {
        inherit ldap;
        default = ldap;
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          (haskellPackages.ghcWithPackages (ps: haskellDeps ldap))
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hpack
        ];
      };
    });
}
