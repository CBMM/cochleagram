{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib   = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/reflex-dom-contrib) {});
  };
}
