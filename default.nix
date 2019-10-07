{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }:
with pkgs.haskell.lib; {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  __closureCompilerOptimizationLevel = null;
  overrides = self: super: {
    temporary = dontCheck super.temporary;
    email-validate = dontCheck super.email-validate;
    mmark = dontHaddock (self.callCabal2nix "mmark" (hackGet ./dep/mmark) {});
  };
})
