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
  packages = {
    mmark = hackGet ./dep/mmark;
    modern-uri = hackGet ./dep/modern-uri;
    tutorial = ./tutorial;
  };
  overrides = self: super: {
    temporary = dontCheck super.temporary;
    email-validate = dontCheck super.email-validate;
    modern-uri = pkgs.haskell.lib.doJailbreak super.modern-uri;
    frontend = overrideCabal super.frontend (drv: {
      buildTools = (drv.buildTools or []) ++ [ self.buildHaskellPackages.markdown-unlit ];
    });
  };
})
