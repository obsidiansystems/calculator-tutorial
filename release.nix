{ }:
let
  local-self = import ./. {};
in {
  ios = (import ./. {
    obelisk = import ./.obelisk/impl {
      system = "x86_64-darwin";
    };
  }).ios.frontend;
  android = (import ./. {
    obelisk = import ./.obelisk/impl {
      config.android_sdk.accept_license = true;
    };
  }).android.frontend;
  exe = local-self.exe;
  server = local-self.server;
  linuxExe = local-self.linuxExe;
}
