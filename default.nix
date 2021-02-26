let
  nixpkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/9816b99e71c3504b0b4c1f8b2e004148460029d4.tar.gz")
    { };
  name = "podman";
  hsPkgs = nixpkgs.haskellPackages.extend (self: super: {
    linux-capabilities =
      self.callCabal2nix "linux-capabilities" ../linux-capabilities-haskell { };
  });
  drv = hsPkgs.callCabal2nix name ./. { };
  shellDrv = hsPkgs.shellFor {
    withHoogle = false;
    packages = p: [
      drv
      (hsPkgs.callCabal2nix "podman-codegen" ./podman-codegen { })
      (hsPkgs.callCabal2nix "podman-quickcheck" ./podman-quickcheck { })
    ];
    buildInputs = with hsPkgs; [ hlint cabal-install haskell-language-server ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv
