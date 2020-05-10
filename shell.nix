(import ./.).shellFor {
  packages = p: with p; [fae-server hbbft-cloud-haskell];
}
