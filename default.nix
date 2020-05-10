let pkgs = import <nixpkgs> {
  overlays = [ (self: super: {
    haskellPackages = super.haskellPackages.override (oldArgs: with super.haskell.lib; {
      overrides = super.lib.composeExtensions (oldArgs.overrides or (_: _: {})) (hself: hsuper: with hself; {
        fae = callCabal2nix "fae" ./fae {};
        # fae-core = callCabal2nix "fae" ./fae-core {};
        fae-server = callCabal2nix "fae-server" ./fae-server {};
        fae-posttx = callCabal2nix "fae-posttx" ./fae-posttx {};
        dice-entropy-conduit = callCabal2nix "dice-entropy-conduit" ./dice-entropy-conduit {};
        secret-sharing = callCabal2nix "secret-sharing" ./secret-sharing {};
        hbbft = callCabal2nix "hbbft" ./hbbft {};
        hbbft-cli = callCabal2nix "hbbft-cli" ./hbbft-cli {};
        hbbft-cloud-haskell = callCabal2nix "hbbft-cloud-haskell" ./hbbft-cloud-haskell {};
        distributed-process = callCabal2nix "distributed-process" (pkgs.fetchFromGitHub {
          owner = "haskell-distributed";
          repo = "distributed-process";
          rev = "660d554f6acd2dba8b605c84e8fa69e45708bc14";
          sha256 = "0c71b3nc19zic9xiirkc41znv93f9j9qlf2kn89mjjyh9w7dazsn";
        }) {};
        distributed-static = callCabal2nix "distributed-static" (pkgs.fetchFromGitHub {
          owner = "haskell-distributed";
          repo = "distributed-static";
          rev = "d34b5139a0c349fbda6ffea5fbbe960c3c91aeb1";
          sha256 = "1fi6d1svz6l2b8psiyzby89cmcg75vgfsw2jwkhiakr243wrq6bf";
        }) {};
        rank1dynamic = callCabal2nix "rank1dynamic" (pkgs.fetchFromGitHub {
          owner = "haskell-distributed";
          repo = "rank1dynamic";
          rev = "af347302dcd079a21d3f3386b6090ee8d8cd0715";
          sha256 = "0kq2kljwwiw72925cqln9xvy3510fgwblgdamcjbj8a2rdp6swlc";
        }) {};
        network-transport-tcp = dontCheck (callCabal2nix "network-transport-tcp" (pkgs.fetchFromGitHub {
          owner = "haskell-distributed";
          repo = "network-transport-tcp";
          rev = "6fe2aabb390ce63a014d774e04f8a0a0e836ff3c";
          sha256 = "0vjsqlsj5ifxn4zndgq2znx9q47rbq2d4fyaghllvgvsw6pahk2w";
        }) {});
        network-transport-tests = callCabal2nix "network-transport-tests" (pkgs.fetchFromGitHub {
          owner = "haskell-distributed";
          repo = "network-transport-tests";
          rev = "db27516982d8b5c70d09696acb625d9d86f8e51b";
          sha256 = "1rkrq2ijgm8yksh52icjhxr85i2jnvyp45sdrlz9aa0c5bvw999a";
        }) {};
      });
    });
  })];
}; in pkgs.haskellPackages
