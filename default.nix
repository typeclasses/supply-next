let

sources = import ./nix/sources.nix;
nixos-22-11 = import sources."nixos-22.11" {};
nixos-unstable = import sources."nixos-unstable" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    supply-next = ./supply-next;
};

depOverrides = new: old: {
    integer-types = new.callPackage ./nix/integer-types.nix {};
    quaalude = new.callPackage ./nix/quaalude.nix {};
    supply-chain = new.callPackage ./nix/supply-chain.nix {};
    supply-chain-core = new.callPackage ./nix/supply-chain-core.nix {};
};

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        depOverrides
        (new: old: {
            gambler = new.callPackage ./nix/gambler-0.1.0.0.nix {};
        })
    ];
});

ghc."9.4" = nixos-unstable.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        depOverrides
        (new: old: {
            gambler = new.callPackage ./nix/gambler-0.3.0.0.nix {};
        })
    ];
});

in

symlinkJoin {
    name = "supply-next";
    paths = concatMap (x: [x.supply-next]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
