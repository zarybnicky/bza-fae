# Fae, HBBFT

Původně navrhovaný záměr mého projektu byla implementace práce "Cumulus: A
BFT-based Sidechain Protocol for Off-chain Scaling" v jazyce Haskell. Jako
výchozí bod jsem hledal úplnou ale nepříliš rozsáhlou implementaci blockchainu,
který bych mohl použít jako mainchain.

## Fae

Jediný takový projekt, o kterém jsem věděl již dříve, Cardano se všemi svými
podprojekty, se mi zdál neprakticky velký, co do počtu komponent i řádků kódu
(Cardano, Ouroboros, Plutus, Jormungandr) - ač jsem podprojekt `cardano-ops`již
dříve použil jako inspiraci a základ jednoho DevOps systému.

Z menších a relativně úplných projektů jsem našel `TGOlson/blockchain`,
`MichaelBurge/haskoin`, `haskoin/haskoin-core`, `wangwangwar/nanochain`,
`aviaviavi/legion`, `sealchain-project/sealchain`, `ConsenSys/Fae`. Z těchto mě
nejvíce zaujal `Fae` (prý akronym "Functional alternative to Ethereum"), který
sliboval v méně než 10.000 řádcích kódu úplný blockchain systém včetně "líně
vyhodnocovaných" smart contracts psaných přímo v Haskellu a možného napojení na
testnet Etherea.

Zjistil jsem ale, že to je cca dva roky starý projekt, který s aktuální verzí
knihoven a překladače GHC příliš dobře nefunguje, konkrétně dynamická
interpretace smart contracts, která je základ celého systému. Strávil jsem až
příliš času úklidem kódu, refaktorováním, pokusy o zprovoznění. Výsledek mého
snažení je ve složkách `fae`, `fae-server`, `fae-posttx`. Zvažuji, jestli má
vůbec smysl pokoušet se zpět přispět "pull requestem" k projektu - autoři na
projekt už dlouho nesáhli, jejich zajímavý a možná užitečný nápad
("lazily-evaluated smart contracts") se zřejmě v praxi neosvědčil.

Fae se zprovoznit podařilo - pro spuštění je zapotřebí Nix a následující
příkazy. Varování je při překladu stále až příliš, ale na víc mi už nezbyl čas.

```
# V adresáři, tj. tam, kde je shell.nix
nix-shell
cabal build faeServer postTX
$(find . -type f -name faeServer)
# $(find . -type f -name faeServer) --help

cd fae-tx
$(find .. -type f -name postTX) HelloWorldTX
```

## HBBFT

Druhým krokem bylo stavění sidechainu. Po neúměrném množství času stráveném na
Fae jsem musel slevit z původních požadavků a namísto implementace protokolu
Cumulus "na zelené louce" jsem vyšel z jedné nedokončené implementace protokolu
HoneyBadger a pokusil se ji dokončit, uklidit a napojit na síť. Tu se podařilo
dokončit, vyčistit, upgradovat tak, aby pracovala s poslední verzí knihoven i
GHC a napojil na implementaci (m, n)-secret sharing systému (který jsem ale také
musel přiložit, změn tam taky belo potřeba nemálo) a networking jsem přidal
pomocí jednoho staršího projektu CloudHaskell, který jsem už dlouho někde chtěl
použít (v podstatě RPC protokol, podobné systému, co používá Erlang).

```
nix-shell
cabal run hbbft-cloud 127.0.0.1 3001 127.0.0.1:3000
cabal run hbbft-cloud 127.0.0.1 3000 127.0.0.1:3001
```

Musím ale přiznat, že jsem přecenil svoje schopnosti - a podcenil nečekaně
časově náročná překvapení při implementaci - a že poslední krok systému, smart
contract, který by tyto dva systémy propojil, chybí.
