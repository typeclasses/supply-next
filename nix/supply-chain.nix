{ mkDerivation, base, lib, supply-chain-core }:
mkDerivation {
  pname = "supply-chain";
  version = "0.0.0.1";
  sha256 = "4a7cc9d402ef6c8d8a5af51c15cbfead8be7638efd8699e70283668eff72f03c";
  libraryHaskellDepends = [ base supply-chain-core ];
  homepage = "https://github.com/typeclasses/supply-chain";
  description = "Composable request-response pipelines";
  license = lib.licenses.asl20;
}
