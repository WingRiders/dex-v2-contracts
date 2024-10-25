{
  description = "WingRiders Plutarch core contracts";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0;96m\\][\\[\\e[0;96m\\]nix develop:\\[\\e[0;96m\\]\\w\\[\\e[0;96m\\]]\\[\\e[0;96m\\]$ \\[\\e[0m\\]";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    liqwid-libs.url = "github:WingRiders/liqwid-libs";
    plutarch.url = "github:Plutonomicon/plutarch";
    plutus-simple-model.url = "github:mlabs-haskell/plutus-simple-model?ref=f99398b317f2e9d35131178eadcec0fae11a7169";
    ply.url = "github:mlabs-haskell/ply?ref=master";
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
  };

  outputs = inputs@{ self, tooling, plutus-simple-model, plutarch, ply, liqwid-libs }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;
          project.shell.withHoogle = true;
          project.modules = [
            ({ config, ... }: {
              packages.plutus-simple-model.doHaddock = false;
            })
          ];
          project.extraHackage = [
            "${liqwid-libs}/liqwid-plutarch-extra"
            "${liqwid-libs}/plutarch-quickcheck"
            "${liqwid-libs}/plutarch-unit"
            "${liqwid-libs}/liqwid-script-export"
            "${liqwid-libs}/plutarch-context-builder"
            "${plutarch}"
            "${plutarch}/plutarch-extra"
            "${plutus-simple-model}/psm"
            "${plutus-simple-model}/cardano-simple"
            "${ply}/ply-core"
            "${ply}/ply-plutarch"
          ];
        })
      ];
    };
}
