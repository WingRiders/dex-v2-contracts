# Cardano DEX

### Nix setup

#### WSL2 (Windows only)

- Install WSL2, using command `wsl.exe --install` inside an elevated CMD/Powershell [(Source)](https://docs.microsoft.com/en-us/windows/wsl/install-win10#simplified-installation-for-windows-insiders)
- After restarting, Windows should open a Shell window and start downloading Ubuntu, wait until it's done

#### NIX

- Navigate to your user folder in the Ubuntu installation and perform a single-user installation using `curl -L https://nixos.org/nix/install | sh -s -- --no-daemon` [(Source)](https://nixos.org/manual/nix/stable/#sect-single-user-installation)

#### Cachix binary caches

- Install the cachix client from nix shell
  ```bash
  nix-env -iA cachix -f https://cachix.org/api/v1/install
  ```
- Add IOHK caches
  ```
  cachix use iohk
  ```

### Build

**DON'T** use the recommended settings when entering the _nix shell_ for the first time when asked.

```bash
make shell
make build # Run build
make export # Run export
```

## Contracts

Contracts are defined in `src/`. Before they can be added to transactions, they have to be transpiled into a plutus intermediate representation.

### Traces

By default all contracts are stripped of the error messages, if you want them in, please set the `CONTRACTS_TRACING` env variable to true.
