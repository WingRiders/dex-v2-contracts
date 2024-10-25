
usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  shell               -- Start a nix develop shell"
	@echo "  emacs               -- Start an Emacs session"
	@echo "  build               -- Run cabal v2-build"
	@echo "  export              -- Export artifacts for deployment"
ifdef NIXOS
STACK_FLAGS = --no-nix --system-ghc
endif

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

.PHONY: shell
shell:
	nix develop --no-update-lock-file

.PHONY: emacs
emacs:
	nix develop -c emacs

.PHONY: build
build: requires_nix_shell
	cabal v2-build all $(GHC_FLAGS)

.PHONY: export
export: requires_nix_shell
	cabal run export

.PHONY: requires_nix_shell
# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'make shell' first" && false)
