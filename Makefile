.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help   -- show this help"
	@echo "  shell  -- nix develop"
	@echo "  build  -- build the project"
	@echo "  clean  -- clean up"

.PHONY: shell
shell:
	nix develop

.PHONY: build
build:
	cabal build all

.PHONY: clean
clean:
	cabal clean
