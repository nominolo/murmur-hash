.PHONY: default clean all print-dist test docs

default: all

-include config.mk

TOP        ?= .
HC         ?= ghc
HADDOCK    ?= haddock

DIST       ?= $(TOP)/dist

CABAL      ?= cabal

## Compiler Library

$(DIST)/setup-config: murmur-hash.cabal
	@echo ">>> ===== Configuring ======================"
	$(CABAL) configure --builddir=$(DIST) --user

$(DIST)/build//build/libHSmurmur-hash-0.1.a: $(DIST)/setup-config $(wildcard *.hs)
	@echo ">>> ===== Building ========================"
	$(CABAL) build --builddir=$(DIST)

docs: $(DIST)/build//build/libHSmurmur-hash-0.1.a
	$(CABAL) haddock --builddir=$(DIST)

all: $(DIST)/build//build/libHSmurmur-hash-0.1.a

clean:
	$(CABAL) clean --builddir=$(DIST)

print-dist:
	@echo $(DIST)