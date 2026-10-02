EMACS ?= emacs
TESTS := $(wildcard test/*-test.el)
SRC_DIRS := src src/core src/indicators src/renderers src/charts src/integrations src/cli
LOAD_PATHS := $(foreach dir,$(SRC_DIRS),-L $(dir))
SOURCES := $(shell find src -type f -name '*.el' -print)
# Set to a market-data.el checkout to run the suite with it loaded:
#   make test MARKET_DATA=../market-data.el
MARKET_DATA ?=
WITH_MD := $(if $(MARKET_DATA),-L $(MARKET_DATA) --eval "(require 'market-data)")

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch $(LOAD_PATHS) -L test $(WITH_MD) $(foreach t,$(TESTS),-l $(t)) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch $(LOAD_PATHS) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)
	@find src -name '*.elc' -delete

clean:
	find src -name '*.elc' -delete
