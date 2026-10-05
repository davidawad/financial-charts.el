EMACS ?= emacs
TESTS := $(sort $(shell find src -type f -name '*-test.el' -print))
SRC_DIRS := src src/eas src/core src/indicators src/renderers src/charts src/integrations src/cli
LOAD_PATHS := $(foreach dir,$(SRC_DIRS),-L $(dir))
SOURCES := $(shell find src -type f -name '*.el' ! -name '*-test.el' -print)
# Set to a market-data.el checkout to run the suite with it loaded:
#   make test MARKET_DATA=../market-data.el
MARKET_DATA ?=
WITH_MD := $(if $(MARKET_DATA),-L $(MARKET_DATA) --eval "(require 'market-data)")

.PHONY: test compile clean bench bench-budget

test:
	$(EMACS) -Q --batch $(LOAD_PATHS) -L test/eas $(WITH_MD) $(foreach t,$(TESTS),-l $(t)) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch $(LOAD_PATHS) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)
	@find src -name '*.elc' -delete

# Performance ladder (fc-qx1.9), byte-compiled: fails when a stage
# regresses past src/eas/bench-budget.json.  bench-budget re-measures it.
bench:
	scripts/eas-bench.sh

bench-budget:
	scripts/eas-bench.sh --update

clean:
	find src -name '*.elc' -delete
