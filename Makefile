EMACS ?= emacs
TESTS := $(wildcard test/*-test.el)
# Set to a market-data.el checkout to run the suite with it loaded:
#   make test MARKET_DATA=../market-data.el
MARKET_DATA ?=
WITH_MD := $(if $(MARKET_DATA),-L $(MARKET_DATA) --eval "(require 'market-data)")

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -L test $(WITH_MD) $(foreach t,$(TESTS),-l $(t)) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
	@rm -f *.elc

clean:
	rm -f *.elc
