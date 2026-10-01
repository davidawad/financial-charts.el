EMACS ?= emacs
TESTS := $(wildcard test/*-test.el)

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -L test $(foreach t,$(TESTS),-l $(t)) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile *.el
	@rm -f *.elc

clean:
	rm -f *.elc
