EMACS ?= emacs
TESTS := $(sort $(shell find src -type f -name '*-test.el' -print))
# eas.el, the chart engine, is a separate package: a checkout of it beside
# this one by default, overridable (make test EAS=/path/to/eas.el).
EAS ?= ../../Personal/emacs/eas.el
SRC_DIRS := src src/core src/indicators src/renderers src/charts src/integrations
LOAD_PATHS := -L $(EAS)/src $(foreach dir,$(SRC_DIRS),-L $(dir))
SOURCES := $(shell find src -type f -name '*.el' ! -name '*-test.el' -print)

ERT = $(EMACS) -Q --batch $(LOAD_PATHS) -L test $(foreach t,$(1),-l $(t)) \
	--eval '(ert-run-tests-batch-and-exit (quote $(2)))'

.PHONY: test compile clean

test:
	$(call ERT,$(TESTS),t)

compile:
	$(EMACS) -Q --batch $(LOAD_PATHS) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)
	@find src -name '*.elc' -delete

clean:
	find src -name '*.elc' -delete
