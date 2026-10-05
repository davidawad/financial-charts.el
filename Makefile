EMACS ?= emacs
TESTS := $(sort $(shell find src -type f -name '*-test.el' -print))
SRC_DIRS := src src/eas src/core src/indicators src/renderers src/charts src/integrations src/cli
LOAD_PATHS := $(foreach dir,$(SRC_DIRS),-L $(dir))
SOURCES := $(shell find src -type f -name '*.el' ! -name '*-test.el' -print)
# Set to a market-data.el checkout to run the suite with it loaded:
#   make test MARKET_DATA=../market-data.el
MARKET_DATA ?=
WITH_MD := $(if $(MARKET_DATA),-L $(MARKET_DATA) --eval "(require 'market-data)")

# Tests tagged :gallery (the official Vega-Lite gallery and the
# conformance oracle, minutes of work) run in test-gallery, not test.
# One Emacs per gallery group, so `make -j4 test-gallery' parallelizes;
# test-gallery-GROUP runs just that group.
GALLERY_GROUPS := $(sort $(patsubst test/vl-examples/%/status.json,%,$(wildcard test/vl-examples/*/status.json)))
GALLERY_TARGETS := test-gallery-conformance $(addprefix test-gallery-,$(GALLERY_GROUPS))
ERT = $(EMACS) -Q --batch $(LOAD_PATHS) -L test/eas $(WITH_MD) $(foreach t,$(1),-l $(t)) \
	--eval '(ert-run-tests-batch-and-exit (quote $(2)))'

.PHONY: test test-gallery $(GALLERY_TARGETS) compile clean bench bench-budget

test:
	$(call ERT,$(TESTS),(not (tag :gallery)))

test-gallery: $(GALLERY_TARGETS)

$(addprefix test-gallery-,$(GALLERY_GROUPS)): test-gallery-%:
	EAS_GALLERY_GROUPS=$* $(call ERT,src/eas/eas-vl-gallery-test.el,(tag :gallery))

test-gallery-conformance:
	$(call ERT,$(TESTS),(and (tag :gallery) (not "^eas-vl-gallery-groups-")))

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
