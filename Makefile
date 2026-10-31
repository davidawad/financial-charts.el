EMACS ?= emacs
TESTS := $(sort $(shell find src -type f -name '*-test.el' -print))
# eas.el, the chart engine, is a separate package: a checkout of it beside
# this one by default, overridable (make test EAS=/path/to/eas.el).
EAS ?= ../../Personal/emacs/eas.el
SRC_DIRS := src src/core src/indicators src/charts src/integrations src/examples
LOAD_PATHS := -L $(EAS)/src $(foreach dir,$(SRC_DIRS),-L $(dir))
SOURCES := $(shell find src -type f -name '*.el' ! -name '*-test.el' -print)

ERT = $(EMACS) -Q --batch $(LOAD_PATHS) -L test $(foreach t,$(1),-l $(t)) \
	--eval '(ert-run-tests-batch-and-exit (quote $(2)))'

.PHONY: test compile melpa-check clean

test:
	$(call ERT,$(TESTS),t)

compile:
	$(EMACS) -Q --batch $(LOAD_PATHS) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SOURCES)
	@find src -name '*.elc' -delete

# The recipe's files copied flat (as MELPA installs them), compiled,
# rendered and package-linted: scripts/melpa-layout-check.
melpa-check:
	EAS=$(EAS) EMACS=$(EMACS) scripts/melpa-layout-check

clean:
	find src -name '*.elc' -delete

# 3D variants (templates3d/, src/financial-chart-3d.el) need graphics.el
# and the eas.el revision its Makefile pins (EAS_REV): `make deps-3d'
# clones both into test/deps/ (git@gitlab.com, read access to
# davidawad/graphics-el and davidawad/eas-el) and builds graphics.el's
# Rust module.  `make test-3d' runs test/financial-chart-3d-test.el
# against them; `make golden-3d' rewrites test/golden/3d/ and docs/3d.md.
GRAPHICS_REPO ?= git@gitlab.com:davidawad/graphics-el.git
GRAPHICS_REV ?= 6474ca449d7465c3adfdc487ee697638f7700f47
EAS_REPO ?= git@gitlab.com:davidawad/eas-el.git
DEPS := test/deps
GRAPHICS_DIR := $(DEPS)/graphics-el
EAS3D_DIR := $(DEPS)/eas-el
LOAD_PATHS_3D := -L $(EAS3D_DIR)/src $(foreach dir,$(SRC_DIRS),-L $(dir)) -L $(GRAPHICS_DIR)/lisp -L test
BATCH_3D = $(EMACS) -Q --batch $(LOAD_PATHS_3D)

.PHONY: deps-3d test-3d golden-3d templates3d examples-3d

deps-3d:
	@mkdir -p $(DEPS)
	@if [ ! -d $(GRAPHICS_DIR)/.git ]; then git clone $(GRAPHICS_REPO) $(GRAPHICS_DIR); fi
	@git -C $(GRAPHICS_DIR) cat-file -e $(GRAPHICS_REV)^{commit} 2>/dev/null || git -C $(GRAPHICS_DIR) fetch origin
	git -C $(GRAPHICS_DIR) checkout -q --detach $(GRAPHICS_REV)
	@rev=$$(sed -n 's/^EAS_REV := *//p' $(GRAPHICS_DIR)/Makefile); \
	  if [ ! -d $(EAS3D_DIR)/.git ]; then git clone --filter=blob:none $(EAS_REPO) $(EAS3D_DIR); fi; \
	  git -C $(EAS3D_DIR) cat-file -e $$rev^{commit} 2>/dev/null || git -C $(EAS3D_DIR) fetch origin; \
	  echo "eas.el at $$rev (graphics.el's EAS_REV)"; \
	  git -C $(EAS3D_DIR) checkout -q --detach $$rev
	$(MAKE) -C $(GRAPHICS_DIR) module EAS_DIR=$(CURDIR)/$(EAS3D_DIR)/src

test-3d: deps-3d
	$(BATCH_3D) -l test/financial-chart-3d-test.el -l test/financial-chart-3d-risk-test.el -f ert-run-tests-batch-and-exit

templates3d:
	python3 scripts/templates3d.py

examples-3d: deps-3d
	$(BATCH_3D) -l financial-chart-3d -f financial-chart-3d-write-examples

golden-3d: deps-3d
	$(BATCH_3D) -l financial-chart-3d --eval '(progn (financial-chart-3d-write-goldens) (financial-chart-3d-write-gallery))'
