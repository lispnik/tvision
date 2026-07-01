# Makefile --- the Turbo Vision Common Lisp framework (a library).
#
# This is a library with no external dependencies; there is no binary to dump.
# The example application that used to live here, `tvlisp', now ships as a
# sibling project at ../tvlisp.
#
# Usage:
#   make            # compile/load the framework (build check)
#   make test       # headless control test suite
#   make clean      # remove this project's fasl cache

SBCL ?= sbcl

# Load SYSTEM, adding this directory to the source registry explicitly so it
# works even without a global ocicl/ASDF config.
define asdf-load
$(SBCL) --non-interactive \
	--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))' \
	--eval '(handler-bind ((warning (function muffle-warning))) $(1))' \
	--eval '(uiop:quit 0)'
endef

FRAMEWORK := tvision.asd $(wildcard src/*.lisp)

.DEFAULT_GOAL := all
.PHONY: all clean test test-lisp test-tv2 help

# Build check: compile and load the framework.
all: $(FRAMEWORK)
	$(call asdf-load,(asdf:load-system :tvision))

test: test-lisp test-tv2

# Headless tests for the tv2 CLOS kernel: SBCL-specific IDE features and the
# editor's display-width (wide CJK / emoji) layout.
test-tv2: tv2.asd $(wildcard tv2/*.lisp) tests/tv2-sbcl-tests.lisp tests/tv2-editor-tests.lisp
	$(SBCL) --script tests/tv2-sbcl-tests.lisp
	$(SBCL) --script tests/tv2-editor-tests.lisp

# Run the headless control test suite (exit non-zero on any failure).
test-lisp: $(FRAMEWORK) tests/tvision-tests.lisp
	$(SBCL) --non-interactive \
		--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))' \
		--eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :tvision/tests))' \
		--eval '(sb-ext:exit :code (if (zerop (tvision-tests:run-tests)) 0 1))'

clean:
	rm -rf $(HOME)/.cache/common-lisp/*tvision* 2>/dev/null || true

help:
	@echo "Targets: all (default), test, test-lisp, test-tv2, clean"
