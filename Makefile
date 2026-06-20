# Makefile --- build the Turbo Vision example program.
#
# The example is dumped by ASDF's `program-op' (configured via
# :build-operation / :build-pathname / :entry-point in tvision.asd):
#
#   tvlisp  <- system tvision/examples/tvlisp   (entry tvision-tvlisp:toplevel)
#
# Usage:
#   make            # build ./tvlisp
#   make test       # headless control suite + tvlisp pty smoke tests
#   make clean      # remove the binary and this project's fasl cache

SBCL ?= sbcl
PYTHON ?= python3

# Build SYSTEM with asdf:make.  We add this directory to the source registry
# explicitly so the build works even without a global ocicl/ASDF config.
define asdf-make
$(SBCL) --non-interactive \
	--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))' \
	--eval '(asdf:make :$(1))' \
	--eval '(uiop:quit 0)'
endef

# The program rebuilds whenever the framework or its example source changes.
FRAMEWORK := tvision.asd $(wildcard src/*.lisp)

.DEFAULT_GOAL := all
.PHONY: all clean run-tvlisp test test-lisp test-pty help

all: tvlisp

# Full test: the headless control suite plus the tvlisp pty smoke tests.
test: test-lisp test-pty

# Run the headless control test suite (exit non-zero on any failure).
test-lisp: $(FRAMEWORK) tests/tvision-tests.lisp
	$(SBCL) --non-interactive \
		--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))' \
		--eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :tvision/tests))' \
		--eval '(sb-ext:exit :code (if (zerop (tvision-tests:run-tests)) 0 1))'

# Drive the built ./tvlisp through a pty and assert on the screen (end-to-end).
test-pty: tvlisp tests/pty_smoke.py
	$(PYTHON) tests/pty_smoke.py ./tvlisp

tvlisp: $(FRAMEWORK) examples/tvlisp.lisp
	$(call asdf-make,tvision/examples/tvlisp)

run-tvlisp: tvlisp
	./tvlisp

clean:
	rm -f tvlisp
	rm -rf $(HOME)/.cache/common-lisp/*tvision* 2>/dev/null || true

help:
	@echo "Targets: all (default), tvlisp, run-tvlisp, test, test-lisp, test-pty, clean"
