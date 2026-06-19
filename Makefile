# Makefile --- build the Turbo Vision example programs.
#
# Each program is dumped by ASDF's `program-op' (configured via
# :build-operation / :build-pathname / :entry-point in tvision.asd):
#
#   tvision-demo  <- system tvision/examples            (entry tvision-demo:toplevel)
#   textedit      <- system tvision/examples/textedit   (entry tvision-textedit:toplevel)
#
# Usage:
#   make            # build both programs
#   make textedit   # build just the editor
#   make run-demo   # build and launch the demo
#   make clean      # remove the binaries and this project's fasl cache

SBCL ?= sbcl

# Build SYSTEM with asdf:make.  We add this directory to the source registry
# explicitly so the build works even without a global ocicl/ASDF config.
define asdf-make
$(SBCL) --non-interactive \
	--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))' \
	--eval '(asdf:make :$(1))' \
	--eval '(uiop:quit 0)'
endef

# A program rebuilds whenever the framework or its own example source changes.
FRAMEWORK := tvision.asd $(wildcard src/*.lisp)

.DEFAULT_GOAL := all
.PHONY: all clean run-demo run-textedit run-tvlisp test help

all: tvision-demo textedit tvlisp

# Run the headless control test suite (exit non-zero on any failure).
test: $(FRAMEWORK) tests/tvision-tests.lisp
	$(SBCL) --non-interactive \
		--eval '(asdf:initialize-source-registry (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))' \
		--eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :tvision/tests))' \
		--eval '(sb-ext:exit :code (if (zerop (tvision-tests:run-tests)) 0 1))'

tvision-demo: $(FRAMEWORK) examples/demo.lisp
	$(call asdf-make,tvision/examples)

textedit: $(FRAMEWORK) examples/textedit.lisp
	$(call asdf-make,tvision/examples/textedit)

tvlisp: $(FRAMEWORK) examples/tvlisp.lisp
	$(call asdf-make,tvision/examples/tvlisp)

run-demo: tvision-demo
	./tvision-demo

run-textedit: textedit
	./textedit

run-tvlisp: tvlisp
	./tvlisp

clean:
	rm -f tvision-demo textedit tvlisp
	rm -rf $(HOME)/.cache/common-lisp/*tvision* 2>/dev/null || true

help:
	@echo "Targets: all (default), tvision-demo, textedit, tvlisp, run-*, test, clean"
