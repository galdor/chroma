EMACS ?= emacs
EMACS_OPTIONS = --batch -L .

all: test

test:
	$(EMACS) $(EMACS_OPTIONS) \
	    -l ert \
	    -l chroma-test.el \
	    -f ert-run-tests-batch-and-exit

.PHONY: all test
