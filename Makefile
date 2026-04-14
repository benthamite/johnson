EMACS ?= emacs

EL_FILES := $(wildcard *.el)
SRC_FILES := $(filter-out %-test.el,$(EL_FILES))
TEST_FILES := $(sort $(wildcard test/*-test.el) $(wildcard test/johnson-test.el))

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch \
	  -L . -L test \
	  -l johnson.el \
	  $(foreach f,$(TEST_FILES),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch \
	  -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC_FILES)

clean:
	rm -f *.elc
