EMACS = emacs
CASK_EXEC = cask exec

.PHONY: all test

all: lisp/taiga-api.elc

%.elc: %.el
	$(CASK_EXEC) $(EMACS) -Q -batch -L lisp/ -f batch-byte-compile $^

test:
	$(CASK_EXEC) $(EMACS) -Q -batch -l ert -L lisp/ -L tests/ \
		-l taiga-api-tests.el -eval "(ert t)" # -f ert-run-tests-batch-and-exit
