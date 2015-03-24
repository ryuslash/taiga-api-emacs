EMACS = emacs
CASK_EXEC = cask exec

.PHONY: all test clean

all: lisp/taiga-api.elc

%.elc: %.el
	$(CASK_EXEC) $(EMACS) -Q -batch -L lisp/ -f batch-byte-compile $^

test:
	$(CASK_EXEC) $(EMACS) -Q -batch -L lisp/ -L test/ -l taiga-api \
		-l test-helper -l taiga-api-emacs-test -f ert-run-tests-batch

clean:
	rm lisp/taiga-api.elc
