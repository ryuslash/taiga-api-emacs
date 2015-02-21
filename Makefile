.PHONY: all test

all:
	$(MAKE) -C lisp/

test:
	$(MAKE) -C tests/ test
