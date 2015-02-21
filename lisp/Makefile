.PHONY: all

all: taiga-api.elc

%.elc: %.el
	emacs -batch -f batch-byte-compile $^
