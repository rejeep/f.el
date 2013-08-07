EMACS ?= emacs
CASK ?= cask

all: test

test:
	${CASK} exec test/f-test

docs:
	${CASK} exec bin/make-docs

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile f.el

clean-elc:
	rm f.elc

.PHONY:	all test docs
