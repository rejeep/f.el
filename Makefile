EMACS ?= emacs
CARTON ?= carton

all: test

test:
	${CARTON} exec test/f-test

docs:
	${CARTON} exec bin/make-docs

compile:
	${CARTON} exec ${EMACS} -Q -batch -f batch-byte-compile f.el

clean-elc:
	rm f.elc

.PHONY:	all test docs
