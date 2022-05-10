# -*- indent-tabs-mode: t -*-
export EMACS ?= $(shell which emacs)
CASK ?= $(shell which cask)

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

docs:
	${CASK} exec ${EMACS} -Q --script bin/docs.el

compile:
	${CASK} build

clean-elc:
	rm -f f.elc

.PHONY:	all test docs unit
