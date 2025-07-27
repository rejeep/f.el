# -*- indent-tabs-mode: t -*-
export EMACS ?= $(shell which emacs)
EASK ?= $(shell which eask)

all: test

install:
	${EASK} install
	${EASK} install-deps
	${EASK} install-deps --dev

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} package
	${MAKE} clean-elc

unit:
	${EASK} exec ert-runner

docs:
	${EASK} exec ${EMACS} -Q --script bin/docs.el

compile:
	${EASK} compile

clean-all:
	${EASK} clean all

clean-elc:
	${EASK} clean elc

package:
	${EASK} package

.PHONY:	all clean-all test docs unit install
