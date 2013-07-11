CARTON ?= carton

all: test

test:
	${CARTON} exec test/f-test

docs:
	${CARTON} exec bin/make-docs

.PHONY:	all test docs
