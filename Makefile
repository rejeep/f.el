CARTON ?= carton

all: test

test:
	${CARTON} exec test/f-test

.PHONY:	all test
