.PHONY: run test test_c test_opt_c
.DELETE_ON_ERROR:

TGT=app/Main

GHCOPTS=-prof
RTSOPTS=+RTS -xc

$(TGT): build

build: $(wildcard src/*.hs)
	stack build

interp:
	stack ghci

run: build
	stack run

test_c: build $(wildcard testfiles/*.fd4)
	$(foreach file, $(wildcard testfiles/*.fd4), printf "$(file)\n" && stack run -- -c $(file) && printf "RESULT\n" && $(patsubst %.fd4,%,$(file)) &&) true

test_opt_c: build $(wildcard testfiles/*.fd4)
	$(foreach file, $(wildcard testfiles/*.fd4), printf "$(file)\n" && stack run -- -c -o $(file) && printf "RESULT\n" && $(patsubst %.fd4,%,$(file)) &&) true

include testing.mk
test: build vm
	$(MAKE) test_all

vm:
	$(MAKE) -C vm

.PHONY: vm
