MODE ?= strict
BOUNDED ?= false
CODEPEER ?= off
GNATPROVE_OPTS += --prover=z3,cvc4 -j0 --memlimit=2000 --codepeer=$(CODEPEER) --output-header --steps=5500 --checks-as-errors
GPRBUILD_OPTS = -s -p -XMode=$(MODE) -XBounded=$(BOUNDED)
WGET_OPTS = --recursive --continue --progress=dot:mega --show-progress --wait=1 --waitretry=5 --random-wait --no-clobber
GNATPROVE ?= gnatprove
GNATCHECK ?= $(notdir $(firstword $(shell which gnatcheck true 2> /dev/null)))
TEST_OPTS ?= ulimit -s 40; ulimit -a;

all:
	$(GNATCHECK) -P build/SXML
	@gprbuild $(GPRBUILD_OPTS) -P build/SXML
	@time $(GNATPROVE) $(GNATPROVE_OPTS) -P build/SXML
	@time $(GNATPROVE) $(GNATPROVE_OPTS) -P tests/prove/prove

test: build/SXML.gpr
	$(GNATCHECK) -P build/SXML
	@gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

prove: prove_tests prove_lib

prove_lib:
	@time $(GNATPROVE) $(GNATPROVE_OPTS) -P build/SXML

prove_tests: prove_lib
	@time $(GNATPROVE) $(GNATPROVE_OPTS) -P tests/prove/prove

doc: doc/api/index.html

doc/api/index.html: build/SXML.gpr
	@gprbuild -P build/SXML -Xlibtype=dynamic
	@gnatdoc -q -P build/SXML --no-subprojects -Xlibtype=dynamic -XRTS=native -Xcallgraph=none -w -l --enable-build
	@gnatdoc -P build/SXML --no-subprojects -Xlibtype=dynamic -XRTS=native -Xcallgraph=none -w -l --enable-build

testonly: build/SXML.gpr
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

testbulk: export SXML_BULK_TESTS ?= 1
testbulk: build/SXML.gpr rawdata
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

testinsane: export SXML_INSANE_TESTS ?= 1
testinsane: build/SXML.gpr rawdata
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

rawdata:
	@rm -rf obj/rawdata
	@cp -a tests/rawdata obj/rawdata
	@find obj/rawdata -type f -name '*.xz' -print0 | xargs --null unxz

check:
	@gnatcheck -P build/SXML

fuzz: GPRBUILD_OPTS += --compiler-subst=Ada,afl-gcc
fuzz: export AFL_SKIP_CPUFREQ=1
fuzz: MODE=fuzz
fuzz: obj/fuzzdriver
	@afl-fuzz -x tests/afl-dict/xml.dict -t 200 -m 1024 -i tests/afl-data -o obj/fuzz ./obj/fuzzdriver @@

stack: MODE=stack
stack: build/SXML.gpr
stack: BOUNDED = true
stack:
	gprbuild $(GPRBUILD_OPTS) -P build/SXML
	gnatstack $(GPRBUILD_OPTS) -P build/SXML

obj/fuzzdriver::
	gprbuild $(GPRBUILD_OPTS) -P examples/fuzzdriver

examples::
	gprbuild $(GPRBUILD_OPTS) -P examples/examples

clean:
	rm -rf obj
