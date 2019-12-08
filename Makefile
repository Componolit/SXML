MODE ?= strict
BOUNDED ?= false

WGET_OPTS = --recursive --continue --progress=dot:mega --show-progress --wait=1 --waitretry=5 --random-wait --no-clobber
TEST_OPTS ?= ulimit -s 40; ulimit -a;
AFL_OPTS  ?= -x tests/afl-dict/xml.dict -t 200 -m 1024 -i tests/afl-data -o obj/fuzz

GNATDOC  ?= gnatdoc
GPRBUILD ?= gprbuild
GPRBUILD_OPTS = -s -p -XMode=$(MODE) -XBounded=$(BOUNDED)

GNATPROVE ?= gnatprove
GNATPROVE_OPTS += --prover=z3,cvc4 -j0 --memlimit=2000 --output-header --steps=5500 --checks-as-errors

GNATSTACK ?= $(notdir $(firstword $(shell which gnatstack true 2> /dev/null)))
GNATCHECK ?= $(notdir $(firstword $(shell which gnatcheck true 2> /dev/null)))

all: test prove stack

build: check build/SXML.gpr
	@$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/execute/tests

test: build
	@($(TEST_OPTS)time obj/tests/tests)

prove: prove_tests prove_lib

prove_lib:
	@time $(GNATPROVE) $(GNATPROVE_OPTS) -P build/SXML

prove_tests: prove_lib
	@time $(GNATPROVE) $(GNATPROVE_OPTS) -P tests/prove/prove

doc: doc/api/index.html

doc/api/index.html: build/SXML.gpr
	@$(GPRBUILD) -P build/SXML -Xlibtype=dynamic
	@$(GNATDOC) -q -P build/SXML --no-subprojects -Xlibtype=dynamic -XRTS=native -Xcallgraph=none -w -l --enable-build
	@$(GNATDOC) -P build/SXML --no-subprojects -Xlibtype=dynamic -XRTS=native -Xcallgraph=none -w -l --enable-build

testonly: build/SXML.gpr
	$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

testbulk: export SXML_BULK_TESTS ?= 1
testbulk: build/SXML.gpr rawdata
	$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

testinsane: export SXML_INSANE_TESTS ?= 1
testinsane: build/SXML.gpr rawdata
	$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/execute/tests
	@($(TEST_OPTS)time obj/tests/tests)

rawdata:
	@rm -rf obj/rawdata
	@cp -a tests/rawdata obj/rawdata
	@find obj/rawdata -type f -name '*.xz' -print0 | xargs --null unxz

check:
	@mkdir -p obj
	$(GNATCHECK) -P build/SXML

fuzz: GPRBUILD_OPTS += --compiler-subst=Ada,afl-gcc
fuzz: export AFL_SKIP_CPUFREQ=1
fuzz: MODE=fuzz
fuzz: obj/fuzzdriver
	@afl-fuzz $(AFL_OPTS) -M master ./obj/fuzzdriver @@

fuzz-slave: export AFL_SKIP_CPUFREQ=1
fuzz-slave:
	@afl-fuzz $(AFL_OPTS) -S "slave-$$$$" ./obj/fuzzdriver @@

fuzz-status:
	@watch -n1 afl-whatsup -s obj/fuzz

stack: MODE=stack
stack: build/SXML.gpr
stack: BOUNDED = true
stack:
	$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/prove/stack
	$(GNATSTACK) $(GPRBUILD_OPTS) -P tests/prove/stack

obj/fuzzdriver::
	$(GPRBUILD) $(GPRBUILD_OPTS) -P examples/fuzzdriver

examples::
	$(GPRBUILD) $(GPRBUILD_OPTS) -P examples/examples
	$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/prove/prove
	$(GPRBUILD) $(GPRBUILD_OPTS) -P tests/prove/stack

clean:
	rm -rf obj contrib/basalt/obj contrib/basalt/lib graph.vcg undefined.ciu
