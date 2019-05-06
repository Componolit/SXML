MODE ?= strict
CODEPEER ?= off
GNATPROVE_OPTS = --prover=z3,cvc4,altergo -j0 --codepeer=$(CODEPEER) --output-header
GPRBUILD_OPTS = -s -p -XMode=$(MODE)
WGET_OPTS = --recursive --continue --progress=dot:mega --show-progress --wait=1 --waitretry=5 --random-wait --no-clobber
CHECK = \#

all:
	$(CHECK)gnatcheck -P SXML
	@gprbuild $(GPRBUILD_OPTS) -P SXML
	@gnatprove $(GNATPROVE_OPTS) -P SXML

test: SXML.gpr
	$(CHECK)gnatcheck -P SXML
	@gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@obj/tests

prove:
	@gnatprove $(GNATPROVE_OPTS) -P SXML
	@gnatprove $(GNATPROVE_OPTS) -P tests/prove/prove

doc: doc/api/index.html

doc/api/index.html: SXML.gpr
	@gprbuild -P SXML -Xlibtype=dynamic
	@gnatdoc -q -P SXML --no-subprojects -Xlibtype=dynamic -XRTS=native -Xcallgraph=none -w -l --enable-build
	@gnatdoc -P SXML --no-subprojects -Xlibtype=dynamic -XRTS=native -Xcallgraph=none -w -l --enable-build

testonly: SXML.gpr
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@time obj/tests

testbulk: export SXML_BULK_TESTS ?= 1
testbulk: SXML.gpr rawdata
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@time obj/tests

testinsane: export SXML_INSANE_TESTS ?= 1
testinsane: SXML.gpr rawdata
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@time obj/tests

rawdata:
	@rm -rf obj/rawdata
	@cp -a tests/rawdata obj/rawdata
	@find obj/rawdata -type f -name '*.xz' -print0 | xargs --null unxz

check:
	@gnatcheck -P SXML

fuzz: GPRBUILD_OPTS += --compiler-subst=Ada,afl-gcc
fuzz: export AFL_SKIP_CPUFREQ=1
fuzz: MODE=fuzz
fuzz: obj/fuzzdriver
	@afl-fuzz -x tests/afl-dict/xml.dict -t 200 -m 1024 -i tests/afl-data -o obj/fuzz ./obj/fuzzdriver @@

stack: MODE=stack
stack: SXML.gpr
	gprbuild $(GPRBUILD_OPTS) -P SXML
	gnatstack $(GPRBUILD_OPTS) -P SXML

obj/fuzzdriver::
	gprbuild $(GPRBUILD_OPTS) -P examples/fuzzdriver

examples::
	gprbuild $(GPRBUILD_OPTS) -P examples/examples

clean:
	gnatclean -P SXML
	gnatprove -P SXML --clean
	gnatclean -P tests/execute/tests.gpr
	gnatclean -P examples/examples
	gnatprove -P examples/examples --clean
	gnatclean -P examples/fuzzdriver
	rm -rf obj/document
