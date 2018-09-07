MODE ?= strict
CODEPEER ?= off
GNATPROVE_OPTS = --prover=z3,cvc4,altergo -j0 --codepeer=$(CODEPEER) --output-header
GPRBUILD_OPTS = -s -p -XMode=$(MODE)
WGET_OPTS = --recursive --timestamping --continue --progress=dot:mega --show-progress --waitretry=30 --random-wait

all:
	@gprbuild $(GPRBUILD_OPTS) -P SXML
	@gnatprove $(GNATPROVE_OPTS) -P SXML

test: SXML.gpr
	@gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@gnatprove $(GNATPROVE_OPTS) -P tests/prove/prove
	@obj/tests

testonly: SXML.gpr
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@obj/tests

testbulk: export SXML_BULK_TESTS ?= 1
testbulk: SXML.gpr bulkdata
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@obj/tests

testinsane: export SXML_INSANE_TESTS ?= 1
testinsane: SXML.gpr insanedata
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@obj/tests

fuzz: GPRBUILD_OPTS += --compiler-subst=Ada,afl-gcc
fuzz: export AFL_SKIP_CPUFREQ=1
fuzz: MODE=debug
fuzz: examples
	@afl-fuzz -m 1024 -i tests/afl-data -o obj/fuzz ./obj/parse @@

bulkdata:
	wget $(WGET_OPTS) --directory-prefix=obj/document --input-file=tests/data/bulk_urls.txt

insantedata:
	wget $(WGET_OPTS) --directory-prefix=obj/document --input-file=tests/data/bulk_insane_urls.txt

examples::
	gprbuild $(GPRBUILD_OPTS) -P examples/examples

clean:
	gnatclean -P SXML
	gnatprove -P SXML --clean
	gnatclean -P tests/execute/tests.gpr
	gnatclean -P examples/examples
	gnatprove -P examples/examples --clean
	rm -rf obj/document
