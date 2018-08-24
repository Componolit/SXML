MODE ?= strict
CODEPEER ?= off
GNATPROVE_OPTS = --prover=z3,cvc4,altergo -j0 --codepeer=$(CODEPEER) --output-header
GPRBUILD_OPTS = -p -XMode=$(MODE)

all:
	@gprbuild $(GPRBUILD_OPTS) -P SXML
	@gnatprove $(GNATPROVE_OPTS) -P SXML

test: SXML.gpr
	@gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@gnatprove $(GNATPROVE_OPTS) -P tests/prove/prove
	@obj/tests

testonly: MODE ?= sloppy
testonly: SXML.gpr
	gprbuild $(GPRBUILD_OPTS) -P tests/execute/tests
	@obj/tests

examples::
	gprbuild $(GPRBUILD_OPTS) -P examples/examples

clean:
	gnatclean -P SXML
	gnatprove -P SXML --clean
	gnatclean -P tests/execute/tests.gpr
	gnatclean -P examples/examples
	gnatprove -P examples/examples --clean
