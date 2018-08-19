CODEPEER ?= on
GNATPROVE_OPTS = --prover=z3,cvc4,altergo -j0 --codepeer=$(CODEPEER) --output-header
GPRBUILD_OPTS = -gnata -p

all:
	gprbuild $(GPRBUILD_OPTS) -P SXML
	gnatprove $(GNATPROVE_OPTS) -P SXML

test: SXML.gpr
	@gprbuild $(COMMON_OPTS) -P tests/execute/tests -gnata -p
	@gnatprove $(GNATPROVE_OPTS) -P tests/prove/prove
	@obj/tests

examples::
	gprbuild $(GPRBUILD_OPTS) -P examples/examples

clean:
	gnatclean -P SXML
	gnatprove -P SXML --clean
	gnatclean -P tests/execute/tests.gpr
	gnatclean -P examples/examples
	gnatprove -P examples/examples --clean
