CODEPEER ?= on
GNATPROVE_OPTS = --prover=z3,cvc4,altergo -j0 --codepeer=$(CODEPEER) --output-header
GPRBUILD_OPTS = -gnata -p

all:
	gprbuild $(GPRBUILD_OPTS) -P SXML
	gnatprove $(GNATPROVE_OPTS) -P SXML

test: SXML.gpr
	@gprbuild $(COMMON_OPTS) -P tests/execute/tests.gpr -gnata -p
	@obj/tests

example:
	gprbuild $(GPRBUILD_OPTS) -P examples/exec
	gnatprove $(GNATPROVE_OPTS) -P examples/exec

simple:
	gprbuild $(GPRBUILD_OPTS) -P examples/simple
	gnatprove $(GNATPROVE_OPTS) -P examples/simple
	./out/simple

clean:
	gnatclean -P SXML
	gnatprove -P SXML --clean
	gnatclean -P tests/execute/tests.gpr
	gnatclean -P examples/exec
	gnatclean -P examples/simple
	gnatprove -P examples/exec --clean
	gnatprove -P examples/simple --clean
