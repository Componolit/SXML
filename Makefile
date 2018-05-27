all:
	gprbuild -P sxml -p
	gnatprove -P sxml

example:
	gprbuild -P examples/exec -p
	gnatprove -P examples/exec

clean:
	gnatclean -P sxml
	gnatprove -P sxml --clean
	gnatclean -P examples/exec
	gnatprove -P examples/exec --clean
