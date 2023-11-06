utop: 
	find . | grep -v '\./_build/' | grep -E "\.mli?$$" | entr dune utop .
ocaml:
	dune build --watch
