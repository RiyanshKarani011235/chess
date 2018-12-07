build: compile

compile: exceptions.ml utils.ml abstract_syntax.ml static_syntax_checker.ml dynamic_syntax_checker.ml board.ml game.ml 
	ocamlc -c exceptions.ml
	ocamlc -c abstract_syntax.ml
	ocamlc -c utils.ml
	ocamlc -c static_syntax_checker.ml
	ocamlc -c board.ml
	ocamlc -c dynamic_syntax_checker.ml
	ocamlc -c game.ml
	ocamlc -o chess exceptions.cmo abstract_syntax.cmo utils.cmo static_syntax_checker.cmo board.cmo dynamic_syntax_checker.cmo game.cmo
	
clean:
	rm *.cmi
	rm *.cmo