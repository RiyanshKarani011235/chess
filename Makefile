build: compile

compile: exceptions.ml \
	utils.ml \
	abstract_syntax.ml \
	static_syntax_checker.ml \
	semantics_checker.ml \
	board.ml \
	game.ml	\
	strategy.ml \
	strategies.ml

	ocamlc -c exceptions.ml
	ocamlc -c abstract_syntax.ml
	ocamlc -c utils.ml
	ocamlc -c static_syntax_checker.ml
	ocamlc -c board.ml
	ocamlc -c semantics_checker.ml
	ocamlc -c game.ml
	ocamlc -c strategy.ml
	ocamlc -c strategies.ml
	ocamlc -o chess exceptions.cmo abstract_syntax.cmo utils.cmo static_syntax_checker.cmo board.cmo semantics_checker.cmo game.cmo strategy.cmo strategies.cmo
	
clean:
	rm *.cmi
	rm *.cmo