FILES = typeExpr.ml corenode.ml declNode.ml schedule.ml sortNode.ml parseDecl.ml writeCode.ml entry.ml
%: %.ml
	ocamlfind ocamlc -linkpkg -package stdint,str -o $@ $(FILES) $<
