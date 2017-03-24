FILES = typeExpr.ml corenode.ml declNode.ml schedule.ml sortNode.ml writeCode.ml entry.ml
%: %.ml
	ocamlc -o $@ $(FILES) $<
