all:
	happy -gca LogicExpr/Par.y
	alex -g LogicExpr/Lex.x
	(cd LogicExpr/; latex Doc.tex; dvips Doc.dvi -o Doc.ps)
	ghc --make LogicExpr/Test.hs -o LogicExpr/Test
clean:
	-rm -f LogicExpr/*.log LogicExpr/*.aux LogicExpr/*.hi LogicExpr/*.o LogicExpr/*.dvi
	-rm -f LogicExpr/Doc.ps
distclean: clean
	-rm -f LogicExpr/Doc.* LogicExpr/Lex.* LogicExpr/Par.* LogicExpr/Layout.* LogicExpr/Skel.* LogicExpr/Print.* LogicExpr/Test.* LogicExpr/Abs.* LogicExpr/Test LogicExpr/ErrM.* LogicExpr/SharedString.* LogicExpr/LogicExpr.dtd LogicExpr/XML.* Makefile*
	-rmdir -p LogicExpr/
