default: main.pdf


%.view: %.pdf  
	evince $<

%.print: %.pdf
	lpr $<

main.pdf: content/*.tex preambel/*.tex

%.pdf: %.tex # ast.pdf 
	latexmk -pdf $<

# %.pdf: %.svg
# 	inkscape --export-pdf=$@ $<
# #	convert $< $@


# %.tex: %.lhs
# 	lhs2TeX $< > $@

#states.tex: states.dot
#	dot -Txdot $< | dot2tex --texmode=raw --codeonly > $@
#	dot2tex --preproc $< | dot2tex --figonly > $@


# %.tex: %.dot
#	dot -Txdot $< | dot2tex --texmode=raw --figonly > $@
#	dot2tex --preproc $< | dot2tex --figonly > $@

clean:
	@\rm -f \
	*.aux \
	$(TEXFILE:.tex=.log) \
	$(TEXFILE:.tex=.lof) \
	$(TEXFILE:.tex=.lot) \
	$(TEXFILE:.tex=.toc) \
	$(TEXFILE:.tex=.out) \
	$(TEXFILE:.tex=.dvi) \
	$(TEXFILE:.tex=.pdf) \
	$(TEXFILE:.tex=.ps)  \
	$(TEXFILE:.tex=.bm)  \
	$(TEXFILE:.tex=.bbl) \
	$(TEXFILE:.tex=.blg) \
		*~
