NAME = paper

default: $(NAME).pdf

%.pdf: %.tex 
	latexmk -pdf $<

%.tex: %.lhs %.fmt header.tex abstract.tex sec*.tex elements/*/*.tex
	lhs2TeX  $< > $@

clean:
	$(RM) *.aux *.aux.bak *.bbl *.blg *.fdb_latexmk *.log *.ptb
	$(RM) $(NAME).tex $(NAME).pdf
