NAME = paper

.phony: default

default: $(NAME).pdf

%.pdf: %.tex 
	latexmk -pdf $<

%.tex: %.lhs %.fmt *.tex
	lhs2TeX  $< > $@

clean:
	$(RM) *.aux *.aux.bak *.bbl *.blg *.fdb_latexmk *.log *.ptb
	$(RM) $(NAME).tex $(NAME).pdf
