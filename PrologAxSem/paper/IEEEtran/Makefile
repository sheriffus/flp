# Generic make file for LaTeX: requires GNU make


NAME = bare_conf

.PHONY: default

default: $(NAME).pdf


%.pdf: %.tex ../*.tex
	latexmk -pdf $<

#%.tex: %.lhs %.fmt *.bib *.tex ../*.tex
#	lhs2TeX  $< > $@

clean:
	$(RM) *.aux *.aux.bak *.bbl *.blg *.fdb_latexmk *.log *.ptb
	$(RM) $(NAME).tex $(NAME).pdf

