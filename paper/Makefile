TEXSRC = $(wildcard ./*.tex)

acmconference-paper: OFuzz.pdf

all: OFuzz.pdf

pdf: OFuzz.pdf

OFuzz.pdf: biblio.bib $(TEXSRC)
	pdflatex acmconference-paper
	bibtex acmconference-paper
	pdflatex acmconference-paper
	pdflatex acmconference-paper

clean: 
	\rm -f *.dvi *.aux *.ps *~ *.log *.blg *.bbl *.pdf
