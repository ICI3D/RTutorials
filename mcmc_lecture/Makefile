
all: $(patsubst %.tex,%.png,$(filter-out colors.tex,$(wildcard *.tex)))

%.png: %.pdf
	convert -density 600 -trim $< $@

%.pdf: %.tex
	pdflatex $<
	rm $*.aux $*.log

clean:
	rm -f *.pdf *.png *.aux *.log