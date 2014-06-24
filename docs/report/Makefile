all:
	pandoc \
			--variable documentclass=report \
			--variable mainfont=Palatino \
			--variable monofont=Monaco \
			--variable fontsize=12pt \
			--latex-engine=xelatex \
			--chapters --toc \
			--parse-raw -t latex -s \
		00_Acknowledgements.md \
		01_Introduction.md \
		02_Conclusions.md > ReportText.tex
	xelatex ReportText.tex
	xelatex ReportText.tex
	bibtex ReportText.aux
	xelatex ReportText.tex
	xelatex ReportText.tex
	pdftk CoverPage.pdf ReportText.pdf cat output Report.pdf
clean:
	rm -f ReportText.aux ReportText.bbl ReportText.blg ReportText.log ReportText.out ReportText.tex ReportText.toc ReportText.pdf
