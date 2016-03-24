all:
	pandoc \
		  --chapters \
		  --parse-raw -t latex \
		Abstract.md \
		00_Acknowledgements.md > AbstractAcknowledgements.tex
	pandoc \
			--variable documentclass=book \
			--variable classoption=oneside \
			--variable mainfont=Palatino \
			--variable monofont=Monaco \
			--variable mathfont=Monaco \
			--variable fontsize=12pt \
			--variable geometry:margin=1in \
			--latex-engine=xelatex \
			--chapters --toc \
			--number-sections \
			--parse-raw -t latex -s \
			--template=report_template.latex \
		  --include-before=AbstractAcknowledgements.tex \
		01_Introduction.md \
		02_Monads.md \
		03_RelatedWork.md \
		04_MIL.md \
		05_FunLang.md \
		06_OOLang.md \
		07_Optimisations.md \
		08_Conclusions.md \
		09_Bibliography.md > ReportText.tex
	xelatex ReportText.tex
	xelatex ReportText.tex
	bibtex ReportText.aux
	xelatex ReportText.tex
	xelatex ReportText.tex
	pdftk CoverPage.pdf ReportText.pdf cat output Report.pdf
clean:
	rm -f ReportText.aux ReportText.bbl ReportText.blg ReportText.log ReportText.out ReportText.tex ReportText.toc ReportText.pdf Acknowledgements.tex
