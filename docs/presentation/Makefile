all: Presentation.md
	pandoc \
		--variable mainfont=Monaco \
		--variable mathfont=Monaco \
		--variable monofont=Monaco \
		--variable fontsize=9pt \
		--latex-engine=xelatex \
		--template=presentation_template.latex \
		-t beamer Presentation.md -o Presentation.pdf
  # mainfont variable together with a template hack and other font variables gives Monaco math
clean:
	rm -f Presentation.pdf
