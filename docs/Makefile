planning: Planning.md
	pandoc --variable mainfont=Palatino --variable fontsize=12pt Planning.md --latex-engine=xelatex -o Planning.pdf
notes: Notes.md
	pandoc --variable mainfont=Palatino --variable fontsize=12pt --variable geometry:margin=1in Notes.md --latex-engine=xelatex -o Notes.pdf
clean:
	rm -f Planning.aux Planning.pdf Planning.log Planning.synctex.gz
	rm -f Notes.aux Notes.pdf Notes.log Notes.synctex.gz
