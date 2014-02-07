all: planning notes langs
planning: Planning.md
	pandoc --variable mainfont=Palatino --variable fontsize=12pt Planning.md --latex-engine=xelatex -o Planning.pdf
notes: Notes.md
	pandoc --variable mainfont=Palatino --variable fontsize=12pt --variable geometry:margin=1in Notes.md --latex-engine=xelatex -o Notes.pdf
langs: MIL.md FunLang.md OOLang.md
	pandoc --variable mainfont=Palatino --variable fontsize=12pt --variable geometry:margin=1in MIL.md --latex-engine=xelatex -o MIL.pdf
	pandoc --variable mainfont=Palatino --variable fontsize=12pt --variable geometry:margin=1in FunLang.md --latex-engine=xelatex -o FunLang.pdf
	pandoc --variable mainfont=Palatino --variable fontsize=12pt --variable geometry:margin=1in OOLang.md --latex-engine=xelatex -o OOLang.pdf
clean:
	rm -f Planning.aux Planning.pdf Planning.log Planning.synctex.gz
	rm -f Notes.aux Notes.pdf Notes.log Notes.synctex.gz
	rm -f MIL.aux MIL.pdf MIL.log MIL.synctex.gz
	rm -f FunLang.aux FunLang.pdf FunLang.log FunLang.synctex.gz
	rm -f OOLang.aux OOLang.pdf OOLang.log OOLang.synctex.gz
