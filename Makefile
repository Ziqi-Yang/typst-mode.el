load_polymode := -L ~/.emacs.d/.local/straight/repos/polymode
load_typst_mode := -l ~/.emacs.d/modules/languages/typst-mode/typst-mode.el
general := ~/.emacs.d/modules/languages/typst-mode/tests/general.typ
syntax := ~/.emacs.d/modules/languages/typst-mode/tests/syntax.typ
debug := --debug-init 

.PHONY: test_1
general:
	emacs -Q $(debug) $(load_polymode) $(load_typst_mode) $(general)

syntax:
	emacs -Q $(debug) $(load_polymode) $(load_typst_mode) $(syntax)
