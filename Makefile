load_polymode := -L ~/.emacs.d/.local/straight/repos/polymode
load_typst_mode := -l ~/.emacs.d/modules/languages/typst-mode/typst-mode.el
test_file := ~/.emacs.d/modules/languages/typst-mode/tests/1.typ

.PHONY: test_1
test_1:
	emacs -Q $(load_polymode) $(load_typst_mode) $(test_file)
