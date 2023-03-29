load_polymode := -L ~/.emacs.d/.local/straight/repos/polymode
load_typst_mode := -l ~/.emacs.d/modules/languages/typst-mode/typst-mode.el
test_file_1 := ~/.emacs.d/modules/languages/typst-mode/tests/1.typ
test_file_2 := ~/.emacs.d/modules/languages/typst-mode/tests/2.typ

.PHONY: test_1
test_1:
	emacs -Q $(load_polymode) $(load_typst_mode) $(test_file_1)

test_2:
	emacs -Q $(load_polymode) $(load_typst_mode) $(test_file_2)
