;;; typst-mode.el --- major mode for working with typst markup-based typesetting system -*- lexical-binding: t; -*-

;; Copyright (C) 2023, Ziqi Yang

;; This file is NOT part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Version: 0.1
;; Author: Ziqi Yang
;; Keywords: typst editing typesetting writing
;; URL: https://github.com/Ziqi-Yang/typst-mode.el
;; License: GNU General Public License >= 3

;;; Commentary:

;; An Emacs major mode for typst markup-based typesetting system
;; https://github.com/Ziqi-Yang/typst-mode.el
;; See https://typst.app/

;;; Usage:
;;; Customization:
;;; Code:
(defgroup typst nil
  "Typst Writing."
  :group 'text)

;;;###autoload
(define-derived-mode typst-base-mode prog-mode "Typst"
  "Generic major mode for editing Typst files.

This is a generic major mode intended to be inherited by
concrete implementations.  Currently there are two concrete
implementations: `typst-mode' and `typst-ts-mode'."
  ;; :syntax-table typst-syntax-table
  (setq-local tab-width 4))

;;;###autoload
(define-derived-mode typst-mode typst-base-mode "Typst"
  "Major mode for editing Typst files.

\\{typst-mode-map}"
  ;; :syntax-table typst-syntax-table
  (setq font-lock-defaults '('(typst-mode-fontify) t))
  )

;; TODO support treesit
;; ;;;###autoload
;; (define-derived-mode typst-ts-mode typst-base-mode "Typst"
;;   "Major mode for editing Typst files.

;; \\{typst-ts-mode-map}"
;;   ;; :syntax-table typst-syntax-table
;;   (when (treesit-ready-p 'typst)
;;     (treesit-parser-create 'typst)
;;     ;; (setq-local treesit-font-lock-settings typst--treesit-settings)
;;     (setq-local treesit-font-lock-feature-list
;;       '(( comment definition)
;;          ( keyword string type)
;;          ( assignment builtin constant decorator
;;            escape-sequence number property string-interpolation )
;;          ( bracket delimiter function operator variable)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-mode))

;;; FONTIFICATION ===========================================
(defun typst-mode-fontify (limit)
  (save-excursion
    (let ((beg (point))
           (end limit))
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Properties.html
      ;; (remove-list-of-text-properties beg end '(font-lock-face face))
      )
    )
  )

(defun typst-mode-fontify-region (beg end keywords)
  (save-excursion
    (let ((font-lock-keywords keywords)
           (font-lock-multiline nil)
           (font-lock-keywords-case-fold-search nil)
           (font-lock-keywords-only t)
           (font-lock-extend-region-functions nil))
      (when (and (listp font-lock-keywords) global-font-lock-mode)
        (font-lock-fontify-region beg end)))))

(provide 'typst-mode)
;;; typst-mode.el ends here
