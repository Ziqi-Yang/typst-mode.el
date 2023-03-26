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
;; Package-Requires: ("polymode")

;;; Commentary:

;; An Emacs major mode for typst markup-based typesetting system
;; https://github.com/Ziqi-Yang/typst-mode.el
;; See https://typst.app/

;;; Usage:
;;; Customization:
;;; Code:
(require 'polymode)
(require 'rx)

(defgroup typst-mode nil
  "Typst Writing."
  :prefix "typst-"
  :group 'text
  :group 'languages)

(defgroup typst-mode-faces nil
  "Faces for syntax highlighting."
  :group 'typst-mode
  :group 'faces)

(defgroup typst-mode-markup-faces nil
  "Faces for syntax highlighting."
  :group 'typst-mode-faces
  :group 'faces)

(defgroup typst-mode-code-faces nil
  "Faces for syntax highlighting."
  :group 'typst-mode-faces
  :group 'faces)

(defgroup typst-mode-math-faces nil
  "Faces for syntax highlighting."
  :group 'typst-mode-faces
  :group 'faces)

;;; Faces ===================================================
(defface typst-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keyword."
  :group 'typst-mode-faces)

(defface typst-mode-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comment."
  :group 'typst-mode-faces)

(defface typst-mode-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function name."
  :group 'typst-mode-faces)

(defface typst-mode-field-name-face
  '((t :inherit font-lock-string-face))
  "Face for field name."
  :group 'typst-mode-faces)

(defface typst-mode-method-name-face
  '((t :inherit typst-mode-field-name-face))
  "Face for function name."
  :group 'typst-mode-faces)

(defface typst-mode-markup-emphasis-face
  '((t :slant italic))
  "Face for emphasis text."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-strong-face
  '((t :weight bold))
  "Face for strong text."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-underline-face
  '((t :underline t))
  "Face for underline text."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-raw-text-face
  '((t :foreground "dim gray"))
  "Face for raw-text."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-label-reference-face
  '((t :foreground "blue"))
  "Face for label and reference."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-heading-1-face
  '((t :weight bold :height 150))
  "Face for heading 1."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-heading-2-face
  '((t :inherit typst-mode-markup-heading-1-face :height 140))
  "Face for heading 2."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-heading-3-face
  '((t :inherit typst-mode-markup-heading-1-face :height 130))
  "Face for heading 3."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-heading-4-face
  '((t :inherit typst-mode-markup-heading-1-face :height 120))
  "Face for heading 4."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-heading-5-face
  '((t :inherit typst-mode-markup-heading-1-face :height 110))
  "Face for heading 5."
  :group 'typst-mode-markup-faces)

(defface typst-mode-markup-term-list-face
  '((t :weight bold))
  "Face for heading 5."
  :group 'typst-mode-markup-faces)

;; @ corresponding variables ============
(defvar typst-mode-keyword-face  'typst-mode-keyword-face
  "Face name to use for keywords.")

(defvar typst-mode-comment-face  'typst-mode-comment-face
  "Face name to use for comment.")

(defvar typst-mode-function-name-face  'typst-mode-function-name-face
  "Face name to use for function names.")

(defvar typst-mode-field-name-face  'typst-mode-field-name-face
  "Face name to use for field names.")

(defvar typst-mode-method-name-face  'typst-mode-method-name-face
  "Face name to use for method names.")

(defvar typst-mode-markup-emphasis-face  'typst-mode-markup-emphasis-face
  "Face name to use for emphasis text.")

(defvar typst-mode-markup-strong-face  'typst-mode-markup-strong-face
  "Face name to use for strong text.")

(defvar typst-mode-markup-underline-face  'typst-mode-markup-underline-face
  "Face name to use for underline text.")

(defvar typst-mode-markup-raw-text-face  'typst-mode-markup-raw-text-face
  "Face name to use for raw text.")

(defvar typst-mode-markup-label-reference-face  'typst-mode-markup-label-reference-face
  "Face name to use for label and reference.")

(defvar typst-mode-markup-heading-1-face  'typst-mode-markup-heading-1-face
  "Face name to use for heading 1.")

(defvar typst-mode-markup-heading-2-face  'typst-mode-markup-heading-2-face
  "Face name to use for heading 2.")

(defvar typst-mode-markup-heading-3-face  'typst-mode-markup-heading-3-face
  "Face name to use for heading 3.")

(defvar typst-mode-markup-heading-4-face  'typst-mode-markup-heading-4-face
  "Face name to use for heading 4.")

(defvar typst-mode-markup-heading-5-face  'typst-mode-markup-heading-5-face
  "Face name to use for heading 5.")

(defvar typst-mode-markup-term-list-face  'typst-mode-markup-term-list-face
  "Face name to use for heading 5.")

;;; Regexps & Keywords =======================================

(defconst typst--markup-comment-regexp ;; don't interfer URLs
  (rx (or (and (or bol (1+ whitespace)) "//" (*? anything) eol)
        (and (or bol (1+ whitespace)) "/*" (*? anything) "*/"))))

(defconst typst--markup-emphasis-regexp
  (rx (1+ blank) "_" (1+ (not blank)) "_" (1+ blank)))

(defconst typst--markup-raw-text-regexp
  (rx "`" (1+ (not blank)) "`"))

(defconst typst--markup-link-regexp
  (rx (or "http://" "https://") (1+ (or alnum ":" "." "/")))) ;; TODO not perfect

(defconst typst--markup-label-regexp
  (rx "<" (1+ (not (any punct blank))) ">"))

(defconst typst--markup-reference-regexp
  (rx "@" (1+ (not (any punct blank)))))

(defconst typst--markup-heading-1-regexp
  (rx bol (* blank) "=" (1+ blank) (seq (? (not blank)) (* not-newline))))

(defconst typst--markup-heading-2-regexp
  (rx bol (* blank) "==" (1+ blank) (seq (? (not blank)) (* not-newline))))

(defconst typst--markup-heading-3-regexp
  (rx bol (* blank) "===" (1+ blank) (seq (? (not blank)) (* not-newline))))

(defconst typst--markup-heading-4-regexp
  (rx bol (* blank) "====" (1+ blank) (seq (? (not blank)) (* not-newline))))

(defconst typst--markup-heading-5-regexp
  (rx bol (* blank) "=====" (1+ blank) (seq (? (not blank)) (* not-newline))))

(defconst typst--markup-term-list-regexp
  (rx bol (* blank) "/" (1+ blank) (group-n 1 (1+ (not ":") )) ":" (* not-newline)))

(defvar typst--global-keywords
  '("#let" "#set" "#show" "#if" "#for" "#while" "#include" "#import")
  "Keywords for typst mode that are in the global scope.")

(defvar typst--markup-font-lock-keywords
  `((,(regexp-opt typst--global-keywords t) . typst-mode-keyword-face)
     ("#\\w+" . typst-mode-function-name-face)
     (,typst--markup-comment-regexp . font-lock-comment-face)
     ("\\*\\w+\\*" . typst-mode-markup-strong-face) ;; strong
     (,typst--markup-emphasis-regexp . typst-mode-markup-emphasis-face) ;; emphasized
     (,typst--markup-raw-text-regexp . typst-mode-markup-raw-text-face) ;; raw text
     (,typst--markup-link-regexp . typst-mode-markup-underline-face) ;; link
     (,typst--markup-label-regexp . typst-mode-markup-label-reference-face) ;; label
     (,typst--markup-reference-regexp . typst-mode-markup-label-reference-face) ;; reference
     ;; headings
     (,typst--markup-heading-1-regexp . typst-mode-markup-heading-1-face) ;; heading 1
     (,typst--markup-heading-2-regexp . typst-mode-markup-heading-2-face) ;; heading 2
     (,typst--markup-heading-3-regexp . typst-mode-markup-heading-3-face) ;; heading 3
     (,typst--markup-heading-4-regexp . typst-mode-markup-heading-4-face) ;; heading 4
     (,typst--markup-heading-5-regexp . typst-mode-markup-heading-5-face) ;; heading 5
     ;; do nothing to bullet list
     ;; do nothing to Numbered list
     (,typst--markup-term-list-regexp 1 typst-mode-markup-term-list-face) ;; term list
     )
  "Minimal highlighting expressions for typst mode")

(defvar typst--code-font-lock-keywords
  nil
  ;; `((
  ;;     ,(regexp-opt typst--global-keywords t) . typst-mode-keyword-face)
  ;;    ("#\\w+" . typst-mode-function-name-face)
  ;;    )
  "Minimal highlighting expressions for typst mode")


(defvar typst--math-font-lock-keywords
  nil
  "Minimal highlighting expressions for typst mode")

;;; Syntax tables ===============================================
(defvar typst--markup-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "." syntax-table) ;; change the default syntax entry for double quote(string quote character '"')
    (modify-syntax-entry ?\n "> b" syntax-table)
    syntax-table)
  "Syntax table for `typst--markup-mode'.")

(defvar typst--code-syntax-table
  (let ((syntax-table (make-syntax-table typst--markup-syntax-table)))
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?/ ". 124b" syntax-table)
    (modify-syntax-entry ?* ". 23" syntax-table)
    syntax-table)
  "Syntax table for `typst--code-mode'.")

;; (defvar typst--math-syntax-table
;;   (let ((syntax-table (make-syntax-table typst--markup-syntax-table)))
;;     (modify-syntax-entry ?\" "\"" syntax-table)
;;     (modify-syntax-entry ?/ ". 124b" syntax-table)
;;     (modify-syntax-entry ?* ". 23" syntax-table)
;;     syntax-table)
;;   "Syntax table for `typst--code-mode'.")

;;; Mode definition =========================================
(define-derived-mode typst--base-mode prog-mode "Typst"
  "Generic major mode for editing Typst files.

This is a generic major mode intended to be inherited by
concrete implementations.  Currently there are two concrete
implementations: `typst-mode' and `typst-ts-mode'."
  ;; :syntax-table typst-syntax-table
  (setq-local tab-width 4
    font-lock-keywords-only t))

(define-derived-mode typst--markup-mode typst--base-mode "Typst"
  "Major mode for editing Typst files.

\\{typst-mode-map}"
  :syntax-table typst--markup-syntax-table
  (setq-local font-lock-multiline nil
    font-lock-defaults '(typst--markup-font-lock-keywords)))

(define-derived-mode typst--code-mode typst--base-mode "Typst"
  "Major mode for editing Typst files.

\\{typst-mode-map}"
  :syntax-table typst--code-syntax-table
  (setq-local font-lock-multiline nil
    font-lock-defaults '(typst--code-font-lock-keywords)))

(define-derived-mode typst--math-mode typst--base-mode "Typst"
  "Major mode for editing Typst files.

\\{typst-mode-map}"
  ;; :syntax-table typst--code-syntax-table
  (setq-local font-lock-multiline nil
    font-lock-defaults '(typst--math-font-lock-keywords)))

(define-hostmode typst--poly-hostmode
  :mode 'typst--markup-mode)

(define-innermode typst--poly-code-innermode
  :mode 'typst--code-mode
  :head-matcher "[^\\]{"
  :tail-matcher "\\([^\\]}\\|^}\\)"
  :head-mode 'host
  :tail-mode 'host)

;; (define-innermode typst--poly-math-innermode
;;   :mode 'typst--math-mode
;;   ;; header-matcher: the first '$' on the line. 
;;   :head-matcher (cons (rx bol (* (not "$")) (group-n 1 "$") (* not-newline)) 1)
;;   ;; tail-matcher: the second '$' on the line 
;;   :tail-matcher "\\$"
;;   :head-mode 'host
;;   :tail-mode 'host)

;; ;;;###autoload
(define-polymode typst-mode
  :hostmode 'typst--poly-hostmode
  :innermodes '(typst--poly-code-innermode
                 ;; typst--poly-math-innermode
                 ))

;; TODO support treesit
;; (define-polymode typst-mode
;;   :hostmode 'poly-markup-hostmode
;;   :innermodes '())

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

(provide 'typst-mode)
;;; typst-mode.el ends here
