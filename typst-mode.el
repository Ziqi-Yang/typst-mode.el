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

;;; Faces ===================================================
(defface typst-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keyword."
  :group 'typst-mode-faces)

(defface typst-mode-markup-italic-face
  '((t :slant italic))
  "Face for builtins."
  :group 'typst-mode-markup-faces)

;; @ corresponding variables ============
(defvar typst-mode-keyword-face  'typst-mode-keyword-face
  "Face name to use for keywords.")


;;; Keywords ===============================================
(defvar typst-global-keywords
  '("#let" "#set" "#show" "#if" "#for" "#while" "#include" "#import")
  "Keywords for typst mode that are in the global scope.")

(defvar typst--markup-font-lock-keywords
  `((,(regexp-opt typst-global-keywords t) . typst-mode-keyword-face)
     ("#\\w+" . font-lock-function-name-face)
     )
  "Minimal highlighting expressions for typst mode")

(defvar typst--code-font-lock-keywords
  `((,(regexp-opt typst-global-keywords t) . typst-mode-keyword-face)
     ("#\\w+" . font-lock-function-name-face)
     )
  "Minimal highlighting expressions for typst mode")

;;; Syntax tables ===============================================
(defvar typst--markup-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "." syntax-table) ;; change the default syntax entry for double quote(string quote character '"')
    (modify-syntax-entry ?/ ". 124b" syntax-table)
    (modify-syntax-entry ?* ". 23" syntax-table)
    (modify-syntax-entry ?\n "> b" syntax-table)
    syntax-table)
  "Syntax table for `typst--markup-mode'.")

(defvar typst--code-syntax-table
    (let ((syntax-table (make-syntax-table typst--markup-syntax-table)))
        (modify-syntax-entry ?\" "\"" syntax-table)
        syntax-table)
    "Syntax table for `typst--code-mode'.")

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
        font-lock-defaults '(typst--markup-font-lock-keywords))
    )

(define-derived-mode typst--code-mode typst--base-mode "Typst"
    "Major mode for editing Typst files.

\\{typst-mode-map}"
    :syntax-table typst--code-syntax-table
    (setq-local font-lock-multiline nil
        font-lock-defaults '(typst--code-font-lock-keywords))
    )

(define-hostmode typst--poly-hostmode
  :mode 'typst--markup-mode)

(define-innermode typst--poly-innermode
    :mode 'typst--code-mode
    :head-matcher "[^\\]{"
    :tail-matcher "\\([^\\]}\\|^}\\)"
    :head-mode 'host
    :tail-mode 'host)

;; ;;;###autoload
(define-polymode typst-mode
    :hostmode 'typst--poly-hostmode
    :innermodes '(typst--poly-innermode))

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
