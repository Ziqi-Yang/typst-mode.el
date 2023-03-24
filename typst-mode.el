;;; typst-mode.el --- major mode for working with typst markup-based typesetting system -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Adrien Brochard

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

(defvar typst-global-keywords
  '("#let" "#set" "#show" "#if" "#for" "#while" "#include" "#import")
  "Keywords for typst mode that are in the global scope.")

;; (defun typst-inner-face-match (limit)
;;   (catch 'found
;;     (while (re-search-forward "{\\([^}]*\\)}" limit t)
;;       (let ((text-start (match-beginning 1))
;;              (text-end (match-end 1)))
;;         (when (and text-start text-end)
;;           (set-match-data (list text-start text-end))
;;           (throw 'found t))))
;;     nil))

;; (defface typst-custom-face
;;   '((t (:foreground "green")))
;;   "Custom face for text inside braces."
;;   :group 'markup-faces)

(defvar typst-font-lock-keywords
  `((,(regexp-opt typst-global-keywords t) . font-lock-keyword-face)
     ("\\(#\\w+\\)[[(]" . '(1 font-lock-function-name-face))
     ;; (typst-inner-face-match . typst-custom-face)
     )
  "Minimal highlighting expressions for typst mode")

;;;###autoload
(define-derived-mode typst-mode prog-mode "Typst"
  "A major mode for editing the markup-based typesetting language."
  ;; :syntax-table typst-syntax-table
  (setq font-lock-defaults '(typst-font-lock-keywords)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-mode))

(provide 'typst-mode)
;;; typst-mode.el ends here
