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
;; Package-Requires: (polymode)

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

;;; Variables ===============================================
(defcustom typst-markup-tab-width  4
  "Default tab width for typst markup mode"
  :type 'integer
  :group 'typst-mode)

(defcustom typst-code-tab-width  4
  "Default tab width for typst code mode"
  :type 'integer
  :group 'typst-mode)

(defcustom typst-indent-offset 4
  "Default indentation offset for Typst mode.")

(defcustom typst-buffer-name  "*typst-mode*"
  "The output buffer name for some compilation and preview command."
  :type 'string
  :group 'typst-mode)

(defcustom typst-executable-location  "typst"
  "The location for typst executable. If it is in your PATH, use just the name of the executable."
  :type 'string
  :group 'typst-mode)

(defun typst-pdf-preview-command-by-system ()
  (cond
   ((eq system-type 'darwin) "open %s") ;; mac os
   ((eq system-type 'gnu-linux) "xdg-open %s")
   ((eq system-type 'windows-nt) "start %s")))

(defcustom typst-pdf-preview-command (typst-pdf-preview-command-by-system)
  "Command to open/preview pdf. %s stand for the pdf file name."
  :type 'string
  :group 'typst-mode)

;;; Faces ===================================================
(defface typst-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keyword."
  :group 'typst-mode-faces)

(defface typst-mode-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face for operator"
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

;; @ markup mode
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

(defface typst-mode-markup-slash-face
  '((t :foreground "blue"))
  "Face for slash."
  :group 'typst-mode-markup-faces)


;; @ corresponding variables ============
(defvar typst-mode-keyword-face  'typst-mode-keyword-face
  "Face name to use for keywords.")

(defvar typst-mode-operator-face 'typst-mode-operator-face
  "Face name to use for operators.")

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

(defvar typst-mode-markup-slash-face  'typst-mode-markup-slash-face
  "Face name to use for slash.")

;;; Regexps & Keywords =======================================

;; @ base
(defconst typst--base-keywords
  '("let" "set" "show" "if" "for" "while" "include" "import")
  "Keywords for typst mode.")

(defconst typst--base-operators
  ;; 'not in' is composed by 'in' and 'not'
  ;; so as '==', '+=', '-=', '*=', '/='
  '("+" "-" "*" "/" "!=" "<" "<=" ">" ">=" "in" "not" "and" "or" "="))

;; symbol, string, function, arguments, module

(defconst typst--base-constants
  ;; integer, float, length, angle, ratio, fraction, color,
  '("none" "auto" "false" "true" ""))

;; @ code
(defconst typst--code-keywords-regexp
  ;; group 1
  (eval `(rx (or blank bol) (group-n 1 (or ,@typst--base-keywords)) (or blank eol)))
  "Keywords regexp for typst code mode")

(defconst typst--code-operators-regexp
  (eval `(rx blank (group-n 1 (or ,@typst--base-operators)) blank))
  "Operators regexp for typst code mode")

;; @ markup
(defconst typst--markup-keywords-regexp
  (let ((keywords (mapcar #'(lambda (keyword) (concat "#" keyword)) typst--base-keywords)))
    (eval `(rx (or blank bol) (group-n 1 (or ,@keywords)) (or blank eol))))
  "Keywords regexp for typst markup mode")

;; TODO 
(defconst typst--markup-else-keyword ;; else and else if
  (rx bol (*? not-newline) (syntax close-parenthesis)
    (* blank) (group-n 1 (or "else" (seq "else" (* blank) "if")))(* blank) (syntax open-parenthesis)))

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
  ;; don't include label
  (rx bol (* blank) "=" (1+ blank) (seq (? (not blank)) (* (not (or "\n" "<"))))))

(defconst typst--markup-heading-2-regexp
  ;; don't include label
  (rx bol (* blank) "==" (1+ blank) (seq (? (not blank)) (* (not (or "\n" "<"))))))

(defconst typst--markup-heading-3-regexp
  ;; don't include label
  (rx bol (* blank) "===" (1+ blank) (seq (? (not blank)) (* (not (or "\n" "<"))))))

(defconst typst--markup-heading-4-regexp
  ;; don't include label
  (rx bol (* blank) "====" (1+ blank) (seq (? (not blank)) (* (not (or "\n" "<"))))))

(defconst typst--markup-heading-5-regexp
  ;; don't include label
  (rx bol (* blank) "=====" (1+ blank) (seq (? (not blank)) (* (not (or "\n" "<"))))))

(defconst typst--markup-term-list-regexp
  (rx bol (* blank) "/" (1+ blank) (group-n 1 (1+ (not ":") )) ":" (* not-newline)))

(defconst typst--markup-term-list-regexp
  (rx bol (* blank) "/" (1+ blank) (group-n 1 (1+ (not ":") )) ":" (* not-newline)))

(defconst typst--markup-slash-regexp ;; for line break and escape character
  (rx "\\" (? (not blank))))

;; @ Keywords table
(defvar typst--markup-font-lock-keywords
  `((,typst--markup-keywords-regexp . typst-mode-keyword-face)
     (,typst--markup-else-keyword 1 typst-mode-keyword-face)
     ("#\\w+" . typst-mode-function-name-face)
     (,typst--markup-comment-regexp . typst-mode-comment-face)
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
     ;; do nothing with bullet list
     ;; do nothing with Numbered list
     (,typst--markup-term-list-regexp 1 typst-mode-markup-term-list-face) ;; term list
     (,typst--markup-slash-regexp . typst-mode-markup-slash-face) ;; slash(line break and escape character)
     ;; (,typst--base-if-statement-regexp . typst-mode-markup-underline-face) ;; slash(line break and escape character)
     )
  "Minimal highlighting expressions for typst mode")

(defvar typst--code-font-lock-keywords
  `((,typst--code-keywords-regexp 1 typst-mode-keyword-face)
     (,typst--code-operators-regexp . typst-mode-operator-face))
  "Minimal highlighting expressions for typst mode")

(defvar typst--math-font-lock-keywords
  nil
  "Minimal highlighting expressions for typst mode")

;;; Hooks ===================================================
(defvar typst-mode-hook nil)

;;; Syntax tables ===========================================
(defvar typst--base-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" "." syntax-table) ;; change the default syntax entry for double quote(string quote character '"')
  (modify-syntax-entry ?\n "> b" syntax-table)
  (modify-syntax-entry ?\( "(" syntax-table)
  (modify-syntax-entry ?\[ "(" syntax-table)
  (modify-syntax-entry ?\{ "(" syntax-table)
  (modify-syntax-entry ?\) ")" syntax-table)
  (modify-syntax-entry ?\] ")" syntax-table)
  (modify-syntax-entry ?\} ")" syntax-table)
  syntax-table)
  "Syntax table for `typst--markup-mode'.")

(defvar typst--markup-syntax-table
  (let ((syntax-table (make-syntax-table typst--base-syntax-table)))
    (modify-syntax-entry ?\\ "\\" syntax-table)
    syntax-table)
  "Syntax table for `typst--markup-mode'.")

(defvar typst--code-syntax-table
  (let ((syntax-table (make-syntax-table typst--base-syntax-table)))
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?/ ". 124b" syntax-table)
    (modify-syntax-entry ?* ". 23" syntax-table)
    syntax-table)
  "Syntax table for `typst--code-mode'.")

;; (defvar typst--math-syntax-table
;;   (let ((syntax-table (make-syntax-table typst--base-syntax-table)))
;;     (modify-syntax-entry ?\" "\"" syntax-table)
;;     (modify-syntax-entry ?/ ". 124b" syntax-table)
;;     (modify-syntax-entry ?* ". 23" syntax-table)
;;     syntax-table)
;;   "Syntax table for `typst--code-mode'.")

;;; Keymaps =================================================
(defvar typst-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'typst-compile)
    (define-key map (kbd "C-c C-p") 'typst-preview)
    (define-key map (kbd "C-c C-w") 'typst-toggle-watch)
    map))

;;; Indentation =============================================
(defun typst-indent-line ()
  "Indent current line in Typst mode.
(1)If current line is the beginning line of the buffer, then indentation -> 0;
Else [ (2)If the beginning of the visual part of the current line is close delimiter character -> indentation decreases,
      (3)Then check if the end of previous line contains an open delimiter character and there is no close delimiter after it -> indentation increases]"
  (interactive)
  ;; (message "%s" (current-indentation))
  (save-excursion
    (beginning-of-line)
    (let ((not-indented t)
           (cur-indent 0))
      (if (bobp) ;; (1)
        (progn (indent-line-to 0)
          (setq not-indented nil)))
      (when not-indented
        ;; (3)
        (save-excursion
          (forward-line -1)
          (setq cur-indent (current-indentation))
          ;; also works for complex scenario like {{}} [] () { (
          ;; func {(
          ;;   code
          ;; )}
          ;; hello (a: {
          ;;   value
          ;; }, b: {
          ;;   value
          ;; })
          (if (looking-at (rx (*? not-newline) (or (syntax open-parenthesis) "{") (* (not (syntax close-parenthesis))) eol))
            (setq cur-indent (+ cur-indent typst-indent-offset))))
        ;; (2)
        (if (looking-at (rx (* blank) (syntax close-parenthesis)))
          (setq cur-indent  (- cur-indent typst-indent-offset)))
        ;; (message cur-indent)
        (if (< cur-indent 0) ;; special cases
          (setq cur-indent 0))
        (indent-line-to cur-indent)))))

;;; Functions ===============================================
(defun typst--process-exists-p (process-name)
  "Return non-nil if a process with PROCESS-NAME is currently running."
  (let ((process-list (process-list)))
    (catch 'found
      (dolist (process process-list)
        (when (string= process-name (process-name process))
          (when (eq (process-status process) 'run)
            (throw 'found t)))))))

(defun typst-compile ()
  "Compile the current typst file using typst."
  (interactive)
  (compile compile-command))

(defun typst-preview ()
  "Preview the compiled pdf file."
  (interactive)
  (start-process-shell-command "typst preview" typst-buffer-name
    (format typst-pdf-preview-command (concat (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) ".pdf"))))

(defun typst-watch ()
  "Watch(real time compile & preview) the corresponding pdf file."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
         (watch-process-name "typst watch" ))
    (unless (typst--process-exists-p watch-process-name)
      (start-process-shell-command watch-process-name typst-buffer-name
        (format (format (concat typst-executable-location " -w %s %s") file-name (concat (file-name-sans-extension file-name) ".pdf")))))
    (typst-preview)))

(defun typst-stop-watch ()
  "Stop typst watch process"
  (interactive)
  (let ((watch-process-name "typst watch")
         (preview-process-name "typst preview"))
    (delete-process watch-process-name)
    ;; Note some command run outside emacs cannot be deleted (also the process only exists when calling it)
    (delete-process preview-process-name)))

(defun typst-toggle-watch()
  "Toggle tyspt watch."
  (interactive)
  (let ((watch-process-name "typst watch" )
         (preview-process-name "typst watch" ))
    (if (typst--process-exists-p watch-process-name)
      (typst-stop-watch)
      (typst-watch))))

;;; Mode definition =========================================
(define-derived-mode typst--base-mode text-mode "Typst"
  "Generic major mode for editing Typst files.

This is a generic major mode intended to be inherited by
concrete implementations.  Currently there are two concrete
implementations: `typst-mode' and `typst-ts-mode'."
  ;; :syntax-table typst-syntax-table
  (setq-local tab-width 4
    indent-line-function 'typst-indent-line
    tab-width typst-code-tab-width
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

(defconst typst--poly-code-head-multiple-line-keywords
  '("let" "set" "show" "if" "for" "while"))

(defconst typst--poly-code-head-single-line-keywords
  '("include" "import"))

;; Parentheses
(define-innermode typst--poly-code-parentheses-innermode
  ;; code mode inside { }
  :mode 'typst--code-mode
  :head-matcher (eval `(rx bol (* blank) "#" (or ,@typst--poly-code-head-multiple-line-keywords) (*? (not (or "\n" "\\"))) "(" (*? (not (or "{" "(" "[" ")"))) eol))
  :tail-matcher (rx (* blank) ")" (* blank) eol)
  :head-mode 'host
  :tail-mode 'host)

(define-innermode typst--poly-code-curly-brackets-innermode
  ;; code mode inside { }
  :mode 'typst--code-mode
  :head-matcher (eval `(rx bol (* blank) "#" (or ,@typst--poly-code-head-multiple-line-keywords) (*? (not (or "\n" "\\"))) "{" (*? (not (or "{" "(" "[" "}"))) eol))
  :tail-matcher (rx (* blank) "}" (* blank) eol)
  :head-mode 'host
  :tail-mode 'host)

(defun typst--find-unmarked-dollar (type &optional backward)
  "Search for the next unmarked `$` character, where the `math-head` and `math-end` text properties are not set.
If BACKWARD is non-nil, search backward instead of forward."
  (let ((search-fn (if backward 're-search-backward 're-search-forward))
         (opposite-search-fn (if backward 're-search-forward 're-search-backward))
         (search-pattern "\\$")
         (next-type-same-p t)
         (found nil))
    ;; only execute when (type=math-head) or (type=math-tail and backward=nil)
    (if (or (eq type 'math-head)
          (and (eq type 'math-tail) (eq backward nil)))
      (progn
        ;; first search backward to know the previous condition and set the count-need variable
        (save-excursion
          (funcall opposite-search-fn search-pattern nil t)
          (setq next-type-same-p
            ;; There shouldn't be a character that owns both of the math-head and math-tail property
            (let ((math-head (get-text-property (if backward (point) (1- (point) )) 'math-head))
                   (math-tail (get-text-property (if backward (point) (1- (point) )) 'math-tail)))
              (cond
                ((eq type 'math-head)
                  ;; tail: same, head/nil: different
                  (if (or math-tail
                        (and (not backward) (not math-head) (not math-tail)))
                    t))
                ((eq type 'math-tail)
                  (if math-head t))))))
        (while (and (not found)
                 (funcall search-fn search-pattern nil t))
          (let ((math-head (get-text-property (if backward (point) (1- (point) )) 'math-head))
                 (math-tail (get-text-property (if backward (point) (1- (point) )) 'math-tail)))
            (when (and (not math-head) (not math-tail))
              (cond
                ((eq type 'math-head)
                  (cond
                    ((and (not math-head) (not math-tail))
                      (if next-type-same-p
                        (setq found t)))
                    (math-head
                      (setq next-type-same-p nil))
                    (math-tail
                      (setq next-type-same-p t))))
                ((eq type 'math-tail)
                  (cond
                    ((and (not math-head) (not math-tail))
                      (if next-type-same-p
                        (setq found t)))
                    (math-head
                      (setq next-type-same-p t))
                    (math-tail
                      (setq next-type-same-p nil)))))))
          (setq next-type-same-p (not next-type-same-p)))
        (if found
          (if (> (point) 1)
            (progn
              (if backward
                (put-text-property (point) (1+ (point)) type t)
                (put-text-property (1- (point)) (point) type t))
              (1- (point) ))
            (progn
              (put-text-property 1 2 type t)
              1)
            ))))))
;; (typst--poly-math-find-head 1)
;; (typst--poly-math-find-tail)
;; $ $ $ $ $ $ $
;; (typst--poly-math-find-head -1)

(defun typst--poly-math-find-head (ahead)
  "See `pm-fun-matcher'."
  (let ((backward (if (< ahead 0) t)))
    (let ((the_point (typst--find-unmarked-dollar 'math-head backward)))
      (if the_point
        (progn
          (put-text-property the_point (1+ the_point) 'math-head t)
          (cons the_point (1+ the_point)))))))

(defun typst--poly-math-find-tail (&rest _args)
  (message "call find tail")
  (let ((the_point (typst--find-unmarked-dollar 'math-tail)))
    (if the_point
      (progn
        (put-text-property the_point (1+ the_point) 'math-tail t)
        (cons the_point (1+ the_point))))))

(define-innermode typst--poly-math-innermode
  :mode 'typst--math-mode
  ;; :head-matcher (cons (rx bol (* (not "$")) (group-n 1 "$") (* not-newline)) 1)
  ;; :head-matcher "\\$"
  ;; :tail-matcher "\\$"
  :head-matcher #'typst--poly-math-find-head
  :tail-matcher #'typst--poly-math-find-tail
  :head-mode 'host
  :tail-mode 'host)

;; ;;;###autoload
(define-polymode typst-mode
  :hostmode 'typst--poly-hostmode
  :innermodes '(typst--poly-code-curly-brackets-innermode
                 typst--poly-code-parentheses-innermode
                 ;; typst--poly-math-innermode ;; FIXME
                 )
  :keymap typst-mode-map
  nil
  "Major mode for editing Typst files.

\\{typst-mode-map}"
  (run-hooks 'typst-mode-hook)
  ;; set compile-command
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (setq-local compile-command
      (concat typst-executable-location
        " " file-name " " (concat (file-name-sans-extension file-name) ".pdf")))))

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
