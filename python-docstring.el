;;; python-docstring.el --- Smart Python docstring formatting

;; Copyright (c) 2014-2015 The Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; python-docstring-mode.el is a minor mode for intelligently reformatting
;; (refilling) and highlighting Python docstrings.  It understands both epytext
;; and Sphinx formats (even intermingled!), so it knows how to reflow them
;; correctly.  It will also highlight markup in your docstrings, including
;; epytext and reStructuredText.

;;; Code:

(require 'syntax)
(require 'python)
(require 'treesit nil t)                ;optional dependency

(defcustom python-docstring-sentence-end-double-space t
  "If non-nil, use double spaces when formatting text.

Operates simililarly to `sentence-end-double-space'.  When nil, a
single space is used."
  :type 'boolean
  :group 'python-docstring)

(defvar python-docstring-script
  (concat (if load-file-name
              (file-name-directory load-file-name)
            default-directory)
          "docstring_wrap.py")
  "The location of the docstring_wrap.py script.")

(defun python-docstring-bounds-with-ppss ()
  "Bounds the docstring by using parse-partial-sexp-state."
  (let* ((partial-sexp (syntax-ppss))

         (in-string (if (eq (syntax-ppss-context partial-sexp) 'string)
                        t
                      (progn (throw 'not-a-string 'not-a-string))))

         (string-start
          ;; The syntax tree starts the string syntax element on the final
          ;; opening quote. Move forward one character.
          (goto-char (+ 1 (nth 8 partial-sexp))))

         (string-end
          ;; We're looking at the beginning of the string. Back up by 3
          (- (let ((one-hop (progn (backward-char 3)
                                   (python-nav-forward-sexp)
                                   (point))))
               (if (equal one-hop (- string-start 2))
                   ;; python-nav-forward-sexp inside an expression (sometimes?)
                   ;; treats the first two quotes of a triple-quoted string as
                   ;; a string in its own right. We need to jump forward one
                   ;; more expression to get to the end of the string.
                   (progn (python-nav-forward-sexp)
                          (python-nav-forward-sexp)
                          (point))
                 one-hop))
             3)
          ))
    (list string-start string-end)))

(defun python-docstring-bounds-with-tree-sitter ()
  "Bounds the docstring by using tree-sitter.

Return and throw the same values as
`python-docstring-bounds-with-ppss'."

  (if (not (fboundp 'treesit-node-at))
      (error "In tree-sitter mode but tree-sitter is not loaded somehow"))
  (let* ((the-node (treesit-node-at (point)))
         (in-string (if (string-equal (treesit-node-type the-node) "string_content")
                        t
                      (progn (throw 'not-a-string 'not-a-string))))
         (string-start (treesit-node-start the-node))
         (string-end (treesit-node-end the-node)))
    (list string-start
          string-end)))

(defun python-docstring-run-script (string-start string-end orig-offset indent-count)
  "Run the script.

`STRING-START': the beginning of the docstring.
`STRING-END': the end of the docstring.
`ORIG-OFFSET': The original offset, in characters, of the user's cursor within
the string.
`INDENT-COUNT': the horizontal position of the opening quote on the first line
of the string."
  (shell-command-on-region
   string-start string-end
   (format
    (concat "python3 %s --offset %s --indent %s --width %s"
	    (unless python-docstring-sentence-end-double-space
	      " --single-space"))
    (shell-quote-argument python-docstring-script)
    orig-offset
    indent-count
    fill-column
    )
   :replace            ; output-buffer
   t            ; replace
   "*python-docstring-fill errors*" ; error-buffer
   )
  (goto-char string-start)
  (forward-word)
  (let ((offset-result
         (string-to-number
          (buffer-substring-no-properties string-start (point)))))
    ;; the script comes back with our offset as a decimal number at the start
    ;; of the output, clear that out of the buffer before we return.
    (delete-region string-start (+ 1 (point)))
    offset-result))

(defun python-docstring-figure-out-indentation ()
  "Determine the horizontal space before the start of the string literal.

Not literal whitespace / indentation -- as the string may itself
be inside an expression -- but rather horizontal position within
the line, the place that the filled whitespace should align with."

  (save-excursion
    (- (if (looking-back
            "\\(f\\|r\\|rf\\|fr\\|u\\|\\)\"\"\"" nil t)
           (goto-char (match-beginning 0))
         (throw 'not-a-string 'not-a-string))
       (progn (beginning-of-line)
              (point)))))

(defun python-docstring-calc-move ()
  "Wrap the docstring and determine relative cursor position.

Return the number of characters to move forward to retain the
same offset within the string."
  (save-excursion
    (let* ((orig-point (point))
           (bounds-result (if (eq major-mode 'python-ts-mode)
                              (python-docstring-bounds-with-tree-sitter)
                            (python-docstring-bounds-with-ppss)))
           (string-start (goto-char (car bounds-result)))
           (string-end (cadr bounds-result)))

      (python-docstring-run-script
       string-start string-end
       (- orig-point string-start)
       (python-docstring-figure-out-indentation)))))

;;;###autoload
(defun python-docstring-fill ()
  "Wrap Python docstrings as epytext or ReStructured Text."
  (interactive)
  (if (eq (catch 'not-a-string
            (let* ((to-forward (python-docstring-calc-move)))
              (forward-char to-forward)
              nil))
          'not-a-string)
      (call-interactively 'fill-paragraph)))

(defvar python-docstring-field-with-arg-re
  "^\\s-*\\([@:]\\)\\(param\\|parameter\\|arg\\|argument\\|type\\|keyword\\|kwarg\\|kwparam\\|raise\\|raises\\|except\\|exception\\|ivar\\|ivariable\\|cvar\\|cvariable\\|var\\|variable\\|type\\|group\\|todo\\|newfield\\)\\s-+\\(~*[a-zA-Z_][a-zA-Z0-9_,. ]*?\\)\\(:\\)")

(defvar python-docstring-field-no-arg-re
  "^\\s-*\\([@:]\\)\\(raise\\|raises\\|return\\|returns\\|rtype\\|returntype\\|type\\|sort\\|see\\|seealso\\|note\\|attention\\|bug\\|warning\\|warn\\|version\\|todo\\|deprecated\\|since\\|status\\|change\\|changed\\|permission\\|requires\\|require\\|requirement\\|precondition\\|precond\\|postcondition\\|postcod\\|invariant\\|author\\|organization\\|org\\|copyright\\|(c)\\|license\\|contact\\|summary\\|params\\|param\\)\\(:\\)")

(defvar python-docstring-epytext-markup-link "[UL]{\\([^}]*?\\)\\(<.*?>\\|\\)?}")
(defvar python-docstring-epytext-markup-style-code "C{\\(.*?\\)}")
(defvar python-docstring-epytext-markup-style-italic "I{\\(.*?\\)}")
(defvar python-docstring-epytext-markup-style-bold "B{\\(.*?\\)}")

;; hack for sphinx
(defvar python-docstring-sphinx-markup-link "\\(:[^:]+?:\\)\\(`.+?`\\)")
(defvar python-docstring-sphinx-markup-code "``\\(.+?\\)``")

(defvar python-docstring-keywords
  `((,python-docstring-field-with-arg-re 1 font-lock-keyword-face t)
    (,python-docstring-field-with-arg-re 2 font-lock-type-face t)
    (,python-docstring-field-with-arg-re 3 font-lock-variable-name-face t)
    (,python-docstring-field-with-arg-re 4 font-lock-keyword-face t)
    (,python-docstring-field-no-arg-re 1 font-lock-keyword-face t)
    (,python-docstring-field-no-arg-re 2 font-lock-type-face t)
    (,python-docstring-field-no-arg-re 3 font-lock-keyword-face t)

    ;; :foo:`bar`
    (,python-docstring-sphinx-markup-link 1 font-lock-function-name-face t)
    (,python-docstring-sphinx-markup-link 2 font-lock-constant-face t)
    ;; ``bar``
    (,python-docstring-sphinx-markup-code 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,python-docstring-sphinx-markup-code 1 '(bold italic) t)

    ;; L/U - 1
    (,python-docstring-epytext-markup-link 0 font-lock-constant-face t)
    ;; Inline Markup - 1
    (,python-docstring-epytext-markup-link 1 font-lock-function-name-face t)
    ;; Link - 2
    (,python-docstring-epytext-markup-link 2 font-lock-keyword-face t)

    ;; C/I/B - 0
    (,python-docstring-epytext-markup-style-code 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,python-docstring-epytext-markup-style-code 1 '(bold italic) t)
    ;; C/I/B - 0
    (,python-docstring-epytext-markup-style-bold 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,python-docstring-epytext-markup-style-bold 1 (quote bold) t)
    ;; C/I/B - 0
    (,python-docstring-epytext-markup-style-italic 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,python-docstring-epytext-markup-style-italic 1 (quote italic) t)))

;;;###autoload
(define-minor-mode python-docstring-mode
  "Toggle python-docstring-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
 ;; The initial value.
 :init-value nil
 ;; The indicator for the mode line.
 :lighter " DS"
 ;; The minor mode bindings.
 :keymap `(([(meta q)] . python-docstring-fill))
 ;; &rest BODY
 (if python-docstring-mode
     (font-lock-add-keywords nil python-docstring-keywords)
   (font-lock-remove-keywords nil python-docstring-keywords)))

;;;###autoload
(defun python-docstring-install ()
  "Add python-docstring-mode as a hook to python.mode."
  (add-hook 'python-mode-hook (lambda () (python-docstring-mode t))))

(provide 'python-docstring)

;;; python-docstring.el ends here
