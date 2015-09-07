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

;; python-docstring-mode.el is a minor mode for intelligently
;; reformatting (refilling) and highlighting Python docstrings. It
;; understands both epytext and Sphinx formats (even intermingled!),
;; so it knows how to reflow them correctly. It will also highlight
;; markup in your docstrings, including epytext and reStructuredText.

;;; Code:

(defvar python-docstring-script
  (concat (file-name-as-directory (file-name-directory #$))
          "docstring_wrap.py")
  "The location of the docstring_wrap.py script.")

(defun python-docstring-fill ()
  "Wrap Python docstrings as epytext or ReStructured Text."
  (interactive)
  (let ((fill-it-anyway nil))
    (catch 'not-a-string
      (let* ((to-forward
              (save-excursion
                (let* ((orig-point (point))
                       (syx (syntax-ppss))
                       (in-string (if (nth 3 syx) t
                                    (progn
                                      (setf fill-it-anyway t)
                                      (throw 'not-a-string nil))))
                       (string-start (+ (goto-char (nth 8 syx))
                                        3))
                       (rawchar (if (eql (char-before (point)) ?r)
                                    1
                                  0))
                       ;; at the beginning of the screen here
                       (indent-count (- (- string-start (+ rawchar 3))
                                        (save-excursion
                                          (beginning-of-line)
                                          (point))))
                       (string-end
                        (- (condition-case ()        ; for unbalanced quotes
                               (progn (forward-sexp)
                                      (point))
                             (error (point-max)))
                           3))
                       (orig-offset (- orig-point string-start)))
                  (let*
                      ((offset-within
                        (progn
                          (shell-command-on-region
                           string-start string-end
                           (format
                            "python %s --offset %s --indent %s --width %s"
                            (shell-quote-argument python-docstring-script)
                            orig-offset
                            indent-count
                            fill-column
                            )
                           :replace t)
                          (goto-char string-start)
                          (forward-sexp)
                          (string-to-number
                           (buffer-substring-no-properties string-start orig-point))
                          )))
                    (delete-region string-start (+ 1 (point)))
                    offset-within)))))
        (forward-char to-forward)))
    (if fill-it-anyway
        (call-interactively 'fill-paragraph))))

(defvar python-docstring-field-with-arg-re
  "^\\s-*\\([@:]\\)\\(param\\|parameter\\|arg\\|argument\\|type\\|keyword\\|kwarg\\|kwparam\\|raise\\|raises\\|except\\|exception\\|ivar\\|ivariable\\|cvar\\|cvariable\\|var\\|variable\\|type\\|group\\|todo\\|newfield\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_. ]*?\\)\\(:\\)")

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

(define-minor-mode python-docstring-mode
  "Toggle python-docstring-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 " DS"
 ;; The minor mode bindings.
 `(([(meta q)] . python-docstring-fill))
 ;; &rest BODY
 (if python-docstring-mode
     (font-lock-add-keywords nil python-docstring-keywords)
   (font-lock-remove-keywords nil python-docstring-keywords)))

(defun python-docstring-install ()
  "Add python-docstring-mode as a hook to python.mode."
  (add-hook 'python-mode-hook (lambda () (python-docstring-mode t))))

(provide 'python-docstring)

;;; python-docstring.el ends here
