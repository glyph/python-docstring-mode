
(defvar python-docstring-script
  (concat (file-name-as-directory (file-name-directory #$))
          "docstring_wrap.py")
  "The location of the ")

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
                       ;; at the beginning of the screen here
                       (indent-count (- (- string-start 3)
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
                            python-docstring-script
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

(defvar docstring-field-with-arg-re
  "^\\s-*[@:]\\(param\\|parameter\\|arg\\|argument\\|type\\|keyword\\|kwarg\\|kwparam\\|raise\\|raises\\|except\\|exception\\|ivar\\|ivariable\\|cvar\\|cvariable\\|var\\|variable\\|type\\|group\\|todo\\|newfield\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_. ]*?\\):")

(defvar docstring-field-no-arg-re
  "^\\s-*[@:]\\(raise\\|raises\\|return\\|returns\\|rtype\\|returntype\\|type\\|sort\\|see\\|seealso\\|note\\|attention\\|bug\\|warning\\|warn\\|version\\|todo\\|deprecated\\|since\\|status\\|change\\|changed\\|permission\\|requires\\|require\\|requirement\\|precondition\\|precond\\|postcondition\\|postcod\\|invariant\\|author\\|organization\\|org\\|copyright\\|(c)\\|license\\|contact\\|summary\\|params\\|param\\):")

(defvar docstring-epytext-markup-link "[UL]{\\([^}]*?\\)\\(<.*?>\\|\\)?}")
(defvar docstring-epytext-markup-style-code "C{\\(.*?\\)}")
(defvar docstring-epytext-markup-style-italic "I{\\(.*?\\)}")
(defvar docstring-epytext-markup-style-bold "B{\\(.*?\\)}")

;; hack for sphinx
(defvar docstring-sphinx-markup-link "\\(:[^:]+?:\\)\\(`.+?`\\)")
(defvar docstring-sphinx-markup-code "``\\(.+?\\)``")

(defvar docstring-keywords
  `((,docstring-field-with-arg-re 1 font-lock-type-face t)
    (,docstring-field-with-arg-re 2 font-lock-function-name-face t)
    (,docstring-field-no-arg-re 1 font-lock-type-face t)

    ;; :foo:`bar`
    (,docstring-sphinx-markup-link 1 font-lock-function-name-face t)
    (,docstring-sphinx-markup-link 2 font-lock-constant-face t)
    ;; ``bar``
    (,docstring-sphinx-markup-code 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,docstring-sphinx-markup-code 1 '(bold italic) t)

    ;; L/U - 1
    (,docstring-epytext-markup-link 0 font-lock-constant-face t)
    ;; Inline Markup - 1
    (,docstring-epytext-markup-link 1 font-lock-variable-name-face t)
    ;; Link - 2
    (,docstring-epytext-markup-link 2 font-lock-function-name-face t)

    ;; C/I/B - 0
    (,docstring-epytext-markup-style-code 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,docstring-epytext-markup-style-code 1 '(bold italic) t)
    ;; C/I/B - 0
    (,docstring-epytext-markup-style-bold 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,docstring-epytext-markup-style-bold 1 (quote bold) t)
    ;; C/I/B - 0
    (,docstring-epytext-markup-style-italic 0 font-lock-constant-face t)
    ;; inline markup - 1
    (,docstring-epytext-markup-style-italic 1 (quote italic) t)
    )
  )

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
     (font-lock-add-keywords nil docstring-keywords)
   (font-lock-remove-keywords nil docstring-keywords)))

(defun python-docstring-install ()
  (add-hook 'python-mode-hook (lambda () (python-docstring-mode t)))
  )

(provide 'python-docstring)
