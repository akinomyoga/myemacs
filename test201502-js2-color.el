
(defvar mwg-js2x/original-js2-set-face)
(unless (fboundp 'mwg-js2x/original-js2-set-face)
  (fset 'mwg-js2x/original-js2-set-face (symbol-function 'js2-set-face))
  (defun js2-set-face (beg end face &optional record)
    (mwg-js2x/original-js2-set-face beg end face record)))

(defface js2-regexp
  '((((class color) (min-colors 88) (background light))
     (:foreground "forestgreen" :background "color-194" :inherit font-lock-string-face))
    (default :inherit font-lock-string-face))
  "Face used to highlight regular expressions"
  :version "24.1"
  :group 'js2-mode)
(defface js2-regexp-grouping-construct
  '((((class color) (min-colors 88) (background light))
     (:foreground "brightblue" :weight medium :inherit (font-lock-regexp-grouping-construct js2-regexp)))
    (default :inherit (font-lock-regexp-grouping-construct js2-regexp)))
  "Face used to highlight grouping constructs in regular expressions"
  :version "24.1"
  :group 'js2-mode)
(defface js2-regexp-quantifier
  '((((class color) (min-colors 88) (background light))
     (:foreground "brightred" :inherit (font-lock-builtin-face js2-regexp)))
    (default :inherit (font-lock-builtin-face js2-regexp)))
  "Face used to highlight quantifiers in regular expressions"
  :version "24.1"
  :group 'js2-mode)
(defface js2-regexp-character-class
  '((((class color) (min-colors 88) (background light))
     (:foreground "seagreen" :inherit (font-lock-type-face js2-regexp)))
    (default :inherit (font-lock-type-face js2-regexp)))
  "Face used to highlight the contant of a character class in regular expressions"
  :version "24.1"
  :group 'js2-mode)

(defconst mwg-js2x/rex-regex-elements
      (let* ((rex-atomic "(\\(?:\\?[!=:]\\)?\\|[)|^$]")
             ;; (rex-charclass-emacs "\\(\\[\\^?.\\(?:\\[:[a-zA-Z0-9_]+:\\]\\|[^]]\\)*\\]\\)")
             (rex-charclass "\\[\\^?\\(?:[^]\\]\\|\\\\.\\)*\\]")
             (rex-quantifier "\\(?:[*?+]\\|{[0-9]+\\(?:,[0-9]*\\)?}\\)\\??"))
        (concat "\\(" rex-atomic "\\)\\|\\(" rex-charclass "\\)\\|\\(" rex-quantifier "\\)\\|\\\\." )))
(defun js2-set-face (beg end face &optional record)
  (or (when (eq face 'font-lock-string-face)
        (cond
         ((or (= tt js2-DIV) (= tt js2-ASSIGN_DIV))
          ;; regexp
          (save-excursion
            (mwg-js2x/original-js2-set-face beg end 'js2-regexp record)
            (goto-char beg)
            (while (re-search-forward mwg-js2x/rex-regex-elements end t)
              (let ((b1 (match-beginning 0))
                    (e1 (match-end 0)))
                (cond
                 ((match-beginning 1)
                  ;;(mwg-js2x/original-js2-set-face b1 e1 'font-lock-builtin-face record))
                  (mwg-js2x/original-js2-set-face b1 e1 'js2-regexp-grouping-construct record))
                 ((match-beginning 2)
                  (mwg-js2x/original-js2-set-face b1 e1 'js2-regexp-character-class record)
                  (mwg-js2x/original-js2-set-face
                   b1 (+ b1 (if (= (char-after (1+ b1)) ?^) 2 1)) 'js2-regexp-grouping-construct record)
                  (mwg-js2x/original-js2-set-face
                   (- e1 1) e1 'js2-regexp-grouping-construct record))
                 ((match-beginning 3)
                  (mwg-js2x/original-js2-set-face b1 e1 'js2-regexp-quantifier record))
                 ))))
          t)))
      (mwg-js2x/original-js2-set-face beg end face record)))

;; (defun js2-set-face (beg end face &optional record)
;;   (mwg-js2x/original-js2-set-face beg end face record))
