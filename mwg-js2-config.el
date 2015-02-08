(provide 'mwg-js2-config)
(require 'mwg-js2-config)
(require 'js2-mode)

(defface js2-regexp
  '((((class color) (min-colors 88) (background light))
     (:foreground "forestgreen"
                  ;;:background "color-194"
                  :inherit font-lock-string-face))
    (default :inherit font-lock-string-face))
  "Face used to highlight regular expressions"
  :version "24.1"
  :group 'js2-mode)
(defface js2-regexp-grouping-construct
  '((((class color) (min-colors 88) (background light))
     (:foreground "brightblue" :weight normal :inherit (font-lock-regexp-grouping-construct js2-regexp)))
    (default :inherit (font-lock-regexp-grouping-construct js2-regexp)))
  "Face used to highlight grouping constructs in regular expressions"
  :version "24.1"
  :group 'js2-mode)
(defface js2-regexp-quantifier
  '((((class color) (min-colors 88) (background light))
     (:foreground "purple" :inherit (font-lock-builtin-face js2-regexp)))
    (default :inherit (font-lock-builtin-face js2-regexp)))
  "Face used to highlight quantifiers in regular expressions"
  :version "24.1"
  :group 'js2-mode)
(defface js2-regexp-character-class
  '((((class color) (min-colors 88) (background light))
     (:foreground "steelblue" :inherit (font-lock-type-face js2-regexp)))
    (default :inherit (font-lock-type-face js2-regexp)))
  "Face used to highlight the content of a character class in regular expressions"
  :version "24.1"
  :group 'js2-mode)

(defconst mwg-js2x/rex-regex-elements
      (let* ((rex-atomic "(\\(?:\\?[!=:]\\)?\\|[)|^$]")
             ;; (rex-charclass-emacs "\\(\\[\\^?.\\(?:\\[:[a-zA-Z0-9_]+:\\]\\|[^]]\\)*\\]\\)")
             (rex-charclass "\\[\\^?\\(?:[^]\\]\\|\\\\.\\)*\\]")
             (rex-quantifier "\\(?:[*?+]\\|{[0-9]*\\(?:,[0-9]*\\)?}\\)\\??"))
        (concat "\\(" rex-atomic "\\)\\|\\(" rex-charclass "\\)\\|\\(" rex-quantifier "\\)\\|\\\\." )))

;; text property syntax-table の値
;; [[Syntax Table Internals - GNU Emacs Lisp Reference Manual>https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html#Syntax-Table-Internals]]
;; 4 = open parenthesis
;; 5 = close parenthesis

(defvar mwg-js2x/original-js2-set-face)
(unless (fboundp 'mwg-js2x/original-js2-set-face)
  (fset 'mwg-js2x/original-js2-set-face (symbol-function 'js2-set-face))
  (defun js2-set-face (beg end face &optional record)
    (or (when (eq face 'font-lock-string-face)
          (cond
           ((and (boundp 'tt)
                 (or (= tt js2-DIV) (= tt js2-ASSIGN_DIV)))
            ;; 正規表現リテラル
            (save-excursion
              ;; auto-complete が正規表現リテラル内で起動しない様に font-lock-string-face を一緒に設定する。
              ;;   ac-disable-faces に一連の js2-regexp の類を登録するのは嫌なので。
              ;;   ※ac-cursor-on-disable-face-p の修正が必要。
              (mwg-js2x/original-js2-set-face beg end '(js2-regexp font-lock-string-face) record)

              ;; syntax-table の設定について
              ;;   js2-mode では正規表現内の括弧が括弧と認識されない様に syntax-code を上書きしている。
              ;;   この為に複雑な正規表現内での括弧の対応が highlight されない。
              ;;   ここでは正規表現内で括弧として機能している物を括弧と認識される様に syntax-code を自分で設定する事にする。
              ;;   文法的な括弧には '(4) '(5) を設定し、紛らわしいリテラルな括弧に '(2) を設定する事にする。
              ;;   但し、何故か分からないが一度でも syntax-table に '(2) を書き込むと後で上書きできない様なので
              ;;   "全体に '(2) を適用してから文法的な括弧に対して '(4) '(5) を適用する" という方針は断念した。
              (goto-char beg)
              (while (re-search-forward mwg-js2x/rex-regex-elements end t)
                (let ((b0 (match-beginning 0))
                      (e0 (match-end 0)))
                  (cond
                   ((match-beginning 1)
                    (mwg-js2x/original-js2-set-face b0 e0 '(js2-regexp-grouping-construct font-lock-string-face) record)
                    (if (= (char-after b0) ?\()
                        (js2-record-text-property b0 (1+ b0) 'syntax-table '(4))) ;; open paren
                    (if (= (char-after b0) ?\))
                        (js2-record-text-property b0 (1+ b0) 'syntax-table '(5)))) ;; close paren
                   ((match-beginning 2)
                    (mwg-js2x/original-js2-set-face b0 e0 '(js2-regexp-character-class font-lock-string-face) record)
                    (mwg-js2x/original-js2-set-face
                     b0 (+ b0 (if (= (char-after (1+ b0)) ?^) 2 1)) '(js2-regexp-grouping-construct font-lock-string-face) record)
                    (mwg-js2x/original-js2-set-face
                     (1- e0) e0 '(js2-regexp-grouping-construct font-lock-string-face) record)
                    (js2-record-text-property b0 (1+ b0) 'syntax-table '(4))
                    (js2-record-text-property (1+ b0) (1- e0) 'syntax-table '(2)) ;; word
                    (js2-record-text-property (1- e0) e0 'syntax-table '(5)))
                   ((match-beginning 3)
                    (mwg-js2x/original-js2-set-face b0 e0 '(js2-regexp-quantifier font-lock-string-face) record))
                   (t
                    (js2-record-text-property b0 e0 'syntax-table '(2)))))) ;; syntax-code is word for escaped characters

              ;; 後続の (js2-record-text-property px-pos end 'syntax-table '(2)) を無効にする為に範囲を潰す。
              ;; 初めは js2-record-text-property を上書きしようと思ったが defsubst だったのでできなかった。
              ;; 幸い px-pos end は let によるローカルな変数でもう使わない様だったのでこれを弄って握りつぶす。
              (when (boundp 'px-pos) (setq px-pos end)))
            t)
           ((and (boundp 'tt)
                 (= tt js2-STRING))
            ;; 文字列リテラル
            (save-excursion
              (mwg-js2x/original-js2-set-face beg end face record)
              (goto-char beg)
              (while (re-search-forward "\\\\\\(?:[tvfnrb\"'0\\]\\|u[a-fA-F0-9]\\{1,4\\}\\|x[a-fA-F0-9]\\{1,2\\}\\|u{[0-9a-fA-F]\\{1,8\\}}\\|\\(.\\)\\)" end t)
                
                (when (match-beginning 1)
                  (put-text-property (match-beginning 0) (match-end 0) 'help-echo "Invalid escape sequence")
                  (put-text-property (match-beginning 0) (match-end 0) 'point-entered #'js2-echo-error))

                (mwg-js2x/original-js2-set-face (match-beginning 0) (match-end 0)
                                                (if (match-beginning 1) '(js2-warning font-lock-string-face) '(font-lock-constant-face font-lock-string-face))
                                                record)))

            t)))
        (mwg-js2x/original-js2-set-face beg end face record))))

;;;###autoload
(defun mwg-js2-mode () (js2-mode))
