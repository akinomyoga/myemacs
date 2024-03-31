;;; mwg-c++exp.el --- C++ Expression Manipulation for GNU Emacs
;; Filename: mwg-c++exp.el
;; Description: C++ Expression Manipulation
;; 
;; Author: Koichi Murase <myoga.murase@gmail.com>,
;;         Copyright (C) 2012, Koichi Murase, all rights reserved.
;; Created: 2012-03-09 16:42:43
;; Version: 1.0.8
;; Keywords: mwg-c++exp c++exp c++-expression
;; Compatibility: GNU Emacs 23 ~ 24
;;
;;---------------------------------------------------------------------------;;
;;  Myoga C++ Expression (mwg-c++exp.el)                                     ;;
;;---------------------------------------------------------------------------;;
;; Provided interactive functions:                                           ;;
;;                                                                           ;;
;;   (defun `mwg-forward-c++word' (&optional arg))                           ;;
;;   (defun `mwg-backward-c++word' (&optional arg))                          ;;
;;   (defun `mwg-forward-c++sexp' (&optional arg))                           ;;
;;   (defun `mwg-backward-c++sexp' (&optional arg))                          ;;
;;   (defun `mwg-transpose-c++words' (&optional arg))                        ;;
;;   (defun `mwg-backward-transpose-c++words' (&optional arg))               ;;
;;                                                                           ;;
;;---------------------------------------------------------------------------;;
;; ChangeLog                                                                 ;;
;;                                                                           ;;
;;   2013/08/05, v1.0.9, KM,                                                 ;;
;;     * bugfix: 先頭にコメントと見せかけて "::" が今夕していたのを除去      ;;
;;   2012/12/20, v1.0.8, KM,                                                 ;;
;;     * `mwg-backward-transpose-c++words'                                   ;;
;;     * binary-rhs/lhs-wise move in mwg-forward/backward-c++sexp            ;;
;;   2012/10/27, v1.0.6, KM,                                                 ;;
;;     * Added descriptions for some functions                               ;;
;;     * Added version information in comments                               ;;
;;   2012/03/15, v1.0.5, KM,                                                 ;;
;;     * Removed unused functions                                            ;;
;;     * `mwg-transpose-c++words'                                            ;;
;;   2012/03/10, v1.0.4, KM,                                                 ;;
;;     * Rewrited functions                                                  ;;
;;     * Replaced codes using tail recursions with tail-recursion-less code  ;;
;;   2012/03/10, v1.0.3, KM,                                                 ;;
;;     * Renamed constants                                                   ;;
;;   2012/03/09, v1.0.2, KM,                                                 ;;
;;     * Table for operator information                                      ;;
;;     * `mwg-forward-c++word'                                               ;;
;;     * `mwg-backward-c++word'                                              ;;
;;     * `mwg-forward-c++sexp'                                               ;;
;;     * `mwg-backward-c++sexp'                                              ;;
;;   2012/03/09, v1.0.1, KM,                                                 ;;
;;     * `mwg-forward-c++exp' (deprecated function)                          ;;
;;     * `mwg-backward-c++exp' (deprecated function)                         ;;
;;                                                                           ;;
;;---------------------------------------------------------------------------;;

(provide 'mwg-c++exp)
(require 'mwg-c++exp)

(defun mwg/rex-or (&rest args)
  (mapconcat 'identity args "\\|"))

(defconst mwg/rex-any "[^1-0]")
;; (defconst mwg/rex-any "\\(?:.\\|\\n\\)")

(defconst mwg-c++exp-pred::def/err 0)
(defconst mwg-c++exp-pred::def/end 1)
(defconst mwg-c++exp-pred::end0    2)
(defconst mwg-c++exp-pred::end1    3)
(defconst mwg-c++exp-pred::endW    4)
(defconst mwg-c++exp-pred::err     5)
(defconst mwg-c++exp-pred::nxt     6)

(defconst mwg-c++exp-until::ret-succ 0)
(defconst mwg-c++exp-until::ret-fail 1)
(defconst mwg-c++exp-until::ret-skip 2)
(defconst mwg-c++exp-until::ret-next 3)

(defvar mwg-c++exp-arg::prec 0)
(defvar mwg-c++exp-arg::isfwd t)
(defvar mwg-c++exp@@begin 0)
(defvar mwg-c++exp@@end   0)

;;-----------------------------------------------------------------------------
;;  forward-c++word / backward-c++word

(defconst mwg-c++exp/rex-scanner
  (let* ((s             "[[:space:][:cntrl:]]")
         (dqstring      "\"\\([^\"\\]\\|\\\\.\\)*\"")
         (sqstring      "'\\([^'\\]\\|\\\\.\\)*'")
         (numeral       "\\(0x[0-9a-fA-F]\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][+-]?[0-9]*\\)?\\)\\([fFuUlL]\\|[iI]64\\)*")
         (mlcomment     "/\\*\\([^*]\\|\\*+[^/*]\\)*\\*+/")
         (slcomment     "//[^\n]*\n?")
         (op            (concat "\\.\\*\\|\\.\\.\\.\\|::\\|#[#@]?"))
         (operator      (concat "\\(operator" s "*\\)?\\(\\+\\+\\|--\\|->\\*?\\|~\\)"))
         (operator_eq   (concat "\\(operator" s "*\\)?\\(&&?\\|<<?\\|>>?\\|||?\\|[-+*/%^!=]\\)=?"))
         (operator_word (concat "operator" s "+\\(new\\|delete\\|bitand\\|bitor\\|x?or\\|and\\|and_eq\\|x?or_eq\\|eq\\|not_eq\\|not\\|compl\\)\\b"))
         (operator_pair (concat "operator" s "*\\(?:\\[" s "*\\]\\|(" s "*)\\)"))
         (identifier    "[_[:alpha:]][_[:alnum:]]*")
         (space         (concat s "+"))
         (other         mwg/rex-any))
    (mwg/rex-or dqstring sqstring numeral
                mlcomment slcomment
                op operator operator_eq operator_word operator_pair
                identifier space other)))
(defconst mwg-c++exp/rex-scanner.trigraph
  (let* ((s             "[[:space:][:cntrl:]]")
         (trigraph-seq  (concat "\\?\\(?:[^?]\\|\\?+\\(?:[^?/]\\|/" mwg/rex-any "\\)\\)"))
         (dqstring      (concat "\"\\([^\"\\?]\\|\\\\" mwg/rex-any "\\|" trigraph-seq "\\)\\?*\""))
         (sqstring      (concat  "'\\([^'\\?]\\|\\\\"  mwg/rex-any "\\|" trigraph-seq "\\)\\?*'" ))
         (numeral       "\\(0x[0-9a-fA-F]\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][+-]?[0-9]*\\)?\\)\\([fFuUlL]\\|[iI]64\\)*")
         (mlcomment     "/\\*\\([^*]\\|\\*+[^/*]\\)*\\*+/")
         (slcomment     "//[^\n]*\n?")
         (op            (mwg/rex-or "\\.\\*" "\\.\\.\\." "::" "\\(?:#\\|\\?\\?=\\)\\(?:[#@]\\|\\?\\?=\\)?" "\\?\\?[/<>()]"))
         (operator      (concat "\\(operator" s "*\\)?\\(\\+\\+\\|--\\|->\\*?\\|~\\|\\?\\?-\\)"))
         (operator_eq   (concat "\\(operator" s "*\\)?\\(&&?\\|<<?\\|>>?\\|\\(?:|\\|\\?\\?!\\)\\(?:|\\|\\?\\?!\\)?\\|[-\\+*/%^!=]\\|\\?\\?'\\)=?"))
         (operator_word (concat "operator" s "+\\(?:new\\|delete\\|bitand\\|bitor\\|x?or\\|and\\|and_eq\\|x?or_eq\\|eq\\|not_eq\\|not\\|compl\\)\\b"))
         (operator_pair (concat "operator" s "*\\(?:\\(?:\\[\\|\\?\\?(\\)" s "*\\(?:\\?\\?)\\|\\]\\)\\|(" s "*)\\)"))
         (identifier    "[_[:alpha:]][_[:alnum:]]*")
         (space         (concat s "+"))
         (other         mwg/rex-any))
    (mwg/rex-or dqstring sqstring numeral
                mlcomment slcomment
                op operator operator_eq operator_word operator_pair
                identifier space other)))

(defconst mwg-c++exp/rex-scanner.trigraph1
  "\"\\([^\"\\\\\\?]\\|\\\\.\\|\\?\\?/.\\)*\"\\|'\\([^'\\\\]\\|\\\\.\\|\\?\\?/.\\)*'\\|\\(0x[0-9a-fA-F]\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][+-]?[0-9]*\\)?\\)\\([fFuUlL]\\|[iI]64\\)*\\|/\\*\\([^*]\\|\\*[^/]\\)*\\*/\\|//[^\n]*\n?\\|\\(operator[[:space:]\r\n]*\\)?\\(\\+\\+\\|--\\|->\\*?\\|\\.\\*\\|\\.\\.\\.\\|::\\|\\?\\?[=/<>()-]\\)\\|\\(operator[[:space:]\r\n]*\\)?\\(&&?\\|<<?\\|>>?\\|\\(|\\|\\?\\?!\\)\\(|\\|\\?\\?!\\)?\\|[-\\+*/%^!=]\\|\\?\\?'\\)=?\\|\\(operator[[:space:]\r\n]+\\(new\\|delete\\|bitand\\|bitor\\|x?or\\|and\\|and_eq\\|x?or_eq\\|eq\\|not_eq\\|not\\|compl\\)\\)\\|[_a-zA-Z][_a-zA-Z0-9]*\\|[[:space:]\r\n]+\\|.")

(defun mwg-c++exp-word::forward ()
  (if (/= (point) (point-max))
      (re-search-forward mwg-c++exp/rex-scanner)))
(defun mwg-c++exp-word::backward ()
  (let* ((pt0 (point))
         (pt1 pt0))
    (beginning-of-line)
    (if (= (point) pt0) (beginning-of-line 0))
    (while (< (point) pt0)
      (setq pt1 (point))
      (mwg-c++exp-word::forward))
    (goto-char pt1)))

(defun mwg-forward-c++word (&optional arg)
  (interactive "p")
  (let* ((wbeg (point))
         (wend (progn (mwg-c++exp-word::forward) (point)))
         (wlen (- wend wbeg)))
    (cond ((and (= wlen 2)
                (= (char-after wbeg) ?>)
                (or (= (char-before wend) ?=)
                    (= (char-before wend) ?>)))
           ;; whether ">=" (">>") can be split into two tokens
           (backward-char 1)
           (unless (mwg-c++exp-opdict::backward-rangle?)
             (forward-char 1)))
          ((and (= wlen 3)
                (= (char-after wbeg) ?>)
                (= (char-after (1+ wbeg)) ?>)
                (= (char-before wend) ?=))
           (backward-char 2)
           (unless (mwg-c++exp-opdict::backward-rangle?)
               ;; ">>=" cannot be split into ">" ">=" (nor ">" ">" "=")
               (forward-char 2)))
          )))
(defun mwg-backward-c++word (&optional arg)
  (interactive "p")
  (let* ((wbeg (point))
        (wend (progn (mwg-c++exp-word::backward) (point)))
        (wlen (- wbeg wend)))
    (cond ((and (= wlen 2)
                (= (char-after wend) ?>)
                (or (= (char-before wbeg) ?=)
                    (= (char-before wbeg) ?>)))
           ;; whether ">=" (">>") can be split into two tokens
           (if (mwg-c++exp-opdict::forward-rangle?)
               (forward-char 1)))
          ((and (= wlen 3)
                (= (char-after wend) ?>)
                (= (char-after (1+ wend)) ?>)
                (= (char-before wbeg) ?=))
           (forward-char 1)
           (if (mwg-c++exp-opdict::forward-rangle?)
               ;; ">>=" can be split into ">" ">" "="
               (forward-char 1)
             (unless  (mwg-c++exp-opdict::backward-rangle?)
               ;; ">>=" cannot be split into ">" ">="
               (backward-char 1))))
          )))

;;-----------------------------------------------------------------------------
;;  forward-c++sexp

(defun mwg-c++exp-until::forward-string (wlen)
  (let ((wbeg (point)))
    (buffer-substring wbeg (+ wbeg wlen))))
(defun mwg-c++exp-until::forward (predicator)
  (let ((ret mwg-c++exp-until::ret-fail))
    (while (= (setq ret (mwg-c++exp-until::forward-sub predicator))
               mwg-c++exp-until::ret-next))
    ret))
(defun mwg-c++exp-until::forward-sub (predicator)
  (let* ((ch (char-after))
         (wend (save-excursion (mwg-forward-c++word) (point)))
         (wlen (- wend (point)))
         (pred (funcall predicator wlen ch)))
    (cond
     ((= pred mwg-c++exp-pred::end0)
      mwg-c++exp-until::ret-succ)
     ((= pred mwg-c++exp-pred::endW)
      (goto-char wend)
      mwg-c++exp-until::ret-succ)
     ((= pred mwg-c++exp-pred::end1)
      (forward-char 1)
      mwg-c++exp-until::ret-succ)
     ((= pred mwg-c++exp-pred::err)
      mwg-c++exp-until::ret-fail)
     ((= pred mwg-c++exp-pred::nxt)
      (goto-char wend)
      mwg-c++exp-until::ret-next)
     ((or (= wlen 0)
          (and (= wlen 1)
               (or (= ch ?\)) (= ch ?\]) (= ch ?\}) (= ch ?\:) (= ch ?\;)))
          (and (= wlen 3)
               (let ((w (mwg-c++exp-until::backward-string wlen)))
                 (or (string= w "??)") (string= w "??>")))))
      (if (= pred mwg-c++exp-pred::def/end)
          mwg-c++exp-until::ret-succ
        mwg-c++exp-until::ret-fail))
     (t
      (let ((ret (mwg-c++exp-until::forward-nest)))
        (cond
         ;; case that balanced brackets were parsed
         ((= ret mwg-c++exp-until::ret-succ)
          mwg-c++exp-until::ret-next)
         ;; case that it is not a left bracket
         ((= ret mwg-c++exp-until::ret-skip)
          (goto-char wend)
          mwg-c++exp-until::ret-next)
         ;; case that it seems an inequality but a left angle bracket
         ((and (= wlen 1) (= ch ?<))
          (mwg-forward-c++word)
          mwg-c++exp-until::ret-next)
         ;; case that there is no matching right bracket
         (t
          mwg-c++exp-until::ret-fail)
         )))
     )))

(defun mwg-c++exp-until::forward-nest ()
  "Move forward to the closing bracket
which corresponds to the opening bracket on the current point.

1) If there is a opening bracket on the current point,
   and looking up the corresponding closing bracket was succeeded,
   it returns `mwg-c++exp-until::ret-succ'.
2) If looking up the closing bracket was failed
   although there is a opening bracket on the current point,
   it returns `mwg-c++exp-until::ret-fail'.
3) If there are no opening bracket on the current point,
   it just returns `mwg-c++exp-until::ret-skip'."
  (let* ((point0 (point))
         (wend (save-excursion (mwg-forward-c++word) (point)))
         (wlen (- wend (point)))
         (ret (cond
               ((= wlen 1)
                (let ((ch (char-after)))
                  (cond ((= ch ?\()
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-paren))
                        ((= ch ?\[)
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-brack))
                        ((= ch ?\{)
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-brace))
                        ((= ch ?\?)
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-ternary))
                        ((= ch ?\<)
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-angle))
                        (t mwg-c++exp-until::ret-skip))))
               ((= wlen 3)
                (let ((w (mwg-c++exp-until::forward-string wlen)))
                  (cond ((string= w "??(")
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-brack))
                        ((string= w "??<")
                         (goto-char wend)
                         (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-brace))
                        (t mwg-c++exp-until::ret-skip))))
               (t mwg-c++exp-until::ret-skip))))
    (if (/= ret mwg-c++exp-until::ret-succ)
        (goto-char point0))
    ret
    ))
(defun mwg-forward-c++sexp (&optional arg)
  (interactive "p")
  (or (= mwg-c++exp-until::ret-succ (mwg-c++exp-until::forward-nest))
      (mwg-c++exp-goto/forward-binary-rhs)
      (mwg-forward-c++word)))

(defun mwg-c++exp-pred::forward-paren (wlen ch)
  (if (= wlen 1)
      (cond ((= ch ?\))
             mwg-c++exp-pred::endW)
            ((= ch ?\;) ;; OK. e.g. for(;;);
             mwg-c++exp-pred::nxt)
            (t
             mwg-c++exp-pred::def/err))
    mwg-c++exp-pred::def/err))
(defun mwg-c++exp-pred::forward-brace (wlen ch)
  (cond ((= wlen 1)
         (cond ((= ch ?\})
                mwg-c++exp-pred::endW)
               ((or (= ch ?\;) (= ch ?\:)) ;; OK. e.g., {label:;}
                mwg-c++exp-pred::nxt)
               (t
                mwg-c++exp-pred::def/err)))
        ((and (= wlen 3) (string= (mwg-c++exp-until::forward-string wlen) "??>"))
         mwg-c++exp-pred::endW)
        (t
         mwg-c++exp-pred::def/err)))
(defun mwg-c++exp-pred::forward-brack (wlen ch)
  (cond ((= wlen 1)
         (cond ((= ch ?\])
                mwg-c++exp-pred::endW)
               (t
                mwg-c++exp-pred::def/err)))
        ((and (= wlen 3) (string= (mwg-c++exp-until::forward-string wlen) "??)"))
         mwg-c++exp-pred::endW)
        (t
         mwg-c++exp-pred::def/err)))
(defun mwg-c++exp-pred::forward-ternary (wlen ch)
  (if (= wlen 1)
      (cond ((= ch ?\:)
             mwg-c++exp-pred::endW)
            (t
             mwg-c++exp-pred::def/err))
    mwg-c++exp-pred::def/err))
(defun mwg-c++exp-pred::forward-angle (wlen ch)
  (cond ((= wlen 1)
         (cond ((= ch ?\>)
                mwg-c++exp-pred::endW)
               (t
                mwg-c++exp-pred::def/err)))
        ((or (= wlen 2) (= wlen 3))
         (let ((tok (mwg-c++exp-until::forward-string wlen)))
           (cond ((or (string= tok ">=")
                      (string= tok ">>")
                      (string= tok ">>="))
                  mwg-c++exp-pred::end1)
                 (t
                  mwg-c++exp-pred::def/err))))
        (t mwg-c++exp-pred::def/err)))

;;-----------------------------------------------------------------------------
;;  backward-c++sexp

(defun mwg-c++exp-until::backward-string (wlen)
  (let ((wbeg (point)))
    (buffer-substring (- wbeg wlen ) wbeg)))
(defun mwg-c++exp-until::backward (predicator)
  "Move point backward to the end reported by the specified backward
predicator.  The backward predicator is specified by `PREDICATOR'.  The `point'
is set to the start point of the searching.

  The predicator tests whether the current word is the destination point or
not, and gives the information on the destination point through its return
value:

1) `mwg-c++exp-pred::end0' indicates the end of the current word is the
   destination.
2) `mwg-c++exp-pred::endW' indicates the beginning of the current word is the
   destination.
3) `mwg-c++exp-pred::end1' indicates the last character of the current word is
   the destination.
4) `mwg-c++exp-pred::err' indicates that the searching for the destination
   failed.
5) `mwg-c++exp-pred::nxt' indicates that the current word is not yet the
   destination.
6) `mwg-c++exp-pred::def/end' is the default value for the current word.

If the current word is a opening parenthesis, it is the same as the
`mwg-c++exp-pred::end0'.  If the current word is a closing parenthesis and the
corresponding opening parenthesis is found, the point will be moved to the
beginning of the opening parenthesis and the search will be continued.
Otherwise, it is same as the `mwg-c++exp-pred::nxt'."
  (let ((ret mwg-c++exp-until::ret-fail))
    (while (= (setq ret (mwg-c++exp-until::backward-sub predicator))
               mwg-c++exp-until::ret-next))
    ret))

(defun mwg-c++exp-until::backward-sub (predicator)
  "Helper function of `mwg-c++exp-until::backward'."
  (let* ((ch (char-before))
         (wend (save-excursion (mwg-backward-c++word) (point)))
         (wlen (- (point) wend))
         (pred (funcall predicator wlen ch)))
    (cond
     ((= pred mwg-c++exp-pred::end0)
      mwg-c++exp-until::ret-succ)
     ((= pred mwg-c++exp-pred::endW)
      (goto-char wend)
      mwg-c++exp-until::ret-succ)
     ((= pred mwg-c++exp-pred::end1)
      (backward-char 1)
      mwg-c++exp-until::ret-succ)
     ((= pred mwg-c++exp-pred::err)
      mwg-c++exp-until::ret-fail)
     ((= pred mwg-c++exp-pred::nxt)
      (goto-char wend)
      mwg-c++exp-until::ret-next)
     ((or (= wlen 0)
          (and (= wlen 1)
               (or (= ch ?\() (= ch ?\[) (= ch ?\{) (= ch ?\?) (= ch ?\;)))
          (and (= wlen 3)
               (let ((w (mwg-c++exp-until::backward-string wlen)))
                 (or (string= w "??(") (string= w "??<")))))
      (if (= pred mwg-c++exp-pred::def/end)
          mwg-c++exp-until::ret-succ
        mwg-c++exp-until::ret-fail))
     (t
      (let ((ret (mwg-c++exp-until::backward-nest)))
        (cond
         ;; case that balanced brackets were parsed
         ((= ret mwg-c++exp-until::ret-succ)
          mwg-c++exp-until::ret-next)
         ;; case that it is not a left bracket
         ((= ret mwg-c++exp-until::ret-skip)
          (goto-char wend)
          mwg-c++exp-until::ret-next)
         ;; case that it seems an inequality/shift but a left angle bracket
         ((= ch ?>) ;; >, >=, >>, >>=
          (mwg-backward-c++word)
          mwg-c++exp-until::ret-next)
         ;; case that there is no matching right bracket
         (t
          mwg-c++exp-until::ret-fail)
         )))
     )))
(defun mwg-c++exp-until::backward-nest ()
  "Returns: 0=succeeded, 1=failed, 2=skipped"
  (let* ((point0 (point))
         (wend (save-excursion (mwg-backward-c++word) (point)))
         (wlen (- (point) wend))
         (ret (cond
               ((= wlen 1)
                (let ((ch (char-before)))
                  (cond ((= ch ?\))
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-paren))
                        ((= ch ?\])
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-brack))
                        ((= ch ?\})
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-brace))
                        ((= ch ?\:)
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-ternary))
                        ((= ch ?\>)
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-angle))
                        (t 2))))
               ((= wlen 2)
                (let ((w (mwg-c++exp-until::backward-string wlen)))
                  (cond ((string= w ">>")
                         (backward-char 1)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-angle))
                        (t 2))))
               ((= wlen 3)
                (let ((w (mwg-c++exp-until::backward-string wlen)))
                  (cond ((string= w "??)")
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-brack))
                        ((string= w "??>")
                         (goto-char wend)
                         (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-brace))
                        (t 2))))
               (t 2))))
    (if (/= ret 0) (goto-char point0))
    ret
    ))
(defun mwg-backward-c++sexp (&optional arg)
  (interactive "p")
  (or (= mwg-c++exp-until::ret-succ (mwg-c++exp-until::backward-nest))
      (mwg-c++exp-goto/backward-binary-lhs)
      (mwg-backward-c++word)))

(defun mwg-c++exp-pred::backward-paren (wlen ch)
  "Backward predicator for the opening parenthesis \\='(\\='."
  (if (= wlen 1)
      (cond ((= ch ?\()
             mwg-c++exp-pred::endW)
            ((= ch ?\;) ;; OK
             mwg-c++exp-pred::nxt)
            (t
             mwg-c++exp-pred::def/err))
    mwg-c++exp-pred::def/err))
(defun mwg-c++exp-pred::backward-brace (wlen ch)
  "Backward predicator for the opening brace \\='{\\='."
  (cond ((= wlen 1)
         (cond ((= ch ?\{)
                mwg-c++exp-pred::endW)
               ((or (= ch ?\;) (= ch ?\:)) ;; OK
                mwg-c++exp-pred::nxt)
               (t
                mwg-c++exp-pred::def/err)))
        ((and (= wlen 3) (string= (mwg-c++exp-until::backward-string wlen) "??<"))
         mwg-c++exp-pred::endW)
        (t
         mwg-c++exp-pred::def/err)))
(defun mwg-c++exp-pred::backward-brack (wlen ch)
  "Backward predicator for the opening bracket \\='[\\='."
  (cond ((= wlen 1)
         (cond ((= ch ?\[)
                mwg-c++exp-pred::endW)
               (t
                mwg-c++exp-pred::def/err)))
        ((and (= wlen 3) (string= (mwg-c++exp-until::backward-string wlen) "??("))
         mwg-c++exp-pred::endW)
        (t
         mwg-c++exp-pred::def/err)))
(defun mwg-c++exp-pred::backward-ternary (wlen ch)
  "Backward predicator for the first part of a ternary operator \\='?\\='."
  (cond ((= wlen 1)
         (cond ((or (= ch ?\?))
                mwg-c++exp-pred::endW)
               ;; ((or (= ch ?\;) (= ch ?\{)
               ;;      (= ch ?\:) ;; (a?b?1:2:3) nado
               ;;      ) ;; as label
               ;;  mwg-c++exp-pred::err)
               (t
                mwg-c++exp-pred::def/err)))
        ((and (= wlen 3) (string= (mwg-c++exp-until::backward-string wlen) "??<"))
         mwg-c++exp-pred::endW)
        (t
         mwg-c++exp-pred::def/err)))
(defun mwg-c++exp-pred::backward-angle (wlen ch)
  "Backward predicator for the opening-angle bracket \\='<\\='."
  (cond ((= wlen 1)
         (cond ((= ch ?\<)
                mwg-c++exp-pred::endW)
               (t
                mwg-c++exp-pred::def/err)))
        (t
         mwg-c++exp-pred::def/err)))

;;-----------------------------------------------------------------------------
;;  c++exp-opdict

(defun mwg-c++exp-opdict::prefix? ()
  (save-excursion
    (if (not mwg-c++exp-arg::isfwd) (mwg-backward-c++word))
    (mwg-transpose-c++words::skip-bwd-space)
    (let* ((ch (char-before))
           (wend (save-excursion (mwg-backward-c++word) (point)))
           (wlen (- (point) wend))
           (oflag (mwg-c++exp-opdict::backward-get wlen ch))
           (otype (% oflag 10)))
      (or (= wlen 0)
          (= otype 1) (= otype 2) (= otype 4)
          (and (= wlen 1)
               (or (= ch ?\;) (= ch ?\:) (= ch ?\})))
          (and (= wlen 3) (= ch ?\>)
               (string= (mwg-c++exp-until::backward-string wlen) "??>"))))
    ))

(defun mwg-c++exp-opdict::forward-langle? ()
  (save-excursion
    (and (= (char-after) ?<)
         (= (mwg-c++exp-until::forward-nest) mwg-c++exp-until::ret-succ))))
(defun mwg-c++exp-opdict::backward-langle? ()
  (save-excursion
    (cond ((and (= (char-before) ?<)
                (let ((pt1 (point)))
                  (mwg-backward-c++word)
                  (= (point) (- pt1 1))))
           (= (mwg-c++exp-until::forward-nest) mwg-c++exp-until::ret-succ))
          (t nil))))
(defun mwg-c++exp-opdict::langle? ()
  (if mwg-c++exp-arg::isfwd
      (mwg-c++exp-opdict::forward-langle?)
    (mwg-c++exp-opdict::backward-langle?)))

(defun mwg-c++exp-opdict::backward-rangle? ()
  (save-excursion
    (and (= (char-before) ?>)
         (= (mwg-c++exp-until::backward-nest) mwg-c++exp-until::ret-succ))))
(defun mwg-c++exp-opdict::forward-rangle? ()
  (save-excursion
    (cond ((= (char-after) ?>)
           (forward-char 1)
           (= (mwg-c++exp-until::backward-nest) mwg-c++exp-until::ret-succ))
          (t nil))))
(defun mwg-c++exp-opdict::rangle? ()
  (if mwg-c++exp-arg::isfwd
      (mwg-c++exp-opdict::forward-rangle?)
    (mwg-c++exp-opdict::backward-rangle?)))

(defun mwg-c++exp-opdict::backward-rternary? ()
  (save-excursion
    (and (= (char-before) ?:)
         (= (mwg-c++exp-until::backward-nest) mwg-c++exp-until::ret-succ))))
(defun mwg-c++exp-opdict::forward-rternary? ()
  (save-excursion
    (cond ((= (char-after) ?:)
           (forward-char 1)
           (= (mwg-c++exp-until::backward-nest) mwg-c++exp-until::ret-succ))
          (t nil))))
(defun mwg-c++exp-opdict::rternary? ()
  (if mwg-c++exp-arg::isfwd
      (mwg-c++exp-opdict::forward-rternary?)
    (mwg-c++exp-opdict::backward-rternary?)))

(defvar mwg-c++exp-opdict (make-hash-table :test 'equal))
(let* (push
       (push (lambda (expr name &rest names)
               (puthash name expr mwg-c++exp-opdict)
               (if names (apply 'funcall (cons push (cons expr names)))))))
  (funcall push 11 "::")
  (funcall push 21 ".")
  (funcall push 21 "->")
  (funcall push 24 "(") (funcall push 25 ")") ;; TODO: if() switch() for() while() 224 225
  (funcall push 24 "[" "??(") (funcall push 25 "]" "??)")
  (funcall push 32 "!" "not")
  (funcall push 32 "~" "??-" "compl")
  (funcall push '(if (mwg-c++exp-opdict::prefix?) 32 23) "++")
  (funcall push '(if (mwg-c++exp-opdict::prefix?) 32 23) "--")
  (funcall push 32 "new")
  (funcall push 32 "delete")
  (funcall push 32 "sizeof")
  (funcall push 41 ".*")
  (funcall push 41 "->*")
  (funcall push 51 "/") (funcall push 51 "%")
  (funcall push '(if (mwg-c++exp-opdict::prefix?) 32 51) "*")
  (funcall push '(if (mwg-c++exp-opdict::prefix?) 32 61) "+")
  (funcall push '(if (mwg-c++exp-opdict::prefix?) 32 61) "-")
  (funcall push 71 "<<")
  (funcall push 71 ">>")
  (funcall push 81 "<=")
  (funcall push 81 ">=")
  (funcall push '(if (mwg-c++exp-opdict::langle?) 4 81) "<")
  (funcall push '(if (mwg-c++exp-opdict::rangle?) 5 81) ">")
  (funcall push 91 "==" "eq")
  (funcall push 91 "!=" "not_eq")
  (funcall push '(if (mwg-c++exp-opdict::prefix?) 32 101) "&" "bitand")
  (funcall push 111 "^" "??'" "xor")
  (funcall push 121 "|" "??!" "bitor")
  (funcall push 131 "&&" "and")
  (funcall push 141 "||" "??!|" "|??!" "??!??!" "or")
  (funcall push 154 "?")
  (funcall push '(if (mwg-c++exp-opdict::rternary?) 155 201) ":")
  (funcall push 161 "=")
  (funcall push 161 "+=") (funcall push 161 "-=") (funcall push 161 "*=")
  (funcall push 161 "/=") (funcall push 161 "%=")
  (funcall push 161 "&=" "and_eq")
  (funcall push 161 "^=" "??'=" "xor_eq")
  (funcall push 161 "|=" "??!=" "or_eq")
  (funcall push 161 "<<=")
  (funcall push 161 ">>=")
  (funcall push 161 "&&=") ;; omake
  (funcall push 161 "||=" "??!|=" "|??!=" "??!??!=") ;; omake
  (funcall push 172 "throw")
  (funcall push 181 ",")
  (funcall push 224 "{" "??<") (funcall push 225 "}" "??>")
  ;; do a+=b;while(0);
  ;; if(0);else a+=b;
  ;; return a+=b;
  ;; goto x::y;
  (funcall push 222 "do" "else" "return" "goto"))

(defun mwg-c++exp-opdict::get (wlen ch)
  "oflag%10 is the type of the operator.
     1 = binary operator;
     2 = prefix operator;
     3 = suffix operator;
     4 = suffix left bracket;
     5 = suffix right bracket.
   oflag/10 is the precedence of the operator.
     Smaller the value is, stronger the precedence is."
  (if (and (<= 1 wlen) (<= wlen 7))
      (let* ((w (if mwg-c++exp-arg::isfwd
                    (mwg-c++exp-until::forward-string wlen)
                  (mwg-c++exp-until::backward-string wlen)))
             (expr (gethash w mwg-c++exp-opdict)))
        (if expr (eval expr) 0))
    0))

(defun mwg-c++exp-opdict::get-from-region (wbeg wend)
  (or (if (<= (- wend wbeg) 7)
          (let* ((w (buffer-substring wbeg wend))
                 (expr (gethash w mwg-c++exp-opdict)))
            (if expr
                (save-excursion
                  (let ((mwg-c++exp-arg::isfwd t))
                    (goto-char wbeg)
                    (eval expr))))))
      0))
(defun mwg-c++exp-opdict::forward-get (wlen ch)
  (let ((mwg-c++exp-arg::isfwd t))
    (mwg-c++exp-opdict::get wlen ch)))
(defun mwg-c++exp-opdict::backward-get (wlen ch)
  (let ((mwg-c++exp-arg::isfwd nil))
    (mwg-c++exp-opdict::get wlen ch)))

(defun mwg-c++exp-pred::forward-opprec (wlen ch)
  "Forward predicator to determine whether current word is the end point or
not, using the specified operator precedence.

  The operator precidence is specified by `mwg-c++exp-arg::prec', and the
`WLEN' is the length of the current word, and the `CH' is the first character
of the current word, and the current `point' must be placed before the current
word.

  This function returns `mwg-c++exp-pred::end0' when the current word is end
point, otherwise it returns `mwg-c++exp-pred::def/end'."
  (if (mwg-c++exp-opdict::forward-rangle?)
      mwg-c++exp-pred::end0
    (let ((prec (mwg-c++exp-opdict::forward-get wlen ch)))
      (if (<= mwg-c++exp-arg::prec prec)
          mwg-c++exp-pred::end0
        mwg-c++exp-pred::def/end))))

(defun mwg-c++exp-pred::backward-opprec (wlen ch)
  "Backward predicator to determine whether current word is the end point or
not, using the specified operator precedence.

  The operator precidence is specified by `mwg-c++exp-arg::prec', and the
`WLEN' is the length of the current word, and the `CH' is the last character of
the current word, and the current `point' must be placed after the current
word.

  This function returns `mwg-c++exp-pred::end0' when the current word is end
point, otherwise it returns `mwg-c++exp-pred::def/end'."
  (if (mwg-c++exp-opdict::backward-langle?)
      mwg-c++exp-pred::end0
    (let ((prec (mwg-c++exp-opdict::backward-get wlen ch)))
      (if (<= mwg-c++exp-arg::prec prec)
          mwg-c++exp-pred::end0
        mwg-c++exp-pred::def/end))))

;;-----------------------------------------------------------------------------

(defun mwg-c++exp-transpose-region (start1 end1 start2 end2)
  "Swap contents of two specified regions.
`START1' specifies the first point of the first region,
and `END1' specifies the end point of the first region.
`START2' specifies the first point of the first region,
and `END2' specifies the end point of the first region."
  (let ((word1 (buffer-substring start1 end1))
        (word2 (buffer-substring start2 end2)))
    (goto-char start2)
    (delete-region start2 end2)
    (insert word1)
    (goto-char start1)
    (delete-region start1 end1)
    (insert word2)))

(defun mwg-transpose-c++words::skip-fwd-space ()
  (re-search-forward "[[:space:]\r\n]*"))
(defun mwg-transpose-c++words::skip-bwd-space ()
  (if (re-search-backward "[^[:space:]\r\n]" nil 1)
      (forward-char 1)))
(defun mwg-transpose-c++words::try-from (op1b predBwd op1e predFwd &optional isBackward)
  "Determine two regions using specified predicators and try to replcae the
content of the two regions. The end point of the first region is specified by
`OP1B'.  The beginning point of the first region is determined by the backward
predicator, `PREDBWD'.  The beginning point of the second region is specified
by `OP1E'.  The end point of the second region is determined using the forward
predicator, `PREDFWD'.

In the end, it moves the current point to the end of the second region. If the
argument `ISBACKWARD' is set, it moves the current point to the beginning of
the first region instead of the end of the second region."
  (let* ((r1e (progn (goto-char op1b)
                     (mwg-transpose-c++words::skip-bwd-space)
                     (point)))
         (r1b (progn (mwg-c++exp-until::backward predBwd)
                     (if (< (point) r1e) (mwg-transpose-c++words::skip-fwd-space))
                     (point)))
         (r2b_ (progn (goto-char op1e)
                      (mwg-transpose-c++words::skip-fwd-space)
                      (point)))
         (r2e (progn (mwg-c++exp-until::forward predFwd)
                     (mwg-transpose-c++words::skip-bwd-space)
                     (point)))
         (r2b (if (< r2e r2b_) r2e r2b_)))
    (mwg-c++exp-transpose-region r1b r1e r2b r2e)
    (goto-char (if isBackward r1b r2e))
    t))

(defun mwg-transpose-c++words::try-around-operator (op1b op1e &optional isBackward)
  "Try to transpose c++ expressions around the specified binary operator.  The
first point of the operator is specified by `OP1B', and the last point of the
operator is specified by `OP1E'."
  (let* ((op1l (- op1e op1b))
         (prec0 (progn (goto-char op1b)
                       (mwg-c++exp-opdict::forward-get op1l (char-after))))
         (type0 (% prec0 10)))
    (if (= type0 1) ;; binary operator
        (let ((mwg-c++exp-arg::prec prec0))
          (mwg-transpose-c++words::try-from op1b 'mwg-c++exp-pred::backward-opprec
                                            op1e 'mwg-c++exp-pred::forward-opprec
                                            isBackward))
      nil)))

;;-----------------------------------------------
;; Syntactical features out of support
;;-----------------------------------------------
;; 1. unary prefix operator: * & + -
;;        *p*a <-> a**p
;;   (int)&p*a <-> a*(int)&p
;;        -5%2 <-> 2%-5
;;        +5%2 <-> 2%+5
;; 2. cast operation: (type)
;;    (int)x.y <-> (int)y.x
;;> 3. operator names [supported]
;;>    x.operator new() <-> operator new.x()
;; 4. declaration
;;   int a=b <-> int b=a
;;   int a,b <-> int b,a
;;   int *a,b <-> int b,*a
;;-----------------------------------------------

(defun mwg-transpose-c++words-impl (&optional isBackward)
  "Transpose two c++ expressions.
When current point is under some binary operator,
replace two c++ expressions in left- and right-hand sides.
If transposing expressions is failed,
ordinary emacs lisp function `transpose-words' will be called."
  (let ((p0 (point)))
    (or (let* ((op1b (progn (mwg-transpose-c++words::skip-bwd-space)
                            (mwg-backward-c++word)
                            (point)))
               (op1e (progn (mwg-forward-c++word)
                            (point))))
          (if (and (< op1b p0) (< p0 op1e))
              (mwg-transpose-c++words::try-around-operator op1b op1e isBackward)
            (let* ((op2b (progn (mwg-transpose-c++words::skip-fwd-space)
                                (point)))
                   (op2e (progn (mwg-forward-c++word)
                                (point))))
              (or (mwg-transpose-c++words::try-around-operator op2b op2e isBackward)
                  (mwg-transpose-c++words::try-around-operator op1b op1e isBackward)))))
        (progn
          (goto-char p0)
          (call-interactively 'transpose-words))
        )))
(defun mwg-transpose-c++words (&optional arg)
  (interactive "p")
  (mwg-transpose-c++words-impl))
(defun mwg-backward-transpose-c++words (&optional arg)
  (interactive "p")
  (mwg-transpose-c++words-impl t))

;;-----------------------------------------------------------------------------
;; 20121220

(defun mwg-c++exp-get-region/backward-word ()
  (save-excursion
    (setq mwg-c++exp@@begin
          (progn (mwg-transpose-c++words::skip-bwd-space)
                 (mwg-backward-c++word)
                 (point))
          mwg-c++exp@@end
          (progn (mwg-forward-c++word)
                 (point)))))
(defun mwg-c++exp-get-region/forward-word ()
  (save-excursion
    (setq mwg-c++exp@@begin
          (progn (mwg-transpose-c++words::skip-fwd-space)
                 (point))
          mwg-c++exp@@end
          (progn (mwg-forward-c++word)
                 (point)))))
(defun mwg-c++exp-get-region/forward-binary-rhs (prec)
  "Get the region of the right hand side subexpression of the binary operator.
The current point should be the end of the binary operator."
  (if (= (% prec 10) 1) ;; 1 = binary operator
      (save-excursion
        (let* ((mwg-c++exp-arg::prec prec)
               (r2b_ (progn (mwg-transpose-c++words::skip-fwd-space)
                            (point)))
               (r2e (progn (mwg-c++exp-until::forward 'mwg-c++exp-pred::forward-opprec)
                           (mwg-transpose-c++words::skip-bwd-space)
                           (point)))
               (r2b (if (< r2e r2b_) r2e r2b_)))
          (setq mwg-c++exp@@begin r2b
                mwg-c++exp@@end   r2e)
          t))))
(defun mwg-c++exp-get-region/backward-binary-lhs (prec)
  "Get the region of the left hand side subexpression of the binary operator.
The current point should be the beginning of the binary operator."
  (if (= (% prec 10) 1) ;; 1 = binary operator
      (save-excursion
        (let* ((mwg-c++exp-arg::prec prec)
               (r1e (progn (mwg-transpose-c++words::skip-bwd-space)
                           (point)))
               (r1b (progn (mwg-c++exp-until::backward 'mwg-c++exp-pred::backward-opprec)
                           (if (< (point) r1e) (mwg-transpose-c++words::skip-fwd-space))
                           (point))))
          (setq mwg-c++exp@@begin r1b
                mwg-c++exp@@end   r1e)
          t))))

(defun mwg-c++exp-goto/forward-binary-rhs ()
  (let ((p0 (point)))
    (mwg-c++exp-get-region/backward-word)
    (let* ((wbeg mwg-c++exp@@begin)
           (wend mwg-c++exp@@end)
           (prec (mwg-c++exp-opdict::get-from-region wbeg wend)))
      ;; (message "cword = %s, prec = %d" (buffer-substring wbeg wend) prec)
      (goto-char wend)
      (if (and (mwg-c++exp-get-region/forward-binary-rhs prec)
               (< p0 mwg-c++exp@@end))
          (goto-char mwg-c++exp@@end)
        (goto-char p0) nil))))
(defun mwg-c++exp-goto/backward-binary-lhs ()
  (let ((p0 (point)))
    (mwg-c++exp-get-region/forward-word)
    (let* ((wbeg mwg-c++exp@@begin)
           (wend mwg-c++exp@@end)
           (prec (mwg-c++exp-opdict::get-from-region wbeg wend)))
      (goto-char wbeg)
      (if (and (mwg-c++exp-get-region/backward-binary-lhs prec)
               (< mwg-c++exp@@begin p0))
          (goto-char mwg-c++exp@@begin)
        (goto-char p0) nil))))

;;=============================================================================
