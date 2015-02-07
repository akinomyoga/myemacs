;; -*- coding:utf-8 -*-
;; ttx-mode (TTX Mode for GNU Emacs)
;;
;; Filename: ttx-mode.el
;; Description: TTX Mode to edit .ttx files
;; Author: Koichi Murase <myoga.murase@gmail.com>
;;         Copyright (C) 2012-2014, Koichi Murase, all rights reserved
;; Creation: 2012/10/27 12:39:25
;; LastModified: 2013/03/01 00:22:43

;; 設定の仕方
;;   1. ttx-mode.el を "~/.emacs.d/" などに配置する
;;   2. 以下を .emacs などに追加する (~/.emacs.d は ttx-mode.el を配置した場所)
;;     (add-to-list 'load-path "~/.emacs.d")
;;     (autoload 'ttx-mode "ttx-mode" "Major mode for editing Myoga-ttx files." t)
;;     (setq auto-mode-alist (cons '("\\.ttx$" . ttx-mode) auto-mode-alist))
;;
;;-----------------------------------------------------------------------------
;;
;; ChangeLog
;;
;; 2014-04-16, KM
;;   * ttx-indent-line: 修正。カーソル位置保持、} で始まらない行は前行の続きで indent。
;; 2013-08-05, KM
;;   * load の仕方を修正。.ttx ファイルを読み取る時に初めてロードする様にする。
;; 2013-07-02, KM
;;   * bugfix: 正規表現の修正。引用符周り。
;; 2013-03-01, KM
;;   * 色付けを大幅に修正・実装
;; 2013-02-06, KM
;;   * bugfix: 正規表現の修正。空文字列に対応。
;; 2013-10-27, KM
;;   * 作成
;;
;;-----------------------------------------------------------------------------

;; 
;; 構文テーブル
;;

(defvar ttx-mode-syntax-table nil
  "Syntax table in use in ttx-mode buffers.")

(when (not ttx-mode-syntax-table)
  (setq ttx-mode-syntax-table
        (let ((table (make-syntax-table)))
          ;; 単語構成文字
          (modify-syntax-entry ?_ "w" table)
          
          ;; 括弧
          (modify-syntax-entry ?\{ "(}" table)
          (modify-syntax-entry ?\} "){" table)
          
          ;; 文字列リテラル
          (modify-syntax-entry ?\" "\"" table)
          (modify-syntax-entry ?\' "\"" table)
          (modify-syntax-entry ?\\ "\\" table)

          ;; コメント
          (modify-syntax-entry ?# "<\n" table)
          (modify-syntax-entry ?\n ">%" table)
          
          table)))

;;
;; キーワードの強調表示
;; 

(defvar ttx-font-lock-defaults 
  (list 'ttx-font-lock-keywords
        t
        nil
        nil
        'beginning-of-line
        ))

;;-----------------------------------------------------------------------------
;; font-lock settings

(defconst ttx-font-lock-keywords
  (list
   '(ttx-font-lock-common)
   ;; '("\"\\([^\"\\\\]\\|[\r\n]\\|\\\\.\\)*\"\\|\'\\([^\'\\\\]\\|[\r\n]\\|\\\\.\\)*\'" . font-lock-string-face)
   ;; '("#.*$" . font-lock-comment-face)

   ;; 正規表現で強調表示。
   ;; '("[A-Z][a-zA-Z0-9_]*" 0 font-lock-keyword-face)
   )) 
;; (defface ttx-dqstring-face
;;   '((default :inherit font-lock-string-face))
;;   "Face for string literals with double-quotation mark."
;;   :version "24.1"
;;   :group 'ttx)

(defface ttx-string-escape-face
  '((default :inherit font-lock-string-face :weight bold))
  "Face for escape sequences in string literals."
  :version "24.1"
  :group 'ttx)

(defun ttx-syntax-set-string-escapes (s e qch)
  (save-excursion
    (goto-char s)
    (while (search-forward-regexp "\\\\\\([0-7]\\{1,3\\}\\|x[[:xdigit:]]\\{1,2\\}\\|[abtnvfre\"\\\n]\\)?" e t)
      (if (match-beginning 1)
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face ttx-string-escape-face))
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face font-lock-warning-face))))
    (if (/= qch (char-before e))
        (add-text-properties (- e 1) e
                             '(face font-lock-warning-face)))))
(defun ttx-syntax-set-dqstring (s e)
  (add-text-properties s e '(face font-lock-string-face))
  (add-text-properties s (if (= ?\" (char-before e)) (- e 1) e)
                       '(ttx-syntax dqstring))
  (ttx-syntax-set-string-escapes s e ?\")
  e)
(defun ttx-syntax-set-sqstring (s e)
  (add-text-properties s e '(face font-lock-string-face))
  (add-text-properties s (if (= ?' (char-before e)) (- e 1) e)
                       '(ttx-syntax sqstring))
  (ttx-syntax-set-string-escapes s e ?')
  e)

(defun ttx-syntax-clear (s e)
  (if (> e s)
      (remove-text-properties s e '(ttx-syntax nil))))
(defun ttx-syntax-span/impl (p0 state)
  (if (and (/= p0 (point-max))
           (eq state (get-text-property p0 'ttx-syntax)))
      (ttx-syntax-span/impl (1+ p0) state)
    (point)))
(defun ttx-syntax-span (state)
  (ttx-syntax-span/impl (point) state))

(defun ttx-font-lock/until (limit)
  (if (< (point) limit)
      (progn
        (ttx-font-lock-common limit)
        (ttx-font-lock/until limit))))

(defun ttx-font-lock-common (limit)
  (let* ((p0 (point))
         (s0 (if (< p0 2) nil
               (get-text-property (- p0 1) 'ttx-syntax))))
    (cond
     ;; "文字列リテラル" 伸張
     ((eq s0 'dqstring)
      (search-forward-regexp "\\=\\(?:[^\"\\\\]\\|\\\\.\\|\\\\\n\\)*\\(?:\"\\|\\\\?\\\"\\)")
      (ttx-syntax-clear p0 (ttx-syntax-span 'dqstring))
      (ttx-syntax-set-dqstring (match-beginning 0) (match-end 0)))
     ;; '文字列リテラル' 伸張
     ((eq s0 'sqstring)
      (search-forward-regexp "\\=\\(?:[^\'\\\\]\\|\\\\.\\|\\\\\n\\)*\\(?:\'\\|\\\\?\\'\\)")
      (ttx-syntax-clear p0 (ttx-syntax-span 'sqstring))
      (ttx-syntax-set-sqstring (match-beginning 0) (match-end 0)))
     ;; 先頭識別子
     ((and (save-excursion (search-backward-regexp "\\(?:\\`\\|[{,}]\\)\\(?:[[:space:]\r\n]\\|#[^\n]*\\)*\\=" nil t))
           (search-forward-regexp "\\=[[:space:]\r\n]*" limit t)
           (search-forward-regexp "\\=[^{=,}[:space:]\r\n\"'#]+" limit t))
      (ttx-syntax-clear p0 (point))
      (add-text-properties (match-beginning 0) (match-end 0)
                           '(face font-lock-variable-name-face)))
     ((search-forward-regexp "[\"'#]\\|\\(?:\\`\\|[{,}]\\)[[:space:]\r\n]*[^{=,}[:space:]\r\n\"'#]" limit t)
      (backward-char 1)
      (let ((ch (char-after)))
        (cond
         ;; 複数行に亙る文字列
         ((= ch ?\")
          (search-forward-regexp "\\=\"\\(?:[^\"\\\\]\\|\\\\.\\|\\\\\n\\)*\"")
          (ttx-syntax-clear p0 (ttx-syntax-span 'dqstring))
          (ttx-syntax-set-dqstring (match-beginning 0) (match-end 0))
          )
         ((= ch ?')
          (search-forward-regexp "\\=\'\\(?:[^\'\\\\]\\|\\\\.\\|\\\\\n\\)*\'")
          (ttx-syntax-clear p0 (ttx-syntax-span 'sqstring))
          (ttx-syntax-set-sqstring (match-beginning 0) (match-end 0)))
         ;; 行コメント
         ((= ch ?#)
          (search-forward-regexp "\\=#.*$")
          (ttx-syntax-clear p0 (point))
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face font-lock-comment-face)))
         (t
          (search-forward-regexp "\\=[^{=,}[:space:]\r\n\"'#]+")
          (ttx-syntax-clear p0 (point))
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face font-lock-variable-name-face)))
         )))
     (t
      (goto-char (if limit limit (point-max)))
      (ttx-syntax-clear p0 (point))))))
;; (defun ttx-font-lock-common-sample (limit)
;;   (if (search-forward "warning" nil 0)
;;       (add-text-properties 
;;        (match-beginning 0) (match-end 0) '(face font-lock-warning-face))
;;     nil))

;;-----------------------------------------------------------------------------
;; keymap

(defvar ttx-mode-map
  (let ((map (make-keymap)))
    ;; タブを押すとtestが呼ばれる。
    ;; (define-key map "\t" 'test)
    map))

;;-----------------------------------------------------------------------------
;; indent-line-function


;; インデントの規則
;; - isClosingLine は "}" で始まる行の場合に t
;; - "}" で始まる行の場合、行内で一番外側のインデントレベルに合わせる。
;; - それ以外の行の場合、行頭に於けるインデントレベルを使用する。


(defun ttx-mode/count-close-level()
  "Determine the relative level of the current line to the previous eol"
  (save-excursion
    (let ((peol (progn (end-of-line) (point)))
          (lev 0)
          (min-lev 0)
          (isClosingLine (progn
                           (beginning-of-line)
                           (re-search-forward "[[:space:]]*")
                           (and (not (eolp))
                                (= (char-after) ?})))))
      (if (not isClosingLine) 0
        (while
            (re-search-forward
             "[{}]\\|\"\\([^\"\\\\]\\|[\r\n]\\|\\\\.\\)+\"\\|\'\\([^\'\\\\]\\|[\r\n]\\|\\\\.\\)+\'"
             peol t)
          (cond
           ((= ?} (char-before))
            (setq lev (1- lev)))
           ((= ?{ (char-before))
            (setq lev (1+ lev))))
          (if (< lev min-lev)
              (setq min-lev lev)))
        min-lev))))

(defun ttx-mode/count-open-level()
  (save-excursion
    (let ((peol (progn (end-of-line) (point)))
          (lev 0)
          (min-lev 0)
          (isClosingLine (progn
                           (beginning-of-line)
                           (re-search-forward "[[:space:]]*")
                           (and (not (eolp))
                                (= (char-after) ?})))))
      (while (re-search-forward 
              "[{}]\\|\"\\([^\"\\\\]\\|\\\\.\\)+\"\\|\'\\([^\'\\\\]\\|\\\\.\\)+\'\\|#.*" peol t)
        (cond
         ((= ?} (char-before))
          (setq lev (1- lev)))
         ((= ?{ (char-before))
          (setq lev (1+ lev))))
        (if (< lev min-lev)
            (setq min-lev lev)))
      (if isClosingLine
          (- lev min-lev)
        lev))))

(defun ttx-indent-line()
  (let* ((margin (save-excursion
                   (beginning-of-line)
                   (if (bobp) 0
                     (let ((close (ttx-mode/count-close-level))
                           (open (progn (forward-line -1)
                                        (ttx-mode/count-open-level)))
                           (pindent (progn (back-to-indentation) (point)))
                           (pbeg (progn (beginning-of-line) (point))))
                       (+ (* 2 close) (* 2 open) (- pindent pbeg)))
                     )))
         (inverse-offset (- (save-excursion (end-of-line) (point)) (point))))
    (indent-line-to margin)
    (goto-char
     (max (progn (beginning-of-line)
                 (re-search-forward "[[:space:]]*")
                 (point))
          (- (progn (end-of-line) (point))
             inverse-offset)))))
;;-----------------------------------------------------------------------------

(defun ttx-mode ()
  "Major mode for .ttx (myoga tree-structure text) files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table ttx-mode-syntax-table)
  ;; (setq case-fold-search nil)

  (use-local-map ttx-mode-map)
  (setq mode-name "Myoga-ttx")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults ttx-font-lock-defaults)
  (make-local-variable 'font-lock-multiline)
  (setq font-lock-multiline t)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ttx-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")

  (run-hooks 'ttx-mode-hook))

;; (autoload 'ttx-mode "ttx" "ttx major mode" t)
;; (autoload 'ttx-make-buffer "ttx" "open a buffer in ttx mode" t)
;; (setq auto-mode-alist (append '(("\\.ttx$" . ttx-mode)) auto-mode-alist))
