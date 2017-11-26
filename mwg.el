;; .emacs settings -*- coding:utf-8 -*-


;;*****************************************************************************
;;
;; Input Decodes
;;
;;=============================================================================
;; settings for auto-complete
;;-----------------------------------------------------------------------------

(defun xterm-additional-keymap ()
  (let* ((target-map (if (boundp 'input-decode-map)
                         input-decode-map
                       function-key-map))
         (register (lambda (key def)
                     (define-key target-map key def))))
    
    ;; from GNOME Terminal
    (funcall register "\e[1;2h" [S-home])
    (funcall register "\e[1;3h" [M-home])
    (funcall register "\e[1;4h" [S-M-home])
    (funcall register "\e[1;5h" [C-home])
    (funcall register "\e[1;6h" [C-S-home])
    (funcall register "\e[1;7h" [C-M-home])
    (funcall register "\e[1;8h" [C-S-M-home])
    (funcall register "\e[1;2f" [S-end])
    (funcall register "\e[1;3f" [M-end])
    (funcall register "\e[1;4f" [S-M-end])
    (funcall register "\e[1;5f" [C-end])
    (funcall register "\e[1;6f" [C-S-end])
    (funcall register "\e[1;7f" [C-M-end])
    (funcall register "\e[1;8f" [C-S-M-end])))

(xterm-additional-keymap)

;;*****************************************************************************
;;
;; Loading Other Libraries
;;
;;=============================================================================

;;---- auto-complete-mode -----------------------------------------------------
(eval-when-compile
  ;; variables defined in auto-complete-mode
  (defvar ac-mode-map)
  (defvar ac-completing-map)
  (defvar ac-auto-show-menu)
  (defvar ac-auto-start)
  (defvar ac-delay)
  (defvar ac-dictionary-directories)
  (defvar ac-disable-faces)
  (defvar ac-modes)
  (defvar ac-whole-common-part)
  (declare-function ac-config-default "auto-complete")
  (declare-function ac-update-greedy "auto-complete")
  (declare-function ac-inline-live-p "auto-complete")
  (declare-function ac-inline-hide "auto-complete")
  (declare-function ac-expand-string "auto-complete")
  (declare-function ac-expand-common "auto-complete")
  (declare-function ac-disable-faces/match "auto-complete"))

(defun mwg-add-hook-auto-complete ()
  (require 'auto-complete-config)
  ;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
  (setq ac-modes (append ac-modes (list 'tex-mode 'latex-mode 'awk-mode 'csharp-mode)))
  (ac-config-default)

  ;;---- settings & user defined functions ---------------------------

  (setq ac-auto-start 3)

  ;; TAB で共通部分だけを確定。第一候補選択は C-j で。
  ;; \brief 共通部分だけを確定し続きの入力を求める
  ;; (ac-expand-common) を書き換えて使っていたが、
  ;; let して ac-common-part を置き換えるだけで良かった。)
  (defun ac-expand-whole-common ()
    (interactive)
    (let ((ac-common-part ac-whole-common-part))
      (ac-expand-common)))

  ;; 長らく auto-complete.el の Not to expand ... の行を書き換えていたが、
  ;; 初回の auto-complete で共通部分を補間するのはそういう設定だった様だ。
  ;; 以下の設定を行うだけで突然の補間はしない様になる。
  (custom-set-variables
   '(ac-expand-on-auto-complete nil))

  ;; menu が出る前に ac-expand-common すると先頭候補に確定されてしまう。
  ;; なので、menu を即座 (= 補完開始と同時) に出す事にする。
  ;; ※元から ac-show-menu-immediately-on-auto-complete t
  ;;   という設定がされている様だが、これでは不十分な様だ(?)。
  (setq ac-auto-show-menu ac-delay)

  ;; ac-disable-faces バグの修正 (関数 ac-cursor-on-diable-face-p の上書き)
  (defun ac-disable-faces/match (face)
    (and face
         (if (listp face)
             (or (ac-disable-faces/match (car face))
                 (ac-disable-faces/match (cdr face)))
           (memq face ac-disable-faces))))
  (defun ac-cursor-on-diable-face-p (&optional point)
    "This function overrides the original function `ac-cursor-on-diable-face-p' in auto-complete.el.
This function has the different behaviors with original one in the following two points:
- The original one looks the face after the cursor,
  but this function looks that before the cursor
  because the word to be completed is always located before the cursor.
- This function provides the support for the text with multiple faces
  which are not supported in the original function."
    (let ((pt (1- (or point (point)))))
      (and (>= pt (point-min))
           (ac-disable-faces/match (get-text-property pt 'face)))))

  ;;---- keymap -------------------------------------------
  (define-key ac-completing-map [down]   'mwg-next-line-nomark)
  (define-key ac-completing-map [up]     'mwg-prev-line-nomark)
  (define-key ac-completing-map "\C-n"   'ac-next)
  (define-key ac-completing-map "\C-p"   'ac-previous)
  (define-key ac-completing-map [C-down] 'ac-next)
  (define-key ac-completing-map [C-up]   'ac-previous)
  (define-key ac-completing-map [M-down] 'ac-quick-help-scroll-down)
  (define-key ac-completing-map [M-up]   'ac-quick-help-scroll-up)

  ;; RET が補完確定になっていると、改行を含む貼付ができない場合がある。
  ;; 改行したいのに、補完のせいで RET を無駄に押さなければならない場合がある。
  (define-key ac-completing-map "\r"     nil)
  (define-key ac-completing-map [return] nil)
  (define-key ac-completing-map [C-return] 'ac-complete)

  ;; auto-complete-1.3.1
  (define-key ac-completing-map "\C-s"   'save-buffer)
  (define-key ac-completing-map "\M-s"   'ac-isearch)
  (define-key ac-completing-map "\t"     'ac-expand-whole-common)
  (define-key ac-completing-map "\C-j"   'ac-expand)
  (when (boundp 'ac-menu-map)
    ;; auto-complete-1.4.0
    (define-key ac-menu-map "\C-s"   'save-buffer)
    (define-key ac-menu-map "\M-s"   'ac-isearch)
    (define-key ac-menu-map "\t"     'ac-expand-whole-common)
    (define-key ac-menu-map "\C-j"   'ac-expand))

  (define-key ac-mode-map "\C-j" 'auto-complete)

)

;;---- gnuplot-mode -----------------------------------------------------------
(defun mwg-add-hook-gnuplot (gnuplot-path)
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
  (setq auto-mode-alist (append '(("\\.\\(gp\\|plt\\)$" . gnuplot-mode)) auto-mode-alist))
  
  ;; for MS windows
  (add-hook 'gnuplot-load-hook
            (list 'lambda '() (list 'setq 'gnuplot-program gnuplot-path))))

;;---- csharp-mode ------------------------------------------------------------
(defun mwg-add-hook-csharp ()
  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  (setq auto-mode-alist (cons '("\\.cs$" . csharp-mode) auto-mode-alist))
  (add-hook 'csharp-mode-hook 'mwg-tabwidth-c-mode-hook))

;;---- bashfc/sh-mode ---------------------------------------------------------
(defun mwg-add-hook-bashfc ()
  (setq auto-mode-alist (cons '("\\(^\\|[\\/]\\)bash-fc-[0-9]+$" . sh-mode) auto-mode-alist)))

;; write `-*- mode: sh; mode: sh-bash -*-' in your mode line
(eval-when-compile
  (declare-function sh-mode "sh-script")
  (declare-function sh-set-shell "sh-script"))
(defun sh-bash-mode ()
  (interactive)
  (require 'sh-script)
  (sh-mode)
  (sh-set-shell "bash"))

;; http://unix.stackexchange.com/questions/20121/how-to-disable-emacs-here-document-completion
(add-hook 'sh-mode-hook '(lambda () (sh-electric-here-document-mode -1)))

;;---- xml-mode ---------------------------------------------------------------
(defvar mwg-xml-tag-region/previous-tagname "xml")
(defun mwg-xml-tag-region (tagname)
  (interactive "sEnter a tag name: ")
  ;; (interactive (concat "sEnter a tag name (" mwg-xml-tag-region/previous-tagname "): "))
  (let* ((tag1 (if (and tagname
                        (> (length tagname) 0))
                   (setq mwg-xml-tag-region/previous-tagname tagname)
                 mwg-xml-tag-region/previous-tagname))
         (m_r (string-match "^\\([^.#]*\\)\\(?:\\.\\([^#]*\\)\\)?\\(?:#\\(.*\\)\\)?$" tag1))
         (m_tag (if m_r (match-string 1 tag1) tag1))
         (m_cls (and m_r (match-string 2 tag1)))
         (m_id (and m_r (match-string 3 tag1)))
         (tagE (if (> (length m_tag) 0) m_tag "xml"))
         (tagS1 (if (and m_id (> (length m_id) 0))
                    (concat tagE " m_id=\"" m_id "\"")
                  tagE))
         (tagS (if (and m_cls (> (length m_cls) 0))
                   (concat tagS1 " class=\"" m_cls "\"")
                 tagS1)))
    (if mark-active
        (let* ((rb (region-beginning))
               (re (region-end))
               (ofs (- (point) rb)))
          (goto-char re)
          (insert "</" tagE ">")
          (goto-char rb)
          (insert "<" tagS ">")
          (if (> ofs 0) (forward-char ofs)))
      (insert "<" tagS ">")
      (save-excursion
        (insert "</" tagE ">")))))
(defun mwg-add-hook-xml-mode ()
  (defun mwg-add-hook/xml-mode-hook ()
    (local-set-key [?\C-<] "&lt;")
    (local-set-key [?\C->] "&gt;")
    (local-set-key [?\C-&] "&amp;")
    (local-set-key [?\C-c ?\C-c] 'mwg-xml-tag-region))
  (add-hook 'nxml-mode-hook 'mwg-add-hook/xml-mode-hook))

;;---- mwg-c++exp/cc-mode -----------------------------------------------------
;; settings for mwg-c++exp
(defun mwg-add-hook-mwg-c++exp ()
  (defun mwg-add-hook/c++-mode-hook ()
    (require 'mwg-c++exp)
    (local-set-key [M-left] 'mwg-backward-c++word)
    (local-set-key [M-right] 'mwg-forward-c++word)
    (local-set-key [C-M-left] 'mwg-backward-c++sexp)
    (local-set-key [C-M-right] 'mwg-forward-c++sexp)
    (local-set-key "\M-t" 'mwg-transpose-c++words)
    (local-set-key "\M-T" 'mwg-backward-transpose-c++words))

  (add-hook 'c-mode-hook 'mwg-add-hook/c++-mode-hook)
  (add-hook 'c++-mode-hook 'mwg-add-hook/c++-mode-hook)
  (add-hook 'js-mode-hook 'mwg-add-hook/c++-mode-hook)
  (add-hook 'awk-mode-hook 'mwg-add-hook/c++-mode-hook))

;;---- ttx-mode ---------------------------------------------------------------
(defun mwg-add-hook-mwg-ttx ()
  (autoload 'ttx-mode "ttx-mode" "Major mode for editing Myoga-ttx files." t)
  (setq auto-mode-alist (cons '("\\.ttx$" . ttx-mode) auto-mode-alist)))

;;---- mwg-doxygen/cc-mode ----------------------------------------------------
(eval-when-compile
  ;; variables defined in cc-mode
  (defvar c-doc-comment-style))
(defun mwg-setup-mwg-doxygen ()
  (eval-after-load "cc-mode"
    '(progn
       ;; (require 'cc-mode)
       (autoload 'mwg-doxygen-font-lock-keywords "mwg-doxygen" "Doxygen Syntax Highlighting for cc-mode." t)
       
       (unless (listp c-doc-comment-style)
         ;; instantiate c-doc-comment-style
         (setq c-doc-comment-style
               (or (get 'c-doc-comment-style 'c-stylevar-fallback)
                   '((java-mode . javadoc) (pike-mode . autodoc) (c-mode . gtkdoc)))))
       (setq c-doc-comment-style (append '((c++-mode . mwg-doxygen) (js-mode . mwg-doxygen))
                                         c-doc-comment-style)))))

;;---- js2-mode ----------------------------------------------------

(defun mwg-add-hook-js2-mode ()
  ;; (or (autoload 'js2-mode "mwg-js2-config" nil t)
  ;;     (require 'mwg-js2-config))
  (autoload 'mwg-js2-mode "mwg-js2-config" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . mwg-js2-mode)))

(custom-set-variables  
 '(js2-basic-offset 2)
 '(js2-skip-preprocessor-directives t)
 ;; '(js2-bounce-indent-p t)
 )


;******************************************************************************
;
;  Settings
;
;------------------------------------------------------------------------------
; Tab width
;------------------------------------------------------------------------------
(defun mwg-emacs-version->= (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
           (>= emacs-minor-version minor))))
(eval-when-compile
  ;; variables defined in each mode
  (defvar c-basic-offset)
  (defvar js-indent-level)
  (defvar css-indent-offset)
  (defvar cssm-indent-level)
  (defvar sh-basic-offset)
  (defvar sh-indentation)
  (defvar sh-indent-for-case-label)
  (defvar sh-indent-for-case-alt)
  (defvar perl-indent-level)
  (defvar perl-continued-statement-offset)
  (defvar perl-brace-offset)
  (defvar perl-label-offset))

(defun mwg-init-tabwidth (&optional width0)
  (let ((width (if width0 width0 2)))
    (setq tab-width width)
    (setq-default tab-width width)
    (setq-default indent-tabs-mode nil)
    ;; for older emacs
    (and (not (mwg-emacs-version->= 23 2))
         (boundp 'default-tab-width)
         (setq default-tab-width width))

    ;; using tap-stop-list
    (setq indent-line-function 'tab-to-tab-stop)
    (add-hook 'text-mode-hook
              (lambda ()
                (setq indent-line-function 'tab-to-tab-stop)
                (with-silent-modifications
                  (put-text-property (point-min) (point-max) 'face nil))))

    ;; ※再帰にすると再帰深度エラーになる。
    (let* ((tabstop (* (/ 210 width) width))
           (list ()))
      (while (> tabstop 0)
        (setq list (cons tabstop list)
              tabstop (- tabstop width)))
      (setq-default tab-stop-list list)
      (setq         tab-stop-list list))
    ;;--------------------
    
    (let* ((defunw (lambda (name fun)
                    (fset name (list 'lambda '() (list 'funcall fun width))))))
      (funcall defunw 'mwg-tabwidth-c-mode-hook (lambda (w) (setq c-basic-offset w)))
      (funcall defunw 'mwg-tabwidth-js-mode-hook (lambda (w) (setq js-indent-level w)))
      (funcall defunw 'mwg-tabwidth-css-mode-hook (lambda (w) (setq css-indent-offset w
                                                                    cssm-indent-level w))) ; Lars css-mode.el
      (funcall defunw 'mwg-tabwidth-sh-mode-hook (lambda (w) (setq sh-basic-offset w
                                                                   sh-indentation w
                                                                   sh-indent-for-case-label 0
                                                                   sh-indent-for-case-alt '+)))
      (funcall defunw 'mwg-tabwidth-perl-mode-hook (lambda (w) (setq perl-indent-level w
                                                                     perl-continued-statement-offset w
                                                                     perl-brace-offset (- w)
                                                                     perl-label-offset (- w))))

      (add-hook 'awk-mode-hook 'mwg-tabwidth-c-mode-hook)
      (add-hook 'php-mode-hook 'mwg-tabwidth-c-mode-hook)
      (add-hook 'sh-mode-hook 'mwg-tabwidth-sh-mode-hook)
      (add-hook 'js-mode-hook 'mwg-tabwidth-js-mode-hook)
      (add-hook 'css-mode-hook 'mwg-tabwidth-css-mode-hook)
      (add-hook 'perl-mode-hook 'mwg-tabwidth-perl-mode-hook))
    ;; objc-mode-hook
    ;; java-mode-hook
    ;; idl-mode-hook
    ;; pike-mode-hook
    )

  (add-hook 'makefile-mode-hook '(lambda () (setq tab-width 8))))

(add-hook 'c-mode-hook '(lambda ()
                          (c-set-offset 'arglist-close 0)
                          (c-set-offset 'arglist-intro '+)
                          (c-set-offset 'arglist-cont 0)
                          (c-set-offset 'arglist-cont-nonempty '+)
                          ))
(add-hook 'c++-mode-hook '(lambda ()
                          (c-set-offset 'arglist-close 0)
                          (c-set-offset 'arglist-intro '+)
                          (c-set-offset 'arglist-cont 0)
                          (c-set-offset 'arglist-cont-nonempty '+)
                          (c-set-offset 'template-args-cont '+)
                          ))

;; http://qiita.com/marcy_o/items/a3e9f99baa07d16bef95
;; http://emacs.stackexchange.com/questions/5452/before-save-hook-for-cc-mode
(add-hook 'before-save-hook
          '(lambda()
             (when (and (boundp 'c-buffer-is-cc-mode)
                        c-buffer-is-cc-mode
                        (not (and (boundp 'mwg-no-delete-trailing-whitespaces)
                                  mwg-no-delete-trailing-whitespaces)))
               (delete-trailing-whitespace))))


;;---- regexp \b --------------------------------------------------------------
;; 今まで悩まされていたのだが、実はこれは設定可能だった…

(defun mwg/regexp-word-boundary/.mode-hook ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'text-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'c-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'c++-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'csharp-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'awk-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'sh-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'js-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'css-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'perl-mode-hook 'mwg/regexp-word-boundary/.mode-hook)
(add-hook 'php-mode-hook 'mwg/regexp-word-boundary/.mode-hook)

;******************************************************************************
;
;  Defining Key Translations
;
;------------------------------------------------------------------------------
(progn
  (global-set-key (kbd "ESC <deletechar>") (kbd "<M-delete>"))
  (global-set-key (kbd "ESC <up>"        ) (kbd "<M-up>"))
  (global-set-key (kbd "ESC <down>"      ) (kbd "<M-down>"))
  (global-set-key (kbd "ESC <left>"      ) (kbd "<M-left>"  ))
  (global-set-key (kbd "ESC <right>"     ) (kbd "<M-right>" ))
  ;; (define-key keymap (kbd "ESC <home>"      ) (kbd "<M-home>"  )) ; moto-kara yuko
  ;; (define-key keymap (kbd "ESC <insert>"    ) (kbd "<M-insert>")) ; moto-kara yuko
  ;; (define-key keymap (kbd "ESC <delete>"    ) (kbd "<M-delete>")) ; moto-kara yuko
  ;; (define-key keymap (kbd "ESC <end>"       ) (kbd "<M-end>"   )) ; moto-kara yuko
  ;; (define-key keymap (kbd "ESC <prior>"     ) (kbd "<M-prior>" )) ; moto-kara yuko
  ;; (define-key keymap (kbd "ESC <next>"      ) (kbd "<M-next>"  )) ; moto-kara yuko
  
  (when nil ; dbg
    (global-set-key (kbd "<C-prior>") (kbd "C P g U p"))
    (global-set-key (kbd "<C-next>")  (kbd "C P g D n"))
    (global-set-key (kbd "<C-M-next>")  (kbd "C M P g D n"))
    (global-set-key (kbd "<S-up>")  (kbd "S U p"))
    (global-set-key (kbd "<C-M-S-up>")  (kbd "C M S U p"))
    (global-set-key (kbd "<C-f1>")  (kbd "C - F u n c t i o n 1"))
    ))

;;*****************************************************************************
;;
;;  Key bindings
;;
;;-----------------------------------------------------------------------------

;; change \C-s: isearch-forward -> save-buffer
;; (global-set-key "\C-s"     'save-buffer)

(global-unset-key "\M-g")
(global-set-key "\M-g\M-g" 'goto-line)
(global-set-key "\M-g\M-n" 'next-error)
(global-set-key "\M-g\M-p" 'previous-error)
(global-set-key "\M-gg" 'goto-line)
(global-set-key "\M-gn" 'next-error)
(global-set-key "\M-gp" 'previous-error)

;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
;; change \M-f: forward-word (<M-right>) -> <prefix-key>
(global-unset-key "\M-f")
(global-set-key "\M-f\M-f" 'forward-word)
(global-set-key "\M-f\M-r" 'query-replace)
(global-set-key "\M-f\M-e" 'query-replace-regexp)
(global-set-key "\M-fs"    'isearch-forward-regexp)
(global-set-key "\M-fr"    'query-replace-regexp)

(set-default 'case-fold-search nil)

;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
; change \M-c: capitalize-word -> <prefix-key>
(global-unset-key "\M-c")
(global-set-key "\M-c\M-c" 'capitalize-word)
(global-set-key "\M-c\M-w" 'mwg-copy-as-kill-whole-word)
(global-set-key "\M-cw"    'mwg-kill-whole-word)

(defun mwg-copy-as-kill-whole-word ()
  (interactive)
  (save-excursion
    (kill-ring-save (progn (skip-syntax-backward "w_") (point))
                    (progn (skip-syntax-forward "w_") (point)))))
(defun mwg-kill-whole-word ()
  (interactive)
  (save-excursion
    (kill-region (progn (skip-syntax-backward "w_") (point))
                    (progn (skip-syntax-forward "w_") (point)))))

;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

;------------------------------------------------------------------------------
;  defining special keys
;------------------------------------------------------------------------------
(defun mwg-kill-region (&optional count)
  (interactive "p")
  (if mark-active
      (call-interactively 'kill-region)
    (beginning-of-line 1)
    (kill-region (line-beginning-position 1)
                 (line-beginning-position (1+ (if count count 1))))))
(defun mwg-copy-as-kill (&optional count)
  (interactive "p")
  (if mark-active
      (call-interactively 'kill-ring-save)
    (kill-ring-save (line-beginning-position 1)
                    (line-beginning-position (1+ (if count count 1))))
    (beginning-of-line)))

(global-set-key (kbd "<S-insert>")   (kbd "<home> \C-y"))
(global-set-key (kbd "<S-delete>")   'mwg-kill-region)
(global-set-key (kbd "<C-S-delete>") 'mwg-copy-as-kill)
(global-set-key (kbd "<M-S-delete>") 'mwg-copy-as-kill)

(global-set-key [?\C-\M-y]   (kbd "<home> \C-y"))
(global-set-key [?\C-\M-d]   'mwg-kill-region)
(global-set-key [?\C-\M-h]   'mwg-kill-region)

;--- function keys --------------------
;; (global-set-key (kbd "<f5>") 'delete-other-windows) ;; C-x 0
;; (global-set-key (kbd "<f6>") 'other-window) ;; C-x o

(global-set-key (kbd "<f9>") 'undo)
;;                    <f10>  'menu-bar-open
;;                    <f11>
;;                    <f12>

(global-set-key
 [f5] '(lambda ()
         (interactive)
         (insert (format-time-string "%Y-%m-%d %H:%M:%S"))))
(global-set-key
 [C-f5] '(lambda ()
           (interactive)
           (insert (format-time-string "%Y-%m-%d"))))

;;-----------------------------------------------------------------------------
;;  switching buffers

(global-set-key (kbd "<f7>") (kbd "C-x C-b RET"))
(global-set-key (kbd "<f8>") 'switch-to-buffer)
(global-set-key (kbd "<C-f7>") 'previous-buffer)
(global-set-key (kbd "<C-f8>") 'next-buffer)

(global-set-key "\C-x\C-b" 'switch-to-buffer)
(add-hook 'iswitchb-define-mode-map-hook
          '(lambda()
             (define-key iswitchb-mode-map "\C-b" 'iswitchb-exit-minibuffer)
             (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
             (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)))

;; 現在の選択候補の内容を表示する設定 (from http://yosshi.typepad.jp/blog/2009/11/emacs-%E3%81%AE%E3%83%90%E3%83%83%E3%83%95%E3%82%A1%E5%88%87%E3%82%8A%E6%9B%BF%E3%81%88%E3%82%92-iswitchb-%E3%81%A7%E3%82%AB%E3%82%B9%E3%82%BF%E3%83%9E%E3%82%A4%E3%82%BA.html)
;; (defadvice iswitchb-exhibit (after iswitchb-exhibit-with-display-buffer activate)
;;   (when (and (eq iswitchb-method iswitchb-default-method)
;;              iswitchb-matches)
;;     (select-window (get-buffer-window (cadr (buffer-list))))
;;     (let ((iswitchb-method 'samewindow))
;;       (iswitchb-visit-buffer (get-buffer (car iswitchb-matches))))
;;     (select-window (minibuffer-window))))

;; 元々 \C-x\C-b に割り当てられていた 'list-buffers を \C-xb にする:
;; (global-set-key "\C-xb" 'list-buffers)

;;-----------------------------------------------------------------------------
;;  replaces

(defun mreplace/replace-string (FROM TO START END)
  (save-excursion
    (goto-char START)
    (while (search-forward FROM END t)
      (let ((bp (point)))
        (replace-match TO nil t)
        (setq END (+ END (- (point) bp))))
      )))
(defun mreplace-html-escape (&optional START END)
  (interactive "r")
  (let ((s (if mark-active
               (or START (region-beginning))
             (point-min)))
        (e (if mark-active
               (or END (region-end))
             nil)))
    (mreplace/replace-string "&" "&amp;" s e)
    (mreplace/replace-string "<" "&lt;" s e)
    (mreplace/replace-string ">" "&gt;" s e)))
;;-----------------------------------------------------------------------------
;;  how to define key mappings
;;-----------------------------------------------------------------------------
;; * how to specify key sequence
;; a. "\C-f\C-s"
;; b. [?\C-, ]
;; c. (kbd "<f2>")
;;    (kbd "C-<left>")
;;    (kbd "C-f C-s")
;; d. #\C-a
;;
;; * how to emulate key sequence
;; e.g. (fset 'km-duplicate-line [home ?\C-  down ?\C-w ?\C-y ?\C-y up])
;;      (global-set-key "\C-a" 'km-duplicate-line)
;; e.g. (global-set-key "\C-b" [ ?a ?b ?c ])

;;*****************************************************************************
;;
;;  Incremental Search Extensions
;;
;;-----------------------------------------------------------------------------
;; defined later, referenced here
(defvar mwg-pc-mark-active)

; http://stackoverflow.com/questions/589691/how-can-i-emulate-vims-search-in-gnu-emacs
(defconst mwg-rex-word-begin
  (if (< emacs-major-version 22) "\\<" "\\_<"))
(defconst mwg-rex-word-end
  (if (< emacs-major-version 22) "\\>" "\\_>"))

(defconst mwg-isearch-forward-word/rex-iswb
  (if (< emacs-major-version 22) "\\=\\<" "\\=\\_<"))
(defconst mwg-isearch-forward-word/rex-iswe
  (if (< emacs-major-version 22) "\\>\\=" "\\_>\\="))
(defun mwg-isearch-forward-word/cwb ()
    (forward-char 1)
    (if (re-search-backward "\\(^\\|[^_$[:alnum:]]\\)[_$[:alnum:]]+?" nil t)
        (if (/= (match-beginning 1) (match-end 1))
            (forward-char 1))
      (if (re-search-forward "[_$[:alnum:]]" nil t)
          (backward-char 1))))
(defun mwg-isearch-forward-word/cwe ()
  (if (re-search-forward "[_$[:alnum:]]+?\\([^_$[:alnum:]]\\|$\\)" nil t)
      (if (/= (match-beginning 1) (match-end 1))
          (backward-char 1))))
(defun mwg-isearch-mode-hook ()
  (cond
   ((or (equal this-command 'mwg-isearch-forward-word)
        (equal this-command 'mwg-isearch-forward-word2))
    (let* ((isw2 (equal this-command 'mwg-isearch-forward-word2))
           (word-begin (progn (if isw2 (skip-syntax-backward "w_")
                                (mwg-isearch-forward-word/cwb)) (point)))
           (rex-wb (if (re-search-forward mwg-isearch-forward-word/rex-iswb nil t)
                            mwg-rex-word-begin ""))
           (word-end (progn (if isw2 (skip-syntax-forward "w_")
                              (mwg-isearch-forward-word/cwe)) (point)))
           (rex-we (if (re-search-forward mwg-isearch-forward-word/rex-iswe nil t)
                       mwg-rex-word-end ""))
           (target-word (concat rex-wb (buffer-substring-no-properties word-begin word-end) rex-we))
           (target-word2 (if (and isearch-case-fold-search
                                  (eq 'not-yanks search-upper-case))
                             (downcase target-word)
                           target-word)))
      (goto-char word-begin)
      (setq isearch-string target-word2
            isearch-message (concat isearch-message
                                    (mapconcat 'isearch-text-char-description
                                               target-word2 ""))
            isearch-yank-flag t)
      ;; (isearch-search-and-update) ;; 無駄に (isearch-push-state) される
      (isearch-search)
      (isearch-update)))
   ((equal this-command 'mwg-isearch-forward)
    ;; TODO 何故か検索候補が反転されない時がある。
    (when (and mwg-pc-mark-active (/= (region-beginning) (region-end)))
      (mwg-pc-nomark)
      (let* ((target (buffer-substring-no-properties (region-beginning) (region-end))))
        (setq isearch-string target)
        (setq isearch-message (mapconcat 'isearch-text-char-description isearch-string ""))
        (goto-char (region-beginning))
        (isearch-search)
        (isearch-update))))))
(add-hook 'isearch-mode-hook 'mwg-isearch-mode-hook)
(defun mwg-isearch-forward-word ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))
(defun mwg-isearch-forward-word2 ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))
(defun mwg-isearch-forward ()
  (interactive)
  (call-interactively 'isearch-forward))
(global-set-key "\M-f\M-s" 'mwg-isearch-forward)
(global-set-key "\M-f\M-w" 'mwg-isearch-forward-word)
(global-set-key "\M-fw" 'mwg-isearch-forward-word2)

; isearch functions
(define-key isearch-mode-map "\C-d"       'mwg-isearch-delete-and-exit)
(define-key isearch-mode-map [delete]     'mwg-isearch-delete-and-exit)
(define-key isearch-mode-map [deletechar] 'mwg-isearch-delete-and-exit)
(define-key isearch-mode-map "\M-[3~"     'mwg-isearch-delete-and-exit) ; delete
(define-key isearch-mode-map [C-delete]   'mwg-isearch-delete-and-exit)
(define-key isearch-mode-map "\M-[3;5~"   'mwg-isearch-delete-and-exit) ; C-delete
(define-key isearch-mode-map [M-delete]   'mwg-isearch-kill-and-exit)
(define-key isearch-mode-map "\M-[3~"   'mwg-isearch-kill-and-exit) ; M-delete
(define-key isearch-mode-map "\M-w"       'mwg-isearch-copy-as-kill-and-exit)
(defun mwg-isearch-delete-and-exit ()
  (interactive)
  (when isearch-success
    (let ((end1 (point))
          (end2 (if mark-active (mark) isearch-other-end)))
      (isearch-exit)
      (delete-region end1 end2))))
(defun mwg-isearch-kill-and-exit ()
  (interactive)
  (when isearch-success
    (let ((end1 (point))
          (end2 (if mark-active (mark) isearch-other-end)))
      (isearch-exit)
      (kill-region end1 end2))))
(defun mwg-isearch-copy-as-kill-and-exit ()
  (interactive)
  (when isearch-success
    (let ((end1 (point))
          (end2 (if mark-active (mark) isearch-other-end)))
      (isearch-exit)
      (kill-ring-save end1 end2))))

(define-key isearch-mode-map "\M-i"       'mwg-isearch-beginning-of-match-and-exit)
(define-key isearch-mode-map "\M-a"       'mwg-isearch-end-of-match-and-exit)
(defun mwg-isearch-beginning-of-match-and-exit ()
  (interactive)
  (when isearch-success
    (let ((end1 (point))
          (end2 isearch-other-end))
      (isearch-exit)
      (goto-char (if (< end1 end2) end1 end2)))))
(defun mwg-isearch-end-of-match-and-exit ()
  (interactive)
  (when isearch-success
    (let ((end1 (point))
          (end2 isearch-other-end))
      (isearch-exit)
      (goto-char (if (> end1 end2) end1 end2)))))

(define-key isearch-mode-map (kbd "C-SPC") 'mwg-isearch-select-and-exit)
(define-key isearch-mode-map "\C-@" 'mwg-isearch-select-and-exit)
(defun mwg-isearch-select-and-exit ()
  ;; 一致領域を選択領域にして終了する。
  ;; 一致していない時は単に set-mark する。
  (interactive)
  (if isearch-success
    (let ((end1 (point))
          (end2 isearch-other-end))
      (isearch-exit)
      (goto-char end2)
      (setq mark-active nil)
      (set-mark-command nil)
      (goto-char end1))
    (mwg-set-mark-command)))

;;*****************************************************************************
;;
;;  Moving and Selection Commands
;;
;;-----------------------------------------------------------------------------

;;---- scroll ---------------------------------------------
(defun mwg-scroll-down-impl2 (&optional count)
 (interactive "p")
 (let ((line-delta (- (- (window-height) 1))))
   (let ((final-window-start (save-excursion
                               (goto-char (window-start))
                               (forward-line line-delta)
                               (point))))
     (forward-line line-delta)
     (set-window-start (selected-window) final-window-start))))
(defun mwg-scroll-up-impl2 (&optional count)
 (interactive "p")
 (let ((line-delta (- (window-height) 1)))
   (let ((final-window-start (save-excursion 
                               (goto-char (window-start))
                               (forward-line line-delta)
                               (point))))
     (forward-line line-delta)
     (set-window-start (selected-window) final-window-start))))
(defun mwg-screen-down (&optional count)
  (interactive "p")
  (let ((final-window-start (save-excursion
                              (goto-char (window-start))
                              (forward-line (- (* 3 (if count count 3))))
                              (point))))
    (set-window-start (selected-window) final-window-start)))
(defun mwg-screen-up (&optional count)
  (interactive "p")
  (let ((final-window-start (save-excursion
                              (goto-char (window-start))
                              (forward-line (* 3 (if count count 3)))
                              (point))))
    (set-window-start (selected-window) final-window-start)))
(global-set-key (kbd "<C-up>") 'mwg-screen-down)
(global-set-key (kbd "<C-down>") 'mwg-screen-up)
(global-set-key (kbd "<prior>") 'mwg-scroll-down-impl2)
(global-set-key (kbd "<next>") 'mwg-scroll-up-impl2)

;;---- wordwise move --------------------------------------

;; (defconst mwg-rex-forward-word "\\([^_[:alnum:]][_[:alnum:]]\\|[_[:alnum:]][^_[:alnum:][:space:]\r\n]\\|[[:space:]\r\n]\\B[^[:space:]\r\n]\\|.[\r\n]\\)")
;; = /(\W\w|\w[^\w\s\r\n]|[\s\r\n]\B[^\s\r\n]|.[\r\n])/
;; = /[ks]w|wk|s\B[wk]|.:n/
;;     k = \p{kigou} = [^\w\s\r\n]
;;     s = \p{kuhaku} = [\s\r\n]
;;     w = \p{word} = [\w]
;; = /kw|sw|wk|s\Bk|.:n/
;; = /kw|s(w|\Bk)|wk|.:n/
;; = /kw|s(w|k)|wk|.:n/
;; = /kw|wk|s(w|k)|.:n/
;;   何か動作が変だと思ったら、漢字・平仮名も [:alnum:] に分類されていた。
;;   また、\b は _ と [:alnum:] の境界にも一致する事に注意。
;;   どうやら、[[:alnum:]]\b[[:alnum:]] で日本語の文節境界に一致する様だ。
;; ->/kw|wk|s(w|k)|[:alnum:]\b[:alnum:]|.:n/
(defconst mwg-rex-forward-word
  (let ((k "[^_[:alnum:][:space:]\r\n]")
        (s "[[:space:]\r\n]")
        (w "[_[:alnum:]]")
        (w-or-k "[^[:space:]\r\n]"))
    ;; (concat "\\(" k w "\\|" s "\\(" w "\\|\\B" k "\\)\\|" w k "\\|.[\r\n]\\)")
    (concat "\\(" k w "\\|" w k "\\|" s w-or-k "\\|[[:alnum:]]\\b[[:alnum:]]\\|.[\r\n]\\)")))

;; (defconst mwg-rex-backward-word "\\([^_[:alnum:][:space:]\r\n][_[:alnum:]]\\|[_[:alnum:]][^_[:alnum:]]\\|[^[:space:]\r\n]\\B[[:space:]\r\n]\\|[\r\n]\\([ ]\\|[^ ]\\)\\)")
;; = /kw|w[ks]|[wk]\Bs|:na/
;;   a = \p{any} = ([ ]|[^ ])
;; = /kw|w[ks]|w\Bs|ks|:na/
;; = /kw|wk|ws|_s|ks|:na/
;; = /kw|wk|(w|k)s|:na/
;; ->/kw|wk|s(w|k)|[:alnum:]\b[:alnum:]|:na/
(defconst mwg-rex-backward-word
  (let ((k "[^_[:alnum:][:space:]\r\n]")
        (s "[[:space:]\r\n]")
        (w "[_[:alnum:]]")
        (w-or-k "[^[:space:]\r\n]")
        (a "\\([ ]\\|[^ ]\\)"))
    (concat "\\(" k w "\\|" w k "\\|" w-or-k s "\\|[[:alnum:]]\\b[[:alnum:]]\\|[\r\n]" a "\\)")))

(defun mwg-forward-word2 (&optional arg)
  (interactive "p")
  (cond
   ((re-search-forward mwg-rex-forward-word nil t arg)
    (backward-char 1))
   (t
    (goto-char (point-max)))))
(defun mwg-backward-word2 (&optional arg)
  (interactive "p")
  (cond
   ((re-search-backward mwg-rex-backward-word nil t arg)
    (forward-char 1))
   (t
    (goto-char (point-min)))))
(global-set-key (kbd "<C-right>") 'mwg-forward-word2)
(global-set-key (kbd "<C-left>") 'mwg-backward-word2)


(defun mwg-kill-forward-word (&optional arg)
  (interactive "p")
  (let ((st (point)))
    (mwg-forward-word2 arg)
    (kill-region st (point))))
(defun mwg-kill-backward-word (&optional arg)
  (interactive "p")
  (let ((st (point)))
    (mwg-backward-word2 arg)
    (kill-region st (point))))
(defun mwg-delete-forward-word (&optional arg)
  (interactive "p")
  (let ((st (point)))
    (mwg-forward-word2 arg)
    (delete-region st (point))))
(defun mwg-delete-backward-word (&optional arg)
  (interactive "p")
  (let ((st (point)))
    (mwg-backward-word2 arg)
    (delete-region st (point))))
(global-set-key (kbd "<delete>")      'delete-char)
(global-set-key (kbd "<deletechar>")  'delete-char)
(global-set-key (kbd "<C-delete>")    'mwg-delete-forward-word)
(global-set-key (kbd "<C-backspace>") 'mwg-delete-backward-word)
(global-set-key "\C-_"                'mwg-delete-backward-word)
(global-set-key (kbd "<M-delete>")    'mwg-kill-forward-word)
(global-set-key (kbd "<M-backspace>") 'mwg-kill-backward-word)
(global-set-key (kbd "M-DEL")         'mwg-kill-backward-word)

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\M-h] 'mwg-kill-backward-word)

(defun backward-transpose-chars (&optional arg)
  (interactive "p")
  ;; (save-excursion (transpose-chars 1))
  ;; (backward-char)
  (transpose-chars 1)
  (backward-char 2))
(global-set-key [?\C-\S-t] 'backward-transpose-chars)

;--- indentation --------------------

(defun mwg-back-to-indentation (&optional arg)
  (interactive "p")
  (if (eq last-command this-command)
      (progn (call-interactively 'move-beginning-of-line)
             (setq this-command 'move-beginning-of-line))
    (back-to-indentation)))

(global-set-key "\M-m" 'mwg-back-to-indentation)

;-------------------------------------------------------------------------------
; selection commands

;---- mwg-pc-mark ---------------------
(defvar mwg-pc-mark-active nil)

;; 2013-08-26
(defmacro mwg-pcmark/define-both (funcname &optional form)
  (let* ((fname (replace-regexp-in-string "^mwg-" "" (symbol-name funcname)))
         (funcname-nomark (intern (concat "mwg-" fname "-nomark")))
         (funcname-mark (intern (concat "mwg-" fname "-mark")))
         (proc (if (and form (listp form))
                   form
                 `(,funcname count))))
    `(progn
       (defun ,funcname-nomark (&optional count)
         (interactive "p")
         (mwg-pc-nomark)
         ,proc)
       (defun ,funcname-mark (&optional count)
         (interactive "p")
         (mwg-pc-mark)
         ,proc))))
(defmacro mwg-pcmark/define-nomarking (funcname &optional form)
  (let* ((fname (replace-regexp-in-string "^mwg-" "" (symbol-name funcname)))
         (funcname-nomark (intern (concat "mwg-" fname "-nomark")))
         (proc (if (and form (listp form))
                   form
                 `(,funcname count))))
    `(defun ,funcname-nomark (&optional count)
       (interactive "p")
       ,proc
       (mwg-pc-nomark))))
;; 何故か emacs-23.1 以前では (kbd (car '("a" "b"))) がエラーになる。
;; kbd がマクロとして実装されていて引数が文字列かどうかで場合分けしているから?
;; 引数が文字列と分かっているのであれば、read-kbd-macro を使えば良い。
;; 参考: http://toro.2ch.net/test/read.cgi/unix/1322556644/300-304
(defmacro mwg-pcmark/global-set-key-both (keyspec funcname)
  (let* ((fname (replace-regexp-in-string "^mwg-" "" (symbol-name funcname)))
         (funcname-mark (intern (concat "mwg-" fname "-mark")))
         (funcname-nomark (intern (concat "mwg-" fname "-nomark")))
         generate
         (generate
          (lambda (klist)
            (and klist
                 (let* ((k (car klist))
                        (key-nomark (if (consp k)
                                        (if (stringp (car k))
                                            (read-kbd-macro (car k))
                                          (car k))
                                      (read-kbd-macro k)))
                        (key-mark (if (consp k)
                                      (if (stringp (cdr k))
                                          (read-kbd-macro (cdr k))
                                        (cdr k))
                                    (read-kbd-macro (concat "S-" k)))))
                   `((global-set-key ,key-nomark ',funcname-nomark)
                     (global-set-key ,key-mark ',funcname-mark)
                     . ,(funcall generate (cdr klist)))))))
         (klist0 (if (listp keyspec) keyspec (list keyspec))))
    `(progn . ,(funcall generate klist0))))
(defmacro mwg-pcmark/register-both (keyname funcname &optional form)
  `(progn
     (mwg-pcmark/define-both ,funcname ,form)
     (mwg-pcmark/global-set-key-both ,keyname ,funcname)))

;; 2015-02-01
;; byte-compile 時にエラーが出るので、
;; 以下の関数は使用していなくても常にグローバルに定義しておく必要がある。
;;
;; mark
(defun mwg-pc-mark ()
  (or (and mwg-pc-mark-active mark-active)
      (progn
        (set-mark-command nil)
        (setq mwg-pc-mark-active t))))
(defun mwg-pc-nomark ()
  (when mwg-pc-mark-active
    (setq mark-active nil)
    (setq mwg-pc-mark-active nil)))
(defun mwg-set-mark-command (&optional count)
  "set mark (after clearing S-mark flag)"
  (interactive "p")
  (setq mwg-pc-mark-active nil)
  (set-mark-command nil))

;; forward/backward
(defun mwg-backward-line (&optional count)
  (interactive "p")
  (forward-line (- (if count count 1))))

;; 上下移動
(defun mwg-next-line (&optional count)
  (interactive "p")
  (setq current-prefix-arg (if count count 1))
  (call-interactively 'next-line))
(defun mwg-prev-line (&optional count)
  (interactive "p")
  (setq current-prefix-arg (if count count 1))
  (call-interactively 'previous-line))
(defun mwg-beginning-of-buffer (&optional count)
  (interactive "p")
  (call-interactively 'beginning-of-buffer))
(defun mwg-end-of-buffer (&optional count)
  (interactive "p")
  (call-interactively 'end-of-buffer))

;;  による移動
(defun mwg-prev-page (&optional count)
  (interactive "p")
  (or (search-backward "" nil t (if count count 1))
      (goto-char (point-min))))
(defun mwg-next-page (&optional count)
  (interactive "p")
  (if (= (char-after) ?\f) (forward-char 1))
  (if (search-forward "" nil t (if count count 1))
      (backward-char 1)
    (goto-char (point-max))))

(defun mwg-init-pcmark ()
  ;;----------------------------------------------------------------------------
  ;; forward/backward
  (mwg-pcmark/register-both ("<right>"   "C-f"  ) forward-char)
  (mwg-pcmark/register-both ("<left>"    "C-b"  ) backward-char)
  (mwg-pcmark/register-both ("C-<right>" "C-M-f") mwg-forward-word2)
  (mwg-pcmark/register-both ("C-<left>"  "C-M-b") mwg-backward-word2)
  (mwg-pcmark/register-both ("M-<right>"        ) forward-word)
  (mwg-pcmark/register-both ("M-<left>"  "M-b"  ) backward-word)

  ;; M-f は別の用途に登録している
  (global-set-key [?\M-\S-f]    'mwg-forward-word-mark)

  (mwg-pcmark/define-both forward-line)
  (mwg-pcmark/define-both mwg-backward-line)
  (mwg-pcmark/define-both forward-paragraph)
  (mwg-pcmark/define-both backward-paragraph)
  (mwg-pcmark/define-both forward-sexp)
  (mwg-pcmark/define-both backward-sexp)

  ;; (require 'misc) ;; forward-to-word/backward-to-word
  ;; (when (and (fboundp 'forward-to-word) (fboundp 'backward-to-word))
  ;;   (mwg-pcmark/register-both "C-M-right" forward-to-word)
  ;;   (mwg-pcmark/register-both "C-M-left"  backward-to-word))

  ;;----------------------------------------------------------------------------
  ;; beginning/end
  (mwg-pcmark/register-both ("<home>" "C-a") beginning-of-line)
  (mwg-pcmark/register-both ("<end>"  "C-e") end-of-line)
  (mwg-pcmark/register-both ("C-<home>" ([?\M-,] . [?\M-<]) "M-<home>")
                            mwg-beginning-of-buffer
                            (mwg-beginning-of-buffer))
  (mwg-pcmark/register-both ("C-<end>"  ([?\M-.] . [?\M->]) "M-<end>")
                            mwg-end-of-buffer
                            (mwg-end-of-buffer))
  ;; ※ (mwg-beginning-of-buffer) 等とするのは count を渡さない様にする為
  ;; ※ M-home, M-end は実のところ使っていないので、他の物を割り当てても良い。

  (mwg-pcmark/register-both (([?\M-m] . [?\M-M]))
                            mwg-back-to-indentation
                            (if (eq last-command this-command)
                                (progn (call-interactively 'move-beginning-of-line)
                                       (setq this-command 'move-beginning-of-line))
                              (back-to-indentation)))

  ;;----------------------------------------------------------------------------
  ;; 上下移動
  ;; TODO: how to define next/previous-line preserving goal-column in Lisp code?

  (mwg-pcmark/register-both ("<up>"   "C-p") mwg-prev-line)
  (mwg-pcmark/register-both ("<down>" "C-n") mwg-next-line)
  (mwg-pcmark/register-both ("M-<up>"   ([?\M-p] . [?\M-P]))
                            mwg-prev-line3
                            (mwg-prev-line (* 3 (if count count 1))))
  (mwg-pcmark/register-both ("M-<down>" ([?\M-n] . [?\M-N]))
                            mwg-next-line3
                            (mwg-next-line (* 3 (if count count 1))))

  ;;----------------------------------------------------------------------------
  ;; スクロール・ページ移動
  (mwg-pcmark/register-both ("C-<up>"   ([?\C-,] . [?\C-<])) mwg-screen-down)
  (mwg-pcmark/register-both ("C-<down>" ([?\C-.] . [?\C->])) mwg-screen-up)
  (mwg-pcmark/register-both ("<prior>"  ([?\M-v] . [?\M-V]))  mwg-scroll-down-impl2)
  (mwg-pcmark/register-both ("<next>"   "C-v")   mwg-scroll-up-impl2)
  (mwg-pcmark/define-both scroll-down)
  (mwg-pcmark/define-both scroll-up)

  ;;----------------------------------------------------------------------------
  ;; ^L による移動
  (mwg-pcmark/register-both "C-<prior>" mwg-prev-page)
  (mwg-pcmark/register-both "C-<next>"  mwg-next-page)

  ;;----------------------------------------------------------------------------
  ;; 切り取り・コピー・貼り付け
  (mwg-pcmark/define-nomarking mwg-copy-as-kill)
  (mwg-pcmark/define-nomarking mwg-kill-region)
  (mwg-pcmark/define-nomarking exchange-point-and-mark)
  (global-set-key "\C-w"         'mwg-kill-region-nomark)
  (global-set-key "\M-w"         'mwg-copy-as-kill-nomark)
  (global-set-key [C-insert]     'mwg-copy-as-kill-nomark)
  (global-set-key [C-insertchar] 'mwg-copy-as-kill-nomark)
  ;; (global-set-key "\C-x\C-x"     'mwg-exchange-point-and-mark-nomark)

  (global-set-key (kbd "C-SPC") 'mwg-set-mark-command)
  (global-set-key "\C-@" 'mwg-set-mark-command))

(defun mwg-init-mouse ()
  (when (or (string= (getenv "TERM") "rosaterm")
            (and (or (string= (getenv "TERM") "screen")
                     (string= (getenv "TERM") "screen-256color"))
                 (string= (getenv "MWG_LOGINTERM") "rosaterm")))
    (require 'mouse)
    (xterm-mouse-mode 1)
    (global-set-key [mouse-4] 'mwg-screen-down)
    (global-set-key [mouse-5] 'mwg-screen-up)
    (if (not (fboundp 'track-mouse))
        (defun track-mouse (&optional dummy) t))))

;;*****************************************************************************
;;
;;  Color Settings
;;
;;-----------------------------------------------------------------------------
(defun mwg-init-custom-color ()
  (global-font-lock-mode t)
  (setq transient-mark-mode t)

  ;; font-lock faces
  (custom-set-faces
   '(font-lock-type-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "lightseagreen"))
      (t
       (:foreground "cyan"))))
   '(font-lock-function-name-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "darkviolet")) ;; royalblue
      (t
       (:foreground "blue"))))
   '(font-lock-variable-name-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "darkorange"))
      (((class color) (min-colors 16) (background light))
       (:foreground "red1"))
      (t
       (:foreground "red"))
      ))
   '(font-lock-string-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "forestgreen"))
      (t
       ;; (:foreground "black" :background "white")
       (:foreground "green"))))
   '(font-lock-keyword-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "blue1"))
      (t
       (:foreground "blue" :weight bold))))
   '(font-lock-builtin-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "red1"))
      (t
       (:foreground "red" :weight bold))))
   '(font-lock-constant-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "magenta1"))
      (t
       (:foreground "magenta" :weight bold))))
   '(font-lock-comment-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "grey40"))
      (t
       (:foreground "black" :weight bold))))
   '(font-lock-warning-face
     ((((class color) (min-colors 88) (background light))
       (:background "mistyrose" :foreground "black"))
      (((class color) (min-colors 16) (background light))
       (:background "black" :foreground "brightred"))
      (t
       (:background "black" :foreground "red1" :weight bold))))
   '(font-lock-doc-face
     ((((class color) (min-colors 88) (background light))
       (:background "color-230" :foreground "color-52" :inherit font-lock-string-face))
      (t
       (:inherit font-lock-string-face)))))

  (when (boundp 'font-lock-comment-delimiter-face)
    (custom-set-faces
     '(font-lock-comment-delmiter-face
       ((((class color) (min-colors 16) (background light))
         (:foreground "grey40"))
        (t
         (:foreground "black" :weight bold))))
     ))
  
  ;; --- isearch colors ---
  (custom-set-faces
   '(isearch
     ((((class color) (min-colors 16) (background light))
       (:foreground "gray100" :background "purple4"))
      (t
       (:foreground "white" :background "magenta4" :weight bold))))
   '(isearch-fail
     ((((class color) (min-colors 16) (background light))
       (:background "red" :foreground "gray100"))
      (t
       (:background "red" :foreground "white" :weight bold))))
   '(lazy-highlight
     ((((class color) (min-colors 16) (background light))
       (:foreground "gray100" :background "steelblue"))
      (t
       (:foreground "white" :weight bold :background "turquoise3")))))

  ;; --- diff colors ---
  (custom-set-faces
   '(diff-changed
     ((((class color) (min-colors 88) (background light))
       (:background "lightyellow"))
      (t
       (:foreground "red"))))
   '(diff-added
     ((((class color) (min-colors 88) (background light))
       (:background "palegreen"))
      (t
       (:foreground "green"))))
   '(diff-removed
     ((((class color) (min-colors 88) (background light))
       (:background "mistyrose"))
      ))
   '(diff-context
     ((((class color) (min-colors 88) (background light))
       (:forground "black"))
      (t
       (:forground "black" :weight bold))))
   '(diff-header
     ((((class color) (min-colors 88) (background light))
       (:foreground "grey100" :background "grey45"))
      (t
       (:foreground "white" :weight bold :background "blue"))))
   '(diff-file-header
     ((((class color) (min-colors 88) (background light))
       (:foreground "lightcyan" :background "grey60"))
      (t
       (:foreground "cyan" :weight bold :background "blue"))))
   '(diff-hunk-header
     ((((class color) (min-colors 88) (background light))
       (:foreground "blue1" :underline "on"))
      (t
       (:inherit diff-header)))))

  ;; --- ediff colors ---
  (custom-set-faces
   '(ediff-even-diff-A
     ((((class color) (min-colors 88) (background light))
       (:background "color-25" :foreground "brightwhite"))
      (t
       (:background "cyan1" :foreground "yellow1" :weight bold))
      ))
   '(ediff-even-diff-B
     ((t
       (:inherit ediff-even-diff-A))))
   '(ediff-even-diff-C
     ((t
       (:inherit ediff-even-diff-A))))
   '(ediff-odd-diff-A
     ((((class color) (min-colors 88) (background light))
       (:background "color-195" :foreground "black"))
      (t
       (:background "brightcyan" :foreground "black"))))
   '(ediff-odd-diff-B
     ((t
       (:inherit ediff-odd-diff-A))))
   '(ediff-odd-diff-C
     ((t
       (:inherit ediff-odd-diff-A))))
   '(ediff-current-diff-A
     ((((class color) (min-colors 88) (background light))
       (:background "color-225" :foreground "red"))
      (t
       (:background "magenta1" :foreground "red" :weight bold))
      ))
   '(ediff-current-diff-B
     ((t
       (:inherit ediff-current-diff-A))))
   '(ediff-current-diff-C
     ((t
       (:inherit ediff-current-diff-A))))
   '(ediff-fine-diff-A
     ((((class color) (min-colors 88) (background light))
       (:background "red" :foreground "color-225"))
      (t
       (:background "magenta1" :foreground "white" :weight bold))))
   '(ediff-fine-diff-B
     ((t
       (:inherit ediff-fine-diff-A))))
   '(ediff-fine-diff-C
     ((t
       (:inherit ediff-fine-diff-A)))))

  ;; --- js2-mode ---

  (custom-set-faces
   '(js2-jsdoc-tag
     ((((class color) (min-colors 88) (background light))
       ;; default foreground is slategray
       (:foreground "color-23" :inherit font-lock-doc-face))
      (t (:foreground "green" :inherit font-lock-doc-face))))
   '(js2-jsdoc-value
     ((((class color) (min-colors 88) (background light))
       ;; default foreground is peachpuff3
       (:foreground "color-166" :inherit font-lock-doc-face))
      (t (:foreground "magenta" :inherit font-lock-doc-face))))
   '(js2-jsdoc-type
     ((((class color) (min-colors 88) (background light))
       ;; default foreground is steelblue
       (:foreground "color-61" :inherit font-lock-doc-face))
      (t (:foreground "magenta" :inherit font-lock-doc-face))))
   '(js2-jsdoc-html-tag-name
     ((((class color) (min-colors 88) (background light))
       ;; default foreground is rosybrown
       (:foreground "color-91" :inherit font-lock-doc-face))
      (t (:foreground "magenta" :inherit font-lock-doc-face))))
   '(js2-jsdoc-html-tag-delimiter
     ((((class color) (min-colors 88) (background light))
       ;; default foreground is steelblue
       (:foreground "color-18" :inherit font-lock-doc-face))
      (t (:foreground "darkkhaki" :inherit font-lock-doc-face)))))

  ;; --- others ---
  (custom-set-faces
   '(match
     ((((class color) (min-colors 88) (background light))
       (:foreground "gray100" :background "green2"))
      ))
   '(region
     ((((class color) (min-colors 16) (background light))
       (:foreground "gray100" :background "steelblue4"))
      (((class color))
       (:foreground "white" :background "blue"))
      (t
       (:inverse-video 1))
      ))
   '(secondary-selection
     ((((class color) (min-colors 88) (background light))
       (:foreground "gray100" :background "cornflowerblue"))
      ))
   '(custom-variable-tag
     ((((class color) (min-colors 88) (background light))
       (:foreground "skyblue3"))
      ))
   '(show-paren-match
     ((((class color) (min-colors 88) (background light))
       (:background "steelblue2"))
      ))
   ;; --- mode-line colors ---
   '(mode-line
     ((t
       (:inverse-video t))))
   '(mode-line-buffer-id
     ((((class color) (min-colors 88) (background light))
       (:background "aquamarine"))
      (t ;; todo
       (:weight bold))
      ))
   '(mode-line-emphasis
     ((((class color) (min-colors 88) (background light))
       (:background "aquamarine"))
      (t ;; todo
       (:weight bold))))
   '(mode-line-inactive
     ((((class color) (min-colors 88) (background light))
       (:inverse-video t :foreground "grey50" :background "grey100"))
      (t ;; todo
       (:inherit mode-line))))
   '(tty-menu-disabled-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "color-240" :background "color-254"))
      (t
       (:foreground "white" :background "blue"))))
   '(tty-menu-enabled-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "black" :weight: bold :inherit tty-menu-disabled-face))
      (t
       (:foreground "yellow" :background "blue" :weight bold))))
   '(tty-menu-selected-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "color-231" :background "blue" :inherit tty-menu-disabled-face))
      (t
       (:background "red" :weight bold))))
   ;; -- others --
   '(ac-completion-face
     ((((class color) (min-colors 88) (background light))
       (:foreground "blue1" :background "lightgray"))
      (t
       (:background "lightgray" :foreground "blue" :weight bold))))
   '(sh-heredoc
     ((((class color) (min-colors 88) (background light))
       (:foreground "forestgreen" :background "grey95"))
      (t
       (:background "lightgray"))))
   '(anything-ff-file
     ((((class color) (min-colors 88) (background light))
       (:foreground "CadetBlue4" :underline "on"))
      (t
       (:foreground "cyan" :weight bold :underline "on"))
      ))
   '(yas/field-highlight-face
     ((((class color) (min-colors 88) (background light))
       (:background "lightblue"))
      (t
       (:background "cyan1"))))
   '(widget-field
     ((((class color) (min-colors 88) (background light))
       (:background "yellow1"))
      (t
       (:background "black" :foreground "green1" :weight bold))
      ))))

(defvar mwg-option-enable-color nil)
(defvar mwg-option-set-customcolor nil)
(when mwg-option-enable-color
  (message "mwg-option-enabel-color obsoleted, use (mwg-init-custom-color) instead")
  (mwg-init-custom-color))
(when mwg-option-set-customcolor
  (message "mwg-option-set-customcolor obsoleted, use (mwg-init-custom-color) instead.")
  (mwg-init-custom-color))

(defun mwg-revert-color (&optional arg)
  (interactive "p")
  (custom-set-variables
   '(frame-background-mode (quote dark)))
  (custom-set-faces
   '(font-lock-keyword-face
     ((t (:foreground "cyan1"))))
   '(font-lock-function-name-face
     ((t (:foreground "cyan"))))
   '(default
     ((t (:foreground "white" :background "black"))))
   ))
(defun mwg-unrevert-color (&optional arg)
  (interactive "p")
  (custom-set-variables
   '(frame-background-mode 'light))
  (custom-set-faces
   '(font-lock-keyword-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "blue1"))
      (t
       (:foreground "blue" :weight bold))))
   '(font-lock-function-name-face
     ((((class color) (min-colors 16) (background light))
       (:foreground "darkviolet")) ;; royalblue
      (t
       (:foreground "blue"))))
   '(default
     ((t (:foreground "unspecified-fg" :background "unspecified-bg"))))))
