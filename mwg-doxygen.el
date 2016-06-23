;;; mwg-doxygen.el --- Doxygen Syntax Highlighting for GNU Emacs
;; Filename: mwg-doxygen.el
;; Description: Doxygen Syntax Highlighting
;;
;; Author: Koichi Murase <myoga.murase@gmail.com>,
;;         Copyright (C) 2013, Koichi Murase, all rights reserved.
;; Created: 2013-08-05 02:50:50
;; Version: 1.0.0
;; Keywords: mwg-doxygen doxygen c++
;; Compatibility: GNU Emacs 24
;;
;;---------------------------------------------------------------------------;;
;;  Doxygen Syntax Highlighting (mwg-doxygen.el)                             ;;
;;---------------------------------------------------------------------------;;
;; Usage                                                                     ;;
;;                                                                           ;;
;;   Add the following codes to your .emacs.el                               ;;
;;   001| (require 'cc-mode)                                                 ;;
;;   002| (autoload 'mwg-doxygen-font-lock-keywords "mwg-doxygen"            ;;
;;   003|   "Doxygen Syntax Highlighting for cc-mode." t)                    ;;
;;   004| (add-to-list 'c-doc-comment-style '(c++-mode . mwg-doxygen))       ;;
;;                                                                           ;;
;;---------------------------------------------------------------------------;;
;; Provided interactive functions:                                           ;;
;;                                                                           ;;
;;   (defun `mwg-doxygen-font-lock-keywords')                                ;;
;;                                                                           ;;
;; Provided customizable faces                                               ;;
;;                                                                           ;;
;;   (defface `mwg-doxygen/content-face')                                    ;;
;;   (defface `mwg-doxygen/keyword-face')                                    ;;
;;   (defface `mwg-doxygen/title-face')                                      ;;
;;   (defface `mwg-doxygen/em-face')                                         ;;
;;   (defface `mwg-doxygen/file-face')                                       ;;
;;   (defface `mwg-doxygen/item-face')                                       ;;
;;   (defface `mwg-doxygen/label-face')                                      ;;
;;   (defface `mwg-doxygen/warning-face')                                    ;;
;;   (defface `mwg-doxygen/code-face')                                       ;;
;;   (defface `mwg-doxygen/xml-delim-face')                                  ;;
;;   (defface `mwg-doxygen/lwiki-bold-face')                                 ;;
;;   (defface `mwg-doxygen/mwgpp-face'                                       ;;
;;   (defface `mwg-doxygen/mwgpp-directive-name'                             ;;
;;   (defface `mwg-doxygen/mwgpp-macro-name'                                 ;;
;;                                                                           ;;
;;---------------------------------------------------------------------------;;
;; ChangeLog                                                                 ;;
;;                                                                           ;;
;;   2016-04-04, v1.0.5, KM,                                                 ;;
;;     * new: support mwgpp                                                  ;;
;;   2016-04-01, v1.0.4, KM,                                                 ;;
;;     * new: support lwiki                                                  ;;
;;     * bugfix: 行コメント結合時のインデント                                ;;
;;   2013-08-08, v1.0.3, KM,                                                 ;;
;;     * bugfix: cc-fonts, fake mail address, regex for content-face         ;;
;;     * multiline tex markup,                                               ;;
;;     * tex coloring, xml coloring                                          ;;
;;     * modified font-lock-apply-highlight with remove and chkset           ;;
;;   2013-08-06, v1.0.2, KM,                                                 ;;
;;     * 読込方法の変更, bugfix of faces, \f$...\f$ など                     ;;
;;   2013-08-05, v1.0.1, KM,                                                 ;;
;;     * .emacs ファイルから分離し独立したファイルに                         ;;
;;   2013-08-04, v1.0.0, KM,                                                 ;;
;;     * doxygen の色付けが出来ないかと考えて実装開始                        ;;
;;                                                                           ;;
;;---------------------------------------------------------------------------;;

(provide 'mwg-doxygen)
(require 'mwg-doxygen)
(require 'cc-mode)

(defun mwg/rex-or (&rest args)
  (mapconcat 'identity args "\\|"))

(defgroup mwg-doxygen nil "Myoga Doxygen Syntax Highlighting" :group 'c)
(defface mwg-doxygen/content-face
  '((((class color) (min-colors 88) (background light))
     ;; (:background "color-254" :foreground "red" :inherit font-lock-doc-face)
     (:background "color-230" :foreground "color-52" :inherit font-lock-doc-face))
    (default :foreground "red" :inherit font-lock-doc-face))
  "Face used to highlight document contents in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/keyword-face
  ;; "@param" 等の face
  '((((class color) (min-colors 88) (background light))
     (:foreground "color-23" :inherit font-lock-function-name-face))
    (default :inherit font-lock-function-name-face))
  "Face used to highlight keywords in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/argument-face
  ;; "@param[in]" の "in" 等の face
  '((((class color) (min-colors 88) (background light))
     (:foreground "color-61" :inherit font-lock-type-face))
    (default :inherit font-lock-type-face))
  "Face used to highlight arguments of a command in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/title-face
  '((((background light))
     (:foreground "black" :weight bold))
    (default :foreground "white1" :weight bold))
  "Face used to highlight titles in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/em-face
  '((default :foreground "brightblue" :underline t))
  "Face used to highlight emphasized texts in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/item-face
  '((default :inverse-video t))
  "Face used to highlight emphasized texts in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/file-face
  '((default :inherit font-lock-string-face))
  "Face used to highlight filenames, uris and e-mail addresses in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/label-face
  '((default :inherit font-lock-constant-face))
  "Face used to highlight filenames, uris and e-mail addresses in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/warning-face
  '((default :inherit font-lock-warning-face))
  "Face used to highlight errors in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/code-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "black" :background "color-255"))
    (default :foreground "black"))
  "Face used to highlight embedded codes in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/xml-delim-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "color-18" :inherit font-lock-builtin-face))
    (default :inherit font-lock-builtin-face))
  "Face used to highlight xml tag in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/xml-name-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "color-91" :inherit font-lock-function-name-face))
    (default :inherit font-lock-function-name-face))
  "Face used to highlight xml tag name in documentations."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/lwiki-bold-face
  '((default :weight bold))
  "Face used to emphasize phrases in lwiki."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/mwgpp-face
  '((((class color) (min-colors 88) (background light))
     (:inherit font-lock-comment-face)) ;; :background "color-225"
    (default :inherit font-lock-comment-face))
  "Face used to highlight mwgpp commands."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/mwgpp-macro-name
  '((((class color) (min-colors 88) (background light))
     (:inherit font-lock-variable-name-face)) ;; :foreground "color-198"
    (default :inherit font-lock-variable-name-face))
  "Face used to highlight mwgpp macro names."
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/mwgpp-directive-name
  '((default :foreground "brightblue" :inherit font-lock-preprocessor-face))
  "Face used to highlight mwgpp directive names."
  :version "24.1"
  :group 'mwg-doxygen)

(defconst mwg-doxygen/pragma-prefix "^[[:space:]]*#[[:space:]]*pragma\\_>[[:space:]]*")

(defconst mwg-doxygen/rex-NCName "[_[:alpha:]][-._[:alnum:]]*")
(defconst mwg-doxygen/rex-csymf "[^ -/:-@[-^`{-~[:cntrl:][:space:]0-9]")
(defconst mwg-doxygen/rex-csym  "[^ -/:-@[-^`{-~[:cntrl:][:space:]]")
(let* ((rex_str "\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\|'\\(?:[^'\\]\\|\\\\.\\)*'")
       (rex_pr1 "([^()'\"]*)")
       (rex_pr2 (concat "(\\(?:[^()'\"]\\|" rex_pr1 "\\|" rex_str "\\)*)"))
       (rex_pr3 (concat "(\\(?:[^()'\"]\\|" rex_pr2 "\\|" rex_str "\\)*)"))
       (rex_pr4 (concat "(\\(?:[^()'\"]\\|" rex_pr3 "\\|" rex_str "\\)*)"))
       (rex_pr5 (concat "(\\(?:[^()'\"]\\|" rex_pr4 "\\|" rex_str "\\)*)"))
       (rex_tmpl1 "<[^<>()'\"]>")
       (rex_tmpl2 (concat "<\\(?:[^<>()'\"]\\|" rex_tmpl1 "\\|" rex_pr1 "\\|" rex_str "\\)*>"))
       (rex_tmpl3 (concat "<\\(?:[^<>()'\"]\\|" rex_tmpl2 "\\|" rex_pr2 "\\|" rex_str "\\)*>"))
       (rex_tmpl4 (concat "<\\(?:[^<>()'\"]\\|" rex_tmpl3 "\\|" rex_pr3 "\\|" rex_str "\\)*>"))
       (rex_tmpl5 (concat "<\\(?:[^<>()'\"]\\|" rex_tmpl4 "\\|" rex_pr4 "\\|" rex_str "\\)*>"))
       (lwiki_br1 "{[^{}]*}")
       (lwiki_br2 (concat "{\\(?:[^{}]\\|" lwiki_br1 "\\)*}"))
       (lwiki_br3 (concat "{\\(?:[^{}]\\|" lwiki_br2 "\\)*}"))
       (lwiki_br4 (concat "{\\(?:[^{}]\\|" lwiki_br3 "\\)*}"))
       (lwiki_br5 (concat "{\\(?:[^{}]\\|" lwiki_br4 "\\)*}")))
  (defconst mwg-doxygen/rex:c++-string-literal rex_str)
  (defconst mwg-doxygen/rex:c++-nested-paren rex_pr5)
  (defconst mwg-doxygen/rex:c++-template-argument rex_tmpl5)
  (defconst mwg-doxygen/rex:lwiki-nested-brace lwiki_br5))

(defconst mwg-doxygen/doc:doxygen/rex-line-head
  "\\(?:^[[:space:]]*\\(?:/\\*[*!]<?\\|//[/!]<?\\|\\*\\)[[:space:]]?\\)")

(defconst mwg-doxygen-font-lock-doc-comments
  (let ((rex_sword "[^[:space:][:cntrl:]]+"))
    `((,(concat mwg-doxygen/doc:doxygen/rex-line-head "\\|^\\(?:[[:space:]]+\\)\\'\\|\\(.*\n\\|.+$\\)") ;; content
       (1 'mwg-doxygen/content-face prepend t))
      (,(let* ((rex_id "\\(?:\\_<.\\)\\(?:\\sw\\|_\\)*")
               ;;(rex_id "\\(?:\\_<\\sw\\|\\B_\\|^_\\)\\(?:\\sw\\|_\\)*")
               ;;↑これだと _ の連続に対し超線形になる。_ の開始をうまく捉えられていない。
               (rex_cv "\\(?:\\s *\\_<\\(?:const\\|volatile\\)\\_>\\)*")
               (rex_end "\\(?:\\Sw\\|$\\)")
               ;; from http://blog.livedoor.jp/dankogai/archives/51189905.html
               (mail_atom      "[a-zA-Z0-9_!#$%&'*+/=?^`{}~|-]+")
               (mail_dot_atom  (concat mail_atom "\\(?:\\." mail_atom "\\)*"))
               (mail_quoted    (concat "\"\\(?:\\\\[^\r\n]\\|[^\\\"]\\)*\""))
               (mail_local     (concat "\\(?:" mail_dot_atom "\\|" mail_quoted "\\)"))
               (mail_domain_lit(concat "\\[\\(?:\\\\\\S \\|[!-Z^-~]\\)*\\]"))
               (mail_domain    (concat "\\(?:" mail_dot_atom "\\|" mail_domain_lit "\\)"))
               (mail_addr_spec (concat mail_local "@" mail_domain )))
          (concat
           "\\("
             ;; 関数名(...) など
             "\\(?:\\(?:#\\|::\\)?" rex_id "\\(?:" mwg-doxygen/rex:c++-template-argument "\\)?\\)+"
             "\\(?:" mwg-doxygen/rex:c++-nested-paren rex_cv "\\)"
           "\\|"
             ;; クラス名::関数名 など
             "\\(?:" rex_id "\\(?:" mwg-doxygen/rex:c++-template-argument "\\)?\\)?"
             "\\(?:\\(?:#\\|::\\)" rex_id "\\(?:" mwg-doxygen/rex:c++-template-argument "\\)?\\)+"
           "\\)" rex_end "\\|\\("
             ;; URL
             "\\<\\(?:https?\\|ftp\\|file\\)://"
             "\\(?:[a-zA-Z0-9]\\|%[[:xdigit:]]\\{2\\}\\|[!#$&'()*+,-./:;=?@_~]\\)*"
             "\\(?:[a-zA-Z0-9]\\b\\|%[[:xdigit:]]\\{2\\}\\b\\|[!#$&'()*+,-./:;=?@_~]\\(?:\\B\\|$\\)\\)"
           "\\|"
             ;; メールアドレス
             mail_addr_spec
           "\\|"
             "\\S *\\.[a-zA-Z0-9_]*\\(?:[a-zA-Z0-9]\\b\\|_\\(?:\\B\\|$\\)\\)"
           "\\)"
           )) ;; func() hoge.txt http://example.com/
       (1 'font-lock-doc-face remove t)
       (1 'mwg-doxygen/label-face prepend t)
       (let ((mbegin (match-beginning 2)))
         (and mbegin
              (not (mwg-doxygen/c-comment-begin-p mbegin))
              (not (string-match-p "^[[:space:][:cntrl:]]*\\*$"
                                   (buffer-substring-no-properties
                                    (1+ mbegin)
                                    (save-excursion
                                      (goto-char mbegin)
                                      (line-beginning-position)))))
              '(2 'mwg-doxygen/file-face prepend t))))
      ;; ("</?\\sw\\(\\sw\\|\\s \\|[_=\n\r*.:]\\|\"[^\"]*\"\\|'[^']*'\\)*>"
      ;;  0 'mwg-doxygen/keyword-face prepend nil)
      ;; Xml Tag
      (,(concat "\\(</?\\)\\(" mwg-doxygen/rex-NCName "\\(?::" mwg-doxygen/rex-NCName "\\)?\\)\\(\\(?:\\sw\\|\\s \\|[_=\n\r*.:]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)\\(/?>\\)")
       (1 'mwg-doxygen/xml-delim-face prepend nil)
       (2 'mwg-doxygen/xml-name-face prepend nil)
       (3 mwg-doxygen/doc:doxygen/xmltag-keylist keylist)
       (4 'mwg-doxygen/xml-delim-face prepend nil))
      (,(concat
         "\\([\\@]\\(?:"
         (mwg/rex-or
          ;; \cmd ... (後で詳細な文法に従って色付けされるコマンド)
          ;;   始めにエラーとしてマークする。
          ;;   文法的に正しければ後続の一致によって上書きされる。
          "\\(?:[abcp]\\|em?\\)\\>"                            ;; \cmd word
          "t?param\\>"                                         ;; \cmd [\[in,out\]] var
          "fn\\>" "var\\>" "typedef\\>"                        ;; \cmd decl...
          "def\\>" "enum\\>" "namespace\\>" "package\\>"       ;; \cmd subj
          "exception\\>" "throw\\>" "implements\\>"            ;; \cmd type
          "defgroup\\>" "page\\>"
          "\\(?:sub\\(?:sub\\)?\\)?section\\>" "paragraph\\>"  ;; \cmd label title...
          "addtogroup\\>" "weakgroup\\>"                       ;; \cmd label [title...]
          "ref\\>" "subpage\\>"                                ;; \cmd label ["text"]
          "class\\>" "struct\\>" "union\\>"
          "category\\>" "protocol\\>" "interface\\>"           ;; \cmd type [header] [header-name]
          "example\\>" "dontinclude\\>" "include\\>"
          "includelineno\\>" "verbinclude\\>" "htmlinclude\\>" ;; \cmd fname
          )
         "\\)\\)\\|\\([\\@]\\(?:"
         (mwg/rex-or
          ;; \cmd (単独のコマンド)
          ;; @{ ... @}
          "addindex\\>" "addtogroup\\>" "anchor\\>" "arg\\>" "attention\\>" "author\\>"
          "brief\\>" "bug\\>" "callgraph\\>" "callergraph\\>" "code\\>" "cond\\>"
          "copybrief\\>" "copydetails\\>" "copydoc\\>" "date\\>"
          "details\\>" "deprecated\\>" "dir\\>" "dot\\>" "dotfile\\>"
          "else\\>" "elseif\\>" "endcode\\>" "endcond\\>" "enddot\\>" "endhtmlonly\\>"
          "endif\\>" "endlatexonly\\>" "endlink\\>" "endmanonly\\>" "endmsc\\>"
          "endverbatim\\>" "endxmlonly\\>" "extends\\>" "file\\>"
          "headerfile\\>" "hideinitializer\\>" "htmlonly\\>"
          "if\\>" "ifnot\\>" "image\\>"
          "ingroup\\>" "internal\\>" "invariant\\>" "latexonly\\>" "li\\>" "line\\>"
          "link\\>" "mainpage\\>" "manonly\\>" "memberof\\>" "msc\\>" "n\\>" "name\\>"
          "nosubgrouping\\>" "note\\>" "overload\\>" "par\\>" "post\\>" "pre\\>"
          "private\\>" "privatesection\\>" "property\\>" "protected\\>"
          "protectedsection\\>" "public\\>" "publicsection\\>" "relates\\>"
          "relatesalso\\>" "remarks\\>" "return\\>" "retval\\>" "sa\\>" "see\\>"
          "showinitializer\\>" "since\\>" "skip\\>" "skipline\\>"
          "test\\>" "todo\\>" "until\\>" "verbatim\\>" "version\\>"
          "warning\\>" "weakgroup\\>" "xmlonly\\>" "xrefitem\\>"
          "f[][$}]" "f{[^{}]+}{" "[$@\\\\&~<>#%]"
          ) "\\)\\|@[{}]\\)")
       (1 'mwg-doxygen/warning-face prepend t)
       (2 'mwg-doxygen/keyword-face prepend t))
      (,(concat "\\([\\@]param\\>\\)\\s *\\(?:\\[\\([^][]*\\)\\]\\)?\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/argument-face prepend t)
       (3 'font-lock-doc-face remove nil) ;; auto-complete が効く様に font-lock-doc-face を削除する
       (3 '(font-lock-variable-name-face mwg-doxygen/title-face) prepend nil))
      (,(concat "\\([\\@]tparam\\>\\)\\s *\\(?:\\[\\([^][]*\\)\\]\\)?\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/argument-face prepend t)
       (3 'font-lock-doc-face remove nil)
       (3 '(font-lock-type-face mwg-doxygen/title-face) prepend nil))
      (,(concat "\\([\\@]\\(?:[abcp]\\|em?\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/em-face prepend nil))
      (,(concat "\\([\\@]\\(?:def\\|enum\\|namespace\\|package\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 'mwg-doxygen/title-face prepend nil))
      (,(concat "\\([\\@]\\(?:exception\\|throw\\|implements\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 '(font-lock-type-face mwg-doxygen/title-face) prepend nil))
      (,(concat "\\([\\@]\\(?:var\\|fn\\|typedef\\)\\>\\)\\s *\\([^\r\n]+\\)$" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 'mwg-doxygen/title-face prepend nil))
      (,(concat "\\([\\@]\\(?:defgroup\\|page\\|\\(?:sub\\(?:sub\\)?\\)?section\\|paragraph\\)\\>\\)\\s *\\(" rex_sword "\\)\\s +\\([^\r\n]+\\)$" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/label-face prepend nil)
       (3 'mwg-doxygen/title-face prepend nil))
      (,(concat "\\([\\@]\\(?:addtogroup\\|weakgroup\\)\\>\\)\\s *\\(" rex_sword "\\)\\(\\s +\\([^\r\n]+\\)$\\)?" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/label-face prepend nil)
       (3 'mwg-doxygen/title-face prepend t))
      (,(concat "\\([\\@]\\(?:ref\\|subpage\\)\\>\\)\\s *\\(" rex_sword "\\)\\(\\s +\\(\"[^\"]+\"\\)\\)?" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/label-face prepend nil)
       (3 'font-lock-string-face prepend t))
      (,(concat "\\([\\@]\\(?:category\\|class\\|interface\\|protocol\\|struct\\|union\\)\\>\\)\\s *\\(" rex_sword "\\)\\(?:\\s +\\(" rex_sword "\\)\\(?:\\s +\\(" rex_sword "\\)\\)?\\)?" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 '(font-lock-type-face mwg-doxygen/title-face) prepend nil)
       (3 'mwg-doxygen/file-face prepend t)
       (4 'font-lock-string-face prepend t))
      (,(concat "\\([\\@]\\(?:example\\|dontinclude\\|include\\|includelineno\\|verbinclude\\|htmlinclude\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/file-face prepend nil))
      ("^\\(?:/\\*[*!]<?\\|//[/!]<?\\)?\\(?:\\s \\|[*]\\)*\\(?:\\(-#?\\) \\|\\(\\.\\)\\s *$\\)" ;; - ul, -# ol, .
       (1 'mwg-doxygen/item-face prepend t)
       (2 'mwg-doxygen/item-face prepend t))
      ("^\\(?:/\\*[*!]\\|//[/!]\\)<\\[\\([^][]*\\)\\]" ;; e.g. param1 //!<[in] this is param1
       (1 'mwg-doxygen/argument-face prepend nil))
      ("[\\@]f\\$\\(\\(?:[^\\@]\\|[\\@]\\(?:[^f]\\|f[^$]\\)\\)+\\)\\(?:[\\@]f\\$\\)?\\|[\\@]f\\[\\(\\(?:[^\\@]\\|[\\@]\\(?:[^f]\\|f[^]]\\)\\)+\\)\\(?:[\\@]f\\]\\)?"
       ;; 2015-02-01
       ;;   /[\\@]f\$((?:(?![\\@]f\$).)+)(?:[\\@]f\$)?|[\\@]f\[((?:(?![\\@]f\]).)+)(?:[\\@]f\])?/
       (1 mwg-doxygen/simple-tex-keylist keylist t)
       (2 mwg-doxygen/simple-tex-keylist keylist t))
      ;; TeX \f{env}{ ... }
      ("[\\@]f{\\([[:alnum:][:space:]@*]+\\)}\\s *{\\(\\(?:[^\\@]\\|[\\@]\\(?:[^f]\\|f[^}]\\)\\)+\\)\\(?:[\\@]f}\\)?"
       ;; 2015-02-01
       ;;   終端の "\f}" も要求すると limit で切れている場合に正しく色付けされない。
       ;;   特に "\f{}{～\f}" の中身を編集している時は編集行の行末に limit が設定されるので不都合。
       ;;   従って、"\f}" が登場しない限りできるだけ読み取る。
       ;;   上の正規表現は ES でいう /[\\@]f\{([\w\s@*]+)\}\s*\{((?:(?![\\@]f\}).)+)(?:[\\@]f\})?/ に等価
       ;;   (emacs regex には forward_negation が存在しないので分かりにくい指定になっている)。
       (1 'mwg-doxygen/code-face prepend nil)
       (2 mwg-doxygen/simple-tex-keylist keylist))
      (,(concat
         "&\\(?:"
         (mwg/rex-or
          "copy" "tm" "reg" "lt" "gt" "amp" "apos" "quot" "lsquo"
          "rsquo" "ldquo" "rdquo" "ndash" "mdash"
          "[AEIOUYaeiouy]\\(?:uml\\|acute\\)"
          "[AEIOUaeiouy]\\(?:grave\\|circ\\)"
          "[ANOano]tilde" "szlig" "[cC]cedil" "[aA]ring" "nbsp"
          ) "\\);") ;; HTML Entities
       (0 'mwg-doxygen/keyword-face prepend nil))
      ;; -- javadoc --
      ;; ("{@[a-z]+[^}\n\r]*}" ; "{@foo ...}" markup.
      ;;  0 'mwg-doxygen/keyword-face prepend nil)
      )))

(defconst mwg-doxygen/doc:doxygen/xmltag-keylist
  `((,(concat "\\(" mwg-doxygen/rex-NCName "\\(?::" mwg-doxygen/rex-NCName "\\)?\\)\\|\\(\"[^\"]*\"\\|'[^']*'\\)")
     (1 'font-lock-variable-name-face prepend t)
     (2 'font-lock-string-face prepend t))))

(defconst mwg-doxygen/doc:mwgpp/rex-prefix (concat "//%\\|" mwg-doxygen/pragma-prefix "%"))
(defconst mwg-doxygen/doc:mwgpp/rex-line-head (concat "\\(?:^[[:space:]]*//\\|" mwg-doxygen/pragma-prefix "\\)%[[:space:]%]*"))
(defconst mwg-doxygen/doc:mwgpp/keywords
  `(("^[[:space:]]*\\(?://\\|#pragma[[:space:]]*\\)\\(%[[:space:]%]*\\)\\|\\(.+\\)"
     (1 '(mwg-doxygen/mwgpp-directive-name mwg-doxygen/mwgpp-face) prepend t)
     (2 'mwg-doxygen/mwgpp-face prepend t))
    (,(concat mwg-doxygen/doc:mwgpp/rex-line-head "\\(\\[\\)\\(.*\\)\\(\\]\\)$")
     (1 '(mwg-doxygen/mwgpp-directive-name bold) prepend nil)
     (2 mwg-doxygen/simple-c++-keylist keylist)
     (3 '(mwg-doxygen/mwgpp-directive-name mwg-doxygen/lwiki-bold-face) prepend nil))
    (,(concat mwg-doxygen/doc:mwgpp/rex-line-head "\\([[:alpha:]_][[:alnum:]_]*\\|[<>$]\\)")
     (1 'mwg-doxygen/mwgpp-directive-name prepend nil))
    (,(concat mwg-doxygen/doc:mwgpp/rex-line-head "\\(?:m\\|define\\)[[:space:]]+\\([[:alnum:]_:]+\\)\\(?:[[:space:]]+\\([[:alnum:]_:]+\\)\\(.+\\)?\\)?")
     (1 '(mwg-doxygen/mwgpp-macro-name bold) prepend nil)
     (2 'mwg-doxygen/mwgpp-macro-name prepend t)
     (3 mwg-doxygen/doc:mwgpp/modifiers-keylist keylist t))
    (,(concat mwg-doxygen/doc:mwgpp/rex-line-head "\\(?:x\\|expand\\)[[:space:]]+\\([[:alnum:]_:]+\\)\\(.+\\)?")
     (1 'mwg-doxygen/mwgpp-macro-name prepend nil)
     (2 mwg-doxygen/doc:mwgpp/modifiers-keylist keylist t))
    (,(concat mwg-doxygen/doc:mwgpp/rex-line-head "\\(?:[)]\\|end\\)\\(.+\\)?")
     (1 mwg-doxygen/doc:mwgpp/modifiers-keylist keylist t))))
(defconst mwg-doxygen/doc:mwgpp/modifiers-keylist
  (let ((seq "\\(?:[^\\]\\|\\\\.\\)*?"))
    `((,(concat
       "[[:space:]]+"
       "\\|\\.[rR]\\(.\\)" seq "\\1" seq "\\1"
       "\\|\\.f\\(.\\)" seq "\\2" seq "\\2" seq "\\2"
       "\\|\\.\\(i\\)"
       "\\|\\(.+\\)")
       (cond
        ((match-beginning 1)
         (goto-char (match-beginning 0))
         (if (looking-at ,(concat "\\.\\([rR]\\)\\(.\\)\\(" seq "\\)\\(\\2\\)\\(" seq "\\)\\(\\2\\)"))
             nil (message "rR match failed at %d" (match-beginning 1)))
         '((1 'font-lock-function-name-face prepend t)
           (2 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)
           (3 mwg-doxygen/doc:mwgpp/regexp-keylist keylist t)
           (4 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)
           (5 mwg-doxygen/doc:mwgpp/replacement-keylist keylist t)
           (6 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)))
        ((match-beginning 2)
         (goto-char (match-beginning 0))
         (looking-at ,(concat "\\.\\([f]\\)\\(.\\)\\(" seq "\\)\\(\\2\\)\\(" seq "\\)\\(\\2\\)\\(" seq "\\)\\(\\2\\)"))
         '((1 'font-lock-function-name-face prepend t)
           (2 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)
           (3 mwg-doxygen/doc:mwgpp/regexp-keylist keylist t)
           (4 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)
           (5 mwg-doxygen/simple-c++-keylist keylist t)
           (6 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)
           (7 mwg-doxygen/simple-c++-keylist keylist t)
           (8 '(font-lock-regexp-grouping-backslash font-lock-string-face) prepend t)))
        (t
         '((3 'font-lock-function-name-face prepend t)
           (4 'font-lock-warning-face prepend t)))))
      )))

(defconst mwg-doxygen/doc:mwgpp/regexp-keylist
  (let* ((rex-atomic "(\\(?:\\?[!=:]\\)?\\|[)|^$]")
         (rex-charclass "\\[\\^?\\(?:\\[:[^][]+:\\]\\|\\[=[^][]+=\\]\\|\\[.[^][]+.\\]\\|[^]\\]\\|\\\\.\\)*\\]")
         (rex-quantifier "\\(?:[*?+]\\|{[0-9]*\\(?:,[0-9]*\\)?}\\)\\??"))
    `((".+"
       (0 'mwg-doxygen/regexp prepend nil))
      (,(concat "\\(" rex-atomic "\\)\\|\\(" rex-charclass "\\)\\|\\(" rex-quantifier "\\)\\|\\(\\\\.\\)")
       (1 'mwg-doxygen/regexp-grouping-construct prepend t)
       (when (match-beginning 2)
         (let* ((b0 (match-beginning 2))
                (b1 (+ b0 (if (= (char-after (1+ b0)) ?^) 2 1)))
                (e0 (match-end 2))
                (e1 (1- e0)))
           (font-lock-prepend-text-property b0 e0 'face 'mwg-doxygen/regexp-character-class)
           (font-lock-prepend-text-property b0 b1 'face 'mwg-doxygen/regexp-grouping-construct)
           (font-lock-prepend-text-property e1 e0 'face 'mwg-doxygen/regexp-grouping-construct)))
       (3 'mwg-doxygen/regexp-quantifier prepend t)
       (4 'font-lock-constant-face prepend t)))))
(defconst mwg-doxygen/doc:mwgpp/replacement-keylist
  '((".+"
     (0 'mwg-doxygen/regexp prepend nil))
    ("\\(\\$[0-9]+\\)\\|\\(\\\\.\\)"
     (1 'font-lock-variable-name-face prepend t)
     (2 'font-lock-constant-face prepend t))))

;; the following faces are taken from mwg-js2-config.el
(defface mwg-doxygen/regexp
  '((((class color) (min-colors 88) (background light))
     (:foreground "forestgreen"
                  ;;:background "color-194"
                  :inherit font-lock-string-face))
    (default :inherit font-lock-string-face))
  "Face used to highlight regular expressions"
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/regexp-grouping-construct
  '((((class color) (min-colors 88) (background light))
     (:foreground "brightblue" :weight normal :inherit (font-lock-regexp-grouping-construct mwg-doxygen/regexp)))
    (default :inherit (font-lock-regexp-grouping-construct mwg-doxygen/regexp)))
  "Face used to highlight grouping constructs in regular expressions"
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/regexp-quantifier
  '((((class color) (min-colors 88) (background light))
     (:foreground "purple" :inherit (font-lock-builtin-face mwg-doxygen/regexp)))
    (default :inherit (font-lock-builtin-face mwg-doxygen/regexp)))
  "Face used to highlight quantifiers in regular expressions"
  :version "24.1"
  :group 'mwg-doxygen)
(defface mwg-doxygen/regexp-character-class
  '((((class color) (min-colors 88) (background light))
     (:foreground "steelblue" :inherit (font-lock-type-face mwg-doxygen/regexp)))
    (default :inherit (font-lock-type-face mwg-doxygen/regexp)))
  "Face used to highlight the content of a character class in regular expressions"
  :version "24.1"
  :group 'mwg-doxygen)

(defconst mwg-doxygen/doc:lwiki/rex-line-head
  "\\(?:^[[:space:]]*\\(?:\\(?:/[/*]\\)\\?lwiki\\_>\\|\\*\\)[[:space:]]?\\)")

(defconst mwg-doxygen-font-lock-lwiki-comments
  (let* ((rex_sword "[^[:space:][:cntrl:]]+")
         (rex_braced (concat "\\(?:[^{}]\\|" mwg-doxygen/rex:lwiki-nested-brace "\\)*"))
         (rex_prog_decl (concat "\\(?:"
                                (mwg/rex-or
                                 "fn" "var" "op" "param" "typedef" "class" "namespace"
                                 "def" "enum" "const" "tparam" "struct" "union"
                                 "interface")
                                "\\)")))
    `(("^[[:space:]]*\\(?:/[*/]\\?\\(lwiki\\_>\\)\\(?:[[:space:]]*\n\\|[[:space:]]\\)\\|\\*[[:space:]]?\\)\\|^\\(?:[[:space:]]+\\)\\'\\|\\(.*\n\\|.+$\\)" ;; content
       (1 'mwg-doxygen/lwiki-bold-face prepend t)
       (2 'mwg-doxygen/content-face prepend t))
      ;; lwiki Entities
      ("\\(&[a-zA-Z0-9_]+\\*?;?\\)\\|\\(&.\\)"
       (0 'mwg-doxygen/keyword-face prepend nil))
      ;; lwiki markups
      ("~~\\|\\[\\[\\|\\]\\]\\|'''?\\|##\\|%%\\|__\\|,,\\|^^\\|==\\|\\\\$"
       (0 '(font-lock-builtin-face mwg-doxygen/lwiki-bold-face) prepend nil))
      ;; doxygen like commands
      (,(concat
         "\\(@\\(?:"
         (mwg/rex-or
          ;; \cmd ... (後で詳細な文法に従って色付けされるコマンド)
          ;;   始めにエラーとしてマークする。
          ;;   文法的に正しければ後続の一致によって上書きされる。

          ;; C++ の宣言達 \cmd label [optionss] declaration;
          (concat rex_prog_decl "\\>")

          "include\\>"
          "exception\\>" "throw\\>" "implements\\>"            ;; \cmd type
          "defgroup\\>" "page\\>"
          "\\(?:sub\\(?:sub\\)?\\)?section\\>" "paragraph\\>"  ;; \cmd label title...
          "addtogroup\\>" "weakgroup\\>"                       ;; \cmd label [title...]
          "ref\\>" "subpage\\>"                                ;; \cmd label ["text"]
          "category\\>" "protocol\\>" "interface\\>"           ;; \cmd type [header] [header-name]
          "example\\>" "dontinclude\\>"
          "includelineno\\>" "verbinclude\\>" "htmlinclude\\>" ;; \cmd fname
          )
         "\\)\\)\\|\\(@\\(?:"
         (mwg/rex-or
          ;; \cmd (単独のコマンド)
          "addindex\\>" "addtogroup\\>" "anchor\\>" "arg\\>" "attention\\>" "author\\>"
          "brief\\>" "bug\\>" "callgraph\\>" "callergraph\\>" "code\\>" "cond\\>"
          "copybrief\\>" "copydetails\\>" "copydoc\\>" "date\\>"
          "details\\>" "deprecated\\>" "dir\\>" "dot\\>" "dotfile\\>"
          "else\\>" "elseif\\>" "endcode\\>" "endcond\\>" "enddot\\>" "endhtmlonly\\>"
          "endif\\>" "endlatexonly\\>" "endlink\\>" "endmanonly\\>" "endmsc\\>"
          "endverbatim\\>" "endxmlonly\\>" "extends\\>" "file\\>"
          "headerfile\\>" "hideinitializer\\>" "htmlonly\\>"
          "if\\>" "ifnot\\>" "image\\>"
          "ingroup\\>" "internal\\>" "invariant\\>" "latexonly\\>" "li\\>" "line\\>"
          "link\\>" "mainpage\\>" "manonly\\>" "memberof\\>" "msc\\>" "n\\>" "name\\>"
          "nosubgrouping\\>" "note\\>" "overload\\>" "par\\>" "post\\>" "pre\\>"
          "private\\>" "privatesection\\>" "property\\>" "protected\\>"
          "protectedsection\\>" "public\\>" "publicsection\\>" "relates\\>"
          "relatesalso\\>" "remarks\\>" "return\\>" "retval\\>" "sa\\>" "see\\>"
          "showinitializer\\>" "since\\>" "skip\\>" "skipline\\>"
          "test\\>" "todo\\>" "until\\>" "verbatim\\>" "version\\>"
          "warning\\>" "weakgroup\\>" "xmlonly\\>" "xrefitem\\>"
          ) "\\)\\)")
       (1 'mwg-doxygen/warning-face prepend t)
       (2 'mwg-doxygen/keyword-face prepend t))
      (,(concat "\\([\\@]" rex_prog_decl "\\>\\)\\s *\\(?:\\[\\([^][]*\\)\\]\\)?\\s *\\([^\n]+\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/argument-face prepend t)
       (3 'font-lock-doc-face remove nil) ;; auto-complete が効く様に font-lock-doc-face を削除する
       (3 mwg-doxygen/simple-c++-keylist keylist)
       (3 mwg-doxygen/inline-lwiki-keylist keylist))
      (,(concat "\\([\\@]\\(?:package\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 'mwg-doxygen/title-face prepend nil))
      (,(concat "\\([\\@]\\(?:exception\\|throw\\|implements\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 '(font-lock-type-face mwg-doxygen/title-face) prepend nil))
      (,(concat "\\([\\@]\\(?:defgroup\\|page\\|\\(?:sub\\(?:sub\\)?\\)?section\\|paragraph\\)\\>\\)\\s *\\(" rex_sword "\\)\\s +\\([^\r\n]+\\)$" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/label-face prepend nil)
       (3 'mwg-doxygen/title-face prepend nil))
      (,(concat "\\([\\@]\\(?:addtogroup\\|weakgroup\\)\\>\\)\\s *\\(" rex_sword "\\)\\(\\s +\\([^\r\n]+\\)$\\)?" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/label-face prepend nil)
       (3 'mwg-doxygen/title-face prepend t))
      (,(concat "\\([\\@]\\(?:ref\\|subpage\\)\\>\\)\\s *\\(" rex_sword "\\)\\(\\s +\\(\"[^\"]+\"\\)\\)?" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/label-face prepend nil)
       (3 'font-lock-string-face prepend t))
      (,(concat "\\([\\@]\\(?:category\\|protocol\\)\\>\\)\\s *\\(" rex_sword "\\)\\(?:\\s +\\(" rex_sword "\\)\\(?:\\s +\\(" rex_sword "\\)\\)?\\)?" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'font-lock-doc-face remove nil)
       (2 '(font-lock-type-face mwg-doxygen/title-face) prepend nil)
       (3 'mwg-doxygen/file-face prepend t)
       (4 'font-lock-string-face prepend t))
      (,(concat "\\([\\@]\\(?:example\\|dontinclude\\|include\\|includelineno\\|verbinclude\\|htmlinclude\\)\\>\\)\\s *\\(" rex_sword "\\)" )
       (1 'mwg-doxygen/warning-face remove nil)
       (1 'mwg-doxygen/keyword-face prepend nil)
       (2 'mwg-doxygen/file-face prepend nil))
      ;; lwiki list items
      (,(concat "^" mwg-doxygen/doc:lwiki/rex-line-head "?\\([-+:]+\\|> \\)") ;; - ul, + ol, : dl
       (1 'mwg-doxygen/item-face prepend t))
      ;; <?lang ...?>
      ("\\(<\\?\\([a-zA-Z0-9_]+\\)\\_>\\*?\\)[[:space:]]*\\(.*?\\)\\(\\?>\\)"
       (1 'mwg-doxygen/xml-delim-face prepend nil)
       (4 'mwg-doxygen/xml-delim-face prepend nil)
       (3 'font-lock-doc-face remove nil) ;; for auto-complete
       (let* ((lang (match-string-no-properties 2))
              (klist (mwg-doxygen/select-language-keylist lang)))
         (if klist `(3 (quote ,klist) keylist)
           '(3 'mwg-doxygen/code-face prepend nil)))
       (when (= (char-before (match-end 1)) ?*)
         '(3 mwg-doxygen/inline-lwiki-keylist keylist)))
      ;; `...`
      ("\\(`\\)\\([^`]+\\)\\(`\\)"
       (1 '(mwg-doxygen/xml-delim-face mwg-doxygen/lwiki-bold-face) prepend nil)
       (3 '(mwg-doxygen/xml-delim-face mwg-doxygen/lwiki-bold-face) prepend nil)
       (2 'font-lock-doc-face remove nil) ;; auto-complete
       (2 'mwg-doxygen/code-face prepend nil)
       ;; ↓c++-mode の keywords を使おうとしたが色付けしてくれない。多分コメントだと思われている。
       ;; (2 cpp-font-lock-keywords keylist)
       (2 mwg-doxygen/simple-c++-keylist keylist))
      ;; Code &pre(){ ... }
      (,(concat "&pre\\*?\\(?:(\\([^()]*\\))\\)?[[:space:]]*{[[:space:]]*\n?\\(" rex_braced "?\\)\\(?:^[[:space:]]*\\*[[:space:]]*\\)?}")
       (1 'mwg-doxygen/argument-face prepend t)
       (let ((klist (and (match-beginning 1)
                         (let ((m1 (match-string-no-properties 1)))
                           (save-match-data
                             (and (string-match "\\(?:^\\|,\\)[[:space:]]*!\\([a-zA-Z0-9_]+\\)[[:space:]]*\\(?:,\\|$\\)" m1)
                                  (mwg-doxygen/select-language-keylist (match-string 1 m1))))))))
         (if klist `(2 (quote ,klist) keylist)
           '(2 'mwg-doxygen/code-face prepend nil)))
       (when (= (char-after (+ (match-beginning 0) 4)) ?*)
         '(2 mwg-doxygen/inline-lwiki-keylist keylist)))
      ;; TeX &math{}
      (,(concat "&math{\\(" rex_braced "\\)}")
       (1 mwg-doxygen/simple-tex-keylist keylist))
      ;; TeX $...$
      ("\\(\\$\\)\\([^$]+\\)\\(\\$\\)"
       (1 'mwg-doxygen/keyword-face prepend nil)
       (3 'mwg-doxygen/keyword-face prepend nil)
       (2 mwg-doxygen/simple-tex-keylist keylist))
      ;; TeX &begin(){ ... }
      (,(concat "&begin(\\([[:alnum:][:space:]@*]+\\))\\s *{\\(" rex_braced "\\)}?")
       (1 'mwg-doxygen/argument-face prepend nil)
       (2 mwg-doxygen/simple-tex-keylist keylist))
      )))

(defun mwg-doxygen/select-language-keylist (lang)
  (cond
   ((string= lang "cpp")
    mwg-doxygen/simple-c++-keylist)
   ((string= lang "tex")
    mwg-doxygen/simple-tex-keylist)
   (t nil)))

(defconst mwg-doxygen/simple-tex-keylist
  '(((concat mwg-doxygen/local:document-line-head "\\|\\(.+\n?\\|\n\\)")
     (1 'mwg-doxygen/code-face prepend t))
    ("\\([\\]\\(?:[a-zA-Z@]+\\|[^*[:space:][:cntrl:]]\\)\\*?\\)\\|\\(%[^\r\n]+\\)\\|\\(\\$[^\\$]+\\$\\)"
     (1 'font-lock-keyword-face prepend t)
     (2 'font-lock-comment-face prepend t)
     (3 'font-lock-string-face prepend t))
    ("[{}_^&~]"
     (0 'font-lock-builtin-face prepend t))))

(defconst mwg-doxygen/inline-lwiki-keylist
  `((,(concat "\\(==\\)\\(\\(?:" mwg-doxygen/rex-csym "\\|[[:space:]]\\)+\\)\\(==\\)")
     (1 'font-lock-builtin-face prepend nil)
     (3 'font-lock-builtin-face prepend nil)
     (2 'mwg-doxygen/lwiki-bold-face prepend nil))
    ("~~\\|\\[\\[\\|\\]\\]\\|'''?\\|##\\|%%\\|__\\|,,\\|^^\\|==\\|\\\\$"
     (0 'font-lock-builtin-face prepend nil))))
(defconst mwg-doxygen/simple-c++-keylist
  (let ((rex-composite-keywords
         (concat "\\(?:"
                 (mwg/rex-or
                  ;; C++/CLI composite keywords
                  "ref struct" "ref class" "enum class" "enum struct" "value struct" "value class" "interface class" "interface struct"
                  "for each")
                 "\\)"))
        (rex-builtin-types
         (concat "\\(?:"
                 (mwg/rex-or
                  "int" "signed" "unsigned" "long" "short"
                  "float" "double"
                  "void" "bool" "char" "wchar_t"
                  ;; C++11
                  "char16_t" "char32_t"
                  ;; VC
                  "__wchar_t"
                  "__int8" "__int16" "__int32" "__int64" "__int128"
                  "__m64" "__m128" "__m128d" "__m128i")
                 "\\)"))
        (rex-keyword
         (concat
          "\\(?:"
          (mwg/rex-or ;; keywords from agh.text.color.js (2016-03-31)
           "or" "and" "not" "not_eq" "bitand" "and_eq" "bitor" "or_eq" "xor" "xor_eq" "compl"
           "static_cast" "reinterpret_cast" "const_cast" "dynamic_cast"
           "nullptr" "true" "false" "new" "delete" "typeid" "sizeof"

           "if" "else" "for" "do" "while"
           "switch" "case" "default"
           "continue" "break" "goto" "return"
           "try" "catch" "throw"

           "typedef" "extern" "static" "register" "auto" "inline"
           "const" "volatile" "mutable"
           "using" "namespace"

           "template" "typename" "struct" "class" "enum" "union"
           "private" "protected" "public" "friend" "virtual" "operator" "explicit" "this"

           ;; Reserved identifiers
           "_[_A-Z][a-zA-Z0-9_]*"

           ;; C++11
           "decltype" "constexpr" "static_assert" "alignof" "alignas" "thread_local" "noexcept" "override" "final"

           ;; C++ Concept
           "concept\\(?:_map\\)?" "requires?"

           ;;-----------------------------------------------------------------------
           ;; MS specific extensions

           ;; C++/CLI
           "property" "initonly" "literal"
           "abstract" "sealed"
           "_?_fastcall" "_?_cdecl" "_?_stdcall" "_?_?pascal"
           "__declspec" "__forceinline" "__inline" "__unaligned" "__w64"
           ;; "far" "near"

           "delegate" "__interface" "event" "__event" "safecast"

           "finally" "__try" "__except" "__finally"
           "in" "generic" "__if_exists" "__if_not_exists"

           "gcnew" "__super" "__uuidof" "__alignof" "__asm" "__assume"

           "__leave" "__noop" "__raise" "friend_as" "__identifier" "__based"
           "__single_inheritance" "__multiple_inheritance" "__virtual_inheritance"

           ;; event handling
           "__unhook" "__hook"

           ;; Managed C++
           "__pin" "__value" "__property" "__nogc" "__sealed" "__try_cast"
           "__abstract" "__box" "__delegate" "__gc"

           ;; __declspec
           "property" "uuid" "naked" "thread" "noinline" "noreturn"
           "nothrow" "novtable" "selectany" "deprecated" "dllexport" "dllimport"
           ;;-----------------------------------------------------------------------
           )
          "\\)"))
        (rex-csymf mwg-doxygen/rex-csymf)
        (rex-csym  mwg-doxygen/rex-csym))
    `(((concat mwg-doxygen/local:document-line-head "\\|\\(.+\n?\\|\n\\)")
       (1 'mwg-doxygen/code-face prepend t))
      ((concat
        ;; skip lwiki Line Head
        mwg-doxygen/local:document-line-head
        ,(concat
          ;; Comments
          "\\|\\(//.*\\(?:\n\\|$\\)?\\|/\\*\\(?:[^*/]+\\|[*][^/]\\|[*]\\'\\)*\\(?:\\*/\\)?\\)"
          ;; String Literals
          "\\|\\(" mwg-doxygen/rex:c++-string-literal "\\)"
          ;; Number Literals
          "\\|\\(\\_<0[xX][0-9-a-fA-F]+\\_>\\|\\(?:\\_<\\|\\.\\)[0-9]\\(?:[.0-9]\\|[eE][-+]?\\)*[_a-zA-Z0-9]*\\)"
          ;; Composite Keywords
          "\\|\\(\\_<" rex-composite-keywords "\\_>\\)"
          ;; Identifiers
          "\\|\\(\\_<" rex-csymf rex-csym "*\\_>\\)"
          ))
       (1 'font-lock-comment-face prepend t)
       (2 'font-lock-string-face prepend t)
       (4 'font-lock-keyword-face prepend t)
       (when (match-beginning 5)
         (let ((beg (match-beginning 5))
               (end (match-end 5))
               (identifier (match-string-no-properties 5)))
           (save-match-data
             (save-excursion
               (cond
                ;; C++ builtin-types
                ((string-match-p ,(concat "^" rex-builtin-types "$") identifier)
                 '(5 'font-lock-type-face prepend t))
                ;; C++ keywords
                ((string-match-p ,(concat "^" rex-keyword "$") identifier)
                 '(5 'font-lock-keyword-face prepend t))
                ;; namespace
                ((progn (goto-char end)
                        (looking-at-p "[[:space:]]*::"))
                 '(5 'font-lock-constant-face prepend t))
                ;; typenames
                ((or (progn (goto-char end) nil)
                     (looking-at-p ,(concat "\\(?:[[:space:]]+[*&]*\\|[*&]+[[:space:]]+\\)\\_<\\|[*&]+[[:space:]]*\\(?:[]),/:;<>?`|}]\\|$\\)"))
                     (progn (goto-char beg) nil)
                     (looking-back "\\_<\\(?:struct\\|class\\|typename\\|new\\)[[:space:]]+"
                                   mwg-doxygen/local:apply-keylist-beg)
                     (and (looking-back "\\_<\\(?:const\\|volatile\\)[[:space:]]+" mwg-doxygen/local:apply-keylist-beg)
                          (string-match-p "\\(?:^\\|[!#-&(+-/:-=?@[\\^`{-~]\\)[[:cntrl:][:space:]]*\\_<\\(?:const\\|volatile\\)[[:space:]]+$"
                                          (buffer-substring-no-properties mwg-doxygen/local:apply-keylist-beg beg))))
                 '(5 'font-lock-type-face prepend t))
                ;; function name
                ((progn (goto-char end)
                        (looking-at-p ,(concat "\\(?:" mwg-doxygen/rex:c++-template-argument "\\)?(")))
                 '(5 'font-lock-function-name-face prepend t))
                ;; template name
                ((progn (goto-char end)
                        (looking-at-p ,(concat "\\(?:" mwg-doxygen/rex:c++-template-argument "\\)")))
                 '(5 'font-lock-type-face prepend t))
                ;; variable decl
                ((or (progn (goto-char beg) nil)
                     (looking-back ,(concat "\\_>\\(?:[[:space:]]+[*&]*\\|[*&]+[[:space:]]+\\)\\|>[*&]+[[:space:]]+\\|\\(?:[^>[:space:]]\\|[>\n][[:space:]]+>?\\)>[[:space:]]+[*&]*")
                                   mwg-doxygen/local:apply-keylist-beg))
                 '(5 'font-lock-variable-name-face prepend t))
                ;; guess types?
                ((string-match-p ,(concat "^.+_t$\\|^.+_type$\\|^\\(?:type\\|\\(?:const_\\)?iterator\\|[A-Z][0-9]?\\)$") identifier)
                 '(5 'font-lock-type-face prepend t))
                ;; (t
                ;;  '(5 'font-lock-function-name-face prepend t))
                ))))))
      )))

;;;###autoload
(defun mwg-doxygen-font-lock-keywords ()
  `((,(lambda (limit)
        (let ((mwg-doxygen/local:document-line-head mwg-doxygen/doc:mwgpp/rex-line-head))
          (save-excursion
            (mwg-doxygen/c-font-lock-doc-pragmas mwg-doxygen/doc:mwgpp/rex-prefix limit
                                                 mwg-doxygen/doc:mwgpp/keywords))
          (save-excursion
            (mwg-doxygen/c-font-lock-doc-comments mwg-doxygen/doc:mwgpp/rex-prefix limit
                                                  mwg-doxygen/doc:mwgpp/keywords)))
        (save-excursion
          (let ((mwg-doxygen/local:document-line-head mwg-doxygen/doc:lwiki/rex-line-head))
            (mwg-doxygen/c-font-lock-doc-comments "/[*/]\\?lwiki\\_>" limit
                                                  mwg-doxygen-font-lock-lwiki-comments)))
        (let ((mwg-doxygen/local:document-line-head mwg-doxygen/doc:doxygen/rex-line-head))
          (mwg-doxygen/c-font-lock-doc-comments "/\\*\\(?:\\*[^/]\\|!\\)\\|//[/!]" limit
                                                mwg-doxygen-font-lock-doc-comments))))))

;;*****************************************************************************
;;  Modification of library functions
;;-----------------------------------------------------------------------------

(defun mwg-doxygen/get-pragma-range/end ()
  (end-of-line)
  (if (= (char-after) ?\n)
      (forward-char))
  (point))
(defun mwg-doxygen/get-pragma-range (prefix)
  "現在位置が #pragma 行の途中の場合に #pragma の範囲を取得します。
\(開始位置 . 終端位置\) のペアを返します。
現在位置が #pragma 行の途中でなかった場合は nil を返します。"
  (save-excursion
    (beginning-of-line)
    (and (looking-at-p mwg-doxygen/pragma-prefix)
         (looking-at-p prefix)
         (cons (point) (mwg-doxygen/get-pragma-range/end)))))
(defun mwg-doxygen/get-next-pragma-range (prefix limit)
  "次の #pragma の範囲を取得します。
\(開始位置 . 終端位置\) のペアを返します。"
  (save-excursion
    (let ((found))
      (while (and (not found)
                  (re-search-forward mwg-doxygen/pragma-prefix limit t)
                  (goto-char (match-beginning 0)))
        (if (looking-at-p prefix)
            (setq found t)
          (forward-char)))
      (and found (cons (point) (mwg-doxygen/get-pragma-range/end))))))
(defun mwg-doxygen/c-font-lock-doc-pragmas (prefix limit keylist)
  (let* ((dirty-range (cons (point) limit)) ;; for debug
         (pragma-range (mwg-doxygen/get-pragma-range prefix)))
    (while (or pragma-range
               (setq pragma-range (mwg-doxygen/get-next-pragma-range prefix limit)))
      (let* ((region-beg (car pragma-range))
             (region-end (min limit (cdr pragma-range))))
        ;;(message "mwg-doxygen/c-font-lock-doc-pragmas: dirty-range %S, region %d-%d" dirty-range region-beg region-end)
        (save-restriction
          (narrow-to-region region-beg region-end)
          (mwg-doxygen/font-lock-apply-keylist region-beg region-end keylist)
          (goto-char region-end)))
      (setq pragma-range nil)))
  nil)

;; comments and doc-comments
(defun mwg-doxygen/comment-styleb-p (&optional pos)
  "Checks if a comment has the form of \"// ...\" or not.
POS is the beginning position of a comment."
  (save-excursion
    (let* ((beg (if pos pos (point)))
           (type (elt (parse-partial-sexp beg (+ beg 2)) 7)))
      (or (eq type t)     ;; emacs-21.2 and older
          (eq type 1))))) ;; emacs-24.1 and later
(defun mwg-doxygen/c-comment-begin-p (pt)
  (and (/= pt (point-max))
       (c-got-face-at pt c-literal-faces)
       (let ((current-comment-range
              (save-excursion
                (goto-char (1+ pt))
                (c-literal-limits))))
         (and current-comment-range
              (eq pt (car current-comment-range))))))
(defun mwg-doxygen/get-beginning-of-doc (prefix &optional pos)
  "@return 現在位置が doc-comment 内または先頭ならば doc-comment 開始位置を返します。
それ以外の場合には nil を返します。"
  (if pos (progn (goto-char pos)
                 (mwg-doxygen/get-beginning-of-doc prefix))
    (save-excursion
      (let (range is-set)
        (while (and (progn (and (setq range (c-literal-limits));;
                                (goto-char (car range)))       ;;
                           (looking-at prefix))                ;; 現在 doc-comment 内かどうかテスト
                    (let ((cs (mwg-doxygen/comment-styleb-p (point))))
                      (if (or (not is-set) cs)                 ;; 初回か "///" 型ならば
                          (setq pos (point) is-set t))         ;;   doc-comment 開始位置を更新
                      cs)                                      ;; "///" 型ならば
                    (/= (point) (point-min))                   ;;   更に上に行が無いか確認を続ける
                    (goto-char (1- (point)))))))               ;;   ...
    pos))
(defun mwg-doxygen/get-end-of-doc (prefix doc-beg &optional limit)
  "与えられた位置で始まる doc-comment の終端位置を取得します。"
  (save-excursion
    (goto-char doc-beg)
    (let* ((isb (and (looking-at-p prefix)
                     (mwg-doxygen/comment-styleb-p)))) ;; "//" 形式のドキュメントかどうか
      (c-forward-single-comment)                       ;; 取り敢えず一つコメントを読む
      (if isb
          (while (let ((pos (point))
                       (isb (and (skip-syntax-forward " ")
                            (or (not limit)
                                (< (point) limit))
                            (looking-at-p prefix)
                            (mwg-doxygen/comment-styleb-p))))
                   (if isb ;; "//" 形式のドキュメントが続いているか
                       (progn (c-forward-single-comment) t) ;; true  -> それを読んで更に次のループへ。
                     (progn (goto-char pos) nil))           ;; false -> skip-syntax-forward 前の位置に戻ってループ中断。
                   ))))
    (point)))
(defun mwg-doxygen/get-forward-beginning-of-doc (prefix limit)
  "次の doc-comment の開始位置を取得します。"
  (save-excursion
    (and (re-search-forward prefix limit t)
         (let ((mbeg (match-beginning 0)))
           (if (mwg-doxygen/c-comment-begin-p mbeg) mbeg
             (mwg-doxygen/get-forward-beginning-of-doc prefix limit))))))

;; cc-fonts function
(defun mwg-doxygen/c-font-lock-doc-comments (prefix limit keylist)
  "a modified version of `c-font-lock-doc-comments' in cc-fonts.el"
  (let* ((dirty-range (cons (point) limit))
         (doc-beg (mwg-doxygen/get-beginning-of-doc prefix))
         (region-beg doc-beg))
    (while (or doc-beg
               (setq doc-beg (mwg-doxygen/get-forward-beginning-of-doc prefix limit)
                     region-beg doc-beg))
      (let* ((doc-end (mwg-doxygen/get-end-of-doc prefix doc-beg limit))
             (region-end (min limit
                              (if (and (>= (- doc-end doc-beg) 4)
                                       (= (char-after doc-beg) ?/)
                                       (= (char-after (1+ doc-beg)) ?*)
                                       (= (char-before (1- doc-end)) ?*)
                                       (= (char-before doc-end) ?/))
                                  (- doc-end 2)
                                doc-end))))
        ;; 範囲 [dirty-beg dirty-end]
        ;;   ドキュメントコメントの探索範囲を示す。
        ;; 範囲 [doc-beg doc-end]
        ;;   見付かったドキュメントコメントの範囲を示す。
        ;;   端点 doc-beg, doc-end は必ずしも [dirty-beg dirty-end] の内部にあるとは限らない。
        ;; 範囲 [region-beg region-end]
        ;;   ドキュメントコメント内部の着色を行う範囲を示す。
        ;;   基本的にはドキュメントコメント (/*prefix ... */ や //prefix) の範囲であり、
        ;;   limit がドキュメントコメントの途中にあればそこで途切れる。
        ;; (message "mwg-doxygen: dirty-range %S, region %d-%d" dirty-range region-beg region-end)
        (save-restriction
          ;;(c-put-font-lock-face region-beg region-end c-doc-face-name)
          (narrow-to-region region-beg region-end)
          (mwg-doxygen/font-lock-apply-keylist region-beg region-end keylist)
          (goto-char region-end)))
      (setq doc-beg nil)))
  nil)

;; font-lock functions
(defun mwg-doxygen/font-lock-apply-keylist (begin end keylist)
  (while keylist
    (let ((mwg-doxygen/local:apply-keylist-beg begin) ;; keylist 内部から参照する為の変数
          (mwg-doxygen/local:apply-keylist-end end)   ;; 同上
          (kwdpair (car keylist)))
      (goto-char begin)
      (mwg-doxygen/font-lock-apply-keyword (car kwdpair) (cdr kwdpair) end))
    (setq keylist (cdr keylist))))
(defun mwg-doxygen/font-lock-apply-keyword (regexp highlighter limit)
  "
REGEXP には適用対象を指定します。

* REGEXP を eval した結果が文字列の場合はその文字列を正規表現として検索を行います。
  検索が一致した場合に一致範囲に HIGHLIGHTER を適用します。
* REGEXP を eval した結果が文字列ではない場合は、更にそれを関数として呼び出します。
  関数は第一引数に LIMIT を受け取ります。関数内では正規表現による一致を試行し match-data を設定します。
  関数は、適用対象が見付からなかった場合は nil を返し、それ以外の場合は nil 以外の値を返します。
  関数が nil 以外を返した時に HIGHLIGHTER を適用します。

例:

  REGEXP = \"foo\\\\|bar\"

  REGEXP = (concat var1 \"\\\\|bar\")

  REGEXP = '(lambda (limit) ...内部でmatch-dataを設定しnil以外を返す または nilを返す...)

REGEXP が一致した時、HIGHLIGHTER が `mwg-doxygen/font-lock-apply-recursive-highlight' によって適用されます。
HIGHLIGHTER の形式については `mwg-doxygen/font-lock-apply-recursive-highlight' の説明を参照して下さい。"
  (let ((matcher (eval regexp)))
    (while (if (stringp matcher)
               (re-search-forward matcher limit t)
             (funcall matcher limit))
      ;; 2015-02-01内部で (point) を移動すると同じ箇所に二重に適用したりするので save-excursion で囲む
      (save-excursion
        (mwg-doxygen/font-lock-apply-recursive-highlight highlighter)))))

(defun mwg-doxygen/font-lock-apply-recursive-highlight (highlighter)
"HIGHLIGHTER has one of the followings forms:

* HIGHLIGHTER := <highliter>

* <highlighter> := (match-index 'font-face-to-apply font-lock-override-type ignore-nomatch)
  If the first element is an integer, <highlighter> is treated as this quartet form.
  Applies font face to the range with specified match index.
  It is the argument for the `font-lock-apply-highlight'.

* <highlighter> := (progn ...)
  If the first element is a symbol, <highlighter> is treated as a program.
  This S-expression can return a <highlighter> object which will be recursively processed.

* <highlighter> := (<highlighter> <highlighter> ... <highlighter>)
  Each sub highlighter is applied in order.

* nil
  Nothing applied."
  (if (consp highlighter)
      (let ((head (car highlighter))
            (tail (cdr highlighter)))
        (cond
         ((integerp head)
          (mwg-doxygen/font-lock-apply-highlight highlighter))
         ((symbolp head)
          (mwg-doxygen/font-lock-apply-recursive-highlight (eval highlighter)))
         ((or (not tail) (consp tail))
          (while head
            (mwg-doxygen/font-lock-apply-recursive-highlight head)
            (setq head (car tail)
                  tail (cdr tail))))
         (t
          (error "invalid format of highlighter: %S" highlighter))))
    (if highlighter
        (error "invalid format of highlighter: %S" highlighter))))

(defun mwg-doxygen/font-lock-apply-highlight (highlight)
  "a version of `font-lock-apply-highlight' with 'remove as an override spec.
font-lock の `font-lock-apply-highlight' を拡張します。引数は以下の形式を取ります。

  HIGHLIGHT := (group face override ignore-nomatch)

- group = (nth 0 highlight)
  適用先のマッチグループ番号を指定します。

- face = (nth 1 highlight)

- override = (nth 2 highlight)
  face を処理する方法を指定します。

  'prepend ... 指定した face を追加します。
  'remove  ... 指定した face を削除します。
     face := 適用する font-face を指定します。

  'chkset  ... 指定した face1 が存在する時に face2 を設定します。
     face := (face1 . face2) の形式を取ります。

  'keylist ... 指定した keylist を範囲に再適用します。
     face := keylist の形式を取ります。

- ignore-match = (nth 3 highlight)
  t を指定すると対応するマッチグループが存在しない場合にエラーとしません。"
  (let ((override (nth 2 highlight)))
    (if (memq override '(remove chkset keylist))
        (let* ((mindex (nth 0 highlight))
               (mbegin (match-beginning mindex))
               (mend   (match-end mindex))
               (value  (nth 1 highlight)))
          (if mbegin
              (cond
               ((eq override 'remove)
                (mwg-doxygen/font-lock-remove-text-property mbegin mend 'face (eval value)))
               ((eq override 'chkset)
                (mwg-doxygen/font-lock-chkset-text-property mbegin mend 'face (eval value)))
               ((eq override 'keylist)
                (save-match-data
                  (mwg-doxygen/font-lock-apply-keylist mbegin mend (eval value)))))
            (or (nth 3 highlight)
                (error "No match %d in highlight %S" mindex highlight))))
      (font-lock-apply-highlight highlight))))

;; helper functions
(defun mwg-doxygen/ensure-listp (value)
  (if (listp value) value (list value)))
(defun mwg-doxygen/canonical-property-value (value)
  (if (and value
           (consp value)
           (not (cdr value)))
      (car value) ;; single face
    value)) ;; multiple or zero faces
(defun mwg-doxygen/canonicalize-old-face-prop-form (face-value)
  (or (and (listp face-value)
       (or (keywordp (car face-value))
           (memq (car face-value) '(foreground-color background-color)))
       (setq face-value (list face-value)))
      face-value))

(defun mwg-doxygen/font-lock-remove-text-property (start end prop value &optional object)
  "Remove from one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to be removed from the value
already in place. Optional argument OBJECT is the string or buffer containing the text."
  (let ((vlist (mwg-doxygen/ensure-listp value)) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      (if (memq prop '(face font-lock-face))
          (setq prev (mwg-doxygen/canonicalize-old-face-prop-form prev)))
      (put-text-property start next prop
                         (let ((pvlist vlist)
                               (vals (mwg-doxygen/ensure-listp prev)))
                           (while pvlist
                             (setq vals (remove (car pvlist) vals)
                                   pvlist (cdr pvlist)))
                           (mwg-doxygen/canonical-property-value vals))
                         object)
      (setq start next))))
(defun mwg-doxygen/font-lock-chkset-text-property (start end prop value &optional object)
  "VALUE は (target-value . new-values) の形式で指定します。
target-value が text-property として既に設定されている値に含まれている場合に、
既存の設定を全て削除して new-values を text-property に設定します。"
  (let* ((vndl (car value))
         (new-values (cdr value))
         next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      (if (memq prop '(face font-lock-face))
          (setq prev (mwg-doxygen/canonicalize-old-face-prop-form prev)))
      (let ((vals (if (listp prev) prev (list prev))))
        (when (memq vndl vals)
          (put-text-property start next prop
                             (mwg-doxygen/canonical-property-value new-values)
                             object)))
      (setq start next))))

;;
;; example: put the following code into your init.el
;;
;; (eval-after-load "cc-mode"
;;   '(progn
;;      (autoload 'mwg-doxygen-font-lock-keywords "mwg-doxygen" "Doxygen Syntax Highlighting for cc-mode." t)
;;      (setq c-doc-comment-style
;;            (append '((c++-mode . mwg-doxygen) (js-mode . mwg-doxygen))
;;                    (if (listp c-doc-comment-style)
;;                        c-doc-comment-style
;;                      ;; default c-doc-comment-style
;;                      (or (get 'c-doc-comment-style 'c-stylevar-fallback)
;;                          '((java-mode . javadoc) (pike-mode . autodoc) (c-mode . gtkdoc))))))))
;;
