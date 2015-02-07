
;;=============================================================================
;; diff-ring-region
(defun mwg-diff-ring-region()
  (save-excursion
    (let ((b1 (get-buffer-create "*mwg-diff/ring*"))
          (b2 (get-buffer-create "*mwg-diff/region*")))
          (with-current-buffer b1 (erase-buffer) (yank))
          (let ((content (buffer-substring (region-beginning) (region-end))))
            (with-current-buffer b2 (erase-buffer) (insert content)))
          (ediff-buffers b1 b2))))


;; number-spin-up/down

(defun mwg-number-spin-down (&optional DELTA)
  (interactive "p")
  (mwg-number-spin-up (- (if DELTA DELTA 1))))

;; 色々と動作が怪しいので初めから作り直した方が良いかも。
;; * 符号が反転する時が非自明


;; 浮動小数点に対応
;; 負の数の時の動作について調整
(defun mwg-number-spin-up (&optional DELTA ISCARRY)
  (interactive "p")
  (when (and (<= ?0 (char-after))
             (<= (char-after) ?9))
      (let* ((pchar (point))
             (pend (1+ pchar))
             (pbeg (save-excursion
                     (if (re-search-backward "[^-\.0-9]" nil t)
                         (progn (forward-char)
                                (point))
                       (point-min))))
             (isneg (= ?- (char-after pbeg)))
             (ismost (or (= 1 (- pend pbeg))
                         (and isneg (= 2 (- pend pbeg)))))
             ;; (iszero (and (= 1 (- pend pbeg))
             ;;              (= ?0 (char-after pbeg))))
             (delta (if (and isneg (not ISCARRY))
                        (if DELTA (- DELTA) -1)
                      (if DELTA DELTA 1)))
             (n (- (char-after) ?0))
             (m (+ n delta))
             (m1 (% (+ 10 (% m 10)) 10))
             (m2 (/ (- m m1) 10)))
        (cond (ismost
               (delete-region (point) (1+ (point)))
               (insert (number-to-string m))
               (forward-char -1))
              (t
               (delete-region (point) (1+ (point)))
               (insert (char-to-string (+ ?0 m1)))
               (forward-char -1)
               (when (/= m2 0)
                 (save-excursion
                   (re-search-backward "[^\.]" nil t)
                   (mwg-number-spin-up m2 t)
                  )
               )))))

(global-set-key (kbd "C-c <up>") 'mwg-number-spin-up)
(global-set-key (kbd "C-c C-<up>") 'mwg-number-spin-up)
(global-set-key (kbd "C-c <down>") 'mwg-number-spin-down)
(global-set-key (kbd "C-c C-<down>") 'mwg-number-spin-down)
