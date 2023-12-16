
;; Seems to be a package already included in Emacs from v27 on

(use-package tab-line
  :ensure nil
  :hook (after-init . global-tab-line-mode)
  :config
  (setq tab-line-close-button-show t
      tab-line-new-button-show nil
      tab-line-separator "")
  
  (defun tab-line-close-tab (&optional e)
    "Close the selected tab.

If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing
windows, kill the buffer with `kill-buffer` function.  Lastly, if
no tabs left in the window, it is deleted with `delete-window`
function."
    (interactive "e")
    (let* ((posnp (event-start e))
           (window (posn-window posnp))
           (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
      (with-selected-window window
        (let ((tab-list (tab-line-tabs-window-buffers))
              (buffer-list (flatten-list
                            (seq-reduce (lambda (list window)
                                          (select-window window t)
                                          (cons (tab-line-tabs-window-buffers) list))
                                        (window-list) nil))))
          (select-window window)
          (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
              (progn
                (if (eq buffer (current-buffer))
                    (bury-buffer)
                  (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                  (set-window-next-buffers window (delq buffer (window-next-buffers))))
                (unless (cdr tab-list)
                  (ignore-errors (delete-window window))))
            (and (kill-buffer buffer)
                 (unless (cdr tab-list)
                   (ignore-errors (delete-window window))))))))
)

(defcustom tab-line-tab-min-width 10
  "Minimum width of a tab in characters."
  :type 'integer
  :group 'tab-line)

(defcustom tab-line-tab-max-width 30
  "Maximum width of a tab in characters."
  :type 'integer
  :group 'tab-line)


(defun aorst/tab-line-name-buffer (buffer &rest _buffers)
  "Create name for tab with padding and truncation.

If buffer name is shorter than `tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs.  This function also tries to fit as
many tabs in window as possible, so if there are no room for tabs
with maximum width, it calculates new width for each tab and
truncates text if needed.  Minimal width can be set with
`tab-line-tab-min-width' variable."
    (with-current-buffer buffer
      (let* ((window-width (window-width (get-buffer-window)))
             (tab-amount (length (tab-line-tabs-window-buffers)))
             (window-max-tab-width (if (>= (* (+ tab-line-tab-max-width 3) tab-amount) window-width)
                                       (/ window-width tab-amount)
                                     tab-line-tab-max-width))
             (tab-width (- (cond ((> window-max-tab-width tab-line-tab-max-width)
                                  tab-line-tab-max-width)
                                 ((< window-max-tab-width tab-line-tab-min-width)
                                  tab-line-tab-min-width)
                                 (t window-max-tab-width))
                           3)) ;; compensation for ' x ' button
             (buffer-name (string-trim (buffer-name)))
             (name-width (length buffer-name)))
        (if (>= name-width tab-width)
            (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
          (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
                 (buffer-name (concat padding buffer-name)))
            (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

(setq tab-line-tab-name-function #'aorst/tab-line-name-buffer
      tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                        'keymap tab-line-right-map
                                        'mouse-face 'tab-line-highlight
                                        'help-echo "Click to scroll right")
      tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                       'keymap tab-line-left-map
                                       'mouse-face 'tab-line-highlight
                                       'help-echo "Click to scroll left")
      tab-line-close-button (propertize (if (char-displayable-p ?×) " × " " x ")
                                        'keymap tab-line-tab-close-map
                                        'mouse-face 'tab-line-close-highlight
                                        'help-echo "Click to close tab")
)


;; to set tab line height, tune `tab-line' `:height' attribute

;;----------------------------
;; tab color settings

;; background behind tabs
(set-face-attribute
  'tab-line nil
  :background "gray" :foreground "gray60"
  :distant-foreground "gray50" :height 1.05 :box nil)

;; active tab in another window
(set-face-attribute
  'tab-line-tab nil :inherit 'tab-line
  :foreground "gray70" :background "gray90" :box nil)

;; active tab in current window
(set-face-attribute
  'tab-line-tab-current nil
  :background "#b34cb3" :foreground "white" :box nil)

;; inactive tab
(set-face-attribute
  'tab-line-tab-inactive nil
  :background "gray" :foreground "black" :box nil)

;; mouseover
(set-face-attribute
  'tab-line-highlight nil
  :background "white" :foreground  "#b34cb3")

(dolist (mode '(ediff-mode
                process-menu-mode
                term-mode
                vterm-mode))
  (add-to-list 'tab-line-exclude-modes mode))
)

