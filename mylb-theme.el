(deftheme mylb
  "Created 2021-08-22.")

(custom-theme-set-faces
 'mylb
 '(cursor ((((class color) (min-colors 89)) (:background "red"))))
 '(fringe ((((class color) (min-colors 89)) (:background "gray85"))))
 '(highlight ((((class color) (min-colors 89)) (:background "cyan"))))
 '(region ((((class color) (min-colors 89)) (:background "MediumAquamarine"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "white" :foreground "black"))))
 '(isearch ((((class color) (min-colors 89)) (:background "green" :foreground "Black"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "dark turquoise"))))
 '(query-replace ((((class color) (min-colors 89)) (:inherit isearch :background "white" :foreground "black"))))
 '(match ((((class color) (min-colors 89)) (:background "SkyBlue"))))
 '(mode-line ((((class color) (min-colors 89)) (:background "PaleGoldenrod" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((((class color) (min-colors 89)) (:overline "red" :underline "red"))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:inherit mode-line :background "LightGray" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(escape-glyph ((((class color) (min-colors 89)) (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
 '(homoglyph ((((class color) (min-colors 89)) (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#b35caf"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#00006DE06DE0"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "red"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "Blue3"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "Magenta4"))))
 '(font-lock-warning-face ((((class color) (min-colors 89)) (:foreground "orange red" :weight bold))))
 '(next-error ((((class color) (min-colors 89)) (:inherit region :background "SkyBlue"))))
 '(default ((((class color) (min-colors 89)) (:background "#f5fffa" :foreground "black"))))

 ;; hl
 '(diff-hl-insert ((t (:background "#008700" :foreground "#a1db00"))))
 '(diff-hl-change ((t (:background "#005f87" :foreground "#1f5bff"))))
 '(diff-hl-delete ((t (:background "#a40000" :foreground "#ef2929"))))

 ;; flyspell
 '(flyspell-duplicate ((t (:underline (:color "#ff0000" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "#ff0000" :style wave)))))

 ;; org mode
 '(org-block-begin-line
    ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
 '(org-block-background
    ((t (:background "#FFFFEA"))))
 '(org-block-end-line
    ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))

)


(provide-theme 'mylb)
