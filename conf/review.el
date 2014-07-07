;; review-mode
(autoload 'review-mode "review-mode"
"Major mode for editing Re:VIEW files" t)
(add-to-list 'auto-mode-alist '("\\.re\\'" . review-mode))

;(eval-after-load 'review-mode
;  '(progn
;    (set-face-foreground 'review-mode-title-face "limegreen")
;    (set-face-foreground 'review-mode-header1-face "limegreen")
;    (set-face-foreground 'review-mode-header2-face "limegreen")
;    (set-face-foreground 'review-mode-underline-face "aqua")
;    (set-face-foreground 'review-mode-bold-face "aqua")))
