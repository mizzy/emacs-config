(setq howm-directory "~/Dropbox/howm")
(global-set-key "\C-c,," 'howm-menu)

(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)
(autoload 'howm-mode "howm-mode" "Hitori Otegaru Wiki Modoki" t)

(add-hook 'org-mode-hook 'howm-mode)

(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map
       "\C-m" 'my-insert-day)
     (defun my-insert-day ()
       (interactive)
       (let ((day nil)
             (calendar-date-display-form
         '("[" year "-" (format "%02d" (string-to-int month))
           "-" (format "%02d" (string-to-int day)) "]")))
         (setq day (calendar-date-string
                    (calendar-cursor-to-date t)))
         (exit-calendar)
         (insert day)))))
