(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-hook 'puppet-mode-hook
          '(lambda ()
                  (local-set-key "\M-j" (lambda () (interactive)(insert "=>")))))

