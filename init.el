
;(setq debug-on-error t)

(require 'cl)

(let*
    ((user-emacs-directory
      (substring (or load-file-name "~/.emacs.d/init.el") 0 -7))
     (conf-list (list
                 "init.el"
                 "exec-path.el"
                 "el-get.el"
                 "cc-mode.el"
                 "clang-complete.el"
                 "go.el"
                 "php.el"
                 "flymake.el"
                 "midnight.el"
                 "blosxom.el"
                 "misc.el"
                 "review.el"
                 "package.el"
                 "markdown.el"
                 "puppet.el"
                 "yaml.el"
                 )))
  (progn
    (dolist (conf conf-list)
      (load (concat user-emacs-directory "conf/" conf)))
    (and (or (equal window-system 'ns) (equal window-system 'mac))
         (dolist (conf (list "cocoa-init.el"
                             "cocoa-el-get.el"
                             "cocoa-theme.el"
                             "cocoa-font.el"
                             "cocoa-server.el"
                             ))
           (load (concat user-emacs-directory "conf/" conf))))
    (and (null window-system)
         (dolist (conf (list "nw-init.el"))
           (load (concat user-emacs-directory "conf/" conf))))))


;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

 ;; 英語
 (set-face-attribute 'default nil
             :family "Menlo" ;; font
             :height 150)    ;; font size

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
;; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定


(exec-path-from-shell-initialize)

;; from http://d.hatena.ne.jp/akm/20080605#1212644489
(require 'ruby-mode)
(defun ruby-mode-set-encoding () ())


(exec-path-from-shell-copy-envs '("GOROOT" "GOPATH"))


(add-hook 'makefile-mode-hook
          (function
           (lambda ()
             (fset 'makefile-warn-suspicious-lines 'ignore))))

