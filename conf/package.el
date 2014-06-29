(require 'package)

;; package.elでelispを入れるdirectoryの設定
(setq package-user-dir "~/.emacs.d/elpa/")

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

