;;; This my init.el file
;;; Emacs 26
;;; Not compatible with Emacs 25 or lower

;;; TODO

(require 'package)
(package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;;; My custom file
(setq custom-file "~/.emacs.d/custom.el")

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package indent-guide)
(indent-guide-global-mode)

(use-package format-all)
(add-hook 'format-all-mode-hook
	  'format-all-ensure-formatter)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq create-lockfiles nil)
(setq kill-do-not-save-duplicates t)
(setq ring-bell-function 'ignore)
(setq indent-tabs-mode nil)

(scroll-bar-mode 0)
(menu-bar-mode 0)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)
