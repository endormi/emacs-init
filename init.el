;;; This my init.el file
;;; Emacs 26
;;; Not compatible with Emacs 25 or lower


;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-

;; Author: Endormi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Initialize Emacs.

;;; TODO

(require 'package)
(package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;;; My custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; TODO:
;;; kbds can be updated later, depending on what I might add later
(global-set-key (kbd "C-x 1") 'beginning-of-buffer)
(global-set-key (kbd "C-x 0") 'end-of-buffer)
;;; multi-occur-in-matching-buffers
(global-set-key (kbd "C-c C-s") 'multi-isearch-buffers)

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

;;; TODO
(use-package expand-region
    :bind ("C-=" . er/expand-region))

(defun er/add-text-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(mark-paragraph
                                mark-page))))

(add-hook 'text-mode-hook 'er/add-text-mode-expansions)

;;; TODO
(use-package ansi-color)
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point)))

(use-package saveplace)
(save-place-mode t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq create-lockfiles nil)
(setq kill-do-not-save-duplicates t)
(setq ring-bell-function 'ignore)
(setq indent-tabs-mode nil)
(setq sentence-end-double-space nil)

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

(show-paren-mode 1)
(delete-selection-mode 1)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

;;; TODO
(setq comment-auto-fill-only-comments t)
(auto-fill-mode 1)
(add-hook 'text-mode-hook
	  (lambda () (auto-fill-mode -1)))
