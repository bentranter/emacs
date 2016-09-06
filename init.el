;;; package --- init.el

;;; Commentary:
;;; This is my Emacs config, tuned for Go dev.

;;; Code:

;; Setup package stuff
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; List all packages
(defvar package-list)
(setq package-list
      '(
	color-theme-sanityinc-tomorrow
	company
	company-go
	evil
	flycheck
	go-mode
	helm
	))

;; Refresh package list
(unless package-archive-contents
  (package-refresh-contents))

;; Install any missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Hide toolbars and menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;;  Display settings
(global-linum-mode t)
(global-hl-line-mode 1)
(setq linum-format "%d ")
(setq column-number-mode t)

;; Bracket matching
(electric-pair-mode 1)

;; Theme setup
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; Evil mode
(require 'evil)
(evil-mode 1)

;; Enable Flycheck
(global-flycheck-mode)

;; Go setup
(require 'company-go)

(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/Users/ben/Code/go/bin")

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

;; Lisp setup
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(provide 'init)