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
        base16-theme
	better-defaults
	company
	company-go
        evil
	exec-path-from-shell
	flycheck
        git-gutter
	go-mode
	))

;; Refresh package list
(unless package-archive-contents
  (package-refresh-contents))

;; Install any missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Read your $PATH properly on stupid macOS
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

;; Wrap lines
(setq visual-line-mode t)

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Enable Git Gutter (like Sublime Text)
(global-git-gutter-mode +1)

;; Evil Mode
(require 'evil)
(evil-mode 1)

;;  Display settings
(global-hl-line-mode 1)
(global-linum-mode 1)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))
(fringe-mode -1)
(setq column-number-mode t)

;; Bracket matching
(electric-pair-mode 1)

;; Use Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

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

;; Theme setup
(load-theme 'base16-ocean t)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell zenburn-theme spacegray-theme material-theme helm flycheck evil company-go color-theme-sanityinc-tomorrow)))
 '(safe-local-variable-values (quote ((hl-sexp-mode) (rainbow-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
