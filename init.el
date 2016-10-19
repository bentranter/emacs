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
        neotree ;; for now, may remove it
        terraform-mode
        tide ;; for now, may remove it
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
(global-visual-line-mode 1)

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

;; Configure Company Mode
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))
(setq-default company-selection-wrap-around t)
(setq-default company-minimum-prefix-length 1)

;; Setup NeoTree so it works well with Evil Mode
(require 'neotree)
(add-hook 'neotree-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
;; Toggle it with C-x C-t since I do it constantly
(global-set-key (kbd "C-x C-t") 'neotree-toggle)

;; Go setup
(require 'company-go)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set
			   (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

;; Terraform Setup
(require 'terraform-mode)
(add-hook 'terraform-mode-hook 'terraform-fmt-hook)
(defun terraform-fmt-hook ()
  (terraform-format-on-save-mode 1))
(setq terraform-indent-level 2)

;; Python setup

;; Lisp setup
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Typescript Setup
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; Fix Path on macOS
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/Users/ben/Code/go/bin")
;; Theme setup
(load-theme 'base16-tomorrow-night t)

;; And that's it!
(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (terraform-mode tide company-anaconda anaconda-mode neotree exec-path-from-shell zenburn-theme spacegray-theme material-theme helm flycheck evil company-go color-theme-sanityinc-tomorrow)))
 '(safe-local-variable-values (quote ((hl-sexp-mode) (rainbow-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
