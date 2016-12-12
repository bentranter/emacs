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
        go-eldoc
        ido
        ido-ubiquitous
        ido-vertical-mode
        neotree ;; for now, may remove
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

;; Use English even if my computer isn't in English
(set-language-environment "English")

;; Read your $PATH properly on stupid macOS
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

;; Wrap lines
(global-visual-line-mode 1)

;; Set default font
(set-face-attribute 'default nil
                    :family "InconsolataGo"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Insane stuff I stole from Coda Hale
(defun coda/configure-cocoa ()
  ;; open up maximized-ish
  (let ((px (display-pixel-width))
        (py (display-pixel-height))
        (fx (frame-char-width))
        (fy (frame-char-height))
        tx ty)
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'default-frame-alist (cons 'width tx))
    (add-to-list 'default-frame-alist (cons 'height ty)))

  ;; don't scroll like a maniac
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))
(if (memq window-system '(mac ns)) (coda/configure-cocoa))

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

;; Enable git gutter
(global-git-gutter-mode +1)

;; Bracket matching
(electric-pair-mode 1)

;; Use Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

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
(setq neo-smart-open t)
(setq neo-theme 'nerd)

;; Go setup
(require 'company-go)
(require 'go-eldoc)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'go-eldoc-setup)
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
(load-theme 'base16-onedark t)

;; And that's it!
(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"])
 '(ansi-term-color-vector
   [unspecified "#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"] t)
 '(custom-safe-themes
   (quote
    ("6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" default)))
 '(package-selected-packages
   (quote
    (diff-hl git-gutter-fringe ido-ubiquitous ido-vertical-mode go-eldoc terraform-mode tide company-anaconda anaconda-mode neotree exec-path-from-shell zenburn-theme spacegray-theme material-theme helm flycheck evil company-go color-theme-sanityinc-tomorrow)))
 '(safe-local-variable-values (quote ((hl-sexp-mode) (rainbow-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
