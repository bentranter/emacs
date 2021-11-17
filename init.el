;;; package --- init.el

;;; Commentary:
;;; This is my Emacs config.

;;; Code:

;;
;; 1. Initialization.
;;

;; Require Emacs package functionality.
(require 'package)

;; Add the Melpa repository to the list of package sources.
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Initialize the package system.
(package-initialize)

;; Keep the installed packages in .emacs.d.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Update local package metadata if the local cache is missing.
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Ben Tranter"
      user-mail-address "bentranter@hey.com")

;; Always load newest byte code.
(setq load-prefer-newer t)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; Disable the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Disable the startup screen.
(setq inhibit-startup-screen t)

;; Nicer scrolling.
(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Maximize the initial frame automatically
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Use English even if my computer isn't in English
(set-language-environment "English")

;; Show line numbers.
(global-display-line-numbers-mode)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)

;; Enable y/n answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; List all packages.
(defvar package-list)
(setq package-list
      '(
        company
        eglot
        evil
        exec-path-from-shell
        git-gutter
        go-mode
        magit
        use-package
        yasnippet
	))

;; Install any missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;
;; 2. Key rebindings.
;;

;; Use Cmd-r to evaluate the current buffer. Useful for quickly reloading your
;; config, or evaluating a scratch buffer.
(global-set-key (kbd "s-r") #'eval-buffer)

;; Use Cmd-s to comment lines or blocks of code, like other modern editors.
(global-set-key (kbd "s-/") #'comment-line)

;; Use Cmd-[ and Cmd-] to unindent or indent lines or blocks of code, like
;; other modern editors.
(global-set-key (kbd "s-[") #'evil-shift-left)
(global-set-key (kbd "s-]") #'evil-shift-right)

;;
;; 3. Packages.
;;

;; Load packages via use-package.
(require 'use-package)

;; Use Evil Mode.
(use-package evil
  :defer t
  :init
  (evil-mode 1))

(use-package eglot
  :ensure t
  :defer t
  :init)

;; Magit setup
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (defun inkel/magit-log-edit-mode-hook ()
      (setq fill-column 72)
      (flyspell-mode t)
      (turn-on-auto-fill))
    (add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))))

;; Read your $PATH properly on stupid macOS
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;; Use YASnippet
(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1))

;; Tabs are 4 spaces in Go
(add-hook 'go-mode-hook #'(lambda ()
                            (setq tab-width 4)))

;; Set default font
(set-face-attribute 'default nil
                     :family "Source Code Pro"
                     :height 130
                     :weight 'normal
                     :width 'normal)

;; Enable git gutter
(global-git-gutter-mode +1)

(require 'company)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)
     (define-key company-active-map (kbd "RET") 'company-complete-selection)))
(setq-default company-selection-wrap-around t)
(setq-default company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

;; Go setup
(require 'company-go)
(require 'go-eldoc)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
 			  (set
 			   (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

;; Lisp setup
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Newline at the end of the file.
(setq require-final-newline t)

;;
;; 4. Colours and theme.
;;;

;; Set the theme.
(load-theme 'nord t)

;; Get rid of the custom stuff that everyone hates
(setq custom-file "~/.emacs.d/custom.el")
;; (load custom-file 'noerror)

;; And that's it!
(provide 'init)

;;; init.el ends here
