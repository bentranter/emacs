;;; package --- init.el

;;; Commentary:
;;; This is my Emacs config, tuned for Go dev.

;;; Code:

;; Don't show the startup screen
(setq inhibit-startup-screen t)

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
        company-irony
        elfeed
        evil
        exec-path-from-shell
        flycheck
        flycheck-irony
        flycheck-swift
        git-gutter
        go-mode
        go-eldoc
        ido
        ido-ubiquitous
        ido-vertical-mode
        irony
        irony-eldoc
        neotree
        nord-theme
        oceanic-theme
        projectile
        swift-mode
        terraform-mode
        tide
        yasnippet
	))

;; Refresh package list
(unless package-archive-contents
  (package-refresh-contents))

;; Install any missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Fnd themes dir
(defvar themes-dir)
(setq themes-dir (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path themes-dir)

;; Use English even if my computer isn't in English
(set-language-environment "English")

;; Use the bar cursor
(setq-default cursor-type 'bar)

;; Make deletion like normal text editors, so that when you start typing with a
;; selection, the selection is deleted and replaced with your text.
(delete-selection-mode 1)

;; Read your $PATH properly on stupid macOS
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

;; Wrap lines
(global-visual-line-mode 0)

;; Use YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Tabs are 4 spaces in Go
(add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))

(add-hook 'html-mode-hook '(lambda ()
                             (setq tab-width 2)
                             (setq indent-tabs-mode nil)))

(add-hook 'css-mode-hook '(lambda ()
                            (setq tab-width 2)
                            (setq indent-tabs-mode nil)))

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Modes for file extensions
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))

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

(require 'company)
;; Configure Company Mode
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))
(setq-default company-selection-wrap-around t)
(setq-default company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

;; Enable Projectile
(projectile-mode 1)

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

;; C Setup
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; Replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'company-mode)
(add-hook 'irony-mode-hook 'irony-eldoc)
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;; Python setup

;; Lisp setup
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Swift Setup
(eval-after-load 'flycheck '(flycheck-swift-setup))

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

;; Delete trailing spaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Theme setup
(load-theme 'nord t)

;; RSS Reader Setup (Elfeed)
(require 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("https://kev.inburke.com/feed/"
        "https://blog.filippo.io/rss/"))



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
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("6c35ffc17f8288be4c7866deb7437e8af33cd09930e195738cdfef911ab77274" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "a2ac0ad75d15cd836ca42348b419ac0070c4147ad385f752f794d28c54aa0aa3" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "1156ddf63f7f49890d723b8d7ec9caed9a0d36e53909f5727191fac81192d484" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "1606c3a5e58d74a10289df3c7a4005b670e2b80a54c87f05263862cbe4626ac5" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" default)))
 '(elfeed-feeds
   (quote
    ("https://www.goinggo.net/index.xml" "https://blog.filippo.io/rss/" "https://kev.inburke.com/feed/")))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (nord-theme sr-speedbar elfeed oceanic-theme yasnippet irony-eldoc flycheck-irony company-irony flycheck-swift swift-mode projectile pomodoro diff-hl git-gutter-fringe ido-ubiquitous ido-vertical-mode go-eldoc terraform-mode tide company-anaconda anaconda-mode neotree exec-path-from-shell zenburn-theme spacegray-theme material-theme helm flycheck evil company-go color-theme-sanityinc-tomorrow)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values (quote ((hl-sexp-mode) (rainbow-mode . t))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term ((t (:inherit default)))))
