;;; general
(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-updates)     ; Tools to upgrade Crafted Emacs
(require 'crafted-use-package) ; Configuration for `use-package`
(require 'crafted-compile)     ; automatically compile some emacs lisp files
(require 'crafted-speedbar)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default scroll-margin 8)

(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.undo.d/")))

;;; UI
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-evil)        ; An `evil-mode` configuration

(crafted-package-install-package 'doom-themes)
(disable-theme 'deeper-blue)
(load-theme 'doom-one t)

;;; org mode
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.

;;; project
(require 'crafted-project)     ; built-in alternative to projectile

;;; IDE
(require 'crafted-ide)
(crafted-package-install-package 'typescript-mode)
(crafted-package-install-package 'python-black)
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(crafted-package-install-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; Shells
(crafted-package-install-package 'vterm)
;; configure vterm for evil-mode
(use-package vterm
  :demand t)
(with-eval-after-load 'evil
  (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point))

;; not sure if needed
;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'typescript-mode-hook 'eglot-ensure)


;;; notes
(crafted-package-install-package 'denote)
(require 'denote)
(setq denote-directory "~/org")

;; keybinds

(custom-set-variables '(evil-want-C-u-scroll t)
                      '(crafted-evil-discourage-arrow-keys t))

(define-key vertico-map (kbd "C-f") 'vertico-exit)

(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key global-map (kbd "C-f") nil)

(define-key corfu-map (kbd "C-j") 'corfu-next)
(define-key corfu-map (kbd "C-k") 'corfu-previous)
(define-key corfu-map (kbd "C-f") 'corfu-quit)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "C-M-j") 'consult-buffer)
(global-set-key (kbd "C-M-k") 'kill-current-buffer)
(global-set-key (kbd "C-M-r") 'consult-ripgrep)
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

;; modules
;; rest client
(crafted-package-install-package 'restclient)
(crafted-package-install-package 'ob-restclient)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))


;;(use-package coffee-mode
;;  :ensure t)

;;(use-package sws-mode
;;  :ensure t)

;;(use-package stylus-mode
;;  :ensure t
;;  :requires sws-mode)

(use-package magit
  :ensure t
  :init
  (message "Loading Magit...")
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(crafted-package-install-package 'minions)
(use-package minions
  :config (minions-mode 1))
