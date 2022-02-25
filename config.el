(require 'rational-defaults)
(require 'rational-ui)
(require 'rational-editing)
(require 'rational-evil)
(require 'rational-completion)
(require 'rational-windows)

(straight-use-package 'use-package)
(require 'package)

;; Emacs 27.x has the gnu elpa as the default for `package-archives'
;; Emacs 28.x has the gnu and nongnu elpa as the default.
;; Only add nongnu elpa for Emacs version < 28
(when (version< emacs-version "28.0")
  (add-to-list 'package-archives (cons "nongnu" "https://elpa.nongnu.org/nongnu/")))

;; Always add melpa to `package-archives'
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))



;; Set further font and theme customizations
(set-face-attribute 'default nil
                  :font "JetBrains Mono"
                  :weight 'light
                  :height 185)

;; fixes the blue default colors when starting
(setf default-frame-alist (assq-delete-all 'background-color default-frame-alist))
(setf default-frame-alist (assq-delete-all 'foreground-color default-frame-alist))
(load-theme 'doom-nord t)

(set-face-attribute 'region nil :background "#5e81ac" :foreground "#dae2eb")
(set-face-attribute 'lazy-highlight nil :background "#5e81ac" :foreground "#dae2eb")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; keybinds
(defcustom rational-windows-evil-style nil
  "When t, window movement bindings will be evil-style.")

(defcustom rational-windows-prefix-key "C-c w"
  "Configure the prefix key for `rational-windows' bindings.")

(winner-mode 1)

(define-prefix-command 'rational-windows-key-map)

(define-key 'rational-windows-key-map (kbd "k") 'windmove-down)
(define-key 'rational-windows-key-map (kbd "j") 'windmove-up)
(define-key 'rational-windows-key-map (kbd "h") 'windmove-left)
(define-key 'rational-windows-key-map (kbd "l") 'windmove-right)


;; modules

;; restclient
(straight-use-package 'restclient)
(straight-use-package 'ob-restclient)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; LSP
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(use-package coffee-mode
  :ensure t)

(use-package magit
  :ensure t
  :init
  (message "Loading Magit...")
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)
