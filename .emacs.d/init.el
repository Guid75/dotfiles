(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
;;(setq visible-bell t)

(delete-selection-mode)

;; M-arrows for switching windows
(windmove-default-keybindings 'meta)

;; remove annoying default pageup/pagedown behavior
(setq scroll-error-top-bottom t)

;; Make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Show matching parenthesis
(show-paren-mode t)

;; full screen toggle using command+[RET]
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)
(toggle-fullscreen)

(set-face-attribute 'default nil :font "Fira Code" :height 102)

(define-key global-map "\C-cc" 'comment-region)
(define-key global-map "\C-cu" 'uncomment-region)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; custom file (based on $USER)
(if (eq system-type 'windows-nt)
    (setq custom-file (expand-file-name "~/.emacs.d/lisp/pb-custom.el"))
  (setq custom-file (expand-file-name
             (concat "~/.emacs.d/lisp/"
                 (getenv "USER")
                 "-custom.el")))
  )

(if (file-exists-p custom-file)
    (load-file custom-file)
  (setq custom-file (expand-file-name "~/.emacs-cust"))
  )

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons)

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; (use-package general
;;   :config
;;   (general-create-definer rune/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (rune/leader-keys
;;     "t"  '(:ignore t :which-key "toggles")
;;     "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))

(defhydra zoom (global-map "C-c z")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra magit (global-map "C-c m")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; (rune/leader-keys
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package magit)

(use-package ligature
  :load-path "manual/ligature.el/"
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  
  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))



;; this package allows to run nvm nodejs
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package prettier-js)
  ;; :config
  ;; (add-hook 'js-mode-hook 'prettier-js-mode))


(add-hook 'js-mode-hook 'add-node-modules-path)
(add-hook 'js-mode-hook 'prettier-js-mode)

;; (eval-after-load 'js-mode
;;     '(progn
;;        (add-hook 'js-mode-hook #'add-node-modules-path)
;;        (add-hook 'js-mode-hook #'prettier-js-mode)))

;; (eval-after-load 'js-mode
;;    '(add-hook 'js-mode-hook #'add-node-modules-path))

(use-package markdown-mode)

(use-package projectile
  :bind (("C-f" . projectile--find-file))
	
  :config
	(projectile-mode +1)
	(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
	(add-to-list 'projectile-globally-ignored-directories "webui/ext")
	(add-to-list 'projectile-globally-ignored-directories "webui/packages/local/vtypes/test")
	(add-to-list 'projectile-globally-ignored-directories "webui/build")
	(add-to-list 'projectile-globally-ignored-directories "node_modules")
	(add-to-list 'projectile-globally-ignored-directories "robotframework")
	(add-to-list 'projectile-globally-ignored-files "webui/synapse/bootstrap*"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package js2-refactor
	:hook (js2-mode . js2-refactor-mode)
	:config
	(js2r-add-keybindings-with-prefix "C-c C-m")
	)

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; (use-package apheleia
;;   :config
;;   (apheleia-global-mode +1))

(use-package lsp-mode
  ;; :straight t
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
							("M-<space>" . completion-at-point)
							("M-." . lsp-find-definition)
							)
  :custom (lsp-headerline-breadcrumb-enable nil)
  :config
	(global-set-key (kbd "<f2>") 'lsp-rename))

;; (use-package lsp-mode
;;   :hook ((c-mode          ; clangd
;;           c++-mode        ; clangd
;;           c-or-c++-mode   ; clangd
;;           java-mode       ; eclipse-jdtls
;;           js-mode         ; ts-ls (tsserver wrapper)
;;           js-jsx-mode     ; ts-ls (tsserver wrapper)
;;           typescript-mode ; ts-ls (tsserver wrapper)
;;           python-mode     ; pyright
;;           web-mode        ; ts-ls/HTML/CSS
;;           ) . lsp-deferred)
;;   :custom-face
;;   (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
;;   (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
;;   (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
;;   (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
;;   :commands lsp
;;   :config
;;   (define-key lsp-mode-map (kbd "C-c l <tab>") #'lsp-execute-code-action)
;;   (define-key lsp-mode-map (kbd "C-c l TAB") #'lsp-execute-code-action)
;;   (global-unset-key (kbd "<f2>"))
;;   (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-log-io nil)
;;   (setq lsp-restart 'auto-restart)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-enable-on-type-formatting nil)
;;   (setq lsp-signature-auto-activate nil)
;;   (setq lsp-signature-render-documentation nil)
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-modeline-code-actions-enable nil)
;;   (setq lsp-modeline-diagnostics-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-semantic-tokens-enable nil)
;;   ;(setq lsp-enable-folding nil)
;;   (setq lsp-enable-imenu nil)
;;   (setq lsp-enable-snippet nil)
;;   (setq read-process-output-max (* 1024 1024)) ;; 1MB
;;   (setq lsp-idle-delay 0.5)
;;   (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package lsp-ui
  :preface
  (defun ian/lsp-ui-doc-show ()
    "Sometimes lsp-ui-doc-show needs more than one call to display correctly."
    (interactive)
    (lsp-ui-doc-hide)
    (lsp-ui-doc-show)
    (lsp-ui-doc-show))
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :config
  (define-key lsp-ui-mode-map (kbd "C-c l s") #'ian/lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c l h") #'lsp-ui-doc-hide)
  (custom-set-faces '(lsp-ui-sideline-global ((t (:italic t)))))
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package multiple-cursors
	:config
	(global-set-key (kbd "C->") 'mc/mark-next-like-this)
	(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
	(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package company
	:config
	(add-hook 'after-init-hook 'global-company-mode)
	(global-set-key (kbd "M-/") 'company-complete))

(use-package docker)

(use-package beacon
 :config
 (beacon-mode 1))

(use-package dap-mode)

(require 'dap-firefox)
(require 'dap-node)
(require 'dap-chrome)

;; ripgrep based
(use-package rg
  :bind (("C-M-g" . rg-project)))
	
;; enable for all programming modes
(add-hook 'prog-mode-hook 'subword-mode)

(use-package expand-region
  :bind (("C-;" . er/expand-region)))

(use-package yasnippet
	:config
	(yas-global-mode 1))

(use-package yasnippet-snippets)
