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

(setq vc-follow-symlinks t)

;; Make the y or n suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Show matching parenthesis
(show-paren-mode t)

(set-face-attribute 'default nil :font "Fira Code" :height 102)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 125 :weight 'regular)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; full screen toggle using command+[RET]
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)
(toggle-fullscreen)


(define-key global-map "\C-cc" 'comment-region)
(define-key global-map "\C-cu" 'uncomment-region)

(define-key global-map "\M-n" 'next-error)
(define-key global-map "\M-p" 'previous-error)

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

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

(with-eval-after-load 'counsel
  (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
        (alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
    (define-key counsel-find-file-map done #'ivy-alt-done)
    (define-key counsel-find-file-map alt  #'ivy-done)))


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

(defhydra js2r (global-map "C-c j")
  "zoom"
  ("c" js2r-extract-const)
  ("f" js2r-extract-function))

;; (rune/leader-keys
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package magit)

;; Install delta for the following to work : https://github.com/dandavison/delta#installation
;; magit-delta is SLOW with big changes => too bad :(
;; (use-package magit-delta
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode))

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

(use-package prettier-js
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  )

(use-package add-node-modules-path)

(add-hook 'js-mode-hook 'add-node-modules-path)

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
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :config
  (setq typescript-indent-level 2)
  )

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
  (add-hook 'json-mode-hook #'dw/set-js-indentation)
  (electric-pair-mode))


(use-package js2-refactor
	:hook (js2-mode . js2-refactor-mode)
	:config
	(js2r-add-keybindings-with-prefix "C-c C-m")
	)

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

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
	      ("M-C-f" . lsp-find-references)
	      )
  :custom (lsp-headerline-breadcrumb-enable nil)
  :config
	(global-set-key (kbd "<f2>") 'lsp-rename))
	;; (add-hook #'ace-jump-mode))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

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
  (defun my/lsp-ui-doc-show ()
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
  (define-key lsp-ui-mode-map (kbd "C-c l s") #'my/lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c l h") #'lsp-ui-doc-hide)
  (define-key lsp-ui-mode-map (kbd "C-c l p") #'lsp-ui-peek-find-references)
	(define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
;;	(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
	(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
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
  :bind (("C-M-g" . counsel-rg))
  :custom
  (rg-ignore-ripgreprc nil))
	
;; enable for all programming modes
;;(add-hook 'prog-mode-hook 'subword-mode)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package yasnippet
	:config
	(yas-global-mode 1))

(use-package yasnippet-snippets)

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun my/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
	(setq org-agenda-start-with-log-mode t)
	(setq org-log-done 'time)
	(setq org-log-into-drawer t)

	(setq org-agenda-files
	      '("~/Tasks.org"
		))
	
  (my/org-font-setup)
	(setq org-todo-keywords
				'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
					(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(use-package yaml-mode)
(put 'downcase-region 'disabled nil)

(save-place-mode 1)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(setq create-lockfiles nil)
(use-package move-text)
(global-set-key [s-up] 'move-text-up)
(global-set-key [s-down] 'move-text-down)
;(move-text-default-bindings)

(setq backup-directory-alist `(("." . "~/.saves")))

(use-package bufler
  :bind (("C-x C-b" . bufler)
         ("C-x b" . bufler-switch-buffer)))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-file-event-delay                1000)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(defun copy-and-comment-region (beg end &optional arg)
  "Duplicate the region and comment-out the copied text.
See `comment-region' for behavior of a prefix arg."
  (interactive "r\nP")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region beg end arg))

(define-key global-map "\C-ck" 'copy-and-comment-region)
