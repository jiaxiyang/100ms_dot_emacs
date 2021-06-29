(message "PROFILE START INIT.EL %5.2f ms acc" (* 1000.0 (float-time
                                                 (time-subtract
                     (current-time)
                     before-init-time))))
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.

(eval-when-compile
  ;; no-load-path.el must be found in `load-path`. Fortunately this is
  ;; only needed at compile time.
  (require 'no-load-path))

(profile-form
 'no-load-path-init
 (no-load-path-init))

(profile-form
 'add-hook
 (progn
   (setq package-enable-at-startup nil)
   (setq inhibit-default-init t)
   ;; (setq initial-buffer-choice nil)
   (setq inhibit-startup-echo-area-message (user-login-name))
   (setq inhibit-startup-screen t)
   (setq initial-major-mode 'text-mode)
   (add-hook 'after-init-hook
             #'(lambda ()
                 (setq initial-scratch-message
                       (format "%5.2f ms for emacs to startup "
                               (* 1000.0 (float-time
                                          (time-subtract
                                           after-init-time
                                           before-init-time)))))))))

;; == basic configurations
(eval-after-load 'simple
  '(progn
    (add-hook 'before-save-hook 'delete-trailing-whitespace t nil)
    (set-default 'tab-width 4)
    (set-default 'indent-tabs-mode nil)
    (add-to-list 'minor-mode-alist '(mark-active " Mark"))
    (pending-delete-mode 1)
    (defalias 'yes-or-no-p #'y-or-n-p)))

;; == xt-mouse
(use-package xt-mouse
  :straight (xt-mouse :type built-in)
  ;; :when (not (display-graphic-p))
  :defer 2
  :config (xterm-mouse-mode 1)
  (require 'mwheel))

;; scroll one line at a time (less "jumpy" than defaults)

;; == mwheel
(use-package mwheel
  :straight (mwheel :type built-in)
  :functions (mouse-wheel-mode)
  :defer t
  :defines (mouse-wheel-scroll-amount mouse-wheel-progressive-speed  mouse-wheel-follow-mouse)
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (mouse-wheel-mode 1))

;; == savehist
(use-package savehist
  :straight (savehist :type built-in)
  ;; it is so important, so we pay 2-3ms to  initialize it, it worth.
  ;; :defer 2
  :config
  (savehist-mode 1))

;; == exec-path-from-shell
(use-package exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
            (memq system-type '(darwin)))
  :disabled t
  :after (simple)
  :functions (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

;; == configuration for win32
(when (eq window-system 'w32)
  (defvar putty-directory)
  (defvar tramp-default-method)
  (setq putty-directory "C:/Program Files/PuTTY/")
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))

;; == diminish
(use-package diminish)

;; == expand-region
(use-package expand-region
  :commands (er/expand-region)
  :bind ("C-@" . er/expand-region))

;; == help
(use-package discover-my-major
  :bind (("C-h RET" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))

;; == ivy mode
(use-package ivy
  :defer 2
  :diminish ivy-mode
  :defines (ivy-use-virtual-buffers
            ivy-count-format
            ivy-minibuffer-map)
  :functions (ivy-mode)
  :hook (after-init-idle .  ivy-mode)
  :bind (:map ivy-minibuffer-map
              (("C-w" . ivy-yank-word)))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; no regexp by default
  (setq ivy-count-format "%d/%d "))

;; == ivy-hydra
(use-package ivy-hydra)

;; == counsel
(use-package counsel
  :defer t
  :defines (ivy-minibuffer-map)
  :bind (("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line)))

;; == swiper
(use-package swiper
  :bind ( ;;("C-s" . swiper-isearch)
          ;; ("C-r" . swiper-isearch)
          ("C-c C-r" . ivy-resume)
          ("M-x" . counsel-M-x)
          ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; == dump jump, jump to definition
(use-package dumb-jump
  :defines (dumb-jump-selector)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g l" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  ;; (setq dumb-jump-selector 'helm)
  :init
  ;; (dumb-jump-mode)
  )

;; == avy
(use-package avy
  :disabled nil
  :after (isearch-mode-map)
  :bind (:map isearch-mode-map
              (("C-l" . avy-isearch))))

;; === rg
(use-package rg
  :commands (rg)
  :bind (("C-c s"  . rg)))

;; == projectile
(use-package projectile
  :defines (projectile-completion-system projectile-command-map)
  :functions (projectile-discover-projects-in-directory)
  ;; :init
  ;; (projectile-mode +1)
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (require `subr-x)
  (setq projectile-indexing-method 'hybrid)
  ;; (setq projectile-sort-order 'recentf)
  (setq projectile-sort-order 'recently-active)
  (projectile-discover-projects-in-directory (getenv "PWD"))
  (setq projectile-completion-system 'ivy))

;; == magit
(use-package magit
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; == forge  deal with PR
(use-package forge
  :after magit)

;; == compile
(use-package compile
  :straight (compile :type built-in)
  :bind (;;("M-7" . compile)
         ("<f7>" . compile))
  :defines (compilation-scroll-output compilation-read-command)
  :config
  (setq compilation-scroll-output t
        compilation-read-command nil
        compilation-ask-about-save nil))

;; == ffap
(use-package ffap
  :defines (ffap-c-path)
  :config
  (if (getenv "INCLUDE")
      (setq ffap-c-path
            (append (split-string (getenv "INCLUDE") ":" nil)
                    ffap-c-path))))

;; == company-mode
(defvar company-backends)

;; == company
(use-package company
  :defer 1
  :defines (
            company-active-map
            company-idle-delay
            company-minimum-prefix-length
            company-show-numbers
            company-tooltip-limit
            company-dabbrev-downcase)
  :bind (:map company-active-map
              (("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :config
  (global-company-mode 1)
  (setq company-idle-delay              nil) ;;关闭自动提示
  (setq company-minimum-prefix-length   4)
  (setq company-show-numbers            t)
  (setq company-tooltip-limit           10)
  (setq company-dabbrev-downcase        nil)
  :bind (:map company-mode-map
              ("C-r" . company-complete)))

;; == company-irony
(use-package company-irony
  :disabled t
  :after company)

;; == company-c-headers
(use-package company-c-headers
  :after company
  :defines (company-backends)
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; c-eldoc for eglot cannot enable eldoc mode(no
;; eldoc-documentation-function), or you can enable
;; lsp-mode
;; (use-package c-eldoc
;;   :config
;;   (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;;   (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode))
;; (use-package irony-mode
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode))

;; == eglot
(use-package eglot
  :defines (eglot-mode-map eglot-server-programs
                           eglot-managed-mode-hook eglot-ignored-server-capabilites)
  :hook (((c-mode c++-mode) . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c h" . eglot-help-at-point)
              ("C-c f r" . xref-find-references)
              ("C-c f d" . eglot-find-declaration ;; xref-find-definitions
               )
              ("C-c f D" . xref-find-definitions-other-window)
              ("C-c f t" . eglot-find-typeDefinition)
              ("C-c f i" . eglot-find-implementation)
              ("C-c =" . eglot-format-buffer)
              ("C-c c" . eglot-completion-at-point)
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :config
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider));; eglot不高亮
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  )

;; == lsp-mode
(use-package lsp-mode
  :defines (lsp-keymap-prefix lsp-enable-symbol-highlighting lsp-ui-doc-enable)
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-l")
  :hook (((rust-mode c-mode c++-mode)
          . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-ui-doc-enable nil) ;; lsp关闭弹窗
  (setq lsp-ui-sideline-mode nil) ;; lsp 关闭右侧错误提示
  (setq lsp-enable-symbol-highlighting nil)) ;; lsp 不高亮

;; == lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)
  ;; :init
  ;; (push "~/.emacs.d/straight/build/lsp-ui/" load-path))

;; == company-lsp
(use-package company-lsp
  :after (company)
  :commands company-lsp
  :config
  ;; no need to do it, lsp already did it
  ;; (push 'company-lsp company-backends)
  )

;; == lsp-ivy
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; == lsp-treemacs
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; == dap-mode
(use-package dap-mode)

;; == which-key
(use-package which-key
  :defer 1
  :config
  (which-key-mode))

;; == hippie-exp
(use-package hippie-exp
  :straight (hippie-exp :type built-in)
  :bind ("M-?" . hippie-expand))

;; == leader-key-mode
(use-package skeleton-snippet
  :straight
  (skeleton-snippet :type git
                    :host github
                    :repo "wcy123/skeleton-snippet")
  :bind (("C-]" . skeleton-snippet))
  )

;; == skeleton-snippet-store
(use-package skeleton-snippet-store
  :straight
  (skeleton-snippet-store :type git
                          :host github
                          :files ("*.el" "snippets")
                          :repo "wcy123/skeleton-snippet-store")
  :after (skeleton-snippet)
  :config
  (skeleton-snippet-store-initialize))

;; == yasnippet
(use-package yasnippet
  :defer 1
  :defines (yas-minor-mode-map yas-maybe-expand)
  :config
  (global-set-key (kbd "M-'") 'company-yasnippet)
  ;; :bind (:map yas-minor-mode-map
  ;;             ("<tab>" . nil)
  ;;             ("TAB" . nil))
  (yas-global-mode 1)
  ;; very useful, complete snippet only when snippet available
  (define-key yas-minor-mode-map (kbd "M-?") yas-maybe-expand)
  (eval-after-load 'hippine-exp
    '(progn
      (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))))

(use-package yasnippet-classic-snippets)  ;; it takes ~300ms to load snippets

;; == yasnippet-snippets
(use-package yasnippet-snippets
  ;; it takes even longger
  :straight (yasnippet-snippets
             :type git
             :host github
             :files ("*.el" "snippets")
             :repo "AndreaCrotti/yasnippet-snippets"))

;; (use-package wcy123-snippets
;;   :straight (wcy123-snippets
;;              :type git
;;              :host github
;;              :files ("*.el" "snippets")
;;              :repo "wcy123/wcy123-emacs-snippets")
;;   :after (yasnippet)
;;   :config
;;   (wcy123-snippets-initialize))


;; == ace-jump-mode
(use-package ace-jump-mode)

;; == recentf
(use-package recentf
  :defines (recentf-max-saved-items)
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 200))

;; == leader-key-mode
(use-package leader-key-mode
  :straight
  (leader-key-mode :type git
                   :host github
                   :repo "jiaxiyang/leader-key-mode")
  :functions (leader-key-mode)
  ;; :defer 1
  :init
  (leader-key-mode))

;; == tmux-cc
(use-package tmux-cc
  :straight
  (tmux-cc :type git
           :host github
           :repo "wcy123/tmux-cc")
  :commands
  (tmux-cc-send-current-line tmux-cc-select-block
                             tmux-cc-send-region))

;; == insert-translated-name
(use-package insert-translated-name
  :straight
  (insert-translated-name-mode :type git
           :host github
           :repo "manateelazycat/insert-translated-name")
  :defer 1)

;; == shell
(use-package sh-script
  :defer t
  :defines (sh-mode-map)
  :bind (:map sh-mode-map
              ("M-z" . tmux-cc-send-current-line)
              ("C-z" . tmux-cc-send-region)))

;; == markdown
(use-package markdown-mode
  :defines (markdown-mode-map)
  :mode "\\.md\\'"
  :mode "\\.markdown\\'"
  ;; :hook (markdown-mode . yas-minor-mode-on)
  :bind (:map markdown-mode-map
              ("ESC <up>"  . markdown-move-up)
              ("ESC <down>" . markdown-move-down)
              ("ESC <left>" . markdown-promote)
              ("ESC  <right>" . markdown-demote)
              ("M-z" . tmux-cc-send-current-line)
              ("C-z" . tmux-cc-send-region)
              ("<M-RET>" . crux-smart-open-line)
              ("M-n" . next-line)
              ("M-p" . previous-line)))

;; == c/c++
(use-package cc-mode
  :after (cc-mode)
  :defines (c-default-style)
  :hook (c-mode-common . subword-mode)
  :config
  ;; 设置缩进风格. 用 M-x c-set-style ,然后用 TAB 查看补全结果,可以看到所有风格名称.
  (setq c-default-style
        '((java-mode . "java")
          ;;(c-mode . "k&r")
          (c-mode . "linux")
          ;;(c++-mode . "ellemtel")
          (c++-mode . "stroustrup")
          (other . "gnu")
          )))

;; == flycheck
(use-package flycheck
  :after (cc-mode)
  :hook (c++-mode . flycheck-mode)
  :hook (c-mode . flycheck-mode))

;; == clang-format
(use-package clang-format
  :defines (clang-format-fallback-style)
  :after (cc-mode)
  :config
  (set-default 'clang-format-fallback-style "Google")
  (add-hook 'c-mode-common-hook #'(lambda()
                                    (add-hook 'before-save-hook
                                              'clang-format-buffer t t))))
;; == gud
(use-package gud
  :defines (gud-chdir-before-run)
  :config
  (setq gud-chdir-before-run nil))

;; == xcscope
;; == xcscope
(use-package xcscope
  :after cc-mode
  :hook (c-mode . cscope-minor-mode)
  :hook (c++-mode . cscope-minor-mode)
  :hook (dired-mode-hook . cscope-minor-mode)
  :defines (cscope-list-entry-keymap)
  :config
  (define-key cscope-list-entry-keymap
    [mouse-2]  nil)
  (define-key cscope-list-entry-keymap [S-mouse-2] nil)
  (define-key cscope-minor-mode-keymap [mouse-3]   nil)
  (define-key cscope-minor-mode-keymap [S-mouse-3] nil))

;; == cmake
(use-package cmake-mode
  :defines (company-backends)
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook (cmake-mode . my-cmake-mode-hook)
  :config
  (defun my-cmake-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-files
                                                   company-cmake))))
;; == cmake-format
(use-package cmake-format
  :after (cmake-mode)
  :disabled t ;; it seems buggy
  :when (locate-file "cmake-format" exec-path)
  :straight (cmake-format
             :type git
             :host github
             :repo "simonfxr/cmake-format.el"
             :fork (:host github
                          :repo "wcy123/cmake-format.el"))
  ;; optional: enable automatic formatting on save
  :hook ((cmake-mode . cmake-format-mode))
  :config
  ;; optional:
  ;; (setq cmake-format-command "/path/to/cmake-format"
  ;;       cmake-format-args '("list" "of" "flags"))
  )


;; == elisp-mode
(use-package elisp-mode
  :straight (elisp-mode :type built-in)
  :hook (emacs-lisp-mode . auto-fill-mode)
  :hook (emacs-lisp-mode . show-paren-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-l" . eval-buffer)
              ("C-c C-c" . eval-defun))
  :config (require 'pp))

;; == elisp-slime-nav
(use-package elisp-slime-nav
  :after (elisp-mode)
  :functions (elisp-slime-nav-mode)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

;; == pp
(use-package pp
  :straight (pp :type built-in)
  :after (pp)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-m" . pp-macroexpand-expression)
              ("C-c C-e" . pp-macroexpand-last-sexp))
  :bind (("M-:" . pp-eval-expression)
         ("C-x C-e" . pp-eval-last-sexp)))

;; == protobuf-mode
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; == adoc
(use-package adoc-mode
  :mode "\\.adoc\\'")

;; == haskell-mode
(use-package haskell-mode
  :mode "\\.hs\\'")

;; == rust-mode
(use-package rust-mode
  :mode "\\.rs\\'"
  :defines (rust-format-on-save)
  :functions (cargo-minor-mode company-indent-or-complete-common)
  :config
  (global-yascroll-bar-mode -1)
  (setq rust-format-on-save t))

;; == cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; == racer
(use-package racer
  :defines (rust-mode-map company-tooltip-align-annotations)
  :functions (company-indent-or-complete-common )
  :after (rust-mode)
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq racer-rust-src-path
              (concat (string-trim
                       (shell-command-to-string "rustc --print sysroot"))
                      "/lib/rustlib/src/rust/library"))
                      ;; "/lib/rustlib/src/rust/src"))
  (setq company-tooltip-align-annotations t)
  (setq racer-loaded 1))

;; == gcmh
(use-package gcmh
  :diminish gcmh-mode
  :config
  (gcmh-mode 1))



;; ********* jiaxiyang's config ********
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; bookmarks+
(use-package bookmark+)

;; evil
(use-package evil)

;; smartparens
(use-package smartparens
  :defines (sp-autoinsert-pair sp-highlight-pair-overlay)
  :defer 1
  :config
  (global-set-key (kbd "C-M-w") 'sp-copy-sexp)
  (global-set-key (kbd "M-k") 'sp-copy-sexp)
  (global-set-key (kbd "C-M-k") 'sp-backward-kill-sexp)
  (global-set-key (kbd "M-s") 'sp-kill-sexp)
  ;; (setq sp-autoinsert-pair nil)
  (setq sp-highlight-pair-overlay nil)
  (smartparens-global-mode 1))

;; multiple-cursors
(use-package multiple-cursors)

;; hungrey-delete
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

;; org-bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; keyfreq
(use-package keyfreq
  :defer 2
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; ;; doom-modeline
;; (use-package doom-modeline
;;   :config
;;   (doom-modeline-mode 1))

;; focus
(use-package focus)

;; ivy-rich Notes: takes 80ms when startup
(use-package ivy-rich
  :defer 2
  :config
  (ivy-rich-mode 1))

;; aggressive-indent
(use-package aggressive-indent)

;; git-gutter
(use-package git-gutter)

;; (use-package dracula-theme
  ;; :ensure t
  ;; :config (load-theme 'dracula t))

;; ;;google translate
;; (use-package google-translate
;;   :defer 1
;;   :defines
;;   (google-translate-base-url google-translate-listen-url
;;                              google-translate--tkk-url
;;                              google-translate-default-source-language
;;                              google-translate-default-target-language)
;;   :functions (google-translate--search-tkk google-translate--get-b-d1)
;;   :custom
;;   (google-translate-backend-method 'curl)
;;   :config
;;   (require 'google-translate)
;;   ;; (defun google-translate--get-b-d1 () (list 427110 1469889687))
;;   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
;;   (setq google-translate-base-url
;;         "http://translate.google.cn/translate_a/single")
;;   (setq google-translate-listen-url
;;         "http://translate.google.cn/translate_tts")
;;   (setq google-translate--tkk-url
;;         "http://translate.google.cn/")
;;   (setq google-translate-default-source-language "en")
;;   (setq google-translate-default-target-language "zh-CN"))

;; go-translate
(use-package go-translate
  :config
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-buffer-follow-p t)
  (setq go-translate-inputs-function #'go-translate-inputs-current-or-prompt))

;; themes
(use-package zenburn-theme)
(use-package solarized-theme)

;; format-all
(use-package format-all)

;; shfmt
(use-package shfmt
  :config
  (add-hook 'sh-mode-hook 'shfmt-on-save-mode))

;; goto-chg: go to last change
(use-package goto-chg)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; move-text
(use-package move-text
  :config
  (global-set-key (kbd "ESC <up>") 'move-text-up)
  (global-set-key (kbd "ESC <down>") 'move-text-down))

;; figlet: convert string to picture
(use-package figlet)

;; origami
(use-package origami
  :defer 1
  :config
  ;; (global-set-key (kbd "M-r") 'hydra-origami/body)
  (global-origami-mode t)
  (defhydra hydra-origami (:color red)
  "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
  ("o" origami-open-node)
  ("c" origami-close-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("f" origami-forward-toggle-node)
  ("a" origami-toggle-all-nodes)))

;; whole-line-or-region: M-w to copy a line if not select
(use-package whole-line-or-region
  :defer 1
  :config
  (whole-line-or-region-global-mode t))

;; indent-guide
(use-package indent-guide)

;; symbol-overlay
(use-package symbol-overlay
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  ;;(global-set-key (kbd "C-i") 'symbol-overlay-remove-all)
  (global-set-key (kbd "ESC <right>") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "ESC <left>") 'symbol-overlay-switch-backward))

;; anzu
(use-package anzu
  :defer 1
  :config
  (global-anzu-mode t)
  ;;(setq anzu-mode-lighter "")
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

;; hydra
(use-package hydra)

;; == awesome-tab-mode
(use-package awesome-tab-mode
  :straight
  (awesome-tab-mode :type git
                    :host github
                    :repo "manateelazycat/awesome-tab")
  :functions (awesome-tab-mode)
  :defines (awesome-tab-terminal-dark-select-background-color awesome-tab-terminal-dark-select-foreground-color)
  :config
  (setq awesome-tab-height 140)
  (setq frame-background-mode 'dark)
  ;; (setq awesome-tab-terminal-dark-select-background-color "#e5e5e5")
  (setq awesome-tab-terminal-dark-select-foreground-color "#FFFFFF")
  ;; (setq awesome-tab-terminal-dark-select-foreground-color "#F0DFAF")
  ;; (setq awesome-tab-terminal-dark-unselect-background-color "")
  ;; (setq awesome-tab-terminal-dark-unselect-foreground-color
  ;; "e5e5e5")
  (global-set-key (kbd "M-h") 'awesome-tab-backward-tab)
  ;; (global-set-key (kbd "M-j") 'awesome-tab-forward-group)
  ;; (global-set-key (kbd "M-k") 'awesome-tab-backward-group)
  (global-set-key (kbd "M-l") 'awesome-tab-forward-tab)

  (defhydra awesome-fast-switch (global-map "M-j")
      ("h" awesome-tab-backward-tab)
      ("j" awesome-tab-forward-group)
      ("M-j" awesome-tab-forward-group)
      ("k" awesome-tab-backward-group)
      ("l" awesome-tab-forward-tab)
      ("C-a" awesome-tab-select-beg-tab)
      ("C-e" awesome-tab-select-end-tab)
      ("C-j" awesome-tab-ace-jump)
      ("C-h" awesome-tab-move-current-tab-to-left)
      ("C-l" awesome-tab-move-current-tab-to-right)
      ("b" ivy-switch-buffer)
      ("g" awesome-tab-counsel-switch-group)
      ("C-k" kill-current-buffer)
      ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
      ("q" nil "quit")
      ("RET" nil "quit"))
  (awesome-tab-mode t))

;; ace-window: C-u M-o to change window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "M-o") 'ace-window))

;; helpful
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; imenu-list
(use-package imenu-list
  :defer 2
  :config
  ;; (setq imenu-list-size 0.2)
  (global-set-key (kbd "C-^") #'imenu-list-smart-toggle))

;; beacon: highlight curson when jump
(use-package beacon
  :defer 1
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (beacon-mode 1))

;; rainbow: show colors with color value; usefull in face config
(use-package rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; dimmer: highlight current buffer
(use-package dimmer)

;; model-line-bell
(use-package mode-line-bell)

;; browse-kill-ring
(use-package browse-kill-ring)

;; simple-modeline
;; (use-package simple-modeline
  ;; :hook (after-init . simple-modeline-mode))

;; mood-line-mode
(use-package mood-line
  :config
  (mood-line-mode))

;; smex: used by counsel-M-x to show recent command
(use-package smex)

;; smart-line-mode
;; (use-package smart-mode-line)
;; (use-package sml-modeline)

;; (use-package goto-line-preview
;;   :defines (goto-line-preview)
;;   :config
;;   (global-set-key [remap goto-line] 'goto-line-preview))

;; ox_reveal
;; (use-package ox-reveal)

;; dired-recent C-x C-d
(use-package dired-recent
  :defer 2
  :config
  (dired-recent-mode  1))

;; keypression: only for GUI
(use-package keypression)

;; perspective
;; (use-package perspective
;;   :config
;;   (persp-mode))

;; move-dum
(use-package move-dup)

;; crus: useful command sets
(use-package crux
  :config
  (global-set-key (kbd "C-M-j") 'crux-smart-open-line)
  (global-set-key (kbd "M-RET") 'crux-smart-open-line)
  (global-set-key (kbd "C-o") 'crux-smart-open-line-above)
  (global-set-key (kbd "<f5>") 'crux-find-user-init-file))

;; ace-jump-zap
(use-package ace-jump-zap)

;; zop-to-char
(use-package zop-to-char)

;; avy-zap
(use-package avy-zap
  :bind
  ("M-z" . avy-zap-to-char-dwim)
  ("C-M-z" . avy-zap-to-char-dwim))

;; string-inflection: upper,  capitalize word
(use-package string-inflection
  :bind
  ("M-u" . string-inflection-python-style-cycle))

;; quickrun
(use-package quickrun)

;; wgrep
;; (use-package wgrep)

;; diredfl
;; (use-package diredfl)

;; dired-sidebar
;; (use-package dired-sidebar)

;; ibuffer-sidebar
;; (use-package ibuffer-sidebar)

;; smooth-scrolling: cursor always in the middle of screen
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; yascroll: show scroll bar even in terminal
(use-package yascroll
  :defer 1
  :config
  (global-yascroll-bar-mode 1))

;; command-log-mode
(use-package command-log-mode
  :defer 2
  :config
  (command-log-mode 1)
  (global-command-log-mode 1))

;; keycast: show keypress at modeline
(use-package keycast
  :config
  ;; keycast doesn't work well with doom-modeline
  ;; thanks https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (with-eval-after-load 'keycast
    (define-minor-mode keycast-mode
      "Show current command and its key binding in the mode line."
      :global t
      (if keycast-mode
          (add-hook 'pre-command-hook 'keycast--update t)
        (remove-hook 'pre-command-hook 'keycast--update)))

    (add-to-list 'global-mode-string '("" mode-line-keycast)))
  (keycast-mode 1))

;; org-download
(use-package org-download)

;; grip-mode: pdf view;  note: cp /usr/bin/grip to /usr/bin/grip.exe on windows10
(use-package grip-mode)
;;  :config
;;  (setq grip-binary-path "grip")
;;  :hook ((markdown-mode org-mode) . grip-mode))

;; calfw
(use-package calfw)
(use-package calfw-org)

;; org-pomodoro
(use-package org-pomodoro)

;; cal-china-x
(use-package cal-china-x)

;; cnfonts
;; (use-package cnfonts
  ;; :config
  ;; (cnfonts-enable))

;; org-mode
;; (use-package org
;;   :config
;;   :bind (:map org-mode-map
;;               (("M-h" . awesome-tab-backward-tab)
;;               ("C-M-j" . org-meta-return)
;;               ("M-RET" . org-meta-return))))

;; pdf-tools note: use pdf-tools-install command to install
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))

;; undo-tree
(use-package undo-tree
  :defer 2
  :config
  (global-set-key (kbd "C-M-_") 'undo-tree-redo)
  (global-undo-tree-mode))

;; valign
(use-package valign
  :straight
  (valign :type git
          :host github
          :repo "casouri/valign")
  :functions (valign-mode))

;; leetcode
(use-package leetcode
    :config
    (setq leetcode-directory "~/Dropbox/Leetcode/"
	  leetcode-save-solutions t
          leetcode-prefer-language "cpp"))

;; aggressive-indent
(use-package aggressive-indent)
;; :config
;; (global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; hl-todo NEXT TODO DONE NOTE OKAY HACK
(use-package hl-todo
  :defer 1
  :config
  (global-hl-todo-mode 1))

;; page-break-lines c-q c-l
(use-package page-break-lines
  :init
  ;; (setq page-break-lines-char ?=)
  (global-page-break-lines-mode 1))

;; eyebrowse: manage window config switch
(use-package eyebrowse
  :defer 1
  :config
  (setq eyebrowse-mode-line-style (quote current))
  (eyebrowse-mode 1))

;; persp-mode
(use-package persp-mode)

;; marginalia: minibuffer command complete
(use-package marginalia
  :init
  (marginalia-mode 1))

;; beginend: move to begin or end content
(use-package beginend
  :config
  (beginend-global-mode))

;; fancy-narrow
(use-package fancy-narrow
  :defer 1
  :config
  (fancy-narrow-mode 1))



;; END
(no-load-path-done)
