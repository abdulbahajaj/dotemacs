;; Setting up Package manager

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Installing and configuring packages
(use-package no-littering)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; EVIL
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (set-face-attribute 'evil-ex-lazy-highlight nil :background "#006501")
  (defalias 'e 'evil-edit))

(use-package dash)

(use-package general
  :config
  (general-evil-setup))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :commands 'evilnc-comment-operator
  :config
  (evilnc-default-hotkeys nil t))

(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-snipe
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode 1))

(use-package magit
  :config
  (transient-define-prefix magit-file-dispatch ()
    "Invoke a Magit command that acts on the visited file.
When invoked outside a file-visiting buffer, then fall back
to `magit-dispatch'."
    :info-manual "(magit) Minor Mode for Buffers Visiting Files"
    ["Actions"
     [("s" "Stage"      magit-stage-file)
      ("u" "Unstage"    magit-unstage-file)
      ("c" "Commit"     magit-commit)
      ("e" "Edit line"  magit-edit-line-commit)]
     [("D" "Diff..."    magit-diff)
      ("d" "Diff"       magit-diff-buffer-file)
      ("g" "Status"     magit-status-here)]
     [("L" "Log..."     magit-log)
      ("l" "Log"        magit-log-buffer-file)
      ("t" "Trace"      magit-log-trace-definition)]
     [("B" "Blame..."   magit-blame)
      ("b" "Blame"      magit-blame-addition)
      ("r" "...removal" magit-blame-removal)
      ("f" "...reverse" magit-blame-reverse)
      ("m" "Blame echo" magit-blame-echo)
      ("q" "Quit blame" magit-blame-quit)]
     [("p" "Prev blob"  magit-blob-previous)
      ("n" "Next blob"  magit-blob-next)
      ("v" "Goto blob"  magit-find-file)
      ("V" "Goto file"  magit-blob-visit-file)]
     [(5 "C-c r" "Rename file"   magit-file-rename)
      (5 "C-c d" "Delete file"   magit-file-delete)
      (5 "C-c u" "Untrack file"  magit-file-untrack)
      (5 "C-c c" "Checkout file" magit-file-checkout)]]
    (interactive)
    (transient-setup
     'magit-file-dispatch))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
	magit-diff-refine-hunk t))


(use-package restart-emacs)

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(;; For things that need to be done, just not today.
	  ("TODO" warning bold)
	  ;; For problems that will become bigger problems later if not
	  ;; fixed ASAP.
	  ("FIXME" error bold)
	  ;; For tidbits that are unconventional and not intended uses of the
	  ;; constituent parts, and may break in a future update.
	  ("HACK" font-lock-constant-face bold)
	  ;; For things that were done hastily and/or hasn't been thoroughly
	  ;; tested. It may not even be necessary!
	  ("REVIEW" font-lock-keyword-face bold)
	  ;; For especially important gotchas with a given implementation,
	  ;; directed at another user other than the author.
	  ("NOTE" success bold)
	  ;; For things that just gotta go and will soon be gone.
	  ("DEPRECATED" font-lock-doc-face bold))))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package vterm
  :ensure t)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package company
  :hook ((text-mode prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0.2))

(use-package highlight-parentheses
  :config
  (setq hl-paren-colors (cons "#d98d8d" hl-paren-colors))
  :hook (prog-mode . highlight-parentheses-mode))

(use-package perspective
  :hook (emacs-startup . persp-mode)
  :config
  (setq persp-initial-frame-name "1"))

(use-package docker
  :ensure t)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.08))

(use-package replel
  :straight
  (replel
   :type git
   :host github
   :repo "abdulbahajaj/repl.el"))

(use-package treemacs
  :config
  (treemacs-resize-icons 16))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package projectile
  :hook (emacs-startup . projectile-mode))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package counsel
  :hook (emacs-startup . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
	ivy-height 17
	ivy-wrap t
	ivy-magic-slash-non-match-action nil
	ivy-fixed-height-minibuffer t
	projectile-completion-system 'ivy
	ivy-use-virtual-buffers nil
	ivy-virtual-abbreviate 'full
	ivy-on-del-error-function #'ignore
	ivy-use-selectable-prompt t
	ivy-count-format "%d/%d")
  (setf (alist-get 't ivy-format-functions-alist)
	#'ivy-format-function-line)
  (add-to-list 'ivy-sort-functions-alist
             '(counsel-recentf . file-newer-than-file-p)))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode))

(use-package ob-async)

(use-package  highlight-thing
  :config
  (setq highlight-thing-delay-seconds 0)
  (global-highlight-thing-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package org-superstar
  :hook ((org-mode . (lambda () (org-superstar-mode 1)))))

(use-package dired-git-info
  ;; :hook ((dired-after-readin . dired-git-info-auto-enable))
  :config
  (setq dgi-auto-hide-details-p nil)
  (define-key dired-mode-map [backtab] 'dired-git-info-mode))

(use-package evil-lion
  :hook (emacs-startup . evil-lion-mode)
  :ensure t)

(use-package lispy
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode))


(use-package counsel-projectile)

(use-package move-text
  :bind (("M-j" . move-text-down)
	 ("M-k" . move-text-up)))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package cider)
(use-package cider-hydra
  :hook (clojure-mode . cider-hydra-mode))

(use-package volatile-highlights
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
			'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme)
  (evil-org-agenda-set-keys))

(use-package org-cliplink)

;; IDE 

(use-package eglot
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package dap-mode
  :config
  (dap-mode)
  (dap-ui-mode)
  (require 'dap-python)
  (setq-default dap-python-debugger 'debugpy)
  (setq-default dap-python-executable "python3")
  (require 'dap-lldb))

(use-package pyvenv)

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq-default gcmh-idle-delay 3))

;; langauge
(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package highlight-indent-guides
  :hook ((org-mode prog-mode) . highlight-indent-guides-mode)
  :init
  (require 'highlight-indent-guides)
  (setq highlight-indent-guides-method 'character 
	highlight-indent-guides-responsive t))

(use-package browse-at-remote)

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1)
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line))

(use-package terraform-mode)

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)

  (set-face-foreground 'git-gutter-fr:modified "#3b4c7d")
  (set-face-foreground 'git-gutter-fr:added    "#3c6e29")
  (set-face-foreground 'git-gutter-fr:deleted  "#941938")
  (global-git-gutter-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package doom-snippets
  :after yasnippet
  :straight
  (doom-snippets
   :type git :host github :repo "hlissner/doom-snippets")
  :init
  (setq doom-snippets-dir (concat (straight--repos-dir) "doom-snippets"))
  (doom-snippets-initialize))

(use-package pyimport)
(use-package pip-requirements)

(use-package py-isort)

(use-package lsp-mode
  :hook ((python-mode ruby-mode) . lsp)
  :config
  (setq-default lsp-headerline-breadcrumb-enable nil))

(use-package lsp-pyright
  :config
  (require 'lsp-pyright))

(use-package go-mode)

(use-package elfeed)

(use-package magit-todos
  :hook (emacs-startup . magit-todos-mode))

(use-package all-the-icons)

(use-package doom-themes
  :ensure t
  :config
  (add-to-list 'load-path
	       (concat (straight--repos-dir)
		        "emacs-doom-themes/extensions/"))
  (require 'doom-themes-ext-org)
  (require 'doom-themes-ext-treemacs)
  
  (setq doom-themes-enable-bold nil)
  (load-theme 'doom-vibrant t)
  (setq doom-themes-treemacs-theme "doom-colors") 
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :config
  (display-time)
  (display-battery-mode)
  (doom-modeline-def-modeline 'rubicon/modeline
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery grip gnus github debug repl lsp input-method indent-info buffer-encoding major-mode process vcs))

  (doom-modeline-mode 1)
  (defun rubicon/setup-modeline ()
    (doom-modeline-set-modeline 'rubicon/modeline 'default))
  (setq mode-line-misc-info (list "" display-time-string))
  (rubicon/setup-modeline)
  (add-hook 'doom-modeline-mode-hook 'rubicon/setup-modeline))

(use-package better-jumper
  :hook (emacs-startup . better-jumper-mode))

(provide 'packages)
;;; packages.el ends here
