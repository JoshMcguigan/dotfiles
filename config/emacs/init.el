;;; Bootstrap

;; Bootstrap straight package manager.
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

;; Install use-package.
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;;; Setup packages

(use-package emacs
  :config
  (add-hook 'emacs-startup-hook (lambda () (set-frame-parameter nil 'fullscreen 'fullboth)))
  (setq
   display-line-numbers-type 'relative
   mouse-drag-copy-region t
   tab-always-indent 'complete
   native-comp-async-report-warnings-errors 'silent
   scroll-conservatively most-positive-fixnum
   confirm-kill-emacs 'y-or-n-p
   ring-bell-function 'ignore)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (savehist-mode)
  (add-hook 'prog-mode-hook (lambda() (display-line-numbers-mode 1)))
  (setq-default
   c-default-style "linux"
   c-basic-offset 4)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Treat underscores as part of words, better matching vim behavior.
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")))
  ;; Setup shortcuts to manage the tab bar.
  (bind-key* (kbd "M-<return>") #'tab-new)
  (bind-key* (kbd "M-l") #'tab-next)
  (bind-key* (kbd "M-h") #'tab-previous)
  (bind-key* (kbd "M-L") #'tab-bar-move-tab)
  (bind-key* (kbd "M-H") #'tab-bar-move-tab-backward))

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"))

(use-package org
  :straight (:type built-in)
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (setq org-confirm-babel-evaluate nil))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '(?◉ ?○))
  (org-superstar-cycle-headline-bullets nil))

(use-package desktop
  :config
  (desktop-save-mode t)
  :init
  (add-hook 'after-init-hook 'desktop-read))

(use-package restart-emacs)

(use-package evil
  :init
  (setq
   evil-undo-system 'undo-redo
   evil-want-C-u-scroll t
   evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (defun me/help-at-point ()
    (interactive)
    (cond
      ((eglot-managed-p) (select-window (eldoc-doc-buffer)))
      ((eq major-mode 'emacs-lisp-mode) (helpful-at-point))
      ;; Fallback to the evil mode default
      (t (woman))))
  (setq evil-lookup-func #'me/help-at-point)

  ;; Configure n/N to scroll through more than just search results.
  ((lambda ()
      (symbol-name 'me/navigate-active-search)
      (symbol-name 'me/navigate-active-hunks)
      (symbol-name 'me/navigate-active-errors)
      ;; Default to assuming a search is active, as that should
      ;; be safe at startup.
      (setq-default me/navigate-active 'me/navigate-active-search)

      ;; We track the direction the user is navigating (for example
      ;; did they use next-hunk or previous-hunk) and make n/N behave
      ;; appropriately.
      ;;
      ;; For search mode, this variable value is unused because
      ;; evil tracks this for us.
      (symbol-name 'me/navigate-forward)
      (symbol-name 'me/navigate-reverse)
      (setq-default me/navigate-direction 'me/navigate-forward)

      (defun me/trace-search (&rest args)
        (setq me/navigate-active 'me/navigate-active-search)
        ;; Evil tracks this for us.
        (setq me/navigate-direction nil)
        )
      (advice-add 'evil-search-forward :before #'me/trace-search)
      (advice-add 'evil-search-backward :before #'me/trace-search)

      (defun me/traced-vcs-next-hunk ()
        "Wrapper function around git-gutter:next-hunk to track
that we are navigating through hunks in a particular direction. Any
interactive user should use this function, but the underlying nagivate-*
functions should not call this wrapper or else they can/will invert
me/navigation-direciton."
        (interactive)
        (setq me/navigate-active 'me/navigate-active-hunks)
        (setq me/navigate-direction 'me/navigate-forward)
        (git-gutter:next-hunk 1))

      (defun me/traced-vcs-previous-hunk ()
        "Wrapper function around git-gutter:previous-hunk to track
that we are navigating through hunks in a particular direction. Any
interactive user should use this function, but the underlying nagivate-*
functions should not call this wrapper or else they can/will invert
me/navigation-direciton."
        (interactive)
        (setq me/navigate-active 'me/navigate-active-hunks)
        (setq me/navigate-direction 'me/navigate-reverse)
        (git-gutter:previous-hunk 1))

      (defun me/traced-next-error ()
        "Wrapper function around flymake-goto-next-error to track
that we are navigating through errors in a particular direction. Any
interactive user should use this function, but the underlying nagivate-*
functions should not call this wrapper or else they can/will invert
me/navigation-direciton."
        (interactive)
        (setq me/navigate-active 'me/navigate-active-errors)
        (setq me/navigate-direction 'me/navigate-forward)
        (call-interactively #'flymake-goto-next-error))

      (defun me/traced-previous-error ()
        "Wrapper function around flymake-goto-prev-error to track
that we are navigating through errors in a particular direction. Any
interactive user should use this function, but the underlying nagivate-*
functions should not call this wrapper or else they can/will invert
me/navigation-direciton."
        (interactive)
        (setq me/navigate-active 'me/navigate-active-errors)
        (setq me/navigate-direction 'me/navigate-reverse)
        (call-interactively #'flymake-goto-prev-error))

      (define-key evil-normal-state-map (kbd "n")
        (defun me/navigate-next ()
          (interactive)
          (cl-case me/navigate-active
            ('me/navigate-active-search (evil-search-next))
            ('me/navigate-active-hunks
             (if (eq me/navigate-direction 'me/navigate-forward)
                 (git-gutter:next-hunk 1)
               (git-gutter:previous-hunk 1))
             )
            ('me/navigate-active-errors
             (if (eq me/navigate-direction 'me/navigate-forward)
                 (call-interactively #'flymake-goto-next-error)
               (call-interactively #'flymake-goto-prev-error))
             )
            (t nil))))
      (apply #'evil-set-command-properties #'me/navigate-next
             (evil-get-command-properties #'evil-search-next))

      (define-key evil-normal-state-map (kbd "N")
        (defun me/navigate-previous ()
          (interactive)
          (cl-case me/navigate-active
            ('me/navigate-active-search (evil-search-previous))
            ('me/navigate-active-hunks
             (if (eq me/navigate-direction 'me/navigate-forward)
                 (git-gutter:previous-hunk 1)
               (git-gutter:next-hunk 1))
             )
            ('me/navigate-active-errors
             (if (eq me/navigate-direction 'me/navigate-forward)
                 (call-interactively #'flymake-goto-prev-error)
               (call-interactively #'flymake-goto-next-error))
             )
            (t nil))))
      (apply #'evil-set-command-properties #'me/navigate-previous
             (evil-get-command-properties #'evil-search-previous))
  )))

(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init)
  (evil-collection-define-key
    'normal
    'dired-mode-map
    "h" #'dired-up-directory
    "l" #'dired-find-file)
  ;; Map Ctrl-Shift-V to paste while in insert mode. Out of insert mode you can
  ;; paste with the standard vim `p`.
  (define-key evil-insert-state-map (kbd "C-S-V") #'yank)
  (evil-collection-define-key '(insert) 'vterm-mode-map (kbd "C-S-V") #'yank))

(use-package vertico
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous))
  :init
  (vertico-mode))

(use-package projectile
  :init
  (setq projectile-project-search-path '(("~/workspace" . 3)))
  :config
  (projectile-mode))

(use-package consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :custom
  (corfu-auto t))

(use-package helpful)

(use-package rust-mode)

;; This is a built-in package, but eglot requires a newer version.
(use-package flymake)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs `(c-mode . ("ccls")))
  (add-to-list 'eglot-server-programs `(rust-mode . ("rust-analyzer")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  ;; eglot puts docs into eldoc, which can cause it to jump
  ;; around if eldoc is allowed to use multiple lines.
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; Add automatic format on save for specific programming languages.
  (add-hook 'eglot-managed-mode-hook
    (lambda ()
    (when (member major-mode '(c-mode rust-mode))
      (add-hook 'before-save-hook 'eglot-format nil t)))))

(use-package git-link)

(use-package vterm)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode 1)
  (custom-set-variables
   ;; Disable confirmation when reverting hunk.
   '(git-gutter:ask-p nil)
   '(git-gutter:modified-sign "|")
   '(git-gutter:added-sign "|")
   '(git-gutter:deleted-sign "|")))

(use-package magit
  :defer t)

(use-package bind-map
  :config
  (setq me/default-map (make-sparse-keymap))

  (defun me/set-leader-keys (key def &rest bindings)
    (while key
      (define-key me/default-map (kbd key) def)
      (setq key (pop bindings) def (pop bindings))))

  (defun me/declare-prefix (prefix name &rest more)
    (declare (indent defun))
    (apply #'which-key-add-keymap-based-replacements me/default-map
          prefix name more))

  (me/declare-prefix
    "f" "find"
    "n" "navigate"
    "R" "run (setup)"
    "s" "split"
    "v" "vcs"
    "y" "copy"
    )

  (me/set-leader-keys
   "SPC" '("commands" . execute-extended-command)
   "\'" '("shell" . vterm)

   "b" '("buffers" . (lambda () (interactive) (buffer-menu 1)))
   "B" '("buffers (all)" . buffer-menu)

   "e" '("next error" . me/traced-next-error)
   "E" '("prev error" . me/traced-previous-error)

   ;; TODO add fF and fP which search in project directory but without
   ;;      checking gitignore
   "ff" '("file" . projectile-find-file)
   "fi" '("in current file" . consult-line)
   "fp" '("in current project" . consult-ripgrep)

   "h" '("next hunk" . me/traced-vcs-next-hunk)
   "H" '("prev hunk" . me/traced-vcs-previous-hunk)

   ;; Save l for local mappings (per project or on a local machine)

   ;; Save m for major mode

   "nc" '("emacs config" . spacemacs/find-dotfile)
   "nC" '("local config" . me/find-local-config)
   "nm" '("messages" . me/switch-to-messages-buffer)
   "np" '("project" . projectile-switch-project)
   "ns" '("scratch" . me/switch-to-scratch-buffer)

   "o" '("open file" . find-file)
   ;; TODO map O to a nice open remote file helper

   "q" '("close window" . evil-quit)

   "r" '("run" . projectile-repeat-last-command)

   "Rc" '("compile" . projectile-compile-project)
   "RC" '("configure" . projectile-configure-project)
   "Ri" '("install" . projectile-install-project)
   "Rp" '("package" . projectile-package-project)
   "Rr" '("run" . projectile-run-project)
   "Rt" '("test" . projectile-test-project)

   "sh" '("split left" . split-window-right)
   "sj" '("split down" . me/split-window-below-and-focus)
   "sk" '("split up" . split-window-below)
   "sl" '("split right" . me/split-window-right-and-focus)

   "t" '("file tree (project)" . me/open-dired)
   "T" '("file tree (current dir)" . dired-jump)

   "vr" '("revert hunk" . git-gutter:revert-hunk)
   "vs" '("status" . magit-status)
   "vv" '("view hunk" . git-gutter:popup-hunk)

   "w" '("save file" . save-buffer)

   "yg" '("git forge link" . git-link))

  (bind-map me/default-map
    :prefix-cmd me-cmds
    :evil-keys ("SPC")
    :override-minor-modes t
    :override-mode-name me-leader-override-mode))

(defun me/switch-to-messages-buffer ()
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (switch-to-buffer (current-buffer))))

(defun me/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun me/split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun me/split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun me/open-dired ()
  "If in a project, open dired at the project root. Otherwise open
dired in the directory of the current buffer."
  (interactive)
  (if (projectile-project-p)
      (projectile-dired)
      (dired-jump)))

(setq me/local-config-file "~/.emacs-local-config.el")

(defun me/find-local-config ()
  "Edit the local emacs configuration, in the current window."
  (interactive)
  (find-file me/local-config-file))
