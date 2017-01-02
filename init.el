(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'use-package)
(require 'package)
(add-to-list 'package-directory-list "/run/current-system/sw/share/emacs/site-lisp/elpa")
(package-initialize)


(require 'linum)
(require 'linum-relative)
(global-linum-mode 1)

(require 'key-chord)
(key-chord-mode 1)
(use-package evil
  :config
  (progn
    (evil-mode t)
    
    ;; eVIl leader options and mappings
    (define-key evil-normal-state-map (kbd ",ci") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map (kbd ",cc") 'evilnc-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map (kbd ",b") 'switch-to-buffer)

    ;; Window motions
    (define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
    (define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
    (define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
    (define-key evil-normal-state-map (kbd "M-l") 'windmove-right)

    ;; Fix window creation
    (define-key evil-normal-state-map (kbd "^W n") 'split-window-below)
    
    ;; PageUp/PageDown/Home/End likes
    (define-key evil-motion-state-map (kbd "C-h") 'evil-first-non-blank)
    (define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-page-down)
    (define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-page-up)
    (define-key evil-motion-state-map (kbd "C-l") 'evil-end-of-line)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map ",c" 'comment-or-uncomment-region)
(define-key evil-visual-state-map ",c" 'comment-or-uncomment-region)
(define-key evil-normal-state-map "\C-j" 'evil-next-buffer)
(define-key evil-visual-state-map "\C-j" 'evil-next-buffer)
(define-key evil-insert-state-map "\C-j" 'evil-next-buffer)
(define-key evil-normal-state-map "\C-k" 'evil-prev-buffer)
(define-key evil-visual-state-map "\C-k" 'evil-prev-buffer)
(define-key evil-insert-state-map "\C-k" 'evil-prev-buffer)

(define-key evil-normal-state-map "\M-h" 'shrink-window-horizontally)
(define-key evil-normal-state-map "\M-j" 'shrink-window)
(define-key evil-normal-state-map "\M-k" 'enlarge-window)
(define-key evil-normal-state-map "\M-l" 'enlarge-window-horizontally)

(define-key evil-visual-state-map "\M-h" 'shrink-window-horizontally)
(define-key evil-visual-state-map "\M-j" 'shrink-window)
(define-key evil-visual-state-map "\M-k" 'enlarge-window)
(define-key evil-visual-state-map "\M-l" 'enlarge-window-horizontally)
    )
  )

(use-package key-chord
  :config
  (progn
    ;; Exit insert mode by pressing j and then k quickly
    (setq key-chord-two-keys-delay 0.5)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-mode 1)
    )
  )

(use-package company
  :config (global-company-mode)
  )

(use-package projectile
  )

(use-package company-quickhelp
  :config (company-quickhelp-mode 1)
  )

(use-package ace-jump-mode
  :config (progn
            (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
            )
  )

(use-package flycheck
  :config (progn
            (add-hook 'after-init-hook 'global-flycheck-mode)
            (setq flycheck-command-wrapper-function
                  (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
                  flycheck-executable-find
                  (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
            )
  )

(use-package flycheck-pos-tip
  :config (flycheck-pos-tip-mode)
  )

(use-package flycheck-haskell
  :config (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  )

(use-package magit
  :config (progn
            (global-set-key (kbd "C-x g") 'magit-status)
            (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
            )
  )

(use-package undo-tree
  :config (global-undo-tree-mode)
  )

(use-package linum-relative
  :config
  (progn
    (global-linum-mode t)
    ;; Auto change between relative and absolute styles
    (add-hook 'post-command-hook
              (lambda ()
                (cond ((evil-insert-state-p)
                       (progn
                         (setq linum-format 'dynamic)
                         (linum-schedule)
                         ))
                      ((evil-normal-state-p)
                       (progn
                         (setq linum-format 'linum-relative)
                         (linum-schedule)
                         ))
                      )))
    )
  )

(use-package sql-indent
  :defer t
  )

(use-package sql
  :defer t
  :config
  (progn
    (sql-set-product 'postgres)
    (add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
    (require 'sql-indent)
    )
  )

;;; newline-and-indent on RET
(add-hook 'lisp-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (require 'rsense nil t)
  )

(setq ruby-insert-encoding-magic-comment nil)

(use-package slim-mode
  :mode "\\.slim\\'"
  )

(require 'window-numbering)
(window-numbering-mode 1)

(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key [M-f3] 'nav-toggle)

(setq-default indent-tabs-mode nil)

(setq evil-default-cursor t)
