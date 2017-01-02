(require 'package)
(setq package-archives '(;("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq inferior-lisp-program "/usr/bin/sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (load (expand-file-name "~/.emacs.d/gas-mode.el"))
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")

(setq tab-stop-list (number-sequence 4 120 4))

(tool-bar-mode 0)
(scroll-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq inhibit-splash-screen t)
;; (setq erlang-root-dir "/usr/lib/erlang")
;; (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;; (require 'erlang-start)

(setq x-select-enable-clipboard t)

(let ((base "~/.emacs.d/site-lisp"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(let ((base "~/.emacs.d/elpa"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; (require 'slime)
;; (slime-setup '(slime-fancy))

(require 'undo-tree)
(global-undo-tree-mode 1)
(require 'evil)
(evil-mode 1)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(setq auto-mode-alist (cons '("\\.ml[iylp]?\\'" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; (setq inferior-scheme-program "/usr/bin/racket")
;; (require 'geiser-install)
;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(define-key evil-insert-state-map "\M-j" 'ac-next)
(define-key evil-insert-state-map "\M-k" 'ac-previous)
(require 'auto-complete-clang)

(append '(ac-source-clang) ac-sources)

(setq ac-override-local-map nil)
(setq ac-fuzzy-enable t)
(setq ac-ignore-case t)
(setq ac-use-fuzzy t)
(setq ac-use-comphist t)
(setq ac-use-quick-help t)

(defun my-ac-config ()
 (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers
                            ac-source-files-in-current-dir ac-source-filename ac-source-semantic ac-source-semantic-raw
                            ac-source-yasnippet))
 (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
 (add-hook 'ruby-mode-hook '(lambda ()
                              (abbrev-mode 1)
                              (electric-pair-mode t)
                              (electric-indent-mode t)
                              (electric-layout-mode t)
                              ac-ruby-mode-setup))
 (add-hook 'css-mode-hook 'ac-css-mode-setup)
 (add-hook 'auto-complete-mode-hook 'ac-common-setup)
 (global-auto-complete-mode t))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.2/../../../../include/c++/4.7.2
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.2/../../../../include/c++/4.7.2/x86_64-unknown-linux-gnu
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.2/../../../../include/c++/4.7.2/backward
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.2/include
 /usr/local/include
 /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.2/include-fixed
 /usr/include
"
               )))
(my-ac-config)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-command-BibTeX "Biber")
 '(TeX-show-compilation nil)
 '(agda2-include-dirs (quote ("." "/home/aske/agda/lib/src")))
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(ansi-term-color-vector [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"])
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes (quote ("8c5ffc9848db0f9ad4e296fa3cba7f6ea3b0e4e00e8981a59592c99d21f99471" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "79243bbd9c07f2baf551c2038009afc866da65fb8073a2efce3a469efc0c1bc5" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "211bb9b24001d066a646809727efb9c9a2665c270c753aa125bace5e899cb523" "be7eadb2971d1057396c20e2eebaa08ec4bfd1efe9382c12917c6fe24352b7c1" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "967c58175840fcea30b56f2a5a326b232d4939393bed59339d21e46cf4798ecf" "93815fc47d9324a7761b56754bc46cd8b8544a60fca513e634dfa16b8c761400" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "fca8ce385e5424064320d2790297f735ecfde494674193b061b9ac371526d059" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "6615e5aefae7d222a0c252c81aac52c4efb2218d35dfbb93c023c4b94d3fa0db" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#073642")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(haskell-font-lock-symbols t)
 '(haskell-interactive-mode-eval-pretty t)
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-font-lock turn-on-haskell-doc-mode haskell-hook)))
 '(haskell-notify-p t)
 '(haskell-process-type (quote ghci))
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(org-format-latex-options (quote (:foreground default :background default :scale 1.6 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-format-latex-signal-error t)
 '(org-highlight-latex-fragments-and-specials t)
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(preview-default-document-pt 12)
 '(preview-scale-function 1.5)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(undo-tree-auto-save-history t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))

;;(load-library "haskell-site-file")
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.scene\\'" . haskell-mode))
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(autoload 'ghc-init "ghc" nil t)

;; (add-hook 'haskell-mode-hook 'haskell-hook)
;; (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
(add-to-list 'completion-ignored-extensions ".~")
(add-to-list 'completion-ignored-extensions ".hi")
(require 'haskell-ac)
;; Haskell main editing mode key bindings.
 (defun haskell-hook ()
  (add-to-list 'ac-sources 'my/ac-source-haskell)
  
;; (ghc-init) (flymake-mode)
;;   (turn-on-haskell-simple-indent)
;;   (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
;;   (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

;;   ;; Load the current file (and make a session if not already made).
   (turn-on-haskell-doc-mode)
;;   (turn-on-haskell-indent)
   (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
   (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;;Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;; 1. Complete imports, letting you choose the module name.
  ;; 2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-,") 'haskell-mode-tag-find)

  ;; Save the current buffer and generate etags (a TAGS file) for the
  ;; whole project.
  (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1))))

;; Useful to have these keybindings for .cabal files, too.
;; (defun haskell-cabal-hook ()
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;   (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;   (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))


;; (require 'ess-site)

;(require 'csharp-mode)
;; (setq auto-mode-alist
;;    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

;;(setq custom-theme-load-path (cons "~/.emacs.d/site-lisp/solarized/" nil))

;; (if (and (string= system-name "steamroller") window-system)
;;   (progn (setq custom-theme-load-path (cons "/home/aske/.emacs.d/site-lisp/solarized" nil))
;;          (load-theme 'solarized-light))
 ;; (progn (setq custom-theme-load-path (cons "/home/aske/.emacs.d/elpa/molokai-theme-20120503.1929" nil))
 ;;          (load-theme 'molokai))

;; (setq custom-theme-load-path (cons "~/.emacs.d/site-lisp/tomorrow" nil))
;; (load-theme 'tomorrow) 
;; (setq custom-theme-load-path (cons "~/.emacs.d/elpa/twilight-anti-bright-theme-20120713.1116" nil))
;; (load-theme 'twilight-anti-bright)

(require 'linum)
(require 'linum-relative)
(global-linum-mode 1)

(require 'window-numbering)
(window-numbering-mode 1)

(load "~/.emacs.d/site-lisp/keychord.el")
(key-chord-mode 1)
(require 'key-chord)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;; Evil
;; (define-key evil-insert-state-map "j" #'cofi/maybe-exit)
;;(define-key evil-insert-state-map "о" #'(lambda () (interactive) (cofi/maybe-exit t)))

;; (evil-define-command cofi/maybe-exit
;;   :repeat change
;;   (interactive)
;;   (let ((modified (buffer-modified-p)))
;;     (insert "j")
;;     (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
;;                                    nil 0.5)))
;;       (cond
;;        ((null evt) (message ""))
;;        ((and (integerp evt) (char-equal evt ?j))
;;         (delete-char -1)
;;         (set-buffer-modified-p modified)
;;         (push 'escape unread-command-events))
;;        (t (setq unread-command-events (append unread-command-events (list evt))))))))

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

(define-key evil-normal-state-map (kbd "DEL")
  (lambda ()
    (interactive)
    (previous-line 10)
    (evil-scroll-line-up 10)))

(define-key evil-normal-state-map " "
  (lambda ()
    (interactive)
    (next-line 10)
    (evil-scroll-line-down 10)))

;; (set-frame-font "Consolas-13")
;; (set-frame-font "-adobe-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
;; (set-frame-font "-adobe-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
;; (set-frame-font "NittiLight-14")
(set-frame-font "Monaco-10")

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point t)
(ido-mode t)
(require 'ido-preview)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


(fset 'yes-or-no-p 'y-or-n-p)
(setq-default tab-width 4)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/global-mode 1)


;; (autoload 'magit-status "magit" nil t)
;; (require 'mercurial)

;; (require 'nyan-mode)
;; (setq nyan-wavy-trail t)
;; (nyan-mode)

;; (require 'markdown-mode)
;; (setq auto-mode-alist
;; 	  (cons '("\.md" . markdown-mode) auto-mode-alist))

;; (setq rsense-home "/opt/rsense/")
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (require 'rsense)

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/nav")
(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key [M-f3] 'nav-toggle)
(require 'ruby-end)
(require 'inf-ruby)


;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/rinari/")
;; (require 'rinari)

;(setq url-http-attempt-keepalives nil)

(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 (setq-default indent-tabs-mode nil)

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(load-file (let ((coding-system-for-read 'utf-8))
               "/home/aske/.cabal/share/Agda-2.3.2/emacs-mode/agda2.el"))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(require 'compile)

;; this means hitting the compile button always saves the buffer
;; having to separately hit C-x C-s is a waste of time
(setq mode-compile-always-save-buffer-p t)
;; make the compile window stick at 12 lines tall
(setq compilation-window-height 12)

;; from enberg on #emacs
;; if the compilation has a zero exit code, 
;; the windows disappears after two seconds
;; otherwise it stays
(setq compilation-finish-function
      (lambda (buf str)
        (unless (string-match "exited abnormally" str)
          ;;no errors, make the compilation window go away in a few seconds
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!"))))

;; one-button testing, tada!
(global-set-key [f12] 'compile)
   (require 'speedbar)
   (speedbar-add-supported-extension ".hs")

(require 'fixme-mode)
(require 'hs-lint)

(setq evil-default-cursor t)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(defun nolinum ()
  (global-linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)
;; (setq org-startup-indented t)
;; (setq org-columns-content t)
;; (setq org-align-all-tags t) 
(require 'unscroll)
(fset 'org-mode-preview-all-latex
   "\C-u\C-u\C-c\C-x\C-l")

;; (add-to-list 'load-path "/usr/share/emacs/scala-mode")

;; (require 'scala-mode-auto)

;; (set-input-method 'russian-computer)

;; (setf (gethash #x70 x-keysym-table) (aref (kbd "s-x") 0))
;; (global-set-key (kbd "s-x") 
;;                 (lambda ()
;;                   (interactive)
;;                   (toggle-input-method)))

;; (defun make-conditional-key-translation (key-from key-to translate-keys-p)
;;   "Make a Key Translation such that if the translate-keys-p function returns true,
;;   key-from translates to key-to, else key-from translates to itself.  translate-keys-p
;;   takes no args. "
;;   (define-key key-translation-map key-from
;;     (lambda (prompt)
;;       (if translate-keys-p key-to key-from))))

;; (defun my-translate-keys-p ()
;;   "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
;;   (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p)))

;; (make-conditional-key-translation (kbd "о") (kbd "j") 'my-translate-keys-p)

;; (defun translate-keystrokes-ru->en ()
;;   "Make emacs output english characters, regardless whether
;; the OS keyboard is english or russian"
;;   (flet ((make-key-stroke (prefix char)
;;            (eval `(kbd ,(if (and (string-match "^C-" prefix)
;;                                  (string-match "[A-Z]" (string char)))
;;                             (concat "S-" prefix (string (downcase char)))
;;                             (concat prefix (string char)))))))
;;     (let ((case-fold-search nil)
;;           (keys-pairs (mapcar* 'cons
;;                                "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
;;                                "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
;;           (prefixes '(""    "s-"    "M-"    "M-s-"
;;                       "C-"  "C-s-"  "C-M-"  "C-M-s-")))
;;       (mapc (lambda (prefix)
;;               (mapc (lambda (pair)
;;                       (define-key key-translation-map
;;                           (make-key-stroke prefix (car pair))
;;                         (make-key-stroke prefix (cdr pair))))
;;                     keys-pairs))
;;             prefixes))))

;; (defun literal-insert ()
;;   (interactive)
;;   (insert-char last-input-event 1))

;; (define-minor-mode literal-insert-mode
;;     "Make emacs output characters corresponging to the OS keyboard,
;;  ignoring the key-translation-map"
;;   :keymap (let ((new-map (make-sparse-keymap))
;;                 (english-chars "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
;;             (mapc (lambda (char)
;;                     (define-key new-map (string char)
;;                       'literal-insert))
;;                   english-chars)
;;             new-map))


;; (translate-keystrokes-ru->en)
;; (add-hook 'text-mode-hook
;;           (lambda () (literal-insert-mode 1)))

;; (setq default-frame-alist '((font . "Monaco-11")))
      
;; (require 'coq)

(load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

(require 'flymake)
(defun flymake-haskell-init ()
  "When flymake triggers, generates a tempfile containing the
  contents of the current buffer, runs `hslint` on it, and
  deletes file. Put this file path (and run `chmod a+x hslint`)
  to enable hslint: https://gist.github.com/1241073"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables flymake-mode for haskell, and sets <C-c d> as command
  to show current error."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

;; Forces flymake to underline bad lines, instead of fully
;; highlighting them; remove this if you prefer full highlighting.


(setq TeX-engine 'xetex)

;;set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook (lambda()
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
(setq TeX-command-default "XeLaTeX")
(setq TeX-save-query nil)
(setq TeX-show-compilation t)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/vendor/emacs-powerline")
(require 'powerline)

;; (setf (gethash #xfe08 x-keysym-table) (aref (kbd "S-q") 0))
;; (global-set-key (kbd "S-q") 'toggle-input-method)
