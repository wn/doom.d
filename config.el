;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Ang Wei Neng"
      user-mail-address "weineng@twosigma.com"
      doom-scratch-buffer-major-mode 'org-mode
      doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :weight 'light)
      doom-serif-font (font-spec :family "Iosevka" :weight 'light)
      doom-theme 'doom-dracula
      display-line-numbers-type t
      load-prefer-newer t
      +zen-text-scale 1
      writeroom-extra-line-spacing 0.3

      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      search-highlight t
      search-whitespace-regexp ".*?"
      org-directory "~/.org/"
      org-ellipsis " ▼ ")

(use-package! multiple-cursors
  :config
  (map! "C-x r t" #'mc/edit-lines)
  (define-key mc/keymap (kbd "<return>") nil))

;;; two sigma stuff
;;(use-package! ligma
;; :config
;; (ligma-startup)
;; (map! "C-c g" #'ligma-magit-status)
;; (map! "M-s" #'ligma-get-sourcegraph-url)
;; (map! "M-r" 'ligma-list-tstest-all))

;; Reset lsp-completion provider
(add-hook 'doom-init-modules-hook
          (lambda ()
            (after! lsp-mode
              (setq lsp-completion-provider :none))))

(add-hook 'lsp-after-open-hook 'lsp-ui-mode)

;; Pad before lsp modeline error info
(add-hook 'lsp-mode-hook
          (lambda ()
            (setf (caadr
                   (assq 'global-mode-string mode-line-misc-info))
                  " "))
          'company-mode)

;; Set orderless filtering for LSP-mode completions
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (dolist (dir '("*c/include/*"))
              (push dir lsp-file-watch-ignored-directories))
            (setq lsp-completion-enable t)
            (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))

(map! :map pdf-isearch-minor-mode-map "C-s" #'isearch-forward)
(map! :map pdf-isearch-minor-mode-map "C-r" #'isearch-backward)
(map! "C-s" #'+default/search-buffer)

(use-package company)

(use-package! parinfer-rust-mode
  :hook emacs-lisp-mode)

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package! orderless
  :when (featurep! +orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package! cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key (kbd "M-o") 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(use-package! smartparens
  :init
  (smartparens-global-mode -1)
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp))

(use-package! ox-hugo
  :after org)

(use-package! tree-sitter
  :hook
  (prog-mode . global-tree-sitter-mode))

(use-package! magit
  :config
  (map!"C-c B" #'magit-blame-addition)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules-overview))

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "jethro/org-capture-slipbox" "<tab>" #'jethro/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/.org/braindump/org/")
        ;; org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  :config
  (org-roam-db-autosync-mode +1)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))
  (add-hook 'org-roam-capture-new-node-hook (lambda () (org-roam-tag-add '("draft"))))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'citar)
  (defun jethro/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))

;; MacOS specific error: Cannot find gls (GNU ls). This may cause issues with dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; resize window
(global-set-key (kbd "C-<up>") (lambda () (interactive) (shrink-window 5)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (enlarge-window 5)))
(global-set-key (kbd "C-<left>") (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "C-<right>") (lambda () (interactive) (enlarge-window-horizontally 5)))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(map! "M-z" #'undo-redo
      "C-z" #'undo-only
      "M-/" #'comment-line
      "C-/" #'completion-at-point)

(map!
 [C-tab] #'+fold/toggle
 [C-iso-lefttab] #'+fold/close-all
 [C-M-tab] #'+fold/open-all)

(global-set-key (kbd "C-v")
    (lambda () (interactive) (forward-line 15)))
(global-set-key (kbd "M-v")
    (lambda () (interactive) (forward-line -15)))
(map! "C-." #'next-buffer
      "C-," #'previous-buffer)


(global-set-key (kbd "<escape>") 'doom/escape)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default t)

(map! "C-x C-k" #'kill-this-buffer)
(map! "C-x C-z" nil)
(map! "C-o" #'other-window)

(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-reveal-mathjax t)
(require 'ox-reveal)

;; improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;;;; open emacs in fullscreen mode
;; (set-frame-parameter (select-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; presentation
(after! org-present
  (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; make lookup 1000x faster for python
(after! gcmh
  (setq gcmh-high-cons-threshold 33554432))  ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb
(setq read-process-output-max  (* 20 (* 1024 1024))) ;; 20mb

;; lint code in org-mode pdf exports
(setq org-latex-src-block-backend 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-minted-options '(("breaklines" "true") ("breakanywhere" "true"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "latexmk -f -xelatex %f"))

(setq org-src-window-setup 'current-window
      org-return-follows-link t
      org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (dot . t)
                                 (R . t))
      org-confirm-babel-evaluate nil
      org-use-speed-commands t
      org-fold-catch-invisible-edits 'show
      org-preview-latex-image-directory "/tmp/ltximg/"
      org-structure-template-alist '(("a" . "export ascii")
                                     ("c" . "center")
                                     ("C" . "comment")
                                     ("e" . "example")
                                     ("E" . "export")
                                     ("h" . "export html")
                                     ("l" . "export latex")
                                     ("q" . "quote")
                                     ("s" . "src")
                                     ("v" . "verse")
                                     ("el" . "src emacs-lisp")
                                     ("d" . "definition")
                                     ("t" . "theorem")))

;; alt up/down moves line
;;(drag-stuff-global-mode 1)
;;(drag-stuff-define-keys)

(map! "M-g" #'goto-line)

(defun my/scratch-buffer-shortcut()
  (interactive)
  (if (string-equal (buffer-name) "*doom:scratch*")
      (delete-window)
      (doom/open-scratch-buffer)))

;; only f5-f9 can be user defined.
(map! [f5] #'revert-buffer
      ;;[f6] #'
      ;;[f7] #'
      ;;[f8] #'projectile-find-file
      [f9] #'my/scratch-buffer-shortcut)

(after! python
  (add-hook
   'python-mode-hook
   (lambda ()
     (setq-local python-indent-offset 4))))

;; disable adding new projects automatically due to two sigma's submodule structure.
;; add projects manually using projectile-add-known-project
(setq projectile-track-known-projects-automatically nil)

;; disable lsp lens
(setq lsp-lens-enable nil)

;; jump better
(map! "M-," 'better-jumper-jump-backward)

(defun weineng/deploy-braindump()
  "build braindump's hugo files and upload to ts3"
  (interactive)
  (let ((default-directory "~/.org/braindump/")) (shell-command-to-string "make")))

(after! org
  (setq org-attach-dir-relative t))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-nil)
  (setq inhibit-compacting-font-caches t)
  (size-indication-mode -1))

(add-hook 'c-mode-common-hook (lambda () (define-key c++-mode-map (kbd "C-c C-c") 'ff-find-other-file)))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))
