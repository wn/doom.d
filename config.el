;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Ang Wei Neng"
      user-mail-address "weineng.a@gmail.com"
      doom-scratch-buffer-major-mode 'org-mode
      doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "Roboto" :weight 'light)
      doom-serif-font (font-spec :family "Iosevka" :weight 'light)
      doom-theme 'doom-dracula
      display-line-numbers-type t
      load-prefer-newer t
      writeroom-extra-line-spacing 0.3

      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      search-highlight t
      search-whitespace-regexp ".*?")

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package! multiple-cursors
  :config
  (map! "C-x r t" #'mc/edit-lines)
  (define-key mc/keymap (kbd "<return>") nil))

(use-package! lsp-mode
  :hook
  (python-mode . lsp)
  :config
  (map! "M-s" #'lsp-ui-doc-glance))

(use-package! ox-reveal
  :init
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
        org-reveal-mathjax t))

(use-package edraw-org
  :config
  (edraw-org-setup-default)
  (defun org-mode-open-edraw(&optional filename)
    (interactive)
    (unless filename
      (setq filename (concat "./" (file-relative-name (read-file-name (edraw-msg "Write svg file: ") default-directory) default-directory))))
    (insert (concat "[[edraw:file=" filename "]]"))
    (backward-char)
    (org-return)))

(use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (setq parinfer-rust-auto-download t))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-width-grow-only t)
  (company-tooltip-maximum-width 120)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Disable company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  (company-box-doc-delay 1))

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

(use-package! elfeed
  :config
  (setq elfeed-feeds '("https://reddit.com/r/emacs.rss"
                       "https://reddit.com/r/linux.rss")))

(use-package! auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t)
  (custom-set-faces! '(auto-dim-other-buffers-face :weight extra-light)))

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

(defun my/scratch-buffer-shortcut()
  "Open scratch buffer as a popup window."
  (interactive)
  (if (string-equal (buffer-name) "*doom:scratch*")
      (delete-window)
      (doom/open-scratch-buffer)))

(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(defun weineng/deploy-braindump()
  "Build braindump's hugo files and upload to TS3."
  (interactive)
  (let ((default-directory "~/.org/braindump/")) (shell-command-to-string "make")))

(map!
    ;; only f5-f9 can be user defined."M-/" #'comment-line
    [f5] #'revert-buffer
    [f6] #'ligma-get-sourcegraph-url
    [f7] #'ligma-upload-region-to-qs
    ;;[f8] #'
    [f9] #'my/scratch-buffer-shortcut

    ;; undo-redo
    "M-z" #'undo-redo
    "C-z" #'undo-only

    ;; buffer nav
    "C-." #'next-buffer
    "C-," #'previous-buffer
    "M-." #'+lookup/definition
    "M-," #'better-jumper-jump-backward
    "C-v"(lambda () (interactive) (forward-line 15))
    "M-v" (lambda () (interactive) (forward-line -15))
    "M-g" #'goto-line

    ;; resize window
    "C-<up>" (lambda () (interactive) (enlarge-window 5))
    "C-<down>" (lambda () (interactive) (shrink-window 5))
    "C-<left>" (lambda () (interactive) (shrink-window-horizontally 5))
    "C-<right>" (lambda () (interactive) (enlarge-window-horizontally 5))

    ;; search
    "C-s" #'swiper

    ;; folding
    [C-tab] #'+fold/toggle
    [C-iso-lefttab] #'+fold/close-all
    [C-M-tab] #'+fold/open-all

    "M-/" #'comment-line
    "C-x C-k" #'kill-this-buffer
    ;; suppress suspend emacs shortcut
    "C-x C-z" nil
    "C-o" #'other-window
    "C-c d" #'dired-jump
    "M-p" #'drag-stuff-up
    "M-n" #'drag-stuff-down
    "C-/" #'+company/complete)

(map! :map pdf-isearch-minor-mode-map
      "C-s" #'isearch-forward
      "C-r" #'isearch-backward)

(global-set-key (kbd "<escape>") 'doom/escape)
(bind-key* "M-t" '+vterm/toggle)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq super-save-auto-save-when-idle t
      auto-save-default t)

;; improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; disable adding new projects automatically due to two sigma's submodule structure.
;; add projects manually using projectile-add-known-project
(setq projectile-track-known-projects-automatically nil)

(setq lsp-lens-enable nil)

;; make lookup 1000x faster for python
(after! gcmh
  (setq gcmh-high-cons-threshold 33554432)  ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb
  (setq read-process-output-max  (* 20 (* 1024 1024)))) ;; 20mb

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

(after! org
  (setq org-attach-dir-relative t
        org-directory "~/.org/"
        org-ellipsis " ▼ "

        ;; lint code in org-mode pdf exports
        org-latex-src-block-backend 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-minted-options '(("breaklines" "true") ("breakanywhere" "true"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "latexmk -f -xelatex %f")

        org-src-window-setup 'current-window
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
                                       ("t" . "theorem"))))

(after! doom-modeline
   (setq doom-modeline-buffer-file-name-style 'truncate-nil)
   (setq inhibit-compacting-font-caches t)
   (size-indication-mode nil)
   (setq doom-modeline-indent-info t))

(after! dired
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (map! :map dired-mode-map
      "C-f" #'dired-find-file
      "C-b" #'dired-up-directory))

(add-hook 'c-mode-common-hook (lambda () (define-key c++-mode-map (kbd "C-c C-c") 'ff-find-other-file)))
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))
(defun my-c-mode-hook ()
  (c-set-offset 4)
  (c-set-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun compile-parent (command)
  (interactive
   (let* ((make-directory (locate-dominating-file (buffer-file-name)
                                                  "Makefile"))
          (command (concat "make -k -C "
                           (shell-quote-argument make-directory))))
     (list (compilation-read-command command))))
  (compile command))
(set-face-foreground 'vertical-border "white")

;###autoload
(defun org-odt-publish-to-odt (plist filename pub-dir)
  "Publish an org file to ODT.

 FILENAME is the filename of the Org file to be published.  PLIST
 is the property list of the given project.  PUB-DIR is the publishing
 directory.

 Return output file name."
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t)
         (visiting (find-buffer-visiting filename))
         (work-buffer (or visiting (find-file-noselect filename))))
    (unwind-protect
     (with-current-buffer work-buffer
       (let ((outfile (org-export-output-file-name ".odt" nil pub-dir)))
         (org-odt--export-wrap
          outfile
          (let* ((org-odt-embedded-images-count 0)
                 (org-odt-embedded-formulas-count 0)
                 (org-odt-object-counters nil)
                 (hfy-user-sheet-assoc nil))
            (let ((output (org-export-as 'odt nil nil nil
                                         (org-combine-plists
                                          plist
                                          `(:crossrefs
                                            ,(org-publish-cache-get-file-property
                                              (expand-file-name filename) :crossrefs nil t)
                                            :filter-final-output
                                            (org-publish--store-crossrefs
                                             org-publish-collect-index
                                             ,@(plist-get plist :filter-final-output))))))
                  (out-buf (progn (require 'nxml-mode)
                                  (let ((nxml-auto-insert-xml-declaration-flag nil))
                                    (find-file-noselect
                                     (concat org-odt-zip-dir "content.xml") t)))))
              (with-current-buffer out-buf (erase-buffer) (insert output))))))))
    (unless visiting (kill-buffer work-buffer))))

(custom-set-faces!
  '(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
  '(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))
