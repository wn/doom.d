;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Ang Wei Neng"
      user-mail-address "weineng.a@gmail.com"
      doom-scratch-buffer-major-mode 'org-mode
      doom-font (font-spec :family "JetBrains Mono" :size 28 :weight 'light)
      doom-big-font (font-spec :family "JetBrains Mono" :size 35)
      doom-variable-pitch-font (font-spec :family "Roboto" :weight 'light)
      doom-serif-font (font-spec :family "Iosevka" :weight 'light)
      doom-theme 'doom-dracula
      display-line-numbers-type t
      load-prefer-newer t
      writeroom-extra-line-spacing 0.3

      visual-fill-column-width 110
      visual-fill-column-centre-text t

      indent-tabs-mode nil
      search-highlight t
      search-whitespace-regexp ".*?")


(use-package! compiler-explorer
  :config
  (defun my/toggle-compiler-explorer ()
    (interactive)
    (if (compiler-explorer--active-p)
        (compiler-explorer-exit)
      (compiler-explorer)))
  (map! [f8] #'my/toggle-compiler-explorer)
  ;;(setq compiler-explorer-url "http://localhost:10240")
  (setq compiler-explorer-url "https://godbolt.org")
  (map! :map compiler-explorer-mode-map
        "M-." #'compiler-explorer-jump
        "M-," #'compiler-explorer-layout
        "M-'" #'compiler-explorer-layout))

(use-package x86-lookup
  :ensure t
  :config
  (defun my/x86-lookup-at-point ()
   "Lookup the x86 instruction under the cursor using `x86-lookup`."
   (interactive)
   (let ((symbol (thing-at-point 'symbol t)))
     (if symbol
         (x86-lookup symbol)
       (message "No symbol under cursor!"))))
  (global-set-key (kbd "C-h x") #'my/x86-lookup-at-point)
  (setq  x86-lookup-pdf "/home/weineng/Downloads/325383-087-sdm-vol-2abcd.pdf"))

(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-info-epdfinfo-program "~/pdf-tools/server/epdfinfo")
  (pdf-tools-install-noverify))

(use-package! clang-format
  :ensure t
  :config
  (setq clang-format-style "file"
        clang-format-fallback-style "google")
  (defun my/find-clang-format-file ()
   "Recursively search for a .clang-format file in the parent directories."
   (let ((dir (locate-dominating-file default-directory ".clang-format")))
     (when dir
       (expand-file-name ".clang-format" dir))))
 (defun my/clang-format-before-save ()
   "Run clang-format on the buffer if a .clang-format file exists."
   (when (and (eq major-mode 'c++-ts-mode) ; Change this if you want to support other modes
              (my/find-clang-format-file))
     (clang-format-buffer)))
 (add-hook 'before-save-hook 'my/clang-format-before-save))

(use-package! flycheck
  :init
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-clang-language-standard "c++2b"
        flycheck-gcc-language-standard "c++2b"
        flycheck-clang-args '("-std=c++2b" "-O0" "-march=native" "-g")))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package! magit-delta
  :after magit
  :hook
  (magit-mode . magit-delta-mode)
  :config
  (setq
    magit-delta-default-dark-theme "Dracula"
    magit-delta-default-light-theme "Github"
    magit-delta-hide-plus-minus-markers nil))

(use-package! multiple-cursors
  :config
  (map! "C-x r t" #'mc/edit-lines)
  (define-key mc/keymap (kbd "<return>") nil))

(use-package! lsp-mode
  :hook
  (python-ts-mode . lsp)
  (c++-ts-mode . lsp)
  :init
  (setq lsp-enable-symbol-highlighting t
        lsp-signature-render-documentation t
        lsp-lens-enable nil
        lsp-eldoc-enable-hover t
        lsp-inhibit-message t
        lsp-message-project-root-warning t))

(after! lsp-ui
  (setq lsp-ui-mode t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil))

(use-package! ox-reveal
  :init
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
        org-reveal-mathjax t))

(use-package! swiper
  :config
  (setq swiper-action-recenter t))

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

(use-package company
  :after lsp-mode
  :init
  (setq lsp-inhibit-message nil
        lsp-message-project-root-warning nil)
  :hook (lsp-mode . company-mode)
  :hook (company-mode . company-box-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
         ("<return>" . nil)
         ("RET" . nil))
  :custom
  (setq completion-ignore-case t
        company-minimum-prefix-length 1
        company-tooltip-width-grow-only t
        company-tooltip-maximum-width 120
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode eshell-mode)
        company-idle-delay 0
        ;; Number the candidates (use M-1, M-2 etc to select completions)
        company-show-numbers t
        company-selection-wrap-around t
        company-box-doc-delay 0))

(use-package! smartparens
  :init
  (smartparens-global-mode -1)
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp)
  (remove-hook `doom-first-buffer-hook 'smartparens-global-mode)
  (smartparens-global-mode nil))

(use-package org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package! ox-hugo
  :after org)

(use-package! magit
  :ensure t
  :config
  (setq magit-git-global-arguments
   `("--no-pager"
     "-c" "core.preloadindex=true"
     "-c" "log.showSignature=false"
     "-c" "color.ui=false"
     "-c" "color.diff=false"))
  (map!"C-c B" #'magit-blame-addition
       "C-c g g" #'magit-status)
  :after nerd-icons
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :init
  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen)))

(use-package! magit-todos
  :after magit
  :config
  (magit-todos-mode))

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
           (file+head "reference/${slug}.org" "#+title: ${title}\n#+hugo_draft: true\n")
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

(use-package! expand-region
  :config
    (setq expand-region-subword-enabled t)
    (map! "C-\\" #'er/expand-region)
  (global-subword-mode))

(use-package! disaster
  :commands (disaster)
  :init
  (setq disaster-cxxflags "-O0 -march=native -std=c++2a -g"
        disaster-cxx "g++"
        disaster-cpp-regexp "\\(h\\|c\\|cc\\|hh\\|hpp\\)$"
        disaster-find-build-root-functions '(projectile-acquire-root)))

(use-package! vertico
  :config
  (map! :map vertico-map "C-l" '+vertico/enter-or-preview))

(use-package ultra-scroll
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; (use-package! copilot
;;   :init
;;   (exec-path-from-shell-initialize)
;;   (doom/reload-env)
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("C-l" . 'copilot-accept-completion))

;;   :config
;;   (defun extract-port-from-url (url)
;;    "Extract the port number from the given URL."
;;    (if (string-match "http://[^:]+:\\([0-9]+\\)" url)
;;        (match-string 1 url)
;;      (error "No port found in URL")))
;;   (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(org-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(text-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(closure-mode . 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2))
;;   (setq copilot-indent-offset-warning-disable t)
;;   (setq copilot-network-proxy '(:host "127.0.0.1" :port 20000))) ;; hard coded from nixfiles

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

(defun my/deploy-braindump()
  "Build braindump's hugo files and upload to TS3."
  (interactive)
  (let ((default-directory "~/.org/braindump/")) (shell-command-to-string "make")))

(defun my/dired-view ()
  "View files as HTML."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (how-many (length files)))
    (if (> how-many 0) (xdg-open-files files)
      (my/xdg-open (car files) t))))

(defun my/xdg-open-impl (file &optional async)
  "Opens FILE with xdg-open. Without optional argument ASYNC, it will wait for the file to finish playing or review."
  (let ((command (format "'%s' '%s'" (if (eq system-type 'darwin) "open" "xdg-open") file)))
    (if async
     (async-shell-command command)
     (shell-command command))))

(defun my/xdg-open-files (files)
  "Opens FILES with xdg-open one by one, waiting for each to finish."
  (dolist (file files)
    (my/xdg-open-impl file)))

(defun my/xdg-open (file)
  (interactive "FFile to open: ")
  (my/xdg-open-impl (file-truename file) t))

(map!
    ;; only f5-f9 can be user defined.
    [f5] #'revert-buffer
    ;; [f6]
    ;; [f7]
    ;; [f8] #' reserved for compiler explorer
    [f9] #'my/scratch-buffer-shortcut

    "M-/" #'comment-line
    ;; undo-redo
    "M-z" #'undo-redo
    "C-z" #'undo-only

    ;; buffer nav
    "M-'" #'previous-buffer
    "M-," #'next-buffer
    "M-." #'+lookup/definition
    "M-U" #'better-jumper-jump-forward
    "M-u" #'better-jumper-jump-backward

    ;;"M-," #'better-jumper-jump-backward
    "C-v"(lambda () (interactive) (forward-line 15))
    "M-v" (lambda () (interactive) (forward-line -15))
    "M-g" #'consult-goto-line

    ;; resize window
    "C-<up>" (lambda () (interactive) (enlarge-window 5))
    "C-<down>" (lambda () (interactive) (shrink-window 5))
    "C-<left>" (lambda () (interactive) (shrink-window-horizontally 5))
    "C-<right>" (lambda () (interactive) (enlarge-window-horizontally 5))

    ;; search
    "C-s" #'swiper
    "M-s" #'consult-flycheck

    "M-/" #'comment-line
    "C-0" #'kill-buffer-and-window
    ;; suppress suspend emacs shortcut
    "C-x C-z" nil
    "C-o" #'other-window
    "C-c d" #'dired-jump
    "M-p" #'drag-stuff-up
    "M-n" #'drag-stuff-down
    "C-/" #'+company/complete
    "C-?" #'lsp-find-references

    "M-1" (lambda () (interactive) (tab-select 1))
    "M-2" (lambda () (interactive) (tab-select 2))
    "M-3" (lambda () (interactive) (tab-select 3))
    "M-4" (lambda () (interactive) (tab-select 4))
    "M-5" (lambda () (interactive) (tab-select 5))
    "M-6" (lambda () (interactive) (tab-select 6))
    "M-7" (lambda () (interactive) (tab-select 7))
    "M-8" (lambda () (interactive) (tab-select 8))

    "M-k" (lambda () (interactive) (tab-close))

    ;; folding
    "M-;" #'+fold/toggle

    "M-0" #'zoom-window-zoom
    "M-o" #'ace-window
    "M-`" #'+popup/toggle)

(bind-key* "C-x 3" (lambda () (interactive) (progn (split-window-right) (other-window 1))))
(bind-key* "M-t" '+vterm/toggle)

(map! :map pdf-isearch-minor-mode-map
      "C-s" #'isearch-forward
      "C-r" #'isearch-backward)

(global-set-key (kbd "<escape>") 'doom/escape)
(setq super-save-auto-save-when-idle t
      auto-save-default t)

;; improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      scroll-step 1 ;; keyboard scroll one line at a time
      use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; make lookup 1000x faster for python
(after! gcmh
  (setq gcmh-high-cons-threshold 33554432  ; 32mb, or 64mb, or *maybe* 128mb, BUT NOT 512mb
        read-process-output-max  (* 20 (* 1024 1024)))) ;; 20mb

(after! org
  (setq org-attach-dir-relative t
        org-directory "~/.org/"
        org-ellipsis " ? "

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
                                       ("t" . "theorem")))
    ;; *** put the latex sizing here ***
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.5))      ;; inline
  (setq org-format-latex-options
        (plist-put org-format-latex-options :display-scale 2.5)) ;; blocks
  (setq org-format-latex-options
        (plist-put org-format-latex-options :dpi 200)))

(after! doom-modeline
   (setq doom-modeline-buffer-file-name-style 'truncate-nil
         inhibit-compacting-font-caches t
         doom-modeline-indent-info t
         find-file-visit-truename nil
         doom-modeline-project-detection 'project)
   (size-indication-mode nil))

(after! dired
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (map! :map dired-mode-map
      "C-f" #'dired-find-file
      "C-b" #'dired-up-directory))

(after! org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (display-line-numbers-mode -1)
              (setq header-line-format " ")
              ;; (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              ;;(org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (setq header-line-format nil)
              (display-line-numbers-mode 1)
              (org-present-read-write))))

(after! c++-ts-mode
  (add-hook 'c++-ts-mode-hook (lambda () (define-key c++-ts-mode-map (kbd "C-c C-c") 'lsp-clangd-find-other-file)))
  (add-hook 'c-ts-mode-hook (lambda() (setq c-ts-mode-indent-offset 4)))
  (add-hook 'c++-ts-mode-hook (lambda() (setq c-ts-mode-indent-offset 4))))

;; Configure vc-root-dir to check for .projectile
(defun my/vc-root-dir ()
  "Return the root of the version controlled directory."
  (when-let ((projectile-root (projectile-project-root)))
    (if (string-match-p ".git" projectile-root)
        projectile-root
      (when-let ((git-root (locate-dominating-file projectile-root ".git")))
        (file-name-directory git-root)))))
(advice-add 'vc-root-dir :override #'my/vc-root-dir)


;; https://github.com/doomemacs/doomemacs/issues/1739
(require 'elec-pair)
(add-hook 'c++-ts-mode-hook (lambda()
                                   (electric-pair-post-self-insert-function)
                                   (indent-according-to-mode)))

;;(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UNPACKED ;;;;;;;;;
(defun quicker-fix ()
  (let ((build-error-file (concat (project-root (project-current)) ".builderrors")))
    (if (string= (buffer-name) (file-name-nondirectory build-error-file))
        (kill-buffer (current-buffer))
      (progn
        (shell-command
          (format "cd %s && find . -name 'build.log' -print0 | xargs -0 -I {} sh -c 'sed -r \"s/\\\\x1B\\\\[([0-9]{1,3}(;[0-9]{1,2};?)?)?[mGK]//g\" \"{}\" | grep \": error:\" | sed \"s|^|%s|\"' | sort | uniq > %s"
           (project-root (project-current))
           ""
           build-error-file)) ; Make sure the number of arguments matches the placeholders
        (message "fetching errors...")
        (let ((buffer (find-file-noselect build-error-file)))
          (with-current-buffer buffer
            (compilation-mode))
          buffer)))))

(setq vterm-shell "zsh")

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)
    (setq-local rustic-format-on-save t)))

(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

(defun my/python-lsp-fix-and-format-on-save ()
  (when (bound-and-true-p lsp-mode)
    (ignore-errors (lsp-execute-code-action-by-kind "source.fixAll"))
    (ignore-errors (lsp-execute-code-action-by-kind "source.organizeImports"))
    (ignore-errors (lsp-format-buffer))))

(add-hook 'python-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/python-lsp-fix-and-format-on-save nil t)))
