;;; init.el -*- lexical-binding: t; -*-
(doom!
      :completion
      (vertico +orderless +icons)
      (company +childframe)

      :ui
      doom              ; what makes DOOM look the way it does
      doom-dashboard    ; a nifty splash screen for Emacs
      hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
      indent-guides     ; highlighted indent columns
      (modeline +light)          ; snazzy, Atom-inspired modeline, plus API
      nav-flash         ; blink cursor line after big motions
      ophints           ; highlight the region an operation acts on
      (popup +defaults)   ; tame sudden yet inevitable temporary windows
      vc-gutter
      vi-tilde-fringe   ; fringe tildes to mark beyond EOB
      workspaces        ; tab emulation, persistence & separate workspaces

      :editor
      fold              ; (nigh) universal code folding
      snippets          ; my elves. They type so I don't have to
      word-wrap         ; soft wrapping with language-aware indent

      :emacs
      dired             ; making dired pretty [functional]
      electric          ; smarter, keyword-based electric-indent
      undo              ; persistent, smarter undo for your inevitable mistakes
      vc                ; version-control and Emacs, sitting in a tree

      :term
      vterm             ; the best terminal emulation in Emacs

      :checkers
      syntax              ; tasing you for every semicolon you forget

      :tools
      (eval +overlay)     ; run code, run (also, repls)
      (lookup +docsets +dictionary)              ; navigate your code and its documentation
      lsp               ; M-x vscode
      magit             ; a git porcelain for Emacs
      make              ; run make tasks from Emacs
      pdf               ; pdf enhancements
      :lang
      (cc +lsp)         ; C > C++ == 1
      data              ; config/data formats
      (latex +latexmk)    ; writing papers in Emacs has never been so fun
      markdown          ; writing docs for people to ignore
      nix               ; I hereby declare "nix geht mehr!"
      org               ; organize your plain life in plain text
      (python +pyright +lsp)            ; beautiful is better than ugly
      (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()

      :config
      (default +bindings)

      :app
      (rss +org))
