(put 'projectile-ripgrep 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-format-fallback-style "google")
 '(disaster-cxx "c++ -O3")
 '(flycheck-clang-args
   '("-std=c++2a" "-O0" "-march=native" "-g" "-Rpass-analysis=loop-vectorize"
     "-Rpass-missed=loop-vectorize" "-Rpass=loop-vectorize"))
 '(package-selected-packages '(treesit-auto)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#16181F")))))
(put 'customize-face 'disabled nil)
