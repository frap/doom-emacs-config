;;; ../../dev/emacs/practicalli-doom/+javascript.el -*- lexical-binding: t; -*-

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;;(after! tree-sitter
;; (setq treesit-language-source-alist
;;       '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil))))


(add-to-list 'treesit-extra-load-path
             (expand-file-name "~/local/external/tree-sitter-module/dist"))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))

(setq +tree-sitter-hl-enabled-modes '(python-mode typescript-ts-mode))
