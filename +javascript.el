;;; ../../dev/emacs/practicalli-doom/+javascript.el -*- lexical-binding: t; -*-

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package! apheleia
  :config
  (apheleia-global-mode +1))


(setq +tree-sitter-hl-enabled-modes '(python-mode typescript-ts-mode))

(defun my-setup-dap-node ()
  "Require dap-node feature and run dap-node-setup if VSCode module isn't already installed"
  (require 'dap-node)
  (unless (file-exists-p dap-node-debug-path) (dap-node-setup)))

(add-hook 'typescript-mode-hook 'my-setup-dap-node)
(add-hook 'javascript-mode-hook 'my-setup-dap-node)
