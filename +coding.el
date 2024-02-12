;;; ../../dev/emacs/practicalli-doom/+coding.el -*- lexical-binding: t; -*-

;; accept completion from copilot and fallback to company
(use-package! copilot
  :custom
  (copilot-disable-predicates '(always))
  :hook (prog-mode . copilot-mode)
  :hook (yaml-mode . copilot-mode)
  :bind
  ("M-`" . copilot-complete)
  :bind  (:map copilot-completion-map
               ("C-g" .  #'copilot-clear-overlay)
               ("M-p" . #'copilot-previous-completion)
               ("M-n" . #'copilot-next-completion)
               ("<tab>" . 'copilot-accept-completion)
               ("TAB" . 'copilot-accept-completion)
               ("M-f" . #'copilot-accept-completion-by-word)
               ("C-TAB" . 'copilot-accept-completion-by-word)
               ("C-<tab>" . 'copilot-accept-completion-by-word)
               ("M-<return>" . copilot-accept-completion-by-line))

  :config
  (setq copilot-max-char -1))

(use-package! yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :defer t
  :custom
  (yaml-indent-offset 2)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
