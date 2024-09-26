;;; ../../dev/emacs/practicalli-doom/+coding.el -*- lexical-binding: t; -*-

;; Projects
;; Define a project path to discover projects using SPC Tab D
;;(setq projectile-project-search-path '(("~/work" . 2)  ("~/.config" . 1) ("~/dev/frap" . 2)))

;; Disable projectile cache - saves requirement to invalidate cache when moving files
(setq ;; projectile-enable-caching nil
 projectile-sort-order 'recentf )

(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;;undo using tree
;;(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

(use-package! yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :defer t
  :custom
  (yaml-indent-offset 2)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


(use-package jinja2-mode
  :mode ("\\.j2\\'" "\\.jinja2\\'"))

;; accept completion from copilot and fallback to company
(use-package! copilot
  ;;  :custom
  ;;  (copilot-disable-predicates '(always))
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

;; (defun rk/copilot-quit ()
;;   "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
;; cleared, make sure the overlay doesn't come back too soon."
;;   (interactive)
;;   (condition-case err
;;       (when copilot--overlay
;;         (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
;;           (setq copilot-disable-predicates (list (lambda () t)))
;;           (copilot-clear-overlay)
;;           (run-with-idle-timer
;;            1.0
;;            nil
;;            (lambda ()
;;              (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
;;     (error handler)))

;; (advice-add 'keyboard-quit :before #'rk/copilot-quit)

;; A Spacemacs like Lisp state menu (without the transient state)

(map! :leader
      (:prefix ("k". "Smartparens")
       :desc "Slurp forward" "s" #'sp-forward-slurp-sexp
       :desc "Slurp backward" "S" #'sp-backward-slurp-sexp
       :desc "" "$"   #'sp-end-of-sexp
       (:prefix ("`" . "Hybrid"))
       :desc "Kill" "k" #'sp-kill-hybrid-sexp
       :desc "Push" "p" #'sp-push-hybrid-sexp
       :desc "Slurp" "s" #'sp-slurp-hybrid-sexp
       :desc "Transpose" "t" #'sp-transpose-hybrid-sexp
       :desc "Absorb" "a" #'sp-absorb-sexp
       :desc "Barf forward" "b" #'sp-forward-barf-sexp
       :desc "Barf backward" "B" #'sp-backward-barf-sexp
       :desc "Convoluted" "c" #'sp-convolute-sexp
       (:prefix ("d" . "Delete")
        :desc "Symbol" "s" #'sp-kill-symbol
        :desc "Symbol Backward" "S" #'sp-backward-kill-symbol
        :desc "Word" "w" #'sp-kill-word
        :desc "Word Backward" "W" #'sp-backward-kill-word
        :desc "Kill" "x" #'sp-kill-sexp
        :desc "Kill Backward" "X" #'sp-backward-kill-sexp)
       :desc "Splice" "e" #'sp-splice-sexp-killing-forward
       :desc "Splice Backward" "E" #'sp-splice-sexp-killing-backward
       :desc "Symbol Backward" "h" #'sp-backward-symbol
       :desc "Sexp Backward" "H" #'sp-backward-sexp
       :desc "Join" "j" #'sp-join-sexp
       :desc "Sexp Forward" "l" #'sp-forward-sexp
       :desc "Sexp Forward" "L" #'sp-forward-sexp
       :desc "Raise" "r" #'sp-raise-sexp
       :desc "Slurp" "s" #'sp-forward-slurp-sexp
       :desc "Slurp Backward" "S" #'sp-backward-slurp-sexp
       :desc "Transpose" "t" #'sp-transpose-sexp
       :desc "Up Backward" "U" #'sp-backward-up-sexp
       (:prefix ("w" . "Wrap")
        :desc "()" "(" #'sp-wrap-round
        :desc "{}" "{" #'sp-wrap-curly
        :desc "[]" "[" #'sp-wrap-square
        :desc "Round" "w" #'sp-wrap-round
        :desc "Curly" "c" #'sp-wrap-curly
        :desc "Square" "s" #'sp-wrap-square
        :desc "Unwrap" "u" #'sp-unwrap-sexp)
       :desc "Copy sexp" "y" #'sp-copy-sexp))
