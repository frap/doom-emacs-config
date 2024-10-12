;;; ../../dev/emacs/practicalli-doom/+coding.el -*- lexical-binding: t; -*-

;; coding with paranthesis aka LISP
;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

(setq show-paren-style 'parenthesis)
;;; Electric Pairs to auto-complete () [] {} "" etc. It works on regions.

(electric-pair-mode)
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t)

(use-package! smartparens
  :config
  ;; Enable smartparens strict mode for Clojure and Emacs Lisp
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

  ;; Load the default smartparens configuration
  (require 'smartparens-config)

  (map! :map smartparens-mode-map
        ;; Movement
        "C-M-f"   #'sp-forward-sexp
        "C-M-b"   #'sp-backward-sexp
        "C-M-u"   #'sp-backward-up-sexp
        "C-M-d"   #'sp-down-sexp
        "C-M-p"   #'sp-backward-down-sexp
        "C-M-n"   #'sp-up-sexp
        "C-M-k"   #'sp-kill-sexp
        "C-M-t"   #'sp-transpose-sexp

        ;; Slurping and barfing
        "C-)"         #'sp-forward-slurp-sexp
        "C-<right>"   #'sp-forward-slurp-sexp
        "C-("         #'sp-backward-slurp-sexp
        "C-M-<right>" #'sp-backward-slurp-sexp
        "M-)"         #'sp-forward-barf-sexp
        "C-<left>"    #'sp-forward-barf-sexp
        "M-("         #'sp-backward-barf-sexp
        "C-M-<left>"  #'sp-backward-barf-sexp

        ;; Wrapping
        "M-["     #'sp-wrap-square
        "M-{"     #'sp-wrap-curly
        "M-\""    #'sp-wrap-doublequote

        ;; Splicing
        "M-s"    #'sp-splice-sexp
        "M-S"    #'sp-split-sexp
        "M-j"    #'sp-join-sexp
        ))

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

;; disallow very long line
(global-so-long-mode 1)

;; Projects
;; Define a project path to discover projects using SPC Tab D
;;(setq projectile-project-search-path
;;  '(("~/work" . 2)  ("~/.config" . 1) ("~/dev/frap" . 2)))

(after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
                                                                         projectile-project-root-files-bottom-up)))
;; Disable projectile cache - saves requirement to invalidate cache when moving files
(setq ;; projectile-enable-caching nil
 projectile-sort-order 'recentf )

;;(require 'pyenv-mode)

;; (defun projectile-pyenv-mode-set ()
;;   "Set pyenv version matching project name."
;;   (let ((project (projectile-project-name)))
;;     (if (member project (pyenv-mode-versions))
;;         (pyenv-mode-set project)
;;       (pyenv-mode-unset))))

;; (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;;undo using tree
;;(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

(use-package! yaml-mode
  :defer t
  :custom
  (yaml-indent-offset 2)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                     (setq copilot-disable-predicates (list (lambda () t)))
                     (copilot-clear-overlay)
                     (run-with-idle-timer
                      1.0
                      nil
                      (lambda ()
                        (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)


;; JavaScript
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)))
