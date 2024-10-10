;;; ../../dev/emacs/practicalli-doom/+coding.el -*- lexical-binding: t; -*-

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
(global-so-long-mode 1)

;; Projects
;; Define a project path to discover projects using SPC Tab D
;;(setq projectile-project-search-path '(("~/work" . 2)  ("~/.config" . 1) ("~/dev/frap" . 2)))

(after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
                                                                         projectile-project-root-files-bottom-up)))
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
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

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


;; JavaScript
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)))
