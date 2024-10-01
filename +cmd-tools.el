;;; +cmd-tools.el -*- lexical-binding: t; -*-

;; setup shell
(setq explicit-shell-file-name "/opt/homebrew/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(use-package bash-completion
  :init (autoload
          'bash-completion-dynamic-complete
          "bash-completion"
          "BASH completion hook")
  (add-hook
   'shell-dynamic-complete-functions
   #'bash-completion-dynamic-complete))

;; GNU TRAMP Configuration
(setq tramp-default-method "ssh"                         ; Default to SSH, that's what I primarily use
      tramp-terminal-type "tramp"                        ; Let other terminal know what client I'm connecting with (might need to configure server)
      tramp-auto-save-directory "$XDG_CACHE_HOME/tramp/" ; Send Tramp info into XDG Cache directory on machine
      tramp-chunksize 2000)                              ; Resonable Chunk size for speed and disk space in mind


;; Load magit-todos
(use-package! magit-todos)

;; Location of developer tokens - default ~/.authinfo
;; Use XDG_CONFIG_HOME location or HOME
;; Optional:  (setq auth-source-cache-expiry nil)   ; default is 7200 (2h)
(setq auth-sources
      (list
       (concat (getenv "XDG_CACHE_HOME") "/authinfo.gpg")
       "~/.authinfo.gpg"))


;; Use Emacs as $EDITOR (or $GIT_EDITOR) for git commits messages
;; when using git commit on the command line
;; (global-git-commit-mode t)


;; Commit message checks
;; ~/.config/emacs/modules/emacs/vc/config.el
;; - checks for overlong-summary-line non-empty-line
;; (setq git-commit-summary-max-length 50
;;       git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
(after! magit
  (setq git-commit-summary-max-length 60

        magit-push-current-set-remote-if-missing nil
        ;; Highlight specific characters changed
        magit-diff-refine-hunk 'all

        ;; Show project TODO lines in Magit Status
        magit-todos-mode t

        ;; Show Libravatar of commit author
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")

        magit-branch-prefer-remote-upstream '("master" "main")
        ))

;; add GH pull requests to log
(defun modi/add-PR-fetch-ref (&optional remote-name)
  "If refs/pull is not defined on a GH repo, define it.

If REMOTE-NAME is not specified, it defaults to the `remote' set
for the \"main\" or \"master\" branch."
  (let* ((remote-name (or remote-name
                          (magit-get "branch" "main" "remote")
                          (magit-get "branch" "master" "remote")))
         (remote-url (magit-get "remote" remote-name "url"))
         (fetch-refs (and (stringp remote-url)
                          (string-match "github" remote-url)
                          (magit-get-all "remote" remote-name "fetch")))
         ;; https://oremacs.com/2015/03/11/git-tricks/
         (fetch-address (format "+refs/pull/*/head:refs/pull/%s/*" remote-name)))
    (when fetch-refs
      (unless (member fetch-address fetch-refs)
        (magit-git-string "config"
                          "--add"
                          (format "remote.%s.fetch" remote-name)
                          fetch-address)))))
(add-hook 'magit-mode-hook #'modi/add-PR-fetch-ref)
;; Location of Git repositories
;; define paths and level of sub-directories to search
(setq magit-repository-directories
      '(("~/money/" . 1)
        ("~/cs/"    . 1)))


;; Number of topics displayed (issues, pull requests)
;; open & closed, negative number for closed topics
;; or `forge-toggle-closed-visibility'
;; set closed to 0 to never show closed issues
;; (setq  forge-topic-list-limit '(100 . 0))
;;(setq  forge-topic-list-limit '(100 . -10))


;; GitHub user and organization accounts owned
;; used by @ c f  to create a fork
;; (setq forge-owned-accounts
;;       '(("trovemoney" "tempo"
;;          "frap" "frap"
;;          )))


;; Blacklist specific accounts, over-riding forge-owned-accounts
;; (setq forge-owned-blacklist
;;       '(("bad-hacks" "really-bad-hacks")))
