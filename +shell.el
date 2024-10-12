;;; ../../dev/emacs/practicalli-doom/+shell.el -*- lexical-binding: t; -*-

(require 'comint)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)
(setq async-shell-command-buffer 'new-buffer)

(defun path-slug (dir)
  "Returns the initials of `dir`s path,
with the last part appended fully

Example:

(path-slug \"/foo/bar/hello\")
=> \"f/b/hello\" "
  (let* ((path (replace-regexp-in-string "\\." "" dir))
	 (path (split-string path "/" t))
	 (path-s (mapconcat
		  (lambda (it)
		    (cl-subseq it 0 1))
		  (nbutlast (copy-sequence path) 1)
		  "/"))
	 (path-s (concat
		  path-s
		  "/"
		  (car (last path)))))
    path-s))

(defun mm/put-command-in-async-buff-name (f &rest args)
  (let* ((path-s (path-slug default-directory))
	 (command (car args))
	 (buffname (concat path-s " " command))
	 (shell-command-buffer-name-async
	  (format
	   "*async-shell-command %s*"
	   (string-trim
	    (substring buffname 0 (min (length buffname) 50))))))
    (apply f args)))

(advice-add 'shell-command :around #'mm/put-command-in-async-buff-name)

(add-hook 'comint-mode-hook
	  (defun mm/do-hack-dir-locals (&rest _)
	    (hack-dir-local-variables-non-file-buffer)))

(advice-add #'start-process-shell-command :before #'mm/do-hack-dir-locals)

(advice-add 'compile :filter-args
	    (defun mm/always-use-comint-for-compile (args) `(,(car args) t)))

(defun mm/shell-command-on-file (command)
  "Execute COMMAND asynchronously on the current file."
  (interactive (list (read-shell-command
                      (concat "Async shell command on " (buffer-name) ": "))))
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (async-shell-command (concat command " " filename))))

(defun mememacs/eval-last-sexp-dwim (arg)
  "Eval last sexp.
If it is a quoted symbol, eval symbol value instead.
See `eval-last-sexp'."
  (interactive "P")
  (let ((s (sexp-at-point)))
    (if (eq 'quote (car-safe s))
	(with-temp-buffer
	  (insert
	   (with-output-to-string
	     (print (cadr s))))
	  (goto-char (point-max))
	  (eval-last-sexp arg))
      (eval-last-sexp arg))))

(use-package bash-completion
  :init (autoload
          'bash-completion-dynamic-complete
          "bash-completion"
          "BASH completion hook")
  (add-hook
   'shell-dynamic-complete-functions
   #'bash-completion-dynamic-complete)
  :config
  (defun bash-completion-capf-1 (bol)
    (bash-completion-dynamic-complete-nocomint (funcall bol) (point) t))
  (defun bash-completion-eshell-capf ()
    (bash-completion-capf-1 (lambda () (save-excursion (eshell-bol) (point)))))
  (defun bash-completion-capf ()
    (bash-completion-capf-1 #'point-at-bol))
  (add-hook
   'sh-mode-hook
   (defun mm/add-bash-completion ()
     (add-hook 'completion-at-point-functions #'bash-completion-capf nil t))))

(defun mememacs/create-script* (file bang setup)
  (find-file file)
  (insert bang)
  (save-buffer)
  (set-file-modes file #o751)
  (funcall setup))

(defun mememacs/create-script (file)
  (interactive "Fnew script: ")
  (mememacs/create-script*
   file
   "#!/bin/sh\n"
   #'shell-script-mode))

(defun mememacs/create-bb-script (file)
  (interactive "Fnew bb: ")
  (mememacs/create-script*
   file
   "#!/usr/bin/env bb\n"
   #'clojure-mode))

(defun mememacs/kill-dangling-buffs (&rest args)
  "Kill all buffers that are connected to a file,
where the file does not exist."
  (interactive)
  (let ((bfs (cl-loop for b in (buffer-list)
		      for f = (buffer-file-name b)
		      when (and f (not (file-exists-p f)))
		      collect b)))
    (when bfs
      (message
       "Kill %d buffers"
       (length bfs)))
    (mapc #'kill-buffer bfs)))

(dolist (fn '(dired-internal-do-deletions
	      dired-do-rename
	      dired-do-rename-regexp))
  (advice-add fn :after #'mememacs/kill-dangling-buffs))
