;; -*- no-byte-compile: t; -*-
;;; frap/packages.el

(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("dist" "*.el")))
