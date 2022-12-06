;;; early-init.el -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Dmitry Dolzhenko <mailbox@dolzhenko.me>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(defun x-use-bash-path ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                          ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(x-use-bash-path)
