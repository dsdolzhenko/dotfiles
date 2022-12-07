;;; init.el -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Dmitry Dolzhenko <mailbox@dolzhenko.me>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

;;
;; Core
;;

(server-start)

;; Contrib
(add-to-list 'load-path "~/.emacs.d/contrib/")

;; Custom set variables
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; Save Emacs session
(desktop-save-mode 1)

;; Show *scratch* buffer by default
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)

;; Emacs GC
(setq gc-cons-threshold (* 50 1024 1024))

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Native-Compilation
(setq native-comp-async-query-on-exit t)
(setq native-comp-async-report-warnings-errors 'silent)

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Minibuffer completion
(add-to-list 'completion-styles 'substring)
(icomplete-vertical-mode t)

;; Allow typing spaces and question marks in the completion minibuffer
(define-key minibuffer-local-completion-map
            " " 'self-insert-command)
(define-key minibuffer-local-must-match-map
            " " 'self-insert-command)
(define-key minibuffer-local-completion-map
            "?" 'self-insert-command)
(define-key minibuffer-local-must-match-map
            "?" 'self-insert-command)

;; Display the default arguments in minibuffer as ‘[default-arg]’ instead of ‘(default default-arg)’,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Minibuffer.html
(minibuffer-electric-default-mode t)
(setq minibuffer-eldef-shorten-default t)

;; Shortcut to kill the current buffer.
;; Unlike the default, kills the buffer *without confirmation* if it is not modified.
(defun x/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'x/kill-this-buffer)

;;
;; Packages
;;
(require 'package)

(setq package-native-compile t)

;; Define package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Load and activate emacs packages
(package-initialize)

;; Download the ELPA archive description if needed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (message "Install use-package.el")
  (package-refresh-contents)
  (package-install 'use-package))

;; See the doc https://jwiegley.github.io/use-package/keywords/
(require 'use-package)

;; Ensure that all other packages are installed
(setq use-package-always-ensure t)

;; ... and updated
(use-package auto-package-update
  :config (setq auto-package-update-delete-old-versions t
                auto-package-update-interval 7
                auto-package-update-hide-results t)
  :init (auto-package-update-maybe))

;;
;; UI
;;

;; Hide unused UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode -1)

;; Hide frame title and frame icon
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)
(setq frame-resize-pixelwise t)

;; Highlight current line
(global-hl-line-mode t)

;; Don't blink
(blink-cursor-mode -1)

;; Narrow cursor
(set-default 'cursor-type  '(bar . 2))

;; Show whitespaces
(require 'whitespace)
(setq whitespace-style '(face spaces space-mark tabs tab-mark))
(set-face-attribute 'whitespace-space nil :background nil :foreground "#606060")
(global-whitespace-mode t)

;; Default font face
(set-face-attribute 'default nil :font "Fira Code 16")
(set-face-attribute 'variable-pitch nil :font "Fira Code 16")
(set-face-attribute 'fixed-pitch nil :font "Fira Code 16")

(use-package solarized-theme
  :custom
  ;; Avoid all font-size changes
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  (solarized-use-variable-pitch nil)
  (solarized-scale-org-headlines nil)

  ;; Disable unnecessary coloring
  (solarized-emphasize-indicators nil)

  ;; Fix rendering of underlines in modeline
  (x-underline-at-descent-line t)

  ;; Highlight modeline of the current buffer
  (solarized-high-contrast-mode-line t)

  :init (load-theme 'solarized-light t))

;;
;; Editing
;;

;; Newline at the and of a file
(setq require-final-newline t)

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Require final newline
(setq require-final-newline t)

;; Replace selected text by typing
(delete-selection-mode t)

;; Display column number
(column-number-mode t)

;; Disable lock files
(setq create-lockfiles nil)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      backup-by-copying t   ;; Copy all files, don't rename them.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      kept-new-versions 6   ;; Number of newest versions to keep.
      kept-old-versions 2   ;; Number of oldest versions to keep.
      version-control t)    ;; Use version numbers for backups.

;; Translate input sequences into English
(use-package reverse-im
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

;;
;; Development
;;

(use-package magit)

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode .  rainbow-delimiters-mode)))

;;
;; Productivity
;;

(use-package which-key
  :config (which-key-mode))

;;
;; Org
;;
(require 'org)
(require 'org-mac-link)

(setq org-directory "~/Documents/org")

(setq org-agenda-files (list "inbox.org" "agenda.org" "projects.org"))
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo   . " %i %-12:c")
                                 (tags   . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-agenda-window-setup 'only-window)


(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|\\(?:One-off t\\|T\\)ask\\)s\\)")))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-capture-templates
      `(("i" "Inbox" entry (file "inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("m" "Meeting" entry (file+headline "agenda.org" "Future")
         ,(concat "* %? :meeting:\n"
                  "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry (file "notes.org")
         ,(concat "* Note (%a)\n"
                  "/Entered on/ %U\n" "\n" "%?"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(setq org-log-done 'time)

(defun x/org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(defun x/org-save-agenda-buffers ()
  "Save `org-agenda-files' buffers without user confirmation."
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
                         (when (member (buffer-file-name) org-agenda-files)
                           t)))
  (message "Saving org-agenda-files buffers... done"))

(defun x/org-log-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(define-key global-map (kbd "C-c i") 'x/org-capture-inbox)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c g") 'org-mac-link-get-link)

;; Use the full window instead of splitting the current one
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(add-hook 'org-after-todo-state-change-hook #'x/org-log-next-creation-date)

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (x/org-save-agenda-buffers)))
