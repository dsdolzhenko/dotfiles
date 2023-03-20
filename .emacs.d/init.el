;;; init.el -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Dmitry Dolzhenko <mailbox@dolzhenko.me>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

;;
;; Core
;;

(setq user-full-name "Dmitry Dolzhenko"
      user-mail-address "mailbox@dolzhenko.me")

(server-start)

;; Contrib
(add-to-list 'load-path (concat user-emacs-directory "contrib"))

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

;; Use shift-{left,right,up,down} to move between windows
(windmove-default-keybindings)

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
(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)

;; Do not round frame size to char size (frame-char-height and frame-char-width)
(setq frame-resize-pixelwise t)

;; Highlight current line
(global-hl-line-mode t)

;; Don't blink
(blink-cursor-mode -1)

;; Narrow cursor
(set-default 'cursor-type '(bar . 2))

;; Show whitespaces
(require 'whitespace)
(setq whitespace-style '(face spaces space-mark tabs tab-mark))
(add-hook 'emacs-lisp-mode-hook #'whitespace-mode)

;; Default font face
(set-face-attribute 'default nil :font "Fira Code 16")
(set-face-attribute 'variable-pitch nil :font "Fira Code 16")
(set-face-attribute 'fixed-pitch nil :font "Fira Code 16")

(load-theme 'modus-operandi t)
(set-face-attribute 'whitespace-space nil :background 'unspecified :foreground "#C0C0C0")

(setq window-divider-default-bottom-width 2
      window-divider-default-places 'bottom-only
      window-divider-mode t)

;;
;; Editing
;;

;; Newline at the and of a file
(setq require-final-newline t)

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make kill-visual-line kill the whole line
(defalias 'kill-visual-line 'kill-line)

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

(use-package treemacs
  :defer t
  :custom
  (treemacs-no-png-images t)
  (treemacs-peek-mode t)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-display-current-project-exclusively t)
  :custom-face (treemacs-root-face ((t :underline nil :height (lambda (_) (face-attribute 'default :height))))))

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode .  rainbow-delimiters-mode)))

(require 'eglot)
;; Otherwise my eyes are bleeding
(set-face-attribute 'eglot-mode-line nil
                    :weight 'normal
                    :inherit '())

(use-package typescript-mode)

(use-package rust-mode
  :hook ((rust-mode . eglot-ensure)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;
;; Productivity
;;

(use-package which-key
  :config (which-key-mode))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;;
;; Org
;;
(require 'org)
(require 'org-mac-link)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("▸"))
  :hook ((org-mode . org-bullets-mode)))

(use-package visual-fill-column)

(setq org-directory "~/Documents/org")

(setq org-hide-emphasis-markers t)

(setq org-agenda-files (list "inbox.org" "projects.org"))
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-prefix-format '((agenda . " %i %-15t%-15c")
                                 (todo   . " %i %-15:c %-4e")
                                 (tags   . " %i %-15:c")
                                 (search . " %i %-15:c")))
(setq org-agenda-window-setup 'only-window)

(setq org-stuck-projects '("project" nil ("next") nil))

(setq org-tags-column 0)
(setq org-tags-exclude-from-inheritance '("project"))

(setq org-refile-targets
      '((("projects.org" "incubator.org")
         :maxlevel . 3)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-capture-templates
      `(("i" "Inbox" entry (file "inbox.org")
         ,(concat "* %?\n"
                  ":PROPERTIES:\n"
                  ":ENTERED: %U\n"
                  ":END:\n"))))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "STARTED(s!)" "HOLD(h!)" "|" "DONE(d!)" "CANCELED(c!)")))

;; Log date and time when TODO is marked as DONE or CANCELED
(setq org-log-done 'time)
;; Log TODO state changes into LOGBOOK drawer
(setq org-log-into-drawer t)
;; ... and log them in the rerersed order, newest last
(setq org-log-states-order-reversed t)

;; Use unique ID in the org links
(setq org-id-link-to-org-use-id 'use-existing)

(defun x/org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun x/org-save-agenda-buffers ()
  "Save `org-agenda-files' buffers without user confirmation."
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (let ((org-agenda-files (org-agenda-files)))
    (save-some-buffers t (lambda ()
                           (when (member (buffer-file-name) org-agenda-files)
                             t))))
  (message "Saving org-agenda-files buffers... done"))

(defun x/org-remove-emphasis ()
  (interactive)
  (save-match-data
    (if (and (org-in-regexp org-emph-re 2)
             (not (region-active-p)))
        (let ((beg (match-beginning 3))
              (end (match-end 4)))
          (when (and (>= (point) (1- beg))
                     (<= (point) (1+ end)))
            (save-excursion
              (goto-char end)
              (delete-char 1)
              (goto-char beg)
              (delete-char 1)))))))

(define-key global-map (kbd "C-c i") 'x/org-capture-inbox)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c g") 'org-mac-link-get-link)

(define-key org-mode-map (kbd "M-r") 'x/org-remove-emphasis)

;; Use the full window instead of splitting the current one
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Word-wrap paragraphs in org mode buffers
(add-hook 'org-mode-hook (lambda (&rest _)
                           (setq fill-column 110)
                           (visual-line-mode)
                           (visual-fill-column-mode)
                           (display-fill-column-indicator-mode)))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (x/org-save-agenda-buffers)))

;; Save the buffer after progress indicator(s) update
(advice-add 'org-update-statistics-cookies :after
            (lambda (&rest _)
              (save-buffer)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

;;
;; org-roam
;;
(use-package f)
(use-package emacsql-sqlite-builtin)

(use-package org-roam
  :after f org

  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-database-connector 'sqlite-builtin)

  :custom
  (org-roam-directory org-directory)
  (org-roam-node-display-template "${hierarchy:*} ${tags:10}")
  (org-roam-capture-templates `(("d" "Default" plain
                                 "%?"
                                 :if-new (file+head "notes/%<%Y%m%d%H%M%S>.org" "#+TITLE: ${title}\n")
                                 :unnarrowed t)
                                ("p" "Project" plain
                                 (file ,(concat user-emacs-directory "org-capture-templates/project.org"))
                                 :if-new (file+head "projects/%<%Y%m%d%H%M%S>.org" "#+TITLE: ${title}\n")
                                 :unnarrowed t)
                                ("c" "Person" plain
                                 (file ,(concat user-emacs-directory "org-capture-templates/person.org"))
                                 :if-new (file+head "people/${slug}.org" "#+TITLE: ${title}\n")
                                 :unnarrowed t)
                                ("r" "Reference" plain
                                 "%?"
                                 :if-new (file+head "refs/${slug}.org" "#+TITLE: ${title}\n")
                                 :unnarrowed t)))
  (org-roam-dailies-directory ".")
  (org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %?"
                                         :target (file+datetree "journal.org" day))))

  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  :config
  (require 'org-roam-dailies)
  (require 'f)

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (directories (org-roam-node-directories node))
          (filetitle (org-roam-node-filetitle node)))
      (concat
       (if directories (format "(%s) " directories))
       (if (> level 0) (concat filetitle " > "))
       (if (> level 1) (concat (string-join olp " > ") " > "))
       title)))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (string-join (f-split dirs) "/")
      nil))

  (org-roam-db-autosync-mode))
