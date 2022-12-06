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

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Minibuffer completion
(add-to-list 'completion-styles 'substring)
(icomplete-vertical-mode t)

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
