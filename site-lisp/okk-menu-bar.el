;; -**- mode: org; fill-column: 78; lexical-binding: t -**-
;;; okk-menu-bar.el --- Extensions to `menu-bar.el'.
;;
;; Filename: okk-menu-bar.el
;; Description: Extensions to `menu-bar.el'.
;; Author: Carlo Dormeletti
;; Maintainer: Carlo Dormeletti (concat "carlo.dormeletti" "@" "gmail" ".com")
;; Copyright (C) 2023, Carlo Dormeletti, all rights reserved.
;; Version: 0
;; Package-Requires: ()
;; Keywords: internal, local, convenience
;; Compatibility: GNU Emacs: 29.1
;;
;;  Usage:
;;
;;    This library should be loaded after loading standard library
;;    `menu-bar.el'.  So, in your `~/.emacs' file, do this:
;;
;;      (eval-after-load "menu-bar" '(require 'okk-menu-bar))
;;
;;    You will also want to do that before loading other libraries
;;    that might modify the following predefined menu-bar menus:
;;
;;      `File'
;;      `Edit'
;;      `Options'
;;      `Search'
;;
;;    This is because those menus correspond to the variables
;;    mentioned at the end of this commentary as being REDEFINED here.
;;    If a library modifies one of those variables before you load
;;    `okk-menu-bar.el' then those changes will be lost when the variable
;;    is redefined.
;;
;;  Main differences:
;;
;;    1. Added Menu: "Search"
;;    2. Modified Menus: "File", "Edit"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; avoid to show CUA mode menu in Menu > Options
(global-unset-key [menu-bar options cua-mode])

;; modifications to "File" menu

(bind-key [menu-bar file new-file]
  `(menu-item "New File..." okk-new-empty-buffer
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Create a new blank buffer"
     :key-sequence ,(kbd "C-w f n"))
 )

(bind-key [menu-bar file open-file]
  `(menu-item "Open File..." find-file
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Read an existing or new file from disk"
     :key-sequence ,(kbd "C-w f o"))
 )

(bind-key [menu-bar file project-open-file]
  `(menu-item "Open File in Project..." project-find-file
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Read exising file in project to a buffer"
     :keys "C-w p f")
 )

(bind-key [menu-bar file dired]
  `(menu-item "Open Directory..." dired
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Browse a directory, to operate on its files"
     :keys "C-w d")
 )

(bind-key [menu-bar file project-dired]
  `(menu-item "Open Directory..." dired
    :enable (menu-bar-non-minibuffer-window-p)
    :help "Browse a directory, to operate on its files"
    :keys "C-x p D")
 )

(bind-key [menu-bar file insert-file]
  `(menu-item "Insert File..." insert-file
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Insert another file into current buffer"
     :keys "C-w f i")
 )

(bind-key [menu-bar file save-buffer]
  `(menu-item "Save" save-buffer
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Save buffer to disk"
     :keys "C-w f s")
 )

(bind-key [menu-bar file write-file]
  `(menu-item "Save as..." write-file
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Write buffer to a specified file"
     :keys "C-w f w")
 )


(bind-key [menu-bar file kill-buffer]
  `(menu-item "Close" kill-this-buffer
      :enable (menu-bar-non-minibuffer-window-p)
      :help "Kill the current buffer"
      :keys "C-w f c")
  )


(bind-key [menu-bar file revert-buffer]
  `(menu-item "Revert Buffer" revert-buffer
      :enable (menu-bar-non-minibuffer-window-p)
      :help "Re-read current buffer from its file"
      :keys "C-w f r")
  )


;; Emacs have no "Search" menu all is put Under "Edit" menu
;; some redefinition are needed

;; make shortcuts correctly appear in Menu-Bar > Edit
(global-set-key (kbd "C-/") nil)
(global-set-key (kbd "C-_") nil)
(global-set-key (kbd "C-=") nil)
(global-set-key (kbd "C-M-_") nil)
(global-set-key (kbd "C-?") nil)
(global-set-key (kbd "M-_") nil)
;; Replace
(global-set-key (kbd "M-%") nil)
(global-set-key (kbd "C-M-%") nil)

(global-unset-key [menu-bar edit search])
(global-unset-key [menu-bar edit separator-search])
(global-unset-key [menu-bar edit i-search])
(global-unset-key [menu-bar edit replace])

;; (global-unset-key [menu-bar edit goto])
;; (global-unset-key [menu-bar edit bookmark])
;; (global-unset-key [menu-bar edit separator-bookmark]))


;; New "Search" menu, after "Edit"

(defconst menu-bar-search-menu (make-sparse-keymap "Search"))

;; menu are defined in backward order, last item will go in first place.
;; so start from the end.

(define-key menu-bar-search-menu [replace-in-project]
  `(menu-item "Replace in project RegExp..." project-query-replace-regexp
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Query-replace REGEXP in all the files of the project"
     :keys "C-w s p")
  )

(define-key menu-bar-search-menu [sep-project] '("--"))

(define-key menu-bar-search-menu [replace-regexp]
  `(menu-item "Replace RegExp..." query-replace-regexp
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Replace regular expression interactively, ask for about each occurrences"
     :keys "C-w s r")
  )

(define-key menu-bar-search-menu [replace-string]
  `(menu-item "Replace String..." query-replace
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Replace strings interactively, ask for about each occurrences"
     :keys "C-w s q")
  )

(define-key menu-bar-search-menu [sep-replace] '("--"))

(define-key menu-bar-search-menu [search-fwd-symb-ap]
  '(menu-item "Forward Symbol at point" isearch-forward-symbol-at-point
     :help "Do i-search forward for a symbol found near point"
     :keys "M-s .")
  )

(define-key menu-bar-search-menu [search-fwd-symb]
  '(menu-item "Forward Symbol" isearch-forward-symbol
     :help "Do i-search forward for a symbol"
     :keys "M-s _")
  )

(define-key menu-bar-search-menu [search-fwd-word]
  '(menu-item "Forward word" isearch-forward-word
     :help "Do i-search forward for a word"
     :keys "M-s w")
  )

(define-key menu-bar-search-menu [search-regexp-backward]
  '(menu-item "RegExp backward" isearch-backward-regexp
     :help "Do i-search backward for a regular expression"
     :keys "C-M-r")
  )

(define-key menu-bar-search-menu [search-regexp-forward]
  '(menu-item "RegExp forward" isearch-forward-regexp
     :help "Do i-search forward for a regular expression"
     :keys "C-M-s")
  )

(define-key menu-bar-search-menu [sep-regexp] '("--"))

(define-key menu-bar-search-menu [search-backward]
  '(menu-item "Search backward" isearch-backward
     :help "Do i-search backward"
     :keys "C-r")
  )

(define-key menu-bar-search-menu [search-forward]
  '(menu-item "Search forward" isearch-forward
     :help "Do i-search forward"
     :keys "C-s")
  )

;; Menu Edit to make a proper order, we have to redefine the original menu
;;
;; WARNING may be buggy. I've not managed to use 'keymap-set-after'.
;; so here I'm using the low-level function 'define-key-after'

;; avoid undo redo appear in first places
(global-unset-key [menu-bar edit undo])
(global-unset-key [menu-bar edit undo-redo])


(define-key-after (lookup-key global-map [menu-bar edit])
  [sep-cut]
  '(menu-item "---")
  'mark-whole-buffer
  )


(define-key-after (lookup-key global-map [menu-bar edit])
  [undo]
  '(menu-item "Undo" undo-fu-only-undo
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Undo fu only undo"
     :keys "C-z")
  'mark-whole-buffer
  )

(define-key-after (lookup-key global-map [menu-bar edit])
  [redo]
  '(menu-item "Redo" undo-fu-only-redo
     :enable (menu-bar-non-minibuffer-window-p)
     :help "Undo fu only redo"
     :keys "C-y")
  'undo
  )

;; stock undo-redo
(define-key-after (lookup-key global-map [menu-bar edit])
  [undo-redo]
  '(menu-item "Undo-Redo" undo-redo
     :enable (and (not buffer-read-only)
               (undo--last-change-was-undo-p buffer-undo-list))
     :help "Redo last undone edits (Emacs undo-redo)"
     :keys "C-S-z")
  'redo)


(define-key-after (lookup-key global-map [menu-bar])
  [search]
  (cons "Search" menu-bar-search-menu) 'edit)


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'okk-menu-bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; okk-menu-bar.el ends here
