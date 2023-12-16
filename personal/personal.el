
(setq
  ;; your full name
  user-full-name ""
  ;; your email address
  user-mail-address ""
  ;; your nickname
  user-nickname ""
  ;; optional data to set a proper location for calendar data used to
  ;; calculate sunrise sunset or lunar values
  ;; calendar-latitude 40.6638800
  ;; calendar-longitude 8.0953700
  ;; calendar-location-name ""
  ;; calendar-time-zone -360)
  ;; calendar-standard-time-zone-name "CST"
  ;; calendar-daylight-time-zone-name "CDT"
  ;; see: https://www.emacswiki.org/emacs/CalendarLocalization
  ;; for localization hints
 )

;; customization of okk
;; used by HunSpell Spellcheckher
;; comma separate list of installed dictionaries
(defvar okk-dict-string "" "Spellcheck dicts string")
;; lisp list of installed dictionaries
(defvar okk-dict-list '("" "" "") "Spellcheck dicts list")
;; optional used here to specify a version controlled dir, as example
;; in my use case for backupy reason I keep some directory managed by Git
;; in a centralized subdirectory, see as example in 'okk-denote-dir'.
(defvar okk-vc-dir "" "User remote directory")
;; This is important, place here the location of the directory containing
;; configuration files, eventually you could use even a lisp function.
;; See as example in 'okk-denote-dir'.
(defvar okk-conf-dir "" "Emacs configuration directory")
;; if you use my template system leave it untouched
(defvar okk-tmpl-dir (file-name-concat okk-conf-dir "templates/"))
;; if you use denote, set it were you want to save the notes
(defvar okk-denote-dir (expand-file-name "Notes/" okk-vc-dir))
;; this is a lisp lisp of your agenda files used by Org.
(defvar okk-agenda-files (list
           (expand-file-name "")
          ))
;; used in 'okk-pers-agenda-open' function definition.
(defvar okk-main-agenda (expand-file-name "" okk-vc-dir))
;; used in 'Preamble' > 'backup' and 'recentf' sections. A lisp list
;; they are used to exclude directories from backup and "recent files" menu subdir.
;; note that directories must be specified without the trailing /
(defvar okk-secret-dirs (list "" ""))
