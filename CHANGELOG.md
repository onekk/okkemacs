# ChangeLog #

  * **2023-11-28** First version posted on GitHub.
  * **2023-12-06** Some tweaks around, some text corrections.
  * **2023-12-07** Major improvements.
    - Deleted "C-d s" map, as it is unreachable in org-mode, moved functions to
      "C-d o" keymap
    - Spellchecking improved to make it work decently in org-mode.
    - Improved some documentation.
  * **2023-12-08** Minor tweakings.
    - Improved documentation in config.org.
    - Moved some data in keymap.org.
  * **2023-12-12** Minor tweakings.
    - Added in "Preamble" a way to set calendar data style. 
    - Added a custom section at the end of the file prior to "Final steps"
      to permit loading custom packages.
    - Modified "okk custom menu" added a link to 'user-emacs-directory' to
      ease editing of configuration files, as the directory is excluded in
        'recentf' setup.
  * **2023-12-13** Minor tweakings.
    - Added CHANGELOG.md
    - Added an explicit "markdown-mode" heading
    - Added an "ediff" settings
    - Improved "elisp" code
    - Unbind "C-;" in flyspell-mode-map as it conflicts with comment code keybingding.
  * **2023-12-16** Major improvements.
    - New menu definitions in a package **okk-menu-bar.el** placed in **site-lisp**.
    - New menu **Search** and altered **Edit** menu to resemble more "mundane editors".
    - Changed  in **personal.el** variable **okk-backup-exclude** to **okk-secret-dirs**.
    - Other tweaking in elisp code in **config.org** .
  * **2023-12-18** Added some packages, various little improvements.
    - Added **embark** to improve interaction.
    - Deleted **multicursor** as I've not find it easy to use.
    - Minor elisp code improvements.
  * **2023-12-22** Major improvements:
    - Corrected some error in lisp code.
    - Added a function in **Org** to easily insert code block.
