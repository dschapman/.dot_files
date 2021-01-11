;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Daniel Chapman"
      user-mail-address "daniel@dschapman.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Fira Code Light" :size 16)
      doom-variable-pitch-font (font-spec :family "Helvetica Neue" :weight 'light)
      doom-unicode-font (font-spec :family "Apple Color Emoji"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-day)

;; set modifed files to yellow
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

;; use super-save for auto saves
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)

;; delete files to trash
(setq-default delete-by-moving-to-trash t)

;;better default buffer names
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; put time in mode line
(display-time-mode 1)

(if (equal "Battery status not available"
           (battery))
  (display-battery-mode 1)                        ; On laptops it's nice to know how much power you have
  (setq password-cache-expiry nil))               ; I can trust my desktops ... can't I? (no battery = desktop)

;; make VLF load incrementally.
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/org/"
      org_notes "~/OneDrive/org/org-roam")

;; enable org-roam
(use-package org-roam
  :init
  (setq org-roam-directory org_notes))

(setq org-roam-dailies-directory "journals/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "journals/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))

;;My Roam Capture Templates
(setq org-roam-capture-templates
        '(
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+roam_alias: \n#+roam_tags: \n"
           :unnarrowed t)

          ("b" "book" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+roam_alias: \n#+roam_tags: \"book\"\n- author ::\n- medium :: [[file:Books.org][Book]]\n- genre ::"
           :unnarrowed t)
          ("B" "book of the bible" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %uU\n#+roam_alias: \n#+roam_tags: \"bible\"\n[[file:20200522110628-the_bible.org][The Bible]]\n"
           :unnarrowed t
           :immediate-finish t)

        ("m" "meeting" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n:participants:\n* %<%m-%d-%Y  %H:%M>"
         :unnarrowed t
         :immediate-finish t))
  )


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Add Bindings
;; Org-Roam-Dailies shortcuts
(map! :leader
      (:prefix-map ("j" . "journal")
       :desc "Capture new journal entry" "n" #'org-roam-dailies-capture-today
       :desc "Go to today's journal entry" "t" #'org-roam-dailies-find-today
       :desc "Go to yesterday's journal entry" "y" #'org-roam-dailies-find-yesterday
       :desc "Go to tomorrow's journal entry" "o" #'org-roam-dailies-find-tomorrow
       :desc "Find date" "f" #'org-roam-dailies-find-date))
(use-package org-chef
  :ensure t)
;; org-chef capture template
(setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/OneDrive/org/org-roam/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/OneDrive/org/org-roam/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))



;; Allows monospace font doom-font and doom-variable-pitch-font to coexist
(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))


;; Allows you to refile into different files - specifically to
;; create new 'parent' headings
;; create 'new' parent headings
(setq org-refile-use-outline-path 'file)
;; makes org-refile outline working with helm/ivy
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
;;
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))

;; deft
;;
(setq deft-extensions '("org"))
(setq deft-directory org-directory)
(setq deft-recursive t)
