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
;;(setq doom-font (font-spec :family "Adobe Garamond Pro" :size 18))
(setq doom-font (font-spec :family "Fira Code Light" :size 16)
      doom-variable-pitch-font (font-spec :family "Adobe Garamond Pro"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-day)

;; Hide Line numbers
(setq display-line-numbers-type nil)


(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; Hide the emphasis markup (e.g * / _ )
(setq org-hide-emphasis-markers t)

;; use visual line-mode automattically for org buffers
(add-hook 'org-mode-hook 'visual-line-mode)

;; Create a bullet for lists
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; add tabs with centaur-tabs
;;
(use-package! centaur-tabs
   :config
     (setq centaur-tabs-set-bar 'over
           centaur-tabs-set-icons t
           centaur-tabs-gray-out-icons 'buffer
           centaur-tabs-height 24
           centaur-tabs-set-modified-marker t
           centaur-tabs-modified-marker "•")
     (centaur-tabs-mode t))


;; Hide LF UTF-8 in the bottom bar
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

;; Slightly nicer default buffer names
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; set directories for org
(setq
 org_notes "~/OneDrive/3-resources/org-roam"
 public_notes "~/github/dschapman/my-website/content/notes"
 website "~/github/dschapman/my-website"
 poetry "~/OneDrive/2-areas/poetry"
 org-directory org_notes
 deft-directory org_notes
 zot_bib "~/OneDrive/3-resources/org-roam/masterLib.bib"
 org-journal-dir org_notes)

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)



;; Shortcuts to different locations
(use-package! dired
  :config
  (defun my/dired-open-public-notes-dir ()
    "Open and switch to `public-notes-directory'."
    (interactive)
    (require 'ido)
    (dired (ido-expand-directory public_notes)))
  (defun my/dired-open-website-dir ()
    "Open and switch to `website-directory'."
    (interactive)
    (require 'ido)
    (dired (ido-expand-directory website)))
  (defun my/dired-open-poetry-dir ()
    "Open and switch to `poetry-directory'."
    (interactive)
    (require 'ido)
    (dired (ido-expand-directory poetry))))



(use-package org-roam
  :init
  (setq org-roam-directory org_notes)
  ;; add markdown extension to org-roam-file-extensions list
  (setq org-roam-file-extensions '("org" "md"))
  (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
  :hook
      (after-init . org-roam-mode)
      :bind (:map org-roam-mode-map
              (("C-c n r" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n h" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph)
               ("C-c n i" . org-roam-insert)
               )
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))


(after! org-roam
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+title: ${title}
#+roam_key: ${ref}
#+roam_tags:
- source :: ${ref}"
           :unnarrowed t))))




(set-company-backend! 'org-mode 'company-org-roam)
(set-company-backend! 'markdown-mode 'company-org-roam)

(use-package org-roam-server
  :ensure t
  )

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(use-package! md-roam ; load immediately, before org-roam
  :config
  (setq md-roam-file-extension-single "md"))

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
           :head "#+title: ${title}\n#+created: %u\n#+roam_alias: \n#+roam_tags: \"book\"\n:author:\n:medium: [[file:Books.org][Book]]\n:GENRE:"
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

;; immediate capture
(setq org-roam-capture-immediate-template
      '("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+roam_alias: \n#+roam_tags: \n"
           :unnarrowed t))


(use-package deft
      :after org
      :bind
      ("C-c n d" . deft))
(setq
      deft-recursive t
      deft-use-filter-string-for-filename t
      deft-use-filename-as-title t
      deft-default-extension '("org" "md")
      deft-directory org_notes)


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
;;
;; org noter
(use-package! org-noter
  :after (:any org pdf-view)
  :config
    (setq
    ;; The WM can handle splits
     org-noter-notes-window-location 'horizontal-split
     ;; Please stop opening frames
     org-noter-always-create-frame nil
     ;; I want to see the whole file
     org-noter-hide-other nil
     ;; Everything is relative to the main notes file
     org-noter-notes-search-path (list org_notes)
   )
    )

;; pdf tools
;;
(use-package pdf-tools
   :ensure t
   :config
   (pdf-tools-install))

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;; helm bibtex
(setq
 bibtex-completion-notes-path org_notes
 bibtex-completion-bibliography zot_bib
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )
;; org-ref
;;
(use-package! org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list zot_bib)
         org-ref-bibliography-notes (concat org_notes "/bibnotes.org")
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory org_notes
         org-ref-notes-function 'orb-edit-notes
         ))
;; org roam bibtext
;;
(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+title: ${=key=}: ${title}\n#+roam_key: ${ref}
#+roam_tags:
- keywords :: ${keywords}
\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
           :unnarrowed t))))

(use-package org-journal
      :bind
      (("C-c n j" . org-journal-new-entry)
      ("C-c j n" . org-journal-new-entry)
      ("C-c j s" . org-journal-search)
      ("C-c j f" . org-journal-open-next-entry)
      ("C-c j b" . org-journal-open-previous-entry)
      ("C-c j o" . org-journal-open-current-journal-file))
      :init
      (setq
      org-journal-date-prefix "#+title: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-carryover-items "TODO=\"TODO\"")
)


;;epub
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))

(setq org-pomodoro-play-sounds t
      org-pomodoro-short-break-sound-p t
      org-pomodoro-long-break-sound-p t
      org-pomodoro-short-break-sound (expand-file-name "/System/Library/Sounds/Glass.aiff")
      org-pomodoro-long-break-sound (expand-file-name "/System/Library/Sounds/Glass.aiff")
      org-pomodoro-finished-sound (expand-file-name "/System/Library/Sounds/Glass.aiff"))

(after! org
  (setq org-capture-templates
      '(("t" "Todo" entry (file "~/Onedrive/3-resources/org-roam/todo.org")
         "* TODO %?\n  %i\n  %a"))))


(setq org-agenda-files (list "~/Onedrive/3-resources/org-roam/todo.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((org-roam-directory . "~/github/dschapman/my-website/content/notes/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
