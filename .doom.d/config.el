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
(setq doom-font (font-spec :family "Adobe Garamond Pro" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)


;; Hide Line numbers
(setq display-line-numbers-type nil)

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


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!


;; set directories for org
(setq
 org_notes "~/OneDrive/3-resources/org-roam"
 public_notes "~/Git-Projects/PersonalBlog/content/notes"
 org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes
 zot_bib "~/OneDrive/3-resources/org-roam/masterLib.bib")

(use-package! md-roam ; load immediately, before org-roam
  :config
  (setq md-roam-file-extension-single "md")) 

    ;you can omit this if md, which is the default.
(use-package org-roam
  :init
  ;; add markdown extension to org-roam-file-extensions list
  (setq org-roam-file-extensions '("org" "md"))
  (setq org-roam-directory org_notes)
  (setq org-roam-title-sources '((mdtitle title mdheadline headline) (mdalias alias)))
      :hook
      (after-init . org-roam-mode)
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n h" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))


(after! org-roam
  (setq org-roam-graph-viewer "/usr/bin/open")
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
- source :: ${ref}"
           :unnarrowed t)))


(after! org-roam
  (set-company-backend! 'org-mode 'company-org-roam)
  (set-company-backend! 'markdown-mode 'company-org-roam))



;;My Roam Capture Templates
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n"
           :unnarrowed t)

          ("b" "book" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n:AUTHOR:\n:MEDIUM: [[file:Books.org][Book]]\n:GENRE:"
           :unnarrowed t)

        ("m" "meeting" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n:PARTICIPANTS:\n* %m-%d-%Y  %H:%M"
         :unnarrowed t))
  )
)


(use-package! dired
  :config
  (defun dired-open-public-notes-dir ()
    "Open and switch to `public-notes-directory'."
    (interactive)
    (require 'ido)
    (dired (ido-expand-directory public_notes))))



(use-package org-roam-server
  :ensure t)


(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory org_notes))

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
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}
- tags ::
- keywords :: ${keywords}
\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
           :unnarrowed t))))

(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir org_notes)
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)


;;epub
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))

