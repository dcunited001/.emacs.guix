;; -*- lexical-binding: t; -*-

(require 'subr-x)

;;* Core

;;** System Identification

(defvar dw/is-guix-system (and (eq system-type 'gnu/linux)
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (search-forward "ID=guix" nil t))
                               t))
;;** Config Paths

;;*** No Littering

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:pkg no-littering)
  (require 'no-littering))

;;*** Custom File

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;*** Features

(setq desktop-dirname (file-name-concat no-littering-var-directory "desktop/")
      bookmark-default-file (file-name-concat no-littering-var-directory "bookmarks.el")
      tabspaces-session-file (file-name-concat no-littering-var-directory "tabsession.el"))

;;*** Native Compilation

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;** Editor

;;*** Better Defaults
(setup (:pkg better-defaults))

(setup (:pkg undo-tree)
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

;;*** Core Key Bindings

(setup (:pkg general)
  ;; dw/leader-key-def
  (general-create-definer leader-def
    :prefix "C-c")

  ;; dw/ctrl-c-key
  (general-create-definer local-leader-def
    :prefix "C-c l"))

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))


;;*** Timers

(setup (:pkg tmr))

(defun dw/tmr-mode-line ()
  (if (not (and (boundp 'tmr--timers)
                tmr--timers))
      ""
    (propertize (format " 🕐 %s: %s"
                        (tmr--format-remaining (car tmr--timers))
                        (tmr--timer-description (car tmr--timers)))
                'tab-bar '(:foreground "orange"))))

;;*** Tab Bar Workspaces

(setup (:pkg tabspaces :straight t)
  (tabspaces-mode 1)
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Main"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*")))

(with-eval-after-load 'consult
  ;; Hide full buffer list by default (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)

  ;; Set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(defun dw/switch-tab-buffer (&optional arg)
  (interactive "P")
  (cond
   ((and arg (> (car arg) 0)) (call-interactively #'consult-buffer))
   ((project-current) (call-interactively #'project-switch-to-buffer))
   (t (call-interactively #'consult-buffer))))

(defun dw/set-tab-bar-faces ()
  (let ((color (face-attribute 'doom-modeline-bar :background nil t)))
    (set-face-attribute 'tab-bar-tab nil :foreground nil :background nil :weight 'semi-bold :underline `(:color ,color) :inherit nil)
    (set-face-attribute 'tab-bar nil :font "Iosevka Aile" :foreground nil :inherit 'mode-line)))

(setq tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       dw/tmr-mode-line
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))

(with-eval-after-load 'doom-modeline
  (dw/set-tab-bar-faces)

  (add-to-list 'global-mode-string '(" " display-time-string))
  (add-to-list 'global-mode-string '(" " doom-modeline--battery-status))
  (add-to-list 'global-mode-string '(" " tracking-mode-line-buffers))

  (display-time-mode 1)
  (display-battery-mode 1)

  (setq tab-bar-show t)
  (tab-bar-mode 1)
  (tab-bar-rename-tab "Main"))

;;*** Notifications

(setup (:pkg alert)
  (:option alert-default-style 'notifications))

;;*** Editing Configuration

(setq-default tab-width 2)

(setq-default indent-tabs-mode nil)

(setup (:pkg ws-butler)
  (:hook-into text-mode prog-mode))

(setup (:pkg super-save)
  (:delay)
  (:when-loaded
    (super-save-mode +1)
    (setq super-save-auto-save-when-idle t)))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(setup (:require paren)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setup (:pkg visual-fill-column)
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (:hook-into org-mode))

(setup (:pkg avy)
  (leader-def
    "j"   '(:ignore t :which-key "jump")
    "jj"  '(avy-goto-char :which-key "jump to char")
    "jw"  '(avy-goto-word-0 :which-key "jump to word")
    "jl"  '(avy-goto-line :which-key "jump to line")))

;;*** Window Management

(setup (:pkg ace-window)
  (:global "M-o" ace-window)
  (:option aw-scope 'frame
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-minibuffer-flag t)
  (ace-window-display-mode 1))

(setup winner
  (winner-mode))

;; (setq display-buffer-base-action
;;       '(display-buffer-reuse-mode-window
;;         display-buffer-reuse-window
;;         display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;;*** Dired

(setup (:pkg all-the-icons-dired :straight t))
;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

;; (setup (:pkg dired-single :straight t))
;; (setup (:pkg dired-ranger))
(setup (:pkg dired-collapse))

(setup dired
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash nil
        dired-dwim-target 'dired-dwim-target-recent)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (unless (s-equals? "/gnu/store/" (expand-file-name default-directory))
                (all-the-icons-dired-mode 1))
              (hl-line-mode 1)))

  ;;  (global-set-key (kbd "s-e") #'dired-jump)

  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "h" 'dired-single-up-directory
  ;;   "H" 'dired-omit-mode
  ;;   "l" 'dired-single-buffer
  ;;   "y" 'dired-ranger-copy
  ;;   "X" 'dired-ranger-move
  ;;   "p" 'dired-ranger-paste)
  )

(setup (:pkg dired-rainbow)
  (:load-after dired
   (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
   (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
   (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
   (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
   (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
   (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
   (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
   (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
   (dired-rainbow-define log "#c17d11" ("log"))
   (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
   (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
   (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
   (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
   (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
   (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
   (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
   (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
   (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
   (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
   (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(setup (:pkg openwith)
  (require 'openwith)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "feh"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "zathura"
               '(file)))))

(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("Europe/Athens" "Athens")
    ("America/Los_Angeles" "Seattle")
    ("America/Denver" "Denver")
    ("America/New_York" "New York")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")
    ("Asia/Kolkata" "Hyderabad")))

(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;;*** Save Minibuffer History

(setup savehist
  (setq history-length 25)
  (savehist-mode 1))

;; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25))

;;*** Make Help More Helpful

;; (setup (:pkg helpful)
;;   (:option counsel-describe-function-function #'helpful-callable
;;            counsel-describe-variable-function #'helpful-variable)
;;   (:global [remap describe-function] helpful-function
;;            [remap describe-symbol] helpful-symbol
;;            [remap describe-variable] helpful-variable
;;            [remap describe-command] helpful-command
;;            [remap describe-key] helpful-key))

;; Load the info system for info files
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

;;*** Convenience Key Bindings

(defun dw/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun dw/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

;; (dw/leader-key-def
;;   "fn" '((lambda () (interactive) (counsel-find-file "~/Notes/")) :which-key "notes")
;;   "fd"  '(:ignore t :which-key "dotfiles")
;;   "fdd" '((lambda () (interactive) (find-file "~/.dotfiles/Desktop.org")) :which-key "desktop")
;;   ;; "fdc" '((lambda () (interactive) (find-file (expand-file-name (concat  "~/.dotfiles/daviwil/systems/" system-name ".scm")))) :which-key "system config")
;;   "fdc" '((lambda () (interactive) (find-file (expand-file-name (concat  "~/.dotfiles/.config/guix/systems/" system-name ".scm")))) :which-key "system config")
;;   "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/.emacs.d/init.el"))) :which-key  "edit config")
;;   "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/Emacs.org")) :which-key "edit config")
;;   "fdm" '((lambda () (interactive) (find-file "~/.dotfiles/Mail.org")) :which-key "mail")
;;   "fdM" '((lambda () (interactive) (counsel-find-file "~/.dotfiles/.config/guix/manifests/")) :which-key "manifests")
;;   "fds" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" "Base Configuration")) :which-key "base system")
;;   "fdS" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" system-name)) :which-key "this system")
;;   "fdp" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Desktop.org" "Panel via Polybar")) :which-key "polybar")
;;   "fdw" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Workflow.org"))) :which-key "workflow")
;;   "fdv" '((lambda () (interactive) (find-file "~/.dotfiles/.config/vimb/config")) :which-key "vimb"))

;;*** Start the Daemon

(server-start)

(provide 'dw-core)
