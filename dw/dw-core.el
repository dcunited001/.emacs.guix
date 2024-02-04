;; -*- lexical-binding: t; -*-
;;
;; Copyright © 2023 David Conner
;; Copyright © 2021 David Wilson
;; Copyright © 2014-2022 Henrik Lissner.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

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

;;*** Core Key Bindings

(setup (:pkg which-key)
  (:option which-key-idle-delay 2.0
           which-key-idle-secondary-delay 0.05
           which-key-lighter "│WK")
  (require 'which-key)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(setup (:pkg general))

;;*** Editing Configuration

;; issues with whitespace added into other buffers (idk why)
;; (setup (:pkg ws-butler)
;;   ;; (:hook-into text-mode prog-mode)
;;   (:hook-into text-mode))

(setup (:pkg super-save)
  (:delay)
  (:when-loaded
    (super-save-mode +1)
    (setq super-save-auto-save-when-idle t)))

(setup (:require paren)
  (:option show-paren-style 'mixed)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;;*** Dired

(setup (:pkg all-the-icons-dired :straight t)
  (:option all-the-icons-dired-lighter "│ic☼ns"))
;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

;; (setup (:pkg dired-single :straight t))
;; (setup (:pkg dired-ranger))
(setup (:pkg dired-collapse))

(defun dc/hide-icons-in-guix ()
  ;; hide icons in guix (not interactive)
  (unless (s-equals? "/gnu/store/" (expand-file-name default-directory))
    (all-the-icons-dired-mode 1)))

(setup dired
  (:option dired-listing-switches "-agho --group-directories-first"
           dired-omit-verbose nil
           dired-hide-details-hide-symlink-targets nil
           delete-by-moving-to-trash nil
           dired-dwim-target 'dired-dwim-target-recent

           ;; NOTE: apparently defaults to: "\\`[.]?#\\|\\`[.][.]?\\'" ...
           dired-omit-files (string-join
                             '("^.DS_Store\\'"
                               "^.project\\(?:ile\\)?\\'"
                               "^.\\(svn\\)\\'"
                               "^.ccls-cache\\'"
                               "\\(?:\\.js\\)?\\.meta\\'"
                               "\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")
                             "\\|"))
  (autoload 'dired-omit-mode "dired-x")
  (:hook #'hl-line-mode)
  (:hook #'dc/hide-icons-in-guix))

;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Other-modes.html#Dired
;; (:hook #'turn-on-gnus-dired-mode)

;; TODO: look into dired-collapse (it seems slow)

(defun dc/dired-rainbow-setup ()
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm"
                                        "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json"
                                       "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf"
                                            "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd"
                                            "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb"
                                            "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg"
                                         "mpg" "flv" "ogg" "mov" "mid" "midi" "wav"
                                         "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg"
                                         "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql"
                                               "pgsql" "sql" "r" "clj" "cljs" "scala"
                                               "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++"
                                            "hpp" "hxx" "m" "cc" "cs" "cp" "cpp"
                                            "go" "f" "for" "ftn" "f90" "f95" "f03"
                                            "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz"
                                              "z" "Z" "jar" "war" "ear" "rar" "sar"
                                              "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab"
                                            "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature"
                                             "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow"
                                             "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*"))

(setup (:require dired-rainbow)
  (:with-hook window-setup-hook
    (:hook #'dc/dired-rainbow-setup)))

;;*** Make Help More Helpful

;; (setup (:pkg helpful)
;;   (:option counsel-describe-function-function #'helpful-callable
;;            counsel-describe-variable-function #'helpful-variable)
;;   (:global [remap describe-function] helpful-function
;;            [remap describe-symbol] helpful-symbol
;;            [remap describe-variable] helpful-variable
;;            [remap describe-command] helpful-command
;;            [remap describe-key] helpful-key))

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

(provide 'dw-core)
