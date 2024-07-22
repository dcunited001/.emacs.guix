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

;; Use no-littering to automatically set common paths to the new user-emacs-directory

;; Set the right directory to store the native comp cache
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;** Editor

;;*** Editing Configuration

;; issues with whitespace added into other buffers (idk why)
(use-package ws-butler :straight t
  :hook
  ((text-mode prog-mode) . ws-butler-mode))

;; (use-package super-save
;;   super-save-mode +1
  
;;   (:delay)
;;   (:when-loaded
;;     (super-save-mode +1)
;;     (setq super-save-auto-save-when-idle t)))

;; TODO: face (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")

(use-package paren :straight (:type built-in)
  :demand t
  :init
  (setq show-paren-style 'mixed)
  :config
  (show-paren-mode 1))

;;*** Dired

(use-package dired :straight (:type built-in))
(use-package dired-collapse :straight t)

;; TODO: GNUS dired mode

;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Other-modes.html#Dired
;; (:hook #'turn-on-gnus-dired-mode)

;;**** Dired Rainbow

;; TODO: customize dired rainbow

;; + pick different colors. one main problem is that subsequent invoations of
;; the macro continue to append to `dired-rainbow-ext-to-face', a nonessential
;; var for config

;; + implement custom font-lock matchers: [[help:font-lock-keywords]]

;; + face-remapping-alist also possible?

;; + make cohesive with ef-themes.

;; + see [[info:elisp#Face Attributes][elisp#Face Attributes]]

;; + also [[info:elisp#Displaying Faces][elisp#Displaying Faces]] for info
;;   on composition and order of application

(defun dc/dired-rainbow-setup ()

  ;; + can use a regexp/string for files (possibly useful as temp rules
  ;;   "face-remapping-alist"

  ;; + can spec a color or (:keywords "for a face") *

  ;; +  can pass a 4th arg HOW (describing composition)

  ;; ---------------------------------------------
  ;; CHMODS
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")

  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")

  ;; ---------------------------------------------
  ;; WEB
  ;; ---------------------------------------------

  (dired-rainbow-define
   fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))

  (dired-rainbow-define
   html "#667788" ("css" "less" "sass" "scss" "htm" "astro"
                   "html" "jhtm" "mht" "eml" "mustache" "xhtml"))

  (dired-rainbow-define
   markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "mdx"
                       "nfo" "pod" "rst" "tex" "textfile" "txt"))

  ;; ---------------------------------------------
  ;; DATA
  ;; ---------------------------------------------

  (dired-rainbow-define
   xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json"
                  "msg" "pgn" "rss" "yaml" "yml" "rdata"))

  (dired-rainbow-define
   database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb"
                       "sqlite" "nc"))
  ;; ---------------------------------------------
  ;; LANGS
  ;; ---------------------------------------------

  (dired-rainbow-define
   interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql"
                          "pgsql" "sql" "r" "clj" "cljs" "scala"
                          "js"))

  (dired-rainbow-define
   compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++"
                       "hpp" "hxx" "m" "cc" "cs" "cp" "cpp"
                       "go" "f" "for" "ftn" "f90" "f95" "f03"
                       "f08" "s" "rs" "hi" "hs" "pyc" ".java"))

  ;; ---------------------------------------------
  ;; LINUX & SYSTEMS
  ;; ---------------------------------------------

  (dired-rainbow-define
   packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab"
                       "pak" "pk3" "vdf" "vpk" "bsp"))

  (dired-rainbow-define
   vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules" ".repo"))

  (dired-rainbow-define
   shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))

  (dired-rainbow-define
   log "#c17d11" ("log"))

  (dired-rainbow-define
   executable "#8cc4ff" ("exe" "msi"))

  ;; ---------------------------------------------
  ;; FILE SYSTEM
  ;; ---------------------------------------------

  (dired-rainbow-define
   partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow"
                        "toast" "vcd" "vmdk" "bak"))

  (dired-rainbow-define
   encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature"
                        "sig" "p12" "pem"))

  (dired-rainbow-define
   compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz"
                         "z" "Z" "jar" "war" "ear" "rar" "sar"
                         "xpi" "apk" "xz" "tar"))

  (dired-rainbow-define
   image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg"
                    "png" "psd" "eps" "svg"))

  (dired-rainbow-define
   document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf"
                       "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))

  (dired-rainbow-define
   media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg"
                    "mpg" "flv" "ogg" "mov" "mid" "midi" "wav"
                    "aiff" "flac")))

(use-package dired-rainbow :straight t
  :hook
  (window-setup-hook #'dc/dired-rainbow-setup))

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
