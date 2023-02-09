;; -*- lexical-binding: t; -*-

;;* Mouse
;; (general-)

;;** TTY
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
(defvar mouse-wheel-down-event nil)
(defvar mouse-wheel-up-event nil)

(provide 'dc-mouse)
