;; -*- lexical-binding: t; -*-

;;* Keys

;;** Toggles

;; call without keybind for now
(dc/toggleable-boolean native-comp-async-report-warnings-errors)
(dc/toggleable-boolean custom-buffer-verbose-help)

(general-define-key
 :keymaps 'leader-toggle-map
 "N" #'toggle-native-comp-async-report-warnings-errors)
