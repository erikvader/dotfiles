(load (second sb-ext:*posix-argv*))

;; platform independent utils
(ql:quickload "uiop")

;; regex
(ql:quickload "cl-ppcre")

;; ->
(ql:quickload "arrows")
(use-package :arrows)

;; utils like compose
(ql:quickload "alexandria")
(sb-ext:add-package-local-nickname :alex :alexandria)

;; pattern matching
(ql:quickload "trivia")

;; nicer loop
(ql:quickload "iterate")
(use-package :iterate)

(sb-ext:save-lisp-and-die (third sb-ext:*posix-argv*))
