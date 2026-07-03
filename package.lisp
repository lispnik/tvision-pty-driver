;;;; package.lisp --- revision-pty-driver: drive a TUI in a pty and assert on the
;;;; reconstructed screen (a Lisp-native alternative to a Python pyte harness).

(defpackage #:revision-pty-driver
  (:nicknames #:rpd)
  (:use #:common-lisp)
  (:export
   ;; terminal emulator
   #:make-terminal #:terminal-feed #:terminal-feed-char #:terminal-line
   #:terminal-text #:terminal-find #:terminal-cols #:terminal-rows
   ;; driver
   #:driver #:launch #:with-driver #:quit-driver
   #:drain #:wait-for #:wait-gone #:screen-text #:line #:found? #:find-text
   #:send #:key #:type-text #:ctrl #:alt #:click #:click-text #:drag #:drag-text
   #:open-menu #:menu-item
   ;; assertions
   #:check #:report #:driver-failures))
