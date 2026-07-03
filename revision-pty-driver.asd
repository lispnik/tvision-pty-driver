;;;; revision-pty-driver.asd

(asdf:defsystem #:revision-pty-driver
  :description "Drive a terminal-UI binary through a pseudo-terminal and assert on
the reconstructed screen -- a Lisp-native alternative to a Python pyte harness.
SBCL-only (uses RUN-PROGRAM :PTY and SB-UNICODE)."
  :author "Matthew Kennedy"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "terminal")
               (:file "driver")))
