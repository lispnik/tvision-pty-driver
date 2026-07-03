# tvision-pty-driver

A small **Lisp-native** harness for testing terminal UIs: it launches a TUI
binary in a pseudo-terminal, reconstructs the screen it paints, and lets a test
send keystrokes / mouse clicks and assert on what appears. It's the Common Lisp
answer to a Python `pyte` harness — SBCL only (uses `run-program :pty` and
`sb-unicode`, no CFFI, no external deps).

Built for the `tv2` / `tvlisp` project, but binary-agnostic.

## Why

Unit tests exercise logic in isolation; they can't catch a broken menu layout, a
misplaced cursor, a scrollbar that stops updating, or a click that no longer
inspects the thing under it. This drives the *actual built binary* through a pty
and checks the reconstructed grid, so end-to-end flows are guarded too.

The key to non-flaky tests is **`wait-for`**: it polls the screen until the
expected text appears (or a timeout), instead of guessing fixed sleeps.

## What's inside

- **`terminal.lisp`** — a small VT/ANSI screen emulator: absolute cursor moves
  (`CSI H/f`), erases (`J`/`K`), CR/LF, and printable text placed into a fixed
  grid with East-Asian **display width** (a wide CJK/emoji glyph occupies two
  cells; its continuation cell is skipped when reading text back, and column
  lookups still report the on-screen position). SGR colours and private modes
  (alt-screen, cursor, mouse) are parsed and ignored.
- **`driver.lisp`** — launch a binary in a pty at a fixed size (`HOME` redirected
  to a temp dir so persisted UI state can't leak between runs), then:
  - `wait-for` / `wait-gone` — poll until text appears / disappears
  - `key` (named keys), `ctrl`, `alt`, `type-text`, `send`
  - `click` / `click-text` — SGR-1006 mouse
  - `open-menu` / `menu-item` — menu navigation
  - `check` / `report` — record assertions, exit non-zero on any failure

## Usage

```lisp
(asdf:load-system :tvision-pty-driver)
(in-package :tvision-pty-driver)

(let ((d (launch "/path/to/my-tui" :cols 100 :rows 30)))
  (unwind-protect
       (progn
         (check d "menu bar" (wait-for d "File"))
         (open-menu d #\f)                    ; Alt-F
         (menu-item d "Open")                 ; click a menu item by label
         (type-text d "hello") (key d "enter")
         (check d "typed" (found? d "hello")))
    (quit-driver d))
  (sb-ext:exit :code (report d)))
```

See `tvlisp/tests/pty_smoke_tv2.lisp` for a full 46-check end-to-end suite.
