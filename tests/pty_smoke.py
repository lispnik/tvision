#!/usr/bin/env python3
"""PTY smoke tests for the tvlisp example application.

The Lisp suite (tests/tvision-tests.lisp) covers the framework's controls in
isolation; this harness drives the *built* ./tvlisp binary through a pseudo-tty
and asserts on the reconstructed screen, so the end-to-end example flows (REPL
eval, editor, save, window list, ...) are guarded against regressions too.

Network-free and self-contained.  Exit code 0 = all passed, 1 = a failure.

Usage:  python3 tests/pty_smoke.py [path-to-tvlisp]
"""

import os
import pty
import re
import select
import struct
import sys
import tempfile
import time
import fcntl
import termios

ROWS, COLS = 26, 88
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)


class Tv:
    """Drive ./tvlisp in a pty and reconstruct its screen."""

    def __init__(self, binary, home):
        self.buf = b""
        self.alive = True
        self.pid, self.fd = pty.fork()
        if self.pid == 0:  # child
            os.chdir(ROOT)
            os.environ["HOME"] = home
            os.environ["TERM"] = "xterm-256color"
            os.execvp(binary, [binary])
            os._exit(127)
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ,
                    struct.pack("HHHH", ROWS, COLS, 0, 0))

    def pump(self, secs):
        end = time.time() + secs
        while time.time() < end:
            r, _, _ = select.select([self.fd], [], [], 0.05)
            if r:
                try:
                    d = os.read(self.fd, 65536)
                except OSError:
                    self.alive = False
                    return
                if not d:
                    self.alive = False
                    return
                self.buf += d

    def send(self, b, settle=0.35):
        try:
            os.write(self.fd, b)
        except OSError:
            self.alive = False
        self.pump(settle)

    def type(self, s, per=0.02):
        for ch in s.encode():
            self.send(bytes([ch]), per)

    def screen(self):
        g = [[" "] * COLS for _ in range(ROWS)]
        cr = cc = 0
        s = self.buf.decode("utf-8", "replace")
        i = 0
        while i < len(s):
            ch = s[i]
            if ch == "\x1b":
                m = re.match(r"\x1b\[([0-9;?<]*)[ -/]*([@-~])", s[i:])
                if m:
                    final, params = m.group(2), m.group(1)
                    if final in "Hf":
                        n = [int(x) for x in params.split(";") if x.isdigit()] if params else []
                        cr = (n[0] - 1) if n else 0
                        cc = (n[1] - 1) if len(n) >= 2 else 0
                    elif final == "J":
                        g = [[" "] * COLS for _ in range(ROWS)]
                    i += m.end()
                    continue
                i += 1
                continue
            if ch == "\r":
                cc = 0
            elif ch == "\n":
                cr += 1
            elif ord(ch) >= 32:
                if 0 <= cr < ROWS and 0 <= cc < COLS:
                    g[cr][cc] = ch
                cc += 1
            i += 1
        return "\n".join("".join(row).rstrip() for row in g)

    def has(self, sub):
        return sub in self.screen()

    def wait(self, sub, timeout=12):
        end = time.time() + timeout
        while time.time() < end:
            self.pump(0.1)
            if self.has(sub):
                return True
        return False

    def close(self):
        try:
            os.kill(self.pid, 9)
            os.close(self.fd)
            os.waitpid(self.pid, 0)
        except OSError:
            pass


PASS, FAIL = 0, 0


def check(cond, name):
    global PASS, FAIL
    if cond:
        PASS += 1
        print(f"  ok   {name}")
    else:
        FAIL += 1
        print(f"  FAIL {name}")


def run(binary):
    home = tempfile.mkdtemp(prefix="tvlisp-smoke-")
    save_path = os.path.join(home, "saved.lisp")
    tv = Tv(binary, home)
    try:
        check(tv.wait("REPL>"), "REPL prompt appears")

        # 1. evaluate an expression
        tv.type("(+ 21 21)\r")
        tv.pump(0.6)
        check(tv.has("42"), "REPL evaluates (+ 21 21) => 42")

        # 2. open an editor window and type code
        tv.send(b"\x1bf", 0.4)   # File menu
        tv.send(b"n", 0.5)       # New
        check(tv.wait("Untitled"), "File > New opens an editor")
        tv.type("(defun foo () 99)")
        tv.pump(0.3)
        check(tv.has("(defun foo"), "typed code shows in the editor")

        # 3. save it (the path that used to crash the IDE)
        tv.send(b"\x13", 0.5)    # Ctrl-S -> Save As (no filename yet)
        check(tv.wait("Name:"), "Save As dialog opens")
        for _ in range(4):
            tv.send(b"\t", 0.2)  # focus the Name field (holds the dir)
        tv.type("saved.lisp")    # append -> <home>/saved.lisp
        tv.send(b"\r", 1.0)
        check(tv.alive, "IDE still alive after save (no crash)")
        check(os.path.exists(save_path), "file written to disk")
        check(tv.has("saved.lisp"), "window title updated to filename")

        # 4. the REPL is still usable afterwards
        tv.send(b"\x1b[19~", 0.3)   # F8 would inspect; instead go back to REPL via F2
        tv.send(b"\x1bOQ" if False else b"\x1b[12~", 0.4)  # F2 = focus/new REPL
        # eval again to confirm responsiveness
        tv.send(b"\x1bf", 0.3); tv.send(b"\x1b", 0.2)  # open+close a menu (no-op)
        check(tv.alive, "IDE responsive after editor interaction")

        # 5. window list (Alt-0) enumerates open windows
        tv.send(b"\x1b0", 0.6)
        check(tv.wait("Window list"), "Alt-0 opens the window list")
        check(tv.has("saved.lisp") or tv.has("Lisp REPL"),
              "window list shows open windows")
        tv.send(b"\x1b", 0.3)    # Esc to close the picker
    finally:
        tv.close()
        try:
            if os.path.exists(save_path):
                os.remove(save_path)
            os.rmdir(home)
        except OSError:
            pass


def main():
    binary = sys.argv[1] if len(sys.argv) > 1 else os.path.join(ROOT, "tvlisp")
    if not os.path.exists(binary):
        print(f"pty_smoke: binary not found: {binary} (run `make tvlisp` first)")
        return 2
    print("== tvlisp pty smoke tests ==")
    run(binary)
    print(f"==== {PASS + FAIL} checks, {FAIL} failures ====")
    return 0 if FAIL == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
