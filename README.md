# Turbo Vision for Common Lisp

A port of Borland's [Turbo Vision](https://en.wikipedia.org/wiki/Turbo_Vision)
character-mode UI framework to Common Lisp (SBCL).  It gives you overlapping
movable windows, dialogs, controls, a mouse-aware event system and a DOS-style
colour/palette model — all rendered with ANSI escape sequences in any modern
terminal.  Views draw in the classic 4-bit palette, but the renderer resolves
it through a **24-bit RGB theme** and matches the terminal automatically
(true-colour → xterm-256 → 16-colour), so colours are exact and themeable — and
a view can also paint **arbitrary per-cell true colour** (`make-rgb`) when it
wants a gradient or image.

```
▒▒▒▒╔═[×]════════════ Window 1 ════════════[↑]═╗▒▒▒▒▒
▒▒▒▒║  This is window number 1.                ║▒▒▒▒▒
▒▒▒▒║  Drag the title bar to move me.          ║▒▒▒▒▒
▒▒▒▒║      Greet           About               ║▒▒▒▒▒
▒▒▒▒╚══════════════════════════════════════════╝▒▒▒▒▒
 Alt-X Exit  F2 New  F3 About  F4 Greet  F5 Tile  ...
```

![True-colour rendering: exact VGA palette and live theme switching](media/truecolor.gif)

![Arbitrary per-cell 24-bit colour: a hue × brightness gradient](media/truecolor-gradient.gif)

![Switching colour themes live: VGA → Modern → green & amber phosphor](media/color-theme.gif)

## Requirements

* [SBCL](http://www.sbcl.org/)
* A POSIX terminal with `stty` (macOS / Linux)
* No external Lisp libraries to **build or run** — the framework depends only on
  SBCL itself.  The threaded REPL / debugger / tooling use SBCL's own facilities
  (`sb-thread`, `sb-mop`, `sb-di`, and the `sb-introspect` contrib, all bundled
  with SBCL).
* Running the **test suite** additionally needs [FiveAM](https://github.com/lispci/fiveam)
  (a test-only dependency).  It is pinned in `systems.csv`, so `ocicl` restores
  it (and its deps) on a fresh checkout; with Quicklisp use `ql:quickload :fiveam`.

The project is structured to be loadable through [ocicl](https://github.com/ocicl/ocicl):
the current directory is on the ASDF source registry (configured by ocicl in
`~/.sbclrc`), so `(asdf:load-system :tvision)` just works.  Because there are no
third-party dependencies there is nothing to `ocicl install`; `systems.csv` is
kept as a placeholder for any dependencies you add later.

## Example application — tvlisp

The framework's flagship example, **`tvlisp`** — a Lisp REPL / mini-IDE that
exercises the whole framework (overlapping windows, menus, dialogs, the editor,
the object inspector, an HTML browser, a threaded debugger, and a suite of
code-intelligence tools) — now lives in its own sibling project:

> **[`../tvlisp`](../tvlisp)** — `cd ../tvlisp && make && ./tvlisp`

It depends on this framework through a `systems/tvision` symlink back to this
directory, so the two build together with no global configuration.  See that
project's README for the full feature tour and demos.

## Using the library

```lisp
(asdf:load-system :tvision)

(defclass my-app (tv:tapplication) ())

(defmethod tv::setup ((app my-app))
  (let ((w (make-instance 'tv:twindow
                          :title "Hello"
                          :bounds (tv:make-trect 5 3 45 15))))
    (tv:insert w (make-instance 'tv:tstatic-text
                                :text "Hello, Turbo Vision!"
                                :bounds (tv:make-trect 2 2 30 3)))
    (tv:insert (tv:program-desktop app) w)))

(tv:run 'my-app)
```

> Tip: always pass `:bounds` to `make-instance` for windows/dialogs/desktops.
> Their frames and backgrounds are built during construction and need the size
> up front.

## Architecture

The port follows Turbo Vision's design closely.  Each source file maps to a
recognisable part of the original framework:

| File | Turbo Vision analogue | Responsibility |
|------|----------------------|----------------|
| `src/geometry.lisp`    | `TPoint`, `TRect`       | points & rectangles |
| `src/colors.lisp`      | colour attributes, palettes | DOS attribute byte ↔ ANSI SGR, palette chains |
| `src/draw-buffer.lisp` | `TDrawBuffer`           | a run of `char+attribute` cells |
| `src/events.lisp`      | `TEvent`, key/command codes | event record and constants |
| `src/screen.lisp`      | `THardwareInfo`/`TScreen` | raw mode, alternate screen, diff-based ANSI rendering, input decoding (keys + SGR mouse) |
| `src/concurrency.lisp` | (new)                   | `sb-thread` mailbox + worker→UI callback queue and self-pipe wakeup (lets background threads drive the single-threaded UI loop) |
| `src/view.lisp`        | `TView`                 | base class: geometry, state, palette mapping, clipped drawing, events |
| `src/group.lisp`       | `TGroup`                | subview ownership, Z-order, focus, event dispatch, modal exec |
| `src/frame.lisp`       | `TFrame`                | window borders, title, close/zoom icons |
| `src/scrollbar.lisp`   | `TScrollBar`            | proportional scroll bar |
| `src/window.lisp`      | `TWindow`               | framed, movable, closable, zoomable window |
| `src/desktop.lisp`     | `TDesktop`/`TBackground`| background fill, tile/cascade |
| `src/widgets.lisp`     | static text, label, button, input line, check boxes | controls |
| `src/dialog.lisp`      | `TDialog`               | modal dialogs, `message-box`, `input-box` |
| `src/statusline.lisp`  | `TStatusLine`           | bottom hint/shortcut bar |
| `src/program.lisp`     | `TProgram`/`TApplication`| application palette, main event loop, modal loop, window dragging |
| `src/menu.lisp`        | `TMenuBar`/`TMenuBox`/`TMenuPopup` | menu bar, dropdowns, submenus, shortcuts, hot-keys, `popup-menu` (context menus) |
| `src/scroller.lisp`    | `TScroller`             | view onto a virtual area, bound to scroll bars |
| `src/textview.lisp`    | `TEditor`/`TMemo`/`TFileEditor`/`TEditWindow` | editable text area + the windowed/in-dialog editor classes |
| `src/cluster.lisp`     | `TCluster`/`TRadioButtons`/`TCheckBoxes`/`TMultiCheckBoxes` | labelled option clusters (incl. multi-state boxes) |
| `src/validator.lisp`   | `TValidator`/`TLookupValidator` family | filter / range / picture / string-lookup input validators |
| `src/collection.lisp`  | `TCollection`           | dynamic + sorted collections |
| `src/listbox.lisp`     | `TListViewer`/`TListBox`/`TSortedListBox` | scrollable, selectable list (multi-column, type-ahead search) |
| `src/outline.lisp`     | `TOutline`              | collapsible tree view |
| `src/history.lisp`     | `THistory`/`THistoryViewer`/`THistoryWindow` | input line with a recallable value history |
| `src/filedialog.lisp`  | `TFileDialog`/`TFileInputLine`/`TFileInfoPane` | file dialog: directory browser, wildcard filter, size/date pane |
| `src/chdir.lisp`       | `TChDirDialog`/`TDirListBox` | change-directory dialog |
| `src/colordialog.lisp` | `TColorDialog`/`TColorSelector`/`TColorDisplay`/`TMonoSelector` | colour-picker controls with a live sample |
| `src/help.lisp`        | help system / `THelpFile` | hypertext topics with links + navigable viewer |
| `src/persist.lisp`     | streams                 | S-expression save/load of the desktop |
| `src/stream.lisp`      | `TStream`/`TResourceFile` | binary object streaming + named resource files |
| `src/threadmon.lisp`   | (new)                   | refreshable thread monitor (list + kill worker threads) |
| `src/repl.lisp`        | (new)                   | `trepl-view` — threaded Lisp REPL, restart/backtrace/frame-locals debugger, inspector, text windows |

The text view (`src/textview.lisp`) carries the editor engine: selection,
clipboard, undo/redo, insert/overwrite, word movement, goto, find,
`text-replace-all`, file load/save, a `tindicator`, and the read-only "protect"
boundary — enough for both the REPL and the editor example below.

### Key design choices

* **CLOS class hierarchy.**  `tview` → `tgroup` → `twindow`/`tdesktop`/
  `tprogram`, with `draw`, `handle-event`, `get-palette`, `set-state` etc. as
  generic functions, so you extend behaviour by subclassing and specialising —
  the Lisp-idiomatic equivalent of overriding C++ virtual methods.

* **Single back-buffer with z-order compositing.**  Rather than giving every
  group its own buffer, all views write into one screen-sized back buffer.
  Correct layering comes from drawing back-to-front; `flush-screen` then diffs
  the back buffer against what is currently displayed and emits the minimal set
  of ANSI sequences.  Each view computes its absolute origin and a clip
  rectangle by walking the owner chain, so drawing never escapes its container.

* **Palette chains.**  A view maps a small colour *index* through its own
  palette, then up through each owning group's palette, until it reaches the
  application palette which holds the only real attribute bytes.  This is how a
  button gets a different colour in a grey dialog than in a blue window without
  knowing anything about its container — exactly as in Turbo Vision.

* **Self-contained terminal driver.**  Raw mode is set via `stty`, the
  alternate screen and mouse tracking via xterm control sequences, and input is
  read non-blocking from fd 0 and decoded (arrow/function keys, SGR-encoded
  mouse reports, and multi-byte UTF-8 assembled into one code-point event).

* **Unicode text.**  Each cell carries a full 21-bit code point (not just the
  BMP), so any Unicode character — Greek, Cyrillic, accents, symbols, even a
  lone emoji — can be typed and rendered.  **Double-width** characters (CJK and
  most emoji) claim two cells (`sb-unicode`'s `east-asian-width`), so following
  text doesn't overlap and the cursor tracks the right visual column.
  **Grapheme clusters** (`sb-unicode:graphemes`) — base+combining marks and
  ZWJ / skin-tone emoji sequences — are interned into a single cell, so they
  render as one glyph and arrow-keys / backspace / mouse / selection treat them
  as one unit.  Word-wrap mode shares the same layout: lines wrap at word
  boundaries (hard-splitting only a word wider than the view), never split a wide
  glyph, and cursor up/down and mouse hits map through display columns, not
  code-point counts.

  ![Editing Greek, Cyrillic, accents and math symbols](media/unicode.gif)

  Word-wrap reflows wide glyphs whole — an emoji that won't fit is pushed to
  the next visual row rather than split across the boundary:

  ![Word-wrap with emoji: wide glyphs stay whole at the wrap boundary](media/wrap-emoji.gif)

## Status / scope

Implemented: views, groups, windows, frames, desktop, dialogs, status line,
pull-down menus (dropdowns/submenus/shortcuts/hot-keys) and **right-click
context menus**, an editable text area
(selection, clipboard, undo, read-only "protect" region), scroller + list box +
collections, clusters/radio-buttons/check-boxes, input validators (filter /
range / picture; **enforced on dialog accept**) and input history, buttons, input
lines, labels, static/param text, scroll bars, modal execution, Tab/Shift-Tab
focus cycling, a command set
(enable/disable with greying), window drag/close/zoom/**resize**/keyboard
move-size/**cycling (F6)**/**Alt-1..9 selection**, **drop shadows**, tiling/
cascading, group-level data exchange, a **tree view (`TOutline`)**, **colour-
picker controls** (`TColorSelector`/`TColorDisplay`/`TMonoSelector`), **per-view
event masks** and **per-control disable/grey**, **hypertext help** (linked
topics) with a context-switched status line, **colour / black-white / monochrome
palettes**, S-expression *and* **binary** persistence (`TResourceFile`), full
mouse (incl. **double/triple-click, wheel, auto-repeat**) and keyboard (incl.
**Alt/Ctrl/Shift modifiers**), configurable cursor shapes, **live terminal
resize**, and a diffing ANSI renderer.

The control set covers essentially all of Borland Turbo Vision's, including the
later additions: **multi-state check boxes**, a **type-ahead sorted list box**, a
**change-directory dialog**, an **in-dialog memo** and the **windowed editor**
classes (`TFileEditor`/`TEditWindow`), **string-lookup validators**, and a
file dialog with **wildcard filtering** and a **size/date info pane**.  Beyond
the original, the port adds a **threaded Lisp REPL** with a SLIME `sldb`-style
debugger (restarts, backtrace, frame-locals, value drill-down) and a **thread
monitor** — built on an `sb-thread` worker model with a worker→UI callback
bridge (`src/concurrency.lisp`).

Deliberately not implemented (invasive core rewrites for little visible gain):
Turbo Vision's per-group buffer + cover-list occlusion model (this port
composites a single back buffer in z-order each frame — correct on screen, just
not the original's partial-repaint optimization); 256-/true-colour (the cell
attribute is a 16-colour DOS byte); editor word-wrap; and the `TColorDialog`
palette-scheme *editor* lists (`TColorGroupList`/`TColorItemList`) — the colour
*picker* controls are present, but editing a whole application palette by colour
group is not.

`Tab`/`Shift-Tab` cycle the focus among a group's controls in layout order
(consumed at the innermost group that holds leaf controls, so the desktop never
cycles windows on Tab).  The command set (`enable-command`, `disable-command`,
`set-command-enabled`, `command-enabled-p`) is consulted by menus, buttons and
status items: disabled commands won't fire (even via their shortcut) and are
drawn greyed out.

`tscroller` shows a window onto a larger virtual area and stays in sync with one
or two `tscrollbar`s (the `cmScrollBarChanged` broadcast moves the view; the view
updates the bars, with a reentrancy guard).  Scroll bars also respond to mouse
clicks (arrows + paging).  The terminal driver installs a `SIGWINCH` handler;
the main loop services it via `apply-resize`, which re-queries the size, resizes
the buffers and reflows the whole view tree through `change-bounds`/grow-modes.

### Text area & the Lisp REPL

`ttext-view` (in `src/textview.lisp`) is a full multi-line editor: line storage,
cursor movement, scrolling, insert/delete/split, selection, clipboard, undo, a
read-only "protect" boundary, and an `append-text` method for streaming output.
The Enter key is routed through the generic `text-return`.

`trepl-view` (in `src/repl.lisp`) is a working Lisp REPL built on exactly those
hooks: it overrides `text-return` to read the text after the prompt, evaluate it
in a dedicated `TV-REPL-USER` package (capturing printed output and binding the
history variables), `append-text` the values back, and write a fresh prompt —
while `set-protect-boundary` keeps the transcript above the prompt read-only.  An
incomplete form (unbalanced parens) continues on the next line instead of
evaluating, and Up/Down recall input history.  `(make-repl-window bounds)`
returns a ready-to-insert window with the REPL and a scroll bar.  Evaluation runs
on a per-listener `sb-thread` worker; the worker→UI bridge in
`src/concurrency.lisp` streams output and drives the cross-thread debugger.  The
**tvlisp** example above turns this into a full mini-IDE.

## Testing

The control suite runs on [FiveAM](https://github.com/lispci/fiveam) — the
**only** external dependency, and a test-only one: the `tvision` library and the
example binaries still build with nothing but SBCL.  Each test constructs a
control, feeds events through `handle-event`, and asserts on state, data or
rendered cells (thin `deftest`/`ok`/`is=` wrappers over FiveAM's `test`/`is`):

```sh
make test         # the FiveAM control suite + the tvlisp pty smoke tests
make test-lisp    # just the FiveAM control suite (220 checks across 35 tests)
make test-pty     # just the end-to-end pty smoke tests (builds & drives ./tvlisp)
# or:  sbcl --eval '(asdf:test-op :tvision/tests)'
# or from Lisp: (asdf:load-system :tvision/tests) (tvision-tests:run-tests)
```

`make test-pty` (in `tests/pty_smoke.py`) launches the built `tvlisp` binary in
a pseudo-tty and asserts on the reconstructed screen, so the end-to-end example
flows (REPL eval, editor, save, window list) are regression-guarded too.

It exits non-zero on any failure (CI-ready) and covers geometry, the draw
buffer, every control (clusters, lists, validators, collections, history,
menus/`TMenuPopup`, colour selectors, file/chdir dialogs, the memo/editor), the
concurrency mailbox, the thread monitor, and the REPL backend.

## License

MIT.
