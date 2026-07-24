# Flowvi

[Give it a try](https://mrsipan.github.io/flowvi)

A tree-based outliner with vim keybindings, built in [Squint](https://github.com/squint-cljs/squint) ClojureScript. Inspired by [Vimflowy](https://github.com/WuTheFWasThat/vimflowy).

Data is stored in localStorage as JSON. Open `index.html` in a browser to start.

## Normal Mode

| Key | Action |
|-----|--------|
| `j` / `k` | Move down / up |
| `h` / `l` | Cursor left / right |
| `w` / `b` | Next / previous word start |
| `e` | Word end |
| `ge` | Previous word end |
| `f{char}` | Find next character |
| `t{char}` | Till next character |
| `gg` | Go to first line |
| `G` | Go to last line |
| `$` | End of line |
| `0` | Start of line |
| `i` / `a` | Enter insert mode / append |
| `o` / `O` | New line below / above + insert |
| `Ctrl+O` | New child node + insert |
| `dd` | Delete line |
| `D` | Delete to end of line |
| `J` | Join with line below |
| `x` | Toggle complete |
| `>` / `<` | Indent / outdent |
| `u` / `Ctrl+R` | Undo / redo |
| `c` `i` `w` / `d` `i` `w` | Change / delete inner word |
| `c` `w` / `d` `w` | Change / delete to word end |
| `?` | Show help |
| `Escape` / `Ctrl+[` | Exit insert mode |

## Insert Mode

| Key | Action |
|-----|--------|
| `Enter` | New line below |
| `Shift+Enter` | New child node |
| `Ctrl+W` | Delete word backward |
| `Escape` | Return to normal mode |

## Data Storage

Data is saved as JSON to `localStorage` under key `vimflowy-data-v2`. Use the **💾 Save** button to force-save, **📤 Export** to download JSON, or **📥 Import** to restore. The **👁 Preview** button opens an HTML view of the tree in a new window.

## Font

Uses [JuliaMono](https://juliamono.netlify.app/) — `JuliaMonoSiesta-Regular.woff2` must be in the same directory.

## License

MIT
