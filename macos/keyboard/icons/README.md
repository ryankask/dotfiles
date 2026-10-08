# Colemak badge

The bundled `CÓ` icon is a black template with transparent lettering and corners.
It uses SF Semibold at 10.5 pt, a 3-point corner radius, and the font's actual
`Ó` glyph. The plain `CO` bounds determine vertical alignment.

Rebuild on macOS with Swift and `iconutil`:

```sh
./keyboard/icons/build.sh
```

The renderer draws 16- and 32-pixel representations separately. It uses public
AppKit/CoreText APIs; the installed icon is static and needs no runtime code.
Keep `TISIconIsTemplate = true` in the bundle's `Info.plist`.

Bootstrap installs `GB-PL-Colemak.bundle` into `~/Library/Keyboard Layouts/`
without rebuilding its icon or selecting an input source. The installer only
copies and verifies the user bundle; it does not change system layouts,
preferences, or Preboot.
