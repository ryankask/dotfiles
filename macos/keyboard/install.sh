#!/usr/bin/env bash
# Install the user bundle without registering or selecting an input source.
set -euo pipefail

if [[ "$#" != 0 ]]; then
  printf 'Usage: %s\n' "$0" >&2
  exit 1
fi

source_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source_bundle="$source_dir/GB-PL-Colemak.bundle"
target="$HOME/Library/Keyboard Layouts/GB-PL-Colemak.bundle"
identifier='com.ryankaskel.keyboardlayout.colemak'
files=('Contents/Info.plist' 'Contents/Resources/GB PL Colemak.keylayout' 'Contents/Resources/GB PL Colemak.icns')

for file in "${files[@]}"; do
  test -f "$source_bundle/$file"
done
plutil -lint "$source_bundle/Contents/Info.plist"
if [[ -e "$target" || -L "$target" ]]; then
  if [[ -L "$target" ]] || [[ "$(plutil -extract CFBundleIdentifier raw -o - "$target/Contents/Info.plist")" != "$identifier" ]]; then
    printf 'Refusing to replace an unexpected bundle: %s\n' "$target" >&2
    exit 1
  fi
fi

mkdir -p "$target"
/usr/bin/rsync -rlt --delete "$source_bundle/" "$target/"
find "$target" -type d -exec chmod 0755 {} +
find "$target" -type f -exec chmod 0644 {} +
for file in "${files[@]}"; do
  cmp "$source_bundle/$file" "$target/$file"
done
printf 'Installed and verified %s\n' "$target"
