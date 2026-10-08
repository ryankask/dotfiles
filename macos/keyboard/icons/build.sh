#!/usr/bin/env bash
# Requires macOS Swift (AppKit/CoreText) and iconutil. Does not install layouts.
set -euo pipefail

if [[ "$#" != 0 ]]; then
  printf 'Usage: %s\n' "$0" >&2
  exit 1
fi

source_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
output="$source_dir/../GB-PL-Colemak.bundle/Contents/Resources/GB PL Colemak.icns"
for tool in swift iconutil; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    printf 'Missing required tool: %s\n' "$tool" >&2
    exit 1
  fi
done

work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT
iconset="$work_dir/Colemak.iconset"
swift "$source_dir/render.swift" "$iconset"
iconutil --convert icns --output "$work_dir/Colemak.icns" "$iconset"
cp "$work_dir/Colemak.icns" "$output"
printf 'Generated %s\n' "$output"
