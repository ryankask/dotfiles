#!/usr/bin/env gawk -f

BEGIN {
  FS = "[ =\"]+"
}

$1 == "[tool.poetry]" {
  ready = 1
  next
}

ready && $1 == "name" {
  printf("%s", $2)
  exit
}

ready && $1 ~ /^[[:space:]]*\[/ {
  exit
}
