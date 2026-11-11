#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# ///
"""Keep global agent skills the same on every machine.

The skills CLI (https://github.com/vercel-labs/skills) can restore a project's
skills-lock.json. It cannot restore global (-g) installs yet; see
vercel-labs/skills#683. This script uses a manifest, skills.toml, in the same
directory instead.

  manifest.py export              Rewrite skills.toml from the CLI's global lock.
  manifest.py install [--dry-run] Run `npx skills add -g` for each source in skills.toml.

The install command adds and updates skills. It does not remove a skill that is
not in the manifest. To remove a skill, run `npx skills remove -g <name>`, then
run export.
"""

import argparse
import json
import os
import shlex
import subprocess
import sys
import tomllib
from collections import defaultdict
from pathlib import Path

MANIFEST = Path(__file__).resolve().with_name("skills.toml")
SKILLS_CLI = ["npx", "--yes", "skills"]

HEADER = """\
# Global agent skills, installed with the skills CLI (npx skills add -g).
# Restore on a new machine: skills/manifest.py install
# After you add or remove skills, regenerate: skills/manifest.py export
# Export overwrites this file. To pin a GitHub source, write owner/repo@tag.
"""


def lock_path() -> Path:
    """Find the global lock with the same lookup as the CLI's getSkillLockPath()."""
    state = os.environ.get("XDG_STATE_HOME")
    if state:
        return Path(state) / "skills" / ".skill-lock.json"
    return Path.home() / ".agents" / ".skill-lock.json"


def toml_list(key: str, values: list[str]) -> str:
    # A JSON string without \u escapes is also a valid TOML basic string.
    items = "".join(f"  {json.dumps(v, ensure_ascii=False)},\n" for v in values)
    return f"{key} = [\n{items}]\n"


def export() -> int:
    lock = json.loads(lock_path().read_text())
    groups: dict[tuple[str, bool], list[str]] = defaultdict(list)
    for name, entry in lock["skills"].items():
        ref = entry.get("ref")
        if entry["sourceType"] == "github":
            source = entry["source"] + (f"@{ref}" if ref else "")
        else:
            source = entry["sourceUrl"]
            if ref:
                print(f"warning: {name}: ref {ref!r} not kept for {source}", file=sys.stderr)
        groups[(source, entry.get("installMode") == "copy")].append(name)

    parts = [HEADER, "\n", toml_list("agents", sorted(lock.get("lastSelectedAgents", [])))]
    for (source, copy), names in sorted(groups.items(), key=lambda g: g[0][0].lower()):
        parts.append(f"\n[[sources]]\nsource = {json.dumps(source, ensure_ascii=False)}\n")
        if copy:
            parts.append("copy = true\n")
        parts.append(toml_list("skills", sorted(names)))
    text = "".join(parts)
    tomllib.loads(text)  # Stop before the write if the output is not valid TOML.
    MANIFEST.write_text(text)
    count = sum(len(names) for names in groups.values())
    print(f"Wrote {count} skills from {len(groups)} sources to {MANIFEST}")
    return 0


def install(dry_run: bool) -> int:
    manifest = tomllib.loads(MANIFEST.read_text())
    agent_args = [arg for agent in manifest.get("agents", []) for arg in ("-a", agent)]
    failed = []
    for source in manifest["sources"]:
        cmd = [*SKILLS_CLI, "add", source["source"], "-g", "-y", *agent_args]
        cmd += [arg for skill in source["skills"] for arg in ("--skill", skill)]
        if source.get("copy"):
            cmd.append("--copy")
        print(f"$ {shlex.join(cmd)}", flush=True)
        if not dry_run and subprocess.run(cmd).returncode != 0:
            failed.append(source["source"])
    if failed:
        print(f"Failed: {', '.join(failed)}", file=sys.stderr)
        return 1
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("export", help="rewrite skills.toml from the global lock")
    install_parser = commands.add_parser("install", help="install the skills in skills.toml")
    install_parser.add_argument(
        "--dry-run", action="store_true", help="print the commands, do not run them"
    )
    args = parser.parse_args()
    if args.command == "export":
        return export()
    return install(args.dry_run)


if __name__ == "__main__":
    sys.exit(main())
