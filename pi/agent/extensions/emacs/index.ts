import {
  type ExtensionAPI,
  type KeybindingsManager,
} from "@earendil-works/pi-coding-agent";
import { EmacsEditor } from "./editor";
import { openCommandPalette } from "./palette";

export default function emacs(pi: ExtensionAPI): void {
  let editor: EmacsEditor | undefined;
  let keybindings: KeybindingsManager | undefined;

  pi.registerShortcut("alt+x", {
    description: "Open command palette",
    handler: async (ctx) => {
      if (ctx.mode !== "tui" || !editor || !keybindings) {
        ctx.ui.notify(
          "The command palette is only available in TUI mode",
          "error",
        );
        return;
      }

      await openCommandPalette(ctx, editor, keybindings);
    },
  });

  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    ctx.ui.setEditorComponent((tui, theme, activeKeybindings) => {
      keybindings = activeKeybindings;
      editor = new EmacsEditor(tui, theme, activeKeybindings);
      return editor;
    });
  });

  pi.on("session_shutdown", () => {
    editor = undefined;
    keybindings = undefined;
  });
}
