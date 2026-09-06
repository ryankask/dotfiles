import {
  DynamicBorder,
  getSelectListTheme,
  keyHint,
  type AppKeybinding,
  type ExtensionContext,
  type KeybindingsManager,
  type Theme,
} from "@earendil-works/pi-coding-agent";
import {
  Container,
  Input,
  type Focusable,
  type SelectItem,
  SelectList,
  Text,
  fuzzyFilter,
} from "@earendil-works/pi-tui";
import type { EmacsEditor } from "./editor";

const ACTION_NAMES: Partial<Record<AppKeybinding, string>> = {
  "app.clear": "clear-editor",
  "app.suspend": "suspend",
  "app.thinking.cycle": "cycle-thinking-level",
  "app.model.cycleForward": "cycle-model-forward",
  "app.model.cycleBackward": "cycle-model-backward",
  "app.model.select": "select-model",
  "app.tools.expand": "toggle-tool-output",
  "app.thinking.toggle": "toggle-thinking-blocks",
  "app.editor.external": "open-external-editor",
  "app.message.copy": "copy-message",
  "app.message.followUp": "queue-follow-up",
  "app.message.dequeue": "restore-queued-messages",
  "app.session.new": "new-session",
  "app.session.tree": "open-session-tree",
  "app.session.fork": "fork-session",
  "app.session.resume": "resume-session",
};

interface PaletteEntry {
  action: AppKeybinding;
  name: string;
  description: string;
}

function fallbackActionName(action: AppKeybinding): string {
  return action.replace(/^app\./, "").replaceAll(".", "-");
}

function getPaletteEntries(
  editor: EmacsEditor,
  keybindings: KeybindingsManager,
): PaletteEntry[] {
  return [...editor.actionHandlers.keys()]
    .map((action) => {
      const keys = keybindings.getKeys(action);
      const binding = keys.length > 0 ? keys.join(", ") : "unbound";

      return {
        action,
        name: ACTION_NAMES[action] ?? fallbackActionName(action),
        description: `${keybindings.getDefinition(action)?.description ?? action} · ${binding}`,
      };
    })
    .sort((a, b) => a.name.localeCompare(b.name));
}

class CommandPalette extends Container implements Focusable {
  private readonly entries: PaletteEntry[];
  private readonly input: Input;
  private readonly listContainer: Container;
  private list: SelectList;
  private readonly keybindings: KeybindingsManager;
  private readonly requestRender: () => void;
  private readonly select: (action: AppKeybinding) => void;
  private readonly cancel: () => void;
  private _focused = false;

  get focused(): boolean {
    return this._focused;
  }

  set focused(value: boolean) {
    this._focused = value;
    this.input.focused = value;
  }

  constructor(
    entries: PaletteEntry[],
    keybindings: KeybindingsManager,
    theme: Theme,
    requestRender: () => void,
    select: (action: AppKeybinding) => void,
    cancel: () => void,
  ) {
    super();
    this.entries = entries;
    this.keybindings = keybindings;
    this.requestRender = requestRender;
    this.select = select;
    this.cancel = cancel;
    this.input = new Input();
    this.listContainer = new Container();
    this.list = this.createList(entries);
    this.listContainer.addChild(this.list);

    this.addChild(
      new DynamicBorder((text: string) => theme.fg("accent", text)),
    );
    this.addChild(new Text(theme.fg("muted", "Command"), 1, 0));
    this.addChild(this.input);
    this.addChild(this.listContainer);
    this.addChild(
      new Text(
        theme.fg(
          "dim",
          `${keyHint("tui.select.up", "previous")}  ${keyHint("tui.select.down", "next")}  ${keyHint("tui.select.confirm", "run")}  ${keyHint("tui.select.cancel", "cancel")}`,
        ),
        1,
        0,
      ),
    );
    this.addChild(
      new DynamicBorder((text: string) => theme.fg("accent", text)),
    );
  }

  private createList(entries: PaletteEntry[]): SelectList {
    const actionsByName = new Map(
      entries.map((entry) => [entry.name, entry.action]),
    );
    const items: SelectItem[] = entries.map((entry) => ({
      value: entry.name,
      label: entry.name,
      description: entry.description,
    }));
    const list = new SelectList(
      items,
      Math.min(items.length, 12),
      getSelectListTheme(),
    );
    list.onSelect = (item) => {
      const action = actionsByName.get(item.value);
      if (action) this.select(action);
    };
    list.onCancel = this.cancel;
    return list;
  }

  private updateMatches(query: string): void {
    const matches = fuzzyFilter(
      this.entries,
      query,
      (entry) => `${entry.name} ${entry.description}`,
    );
    this.list = this.createList(matches);
    this.listContainer.clear();
    this.listContainer.addChild(this.list);
  }

  handleInput(data: string): void {
    if (this.keybindings.matches(data, "tui.select.cancel")) {
      this.cancel();
      return;
    }

    if (
      this.keybindings.matches(data, "tui.select.up") ||
      this.keybindings.matches(data, "tui.select.down") ||
      this.keybindings.matches(data, "tui.select.confirm")
    ) {
      this.list.handleInput(data);
      this.requestRender();
      return;
    }

    this.input.handleInput(data);
    this.updateMatches(this.input.getValue());
    this.requestRender();
  }
}

export async function openCommandPalette(
  ctx: ExtensionContext,
  editor: EmacsEditor,
  keybindings: KeybindingsManager,
): Promise<void> {
  const entries = getPaletteEntries(editor, keybindings);
  const action = await ctx.ui.custom<AppKeybinding | undefined>(
    (tui, theme, activeKeybindings, done) =>
      new CommandPalette(
        entries,
        activeKeybindings,
        theme,
        () => tui.requestRender(),
        done,
        () => done(undefined),
      ),
  );

  if (!action) return;

  const handler = editor.actionHandlers.get(action);
  if (!handler) {
    ctx.ui.notify(`Action is no longer available: ${action}`, "error");
    return;
  }
  handler();
}
