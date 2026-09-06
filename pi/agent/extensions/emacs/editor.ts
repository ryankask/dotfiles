import { CustomEditor } from "@earendil-works/pi-coding-agent";
import { matchesKey } from "@earendil-works/pi-tui";

const graphemeSegmenter = new Intl.Segmenter(undefined, {
  granularity: "grapheme",
});

interface PageableAutocompleteList {
  selectedIndex: number;
  maxVisible: number;
  setSelectedIndex(index: number): void;
}

export class EmacsEditor extends CustomEditor {
  override handleInput(data: string): void {
    if (this.pageAutocomplete(data)) return;

    if (matchesKey(data, "ctrl+t")) {
      this.transposeCharacters();
      return;
    }

    super.handleInput(data);
  }

  private pageAutocomplete(data: string): boolean {
    if (!this.isShowingAutocomplete()) return false;

    const list = (
      this as unknown as { autocompleteList?: PageableAutocompleteList }
    ).autocompleteList;
    if (!list) return false;

    if (this.keybindings.matches(data, "tui.select.pageUp")) {
      list.setSelectedIndex(list.selectedIndex - list.maxVisible);
      return true;
    }

    if (this.keybindings.matches(data, "tui.select.pageDown")) {
      list.setSelectedIndex(list.selectedIndex + list.maxVisible);
      return true;
    }

    return false;
  }

  transposeCharacters(): void {
    const cursor = this.getCursor();
    const lines = this.getLines();
    const line = lines[cursor.line] ?? "";
    const graphemes = [...graphemeSegmenter.segment(line)];

    if (cursor.col === 0 || graphemes.length < 2) return;

    const nextIndex = graphemes.findIndex(
      (grapheme) => grapheme.index === cursor.col,
    );
    let updatedLine: string;
    let targetCol: number;

    if (nextIndex === -1) {
      if (cursor.col !== line.length) return;

      const before = graphemes.at(-2)!;
      const after = graphemes.at(-1)!;
      updatedLine =
        line.slice(0, before.index) + after.segment + before.segment;
      targetCol = updatedLine.length;
    } else {
      if (nextIndex === 0) return;

      const before = graphemes[nextIndex - 1]!;
      const after = graphemes[nextIndex]!;
      updatedLine =
        line.slice(0, before.index) +
        after.segment +
        before.segment +
        line.slice(after.index + after.segment.length);
      targetCol = before.index + after.segment.length + before.segment.length;
    }

    lines[cursor.line] = updatedLine;
    this.setText(lines.join("\n"));

    // setText() leaves point at the end of the buffer. Move it back without
    // changing the new, single undo snapshot created by setText().
    for (;;) {
      const current = this.getCursor();
      if (
        current.line < cursor.line ||
        (current.line === cursor.line && current.col <= targetCol)
      ) {
        break;
      }

      super.handleInput("\x1b[D");
      const moved = this.getCursor();
      if (moved.line === current.line && moved.col === current.col) break;
    }
  }
}
