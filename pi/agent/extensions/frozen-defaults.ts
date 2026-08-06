import { SettingsManager } from "@earendil-works/pi-coding-agent";

const FROZEN_METHODS = [
  "setDefaultModel",
  "setDefaultProvider",
  "setDefaultModelAndProvider",
  "setDefaultThinkingLevel",
] as const;

type SettingsManagerPrototype = Record<string, unknown>;

export default function frozenDefaults(): void {
  const prototype = SettingsManager.prototype as unknown as SettingsManagerPrototype;

  for (const method of FROZEN_METHODS) {
    if (typeof prototype[method] !== "function") {
      throw new Error(
        `frozen-defaults: SettingsManager.prototype.${method}() was not found. ` +
          "Pi's settings API may have changed; refusing to patch it.",
      );
    }
  }

  for (const method of FROZEN_METHODS) {
    prototype[method] = () => undefined;
  }
}
