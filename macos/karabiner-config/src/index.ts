import {
  duoLayer,
  FromKeyParam,
  ifApp,
  ifVar,
  map,
  rule,
  toKey,
  to$,
  ToKeyParam,
  toSetVar,
  withCondition,
  withMapper,
  writeToProfile,
  toApp,
  Rule,
} from "karabiner.ts";

const Apps = {
  EMACS: /^org\.gnu\.Emacs$/,
  KITTY: /^net\.kovidgoyal\.kitty$/,
  CHROME: /^com\.google\.Chrome$/,
  VSCODE: /^com\.microsoft\.VSCode$/,
  SLACK: /^com\.tinyspeck\.slackmacgap$/,
  NOTION: /^notion\.id$/,
  ORION: /^com\.kagi\.kagimacOS$/,
  OUTLOOK: /^com\.microsoft\.Outlook$/,
} as const;

writeToProfile("Default", [
  rule("Caps lock → left control").manipulators([
    map("caps_lock", "optionalAny").to("left_control"),
  ]),
  rule("left control → hyper; caps lock if alone").manipulators([
    map("left_control", "optionalAny")
      .to(toKey("left_control", ["command", "option"], { lazy: true }))
      .toIfAlone("caps_lock")
      .parameters({
        "basic.to_if_alone_timeout_milliseconds": 300,
      }),
  ]),
  rule("C-m → return", ifApp([Apps.CHROME, Apps.VSCODE])).manipulators([
    map("m", ["left_control", "right_control"]).to("return_or_enter"),
  ]),
  rule("C-[ → escape", ifApp([Apps.EMACS, Apps.KITTY]).unless()).manipulators([
    map("[", ["left_control", "right_control"]).to("escape"),
  ]),
  rule("C-' → right control").manipulators([
    map("'", {
      optional: [
        "left_command",
        "left_option",
        "right_command",
        "right_option",
      ],
    })
      .to([
        toSetVar("right-control", 1),
        toKey("right_control", undefined, { lazy: true }),
      ])
      .toIfAlone([
        toSetVar("right-control", 0),
        toKey("'", undefined, { halt: true }),
      ])
      .toDelayedAction(toSetVar("right-control", 2), [])
      .toAfterKeyUp(toSetVar("right-control", 0))
      .parameters({
        "basic.to_if_alone_timeout_milliseconds": 180,
        "basic.to_delayed_action_delay_milliseconds": 181,
      }),
    withCondition(ifVar("right-control", 1))([
      // Activate a prefix map so the below exclusions don't take
      // effect
      withMapper(["x", "c", "h", "o"])((k) =>
        map(k, "right_control").to([
          toSetVar("right-control", 2),
          toKey(k, "right_control"),
        ]),
      ),
      // Exclude letters that commonly appear with apostrophes if a
      // prefix map isn't activated
      withMapper([
        "d", // she's
        "f", // hasn't
        "g", // he'd
        "m", // I'm
        "s", // you're,
        "u", // we'll
        "v", // they've
      ])((k) => map(k, "right_control").to([toKey("'"), toKey(k)])),
    ]),
  ]),
  rule("Emacs emulation").manipulators([
    withCondition(
      ifApp([Apps.CHROME, Apps.SLACK, Apps.NOTION, Apps.ORION, Apps.OUTLOOK]),
    )([
      withMapper<FromKeyParam, ToKeyParam>({
        m: "return_or_enter",
        r: "up_arrow",
        j: "down_arrow",
        t: "escape",
      })((k, v) => map(k, "control").to(v)),
    ]),
  ]),
  appLauncherLayer(),
]);

function appLauncherLayer(): Rule {
  let rule = duoLayer(",", ".", "launch-app")
    .manipulators({
      a: toApp("Activity Monitor"),
      b: toApp("Books"),
      c: toApp("Google Chrome"),
      d: toApp("Slack"),
      e: toApp("Emacs"),
      f: toApp("Kitty"),
      g: toApp("Dictionary"),
      m: toApp("Microsoft Outlook"),
      r: toApp("Spotify"),
      v: toApp("Finder"),
      w: toApp("Calendar"),
      semicolon: toApp("Orion"),
      tab: to$(
        "open alfred://runtrigger/com.alfredapp.vitor.windowswitcher/list_windows",
      ),
    })
    .build();

  // ",." is the right duolayer trigger. Add "zx" as left duolayer trigger.
  let leftManipulator = structuredClone(rule.manipulators[0]);

  if (
    leftManipulator.from === undefined ||
    !("simultaneous" in leftManipulator.from)
  ) {
    throw new Error("from must be a simultaneous FromEvent");
  }

  leftManipulator.from.simultaneous = [
    {
      key_code: "z",
    },
    {
      key_code: "x",
    },
  ];

  rule.manipulators.splice(1, 0, leftManipulator);

  return rule;
}
