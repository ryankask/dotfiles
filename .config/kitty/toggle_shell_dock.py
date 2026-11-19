from kittens.tui.handler import result_handler
from kitty.boss import Boss
from kitty.launch import launch, parse_launch_args


def main(_args: list[str]) -> None:
    pass


@result_handler(no_ui=True)
def handle_result(args: list[str], _answer: object, target_window_id: int, boss: Boss) -> None:
    window = boss.window_id_map.get(target_window_id)
    if window is None or (tab := window.tabref()) is None:
        return

    options, command = parse_launch_args(args[1:])
    if window.title == options.window_title:
        tab.nth_window(-1)
    else:
        launch(boss, options, command, target_tab=tab)
