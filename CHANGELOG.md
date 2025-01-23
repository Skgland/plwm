# [v0.3](https://github.com/Seeker04/plwm/releases/tag/v0.3) (2025-01-23)

## Added
- Support for `$XDG_CONFIG_HOME` and `/etc/` configs
- Separate border width setting for focused window ([#27](https://github.com/Seeker04/plwm/issues/27))
- Support multiple selection in menus `pull_from` and `delete_workspace` ([#41](https://github.com/Seeker04/plwm/issues/41))
- `menu:close_windows` and `menu:keep_windows` ([#40](https://github.com/Seeker04/plwm/issues/40))
- Simple move/resize animations ([#45](https://github.com/Seeker04/plwm/issues/45))
- Feature to run custom action on modkey + mouse scroll ([#71](https://github.com/Seeker04/plwm/issues/71))
- Documentation comments for predicates ([#68](https://github.com/Seeker04/plwm/issues/68))
- Unit tests ([#21](https://github.com/Seeker04/plwm/issues/21))

## Changed
- Smaller code/documentation refactors

## Fixed
- _NET_WORKAREA was not always updated (e.g. when switching monitors)
- XFree deallocation was missing after XineramaQueryScreens call
- Keep monitor focus when a window is destroyed on another ([#55](https://github.com/Seeker04/plwm/issues/55))
- Window moved to other workspace no longer steals input ([#23](https://github.com/Seeker04/plwm/issues/23))
- Smaller issues in README.md

---

# [v0.2](https://github.com/Seeker04/plwm/releases/tag/v0.2) (2024-07-12)

## Added

- **Command FIFO implementation** ([#46](https://github.com/Seeker04/plwm/issues/46))
- Logging of caught exceptions
- Added `attach_bottom` setting ([#13](https://github.com/Seeker04/plwm/issues/13))
- Extended documentation (e.g. dependency install guide, .dekstop file example)
- Screenshot with default settings
- Logo

## Removed
- Gaps from the default config

## Changed
- Omit monitors and workspaces from menus when there's only a single choice ([#2](https://github.com/Seeker04/plwm/issues/2))
- Various code cleanup and refactoring

## Fixed
- Fixed a bug when a visible window got destroyed on not the active monitor
- Snapping now considers whether the window is moved or resized ([#42](https://github.com/Seeker04/plwm/issues/42))
- Fixed inconsistency between reindex_workspace keymap and its documentation
- Fail instead of throw in win_properties/2 to avoid rare crashes ([#38](https://github.com/Seeker04/plwm/issues/38))
- Added error handling for `XGetTextProperty` call ([#36](https://github.com/Seeker04/plwm/issues/36)) - fixes rare crashes
- Comma separation for `SEE ALSO` list in plwm(1)
- Newly spawned floating windows now respect space reserved for bars ([#1](https://github.com/Seeker04/plwm/issues/1))
- Compilation on Ubuntu (different swipl header path, order of lib dependencies)
- Several smaller issues in README.md

---

# [v0.1](https://github.com/Seeker04/plwm/releases/tag/v0.1) (2023-11-10)

Initial version

