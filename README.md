# templates

Reusable reflex-dom UI components built with ClasshSS and reflex-classhss. Provides ready-made widgets (buttons, modals, inputs, lists, containers, etc.) that follow a consistent visual style and can be dropped into any reflex-dom application.

## Exported API Surface

| Module | What it provides |
|--------|-----------------|
| `Templates.Partials.Buttons` | 14 button variants: primary, secondary, icon, navy-blue, image, sized, toggle, greedy (stop-propagation) |
| `Templates.Partials.Checkbox` | Custom `Checkbox` type wrapping `inputElement` with `HasValue` instance |
| `Templates.Partials.Image` | `img`, `imgAttr`, `imgClass` — image elements with auto-generated alt text from filename |
| `Templates.Partials.Containers` | Collapsible containers with animated toggle arrows, `screenContainer` |
| `Templates.Partials.Containers.Dropdown` | `dropdown'` and `dropdownWithDefault` — styled select elements |
| `Templates.Partials.Inputs` | Message input (single-line and textarea), attachment input |
| `Templates.Partials.Lists` | `listItem` with configurable highlighting, icons, subtext, unread badges |
| `Templates.Partials.Modal` | Overlay modal with open/close events and background dimming |
| `Templates.Partials.Searchbar` | Search input with icon and styled placeholder |
| `Templates.Partials.Errors` | `eitherDisplay`, `maybeDisplay`, `displayOn`, `errorMessage` |
| `Templates.Partials.Invitebar` | Email invite input with validation feedback |
| `Templates.Types` | `Template t m` constraint alias, `InputEl`, `TextAreaEl`, `El`, `ImgSrc` |
| `Templates.DomExtras` | `elStyle`, `elDynStyle` — inline-style element helpers |

## Core Types and Semantics

### `Template t m`

```haskell
type Template t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
```

The standard constraint bundle for components that need to build DOM, react to post-build events, hold state, and use `mdo`/`rec`.

### `ListItemConfig t`

Configures list item appearance: clickability, subtext, icon, search highlight token, and unread count. Has a `Default` instance with everything turned off.

### `Checkbox t` / `CheckboxConfig t`

Custom checkbox wrapping reflex-dom's `inputElement`. `Checkbox t` has `HasValue` instance giving `Dynamic t Bool`.

### Type Aliases

- `ImgSrc = Text` — image source URL
- `Height = Text`, `Width = Text` — dimension strings for image buttons
- `InputEl t m`, `TextAreaEl t m`, `El t m` — shortened element type aliases

## Usage Examples

### Basic button
```haskell
clickEv <- primaryButton "Submit"
```

### Collapsible container
```haskell
collapsibleContainer "Settings" $ do
  text "Hidden content here"
```

### List with search highlighting
```haskell
let cfg = def { _listItemConfig_highlight = constDyn (Just "search") }
clickEv <- listItem cfg (constDyn "search results here")
```

### Modal
```haskell
modalResult <- modal "close.png" openEvent $ do
  text "Modal content"
  pure someValue
```

### Image with auto alt-text
```haskell
img "/assets/profile-photo.png"
-- Renders: <img src="..." alt="profile photo">
```

## Anti-patterns / Gotchas

- **`emailParse` is a stub** — always returns `Right True`. The invite bar validation is incomplete.
- **`fromJust` in Dropdown** — `dropdown'` and `dropdownWithDefault` use `fromJust` on map lookup. Will crash if the selected value isn't in the options map.
- **Raw Tailwind mixed with ClasshSS** — Several modules (especially Buttons) use raw Tailwind class strings alongside `classh'` calls. This bypasses type safety.
- **`flex` usage** — Some components use flexbox via raw class strings. Per project conventions, grid layout (`gridCol`/`col`) should be preferred.
- **Modal uses inline styles** — The modal component uses inline `style` attributes for positioning/z-index rather than ClasshSS.

## Visual Component Gallery

Each module has SVG mockups showing what the components look like when rendered. See the `doc/` directory or view them in the Haddock documentation.

## Build & Test

```bash
nix-shell
cabal build
cabal test
cabal haddock  # generates docs with embedded SVG visuals
```
