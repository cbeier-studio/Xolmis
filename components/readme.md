# CBS Pack

Package of custom Lazarus components used by **Xolmis**. All components are
registered in the `CBS` component palette.

## TToggleSwitch

`TToggleSwitch` is a keyboard-accessible Boolean switch rendered with
anti-aliased graphics through `BGRABitmap`. It is intended for compact
on/off settings where a checkbox is less suitable.

- `Checked` stores the current state. Assigning a different value repaints the
	control and triggers `OnChange`.
- `OnColor`, `OffColor`, and `ThumbColor` customize the enabled-state track
	and thumb colors.
- Disabled switches use standard button colors to communicate their state.
- A dotted focus indicator is shown when the control receives keyboard focus.
- Users can toggle the switch with a left mouse click or the Space key.

The component requires the `BGRABitmap` package.

## TTDICardPanel

`TTDICardPanel` is a `TNotebook`-based document host. It presents multiple
forms as pages without providing its own tabs or navigation controls, allowing
the application to choose its own navigation UI.

- `ShowFormInPage` embeds an existing form in a new page, or selects its
	existing page when the form is already open.
- `CreateFormInNewPage` creates a form class and immediately hosts it.
- `SelectForm` and `SelectFormByClass` activate an open form programmatically;
	`FindFormInPages` and `GetFormByClass` locate hosted forms.
- `CloseTab` and `CloseAllTabs` remove pages. `FixedPages` protects the first
	pages when closing all pages.
- `CanCloseAPage` and `CanCloseAllPages` honor the embedded form's
	`CloseQuery` logic. `OnBeforeClosePage` can veto a close, and
	`OnAfterClosePage` reports completed closes.
- `OnPageChange` reports the former and new page indices. `TDIOptions` can
	preserve the last focused control, validate focus changes, and emulate the
	embedded form activation lifecycle.
- Forms are temporarily converted to borderless client-aligned children and
	have their original parent, geometry, alignment, border style, and close
	handler restored when removed.

## TDBEditButton

`TDBEditButton` combines a data-aware `TDBEdit` with a configurable action
button. It is useful for lookup, selection, or auxiliary actions associated
with a bound database field.

- `DataSource` and `DataField` bind the embedded edit to a dataset; `Field`
	exposes the resolved field.
- `EditControl` and `ButtonControl` provide access to the embedded `TDBEdit`
	and `TSpeedButton` for advanced configuration.
- Edit properties such as `Alignment`, `DataField`, `EditMask`, `MaxLength`,
	`PasswordChar`, `Text`, and `TextHint` are forwarded to the edit control.
- Button properties such as caption, hint, cursor, width, images, image
	indices, flat style, and spacing are forwarded to the button control.
- `DirectInput` determines whether users can type into the edit. `ReadOnly`
	also disables the action button.
- `ButtonOnlyWhenFocused` hides the action button until the embedded edit has
	focus. `FocusOnButtonClick` returns focus to the edit before `OnButtonClick`
	runs.
- Edit events are re-raised by the composite component, so handlers receive
	the `TDBEditButton` instance rather than the internal edit.

## TDBImageGallery

`TDBImageGallery` displays image BLOB fields from a dataset as a scrollable
thumbnail gallery. It supports selecting an image while preserving the
dataset's current record after a refresh or paint pass.

- `DataSource` supplies the dataset, `ImageField` identifies its BLOB image
	field, and `CaptionField` optionally supplies text below each thumbnail.
- `Zoom` adjusts the base thumbnail size from 1 to 200 percent.
- Images are laid out in rows and respect the scroll box's horizontal and
	vertical positions.
- Null image fields render as an empty placeholder; unsupported image data is
	reported in the thumbnail rather than interrupting the gallery.
- Clicking a thumbnail sets the read-only `SelectedIndex` and triggers
	`OnChange`.
- `RefreshGallery` requests a repaint. Dataset notifications automatically
	refresh the gallery when its active state or data changes.

## TChipsPanel

`TChipsPanel` is a wrapping panel for interactive chips or tags. Chips can
represent values through display text and an optional identifier, and can be
selected or removed by the user.

- `AddChip` creates a chip from a caption and optional `ID`; without an ID,
	the caption is used as its identifier. `ClearChips` removes all chips.
- `Chips` and `SelectedChips` expose the current chip lists. `GetSelectedIDs`
	writes the selected identifiers to a supplied `TStrings` instance.
- `Selectable` enables selection behavior. With `MultiSelect` disabled, a new
	selection clears the current selection; `ClearSelection` resets all chips.
- `SelectByCaption`, `SelectByID`, `SelectByCaptions`, and `SelectByIDs`
	support programmatic selection.
- With `Selectable` disabled, each chip displays a close control. Clicking it
	hides and asynchronously removes the chip after `OnChipClick` is called.
- `ColorMode` chooses a shared color or a rotating palette for selected chips.
	`DarkMode` selects the corresponding dark colors. Unselected chips remain
	neutral and their fill, border, and text colors are independently
	configurable.
- `ChipCornerRadius`, `ChipSpacing`, and `ChipPadding` control chip geometry.
	`OnChipClick` reports both selection and removal interactions.