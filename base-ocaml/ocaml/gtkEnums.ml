(** gtk enums *)

type align = [ `FILL | `START | `END | `CENTER | `BASELINE ]
type arrow_type = [ `UP | `DOWN | `LEFT | `RIGHT | `NONE ]
type attach_options = [ `EXPAND | `SHRINK | `FILL ]
type baseline_position = [ `TOP | `CENTER | `BOTTOM ]
type delete_type =
  [ `CHARS | `WORD_ENDS | `WORDS | `DISPLAY_LINES | `DISPLAY_LINE_ENDS
  | `PARAGRAPH_ENDS | `PARAGRAPHS | `WHITESPACE ]
type direction_type =
  [ `TAB_FORWARD | `TAB_BACKWARD | `UP | `DOWN | `LEFT | `RIGHT ]
type icon_size =
  [ `INVALID | `MENU | `SMALL_TOOLBAR | `LARGE_TOOLBAR | `BUTTON | `DND
  | `DIALOG ]
type sensitivity_type = [ `AUTO | `ON | `OFF ]
type text_direction = [ `NONE | `LTR | `RTL ]
type justification = [ `LEFT | `RIGHT | `CENTER | `FILL ]
type menu_direction_type = [ `PARENT | `CHILD | `NEXT | `PREV ]
type message_type = [ `INFO | `WARNING | `QUESTION | `ERROR | `OTHER ]
type movement_step =
  [ `LOGICAL_POSITIONS | `VISUAL_POSITIONS | `WORDS | `DISPLAY_LINES
  | `DISPLAY_LINE_ENDS | `PARAGRAPH_ENDS | `PARAGRAPHS | `PAGES
  | `BUFFER_ENDS | `HORIZONTAL_PAGES ]
type orientation = [ `HORIZONTAL | `VERTICAL ]
type pack_type = [ `START | `END ]
type position_type = [ `LEFT | `RIGHT | `TOP | `BOTTOM ]
type relief_style = [ `NORMAL | `HALF | `NONE ]
type scroll_step =
  [ `STEPS | `PAGES | `END | `HORIZONTAL_STEPS | `HORIZONTAL_PAGES
  | `HORIZONTAL_ENDS ]
type scroll_type =
  [ `NONE | `JUMP | `STEP_FORWARD | `STEP_BACKWARD | `PAGE_BACKWARD
  | `PAGE_FORWARD | `STEP_UP | `STEP_DOWN | `PAGE_UP | `PAGE_DOWN
  | `STEP_LEFT | `STEP_RIGHT | `PAGE_LEFT | `PAGE_RIGHT | `START | `END ]
type selection_mode = [ `NONE | `SINGLE | `BROWSE | `MULTIPLE ]
type shadow_type = [ `NONE | `IN | `OUT | `ETCHED_IN | `ETCHED_OUT ]
type state_type =
  [ `NORMAL | `ACTIVE | `PRELIGHT | `SELECTED | `INSENSITIVE | `INCONSISTENT
  | `FOCUSED ]
type toolbar_style = [ `ICONS | `TEXT | `BOTH | `BOTH_HORIZ ]
type wrap_mode = [ `NONE | `CHAR | `WORD | `WORD_CHAR ]
type sort_type = [ `ASCENDING | `DESCENDING ]
type pack_direction = [ `LTR | `RTL | `TTB | `BTT ]
type print_pages = [ `ALL | `CURRENT | `RANGES | `SELECTION ]
type page_set = [ `ALL | `EVEN | `ODD ]
type number_up_layout =
  [ `LEFT_TO_RIGHT_TOP_TO_BOTTOM | `LEFT_TO_RIGHT_BOTTOM_TO_TOP
  | `RIGHT_TO_LEFT_TOP_TO_BOTTOM | `RIGHT_TO_LEFT_BOTTOM_TO_TOP
  | `TOP_TO_BOTTOM_LEFT_TO_RIGHT | `TOP_TO_BOTTOM_RIGHT_TO_LEFT
  | `BOTTOM_TO_TOP_LEFT_TO_RIGHT | `BOTTOM_TO_TOP_RIGHT_TO_LEFT ]
type page_orientation =
  [ `PORTRAIT | `LANDSCAPE | `REVERSE_PORTRAIT | `REVERSE_LANDSCAPE ]
type print_quality = [ `LOW | `NORMAL | `HIGH | `DRAFT ]
type print_duplex = [ `SIMPLEX | `HORIZONTAL | `VERTICAL ]
type gtk_unit = [ `NONE | `POINTS | `INCH | `MM | `PIXEL ]
type tree_view_grid_lines = [ `NONE | `HORIZONTAL | `VERTICAL | `BOTH ]
type drag_result =
  [ `SUCCESS | `NO_TARGET | `USER_CANCELLED | `TIMEOUT_EXPIRED | `GRAB_BROKEN
  | `ERROR ]
type size_group_mode = [ `NONE | `HORIZONTAL | `VERTICAL | `BOTH ]
type size_request_mode =
  [ `HEIGHT_FOR_WIDTH | `WIDTH_FOR_HEIGHT | `CONSTANT_SIZE ]
type scrollable_policy = [ `MINIMUM | `NATURAL ]
type state_flag =
  [ `NORMAL | `ACTIVE | `PRELIGHT | `SELECTED | `INSENSITIVE | `INCONSISTENT
  | `FOCUSED | `BACKDROP | `DIR_LTR | `DIR_RTL | `LINK | `VISITED
  | `CHECKED ]
type region_flag = [ `EVEN | `ODD | `FIRST | `LAST | `ONLY | `SORTED ]
type junction_sides =
  [ `NONE | `CORNER_TOPLEFT | `CORNER_TOPRIGHT | `CORNER_BOTTOMLEFT
  | `CORNER_BOTTOMRIGHT | `TOP | `BOTTOM | `LEFT | `RIGHT ]
type border_style =
  [ `NONE | `SOLID | `INSET | `OUTSET | `HIDDEN | `DOTTED | `DASHED | `DOUBLE
  | `GROOVE | `RIDGE ]
type level_bar_mode = [ `CONTINUOUS | `DISCRETE ]
type input_purpose =
  [ `FREE_FORM | `ALPHA | `DIGITS | `NUMBER | `PHONE | `URL | `EMAIL | `NAME
  | `PASSWORD | `PIN ]
type input_hints =
  [ `NONE | `SPELLCHECK | `NO_SPELLCHECK | `WORD_COMPLETION | `LOWERCASE
  | `UPPERCASE_CHARS | `UPPERCASE_WORDS | `UPPERCASE_SENTENCES
  | `INHIBIT_OSK ]
type propagation_phase = [ `NONE | `CAPTURE | `BUBBLE | `TARGET ]
type event_sequence_state = [ `NONE | `CLAIMED | `DENIED ]
type pan_direction = [ `LEFT | `RIGHT | `UP | `DOWN ]
type text_window_type =
  [ `PRIVATE | `WIDGET | `TEXT | `LEFT | `RIGHT | `TOP | `BOTTOM ]
type text_view_layer = [ `BELOW | `ABOVE ]
type text_extend_selection = [ `WORD | `LINE ]
type text_search_flag = [ `VISIBLE_ONLY | `TEXT_ONLY | `CASE_INSENSITIVE ]
type toolbar_space_style = [ `EMPTY | `LINE ]
type spin_button_update_policy = [ `ALWAYS | `IF_VALID ]
type spin_type =
  [ `STEP_FORWARD | `STEP_BACKWARD | `PAGE_FORWARD | `PAGE_BACKWARD | `HOME
  | `END | `USER_DEFINED ]
type accel_flag = [ `VISIBLE | `LOCKED ]
type button_box_style =
  [ `SPREAD | `EDGE | `START | `END | `CENTER | `EXPAND ]
type calendar_display_options =
  [ `SHOW_HEADING | `SHOW_DAY_NAMES | `NO_MONTH_CHANGE | `SHOW_WEEK_NUMBERS
  | `SHOW_DETAILS ]
type resize_mode = [ `PARENT | `QUEUE | `IMMEDIATE ]
type dest_defaults = [ `MOTION | `HIGHLIGHT | `DROP | `ALL ]
type target_flags = [ `SAME_APP | `SAME_WIDGET | `OTHER_APP | `OTHER_WIDGET ]
type corner_type = [ `TOP_LEFT | `BOTTOM_LEFT | `TOP_RIGHT | `BOTTOM_RIGHT ]
type policy_type = [ `ALWAYS | `AUTOMATIC | `NEVER | `EXTERNAL ]
type tree_model_flags = [ `ITERS_PERSIST | `LIST_ONLY ]
type tree_view_drop_position =
  [ `BEFORE | `AFTER | `INTO_OR_BEFORE | `INTO_OR_AFTER ]
type tree_view_column_sizing = [ `GROW_ONLY | `AUTOSIZE | `FIXED ]
type cell_renderer_state =
  [ `SELECTED | `PRELIT | `INSENSITIVE | `SORTED | `FOCUSED | `EXPANDABLE
  | `EXPANDED ]
type cell_renderer_mode = [ `INERT | `ACTIVATABLE | `EDITABLE ]
type cell_renderer_accel_mode = [ `GTK | `OTHER ]
type buttons_type = [ `NONE | `OK | `CLOSE | `CANCEL | `YES_NO | `OK_CANCEL ]
type dialog_flag = [ `MODAL | `DESTROY_WITH_PARENT | `USE_HEADER_BAR ]
type response =
  [ `NONE | `REJECT | `ACCEPT | `DELETE_EVENT | `OK | `CANCEL | `CLOSE | `YES
  | `NO | `APPLY | `HELP ]
type widget_help_type = [ `TOOLTIP | `WHATS_THIS ]
type window_position =
  [ `NONE | `CENTER | `MOUSE | `CENTER_ALWAYS | `CENTER_ON_PARENT ]
type window_type = [ `TOPLEVEL | `POPUP ]
type image_type =
  [ `EMPTY | `PIXBUF | `STOCK | `ICON_SET | `ANIMATION | `ICON_NAME | `GICON
  | `SURFACE ]
type file_chooser_action =
  [ `OPEN | `SAVE | `SELECT_FOLDER | `CREATE_FOLDER ]
type file_chooser_confirmation =
  [ `CONFIRM | `ACCEPT_FILENAME | `SELECT_AGAIN ]
type file_chooser_errot =
  [ `NONEXISTENT | `BAD_FILENAME | `ALREADY_EXISTS | `INCOMPLETE_HOSTNAME ]
type file_filter_flags = [ `FILENAME | `URI | `DISPLAY_NAME | `MIME_TYPE ]
type ui_manager_item_type =
  [ `AUTO | `MENUBAR | `MENU | `TOOLBAR | `PLACEHOLDER | `POPUP | `MENUITEM
  | `TOOLITEM | `SEPARATOR | `ACCELERATOR | `POPUP_WITH_ACCELS ]
type assistant_page_type =
  [ `CONTENT | `INTRO | `CONFIRM | `SUMMARY | `PROGRESS | `CUSTOM ]
type entry_icon_position = [ `PRIMARY | `SECONDARY ] 

(**/**)

module Conv = struct
  open Gpointer

  external _get_tables : unit ->
      align variant_table
    * arrow_type variant_table
    * attach_options variant_table
    * baseline_position variant_table
    * delete_type variant_table
    * direction_type variant_table
    * icon_size variant_table
    * sensitivity_type variant_table
    * text_direction variant_table
    * justification variant_table
    * menu_direction_type variant_table
    * message_type variant_table
    * movement_step variant_table
    * orientation variant_table
    * pack_type variant_table
    * position_type variant_table
    * relief_style variant_table
    * scroll_step variant_table
    * scroll_type variant_table
    * selection_mode variant_table
    * shadow_type variant_table
    * state_type variant_table
    * toolbar_style variant_table
    * wrap_mode variant_table
    * sort_type variant_table
    * pack_direction variant_table
    * print_pages variant_table
    * page_set variant_table
    * number_up_layout variant_table
    * page_orientation variant_table
    * print_quality variant_table
    * print_duplex variant_table
    * gtk_unit variant_table
    * tree_view_grid_lines variant_table
    * drag_result variant_table
    * size_group_mode variant_table
    * size_request_mode variant_table
    * scrollable_policy variant_table
    * state_flag variant_table
    * region_flag variant_table
    * junction_sides variant_table
    * border_style variant_table
    * level_bar_mode variant_table
    * input_purpose variant_table
    * input_hints variant_table
    * propagation_phase variant_table
    * event_sequence_state variant_table
    * pan_direction variant_table
    * text_window_type variant_table
    * text_view_layer variant_table
    * text_extend_selection variant_table
    * text_search_flag variant_table
    * toolbar_space_style variant_table
    * spin_button_update_policy variant_table
    * spin_type variant_table
    * accel_flag variant_table
    * button_box_style variant_table
    * calendar_display_options variant_table
    * resize_mode variant_table
    * dest_defaults variant_table
    * target_flags variant_table
    * corner_type variant_table
    * policy_type variant_table
    * tree_model_flags variant_table
    * tree_view_drop_position variant_table
    * tree_view_column_sizing variant_table
    * cell_renderer_state variant_table
    * cell_renderer_mode variant_table
    * cell_renderer_accel_mode variant_table
    * buttons_type variant_table
    * dialog_flag variant_table
    * response variant_table
    * widget_help_type variant_table
    * window_position variant_table
    * window_type variant_table
    * image_type variant_table
    * file_chooser_action variant_table
    * file_chooser_confirmation variant_table
    * file_chooser_errot variant_table
    * file_filter_flags variant_table
    * ui_manager_item_type variant_table
    * assistant_page_type variant_table
    * entry_icon_position variant_table
    = "ml_gtk_get_tables"

  let align_tbl, arrow_type_tbl, attach_options_tbl, baseline_position_tbl,
      delete_type_tbl, direction_type_tbl, icon_size_tbl,
      sensitivity_type_tbl, text_direction_tbl, justification_tbl,
      menu_direction_type_tbl, message_type_tbl, movement_step_tbl,
      orientation_tbl, pack_type_tbl, position_type_tbl, relief_style_tbl,
      scroll_step_tbl, scroll_type_tbl, selection_mode_tbl, shadow_type_tbl,
      state_type_tbl, toolbar_style_tbl, wrap_mode_tbl, sort_type_tbl,
      pack_direction_tbl, print_pages_tbl, page_set_tbl,
      number_up_layout_tbl, page_orientation_tbl, print_quality_tbl,
      print_duplex_tbl, gtk_unit_tbl, tree_view_grid_lines_tbl,
      drag_result_tbl, size_group_mode_tbl, size_request_mode_tbl,
      scrollable_policy_tbl, state_flag_tbl, region_flag_tbl,
      junction_sides_tbl, border_style_tbl, level_bar_mode_tbl,
      input_purpose_tbl, input_hints_tbl, propagation_phase_tbl,
      event_sequence_state_tbl, pan_direction_tbl, text_window_type_tbl,
      text_view_layer_tbl, text_extend_selection_tbl, text_search_flag_tbl,
      toolbar_space_style_tbl, spin_button_update_policy_tbl, spin_type_tbl,
      accel_flag_tbl, button_box_style_tbl, calendar_display_options_tbl,
      resize_mode_tbl, dest_defaults_tbl, target_flags_tbl, corner_type_tbl,
      policy_type_tbl, tree_model_flags_tbl, tree_view_drop_position_tbl,
      tree_view_column_sizing_tbl, cell_renderer_state_tbl,
      cell_renderer_mode_tbl, cell_renderer_accel_mode_tbl, buttons_type_tbl,
      dialog_flag_tbl, response_tbl, widget_help_type_tbl,
      window_position_tbl, window_type_tbl, image_type_tbl,
      file_chooser_action_tbl, file_chooser_confirmation_tbl,
      file_chooser_errot_tbl, file_filter_flags_tbl,
      ui_manager_item_type_tbl, assistant_page_type_tbl,
      entry_icon_position_tbl = _get_tables ()

  let _make_enum = Gobject.Data.enum
  let align = _make_enum align_tbl
  let arrow_type = _make_enum arrow_type_tbl
  let attach_options = _make_enum attach_options_tbl
  let baseline_position = _make_enum baseline_position_tbl
  let delete_type = _make_enum delete_type_tbl
  let direction_type = _make_enum direction_type_tbl
  let icon_size = _make_enum icon_size_tbl
  let sensitivity_type = _make_enum sensitivity_type_tbl
  let text_direction = _make_enum text_direction_tbl
  let justification = _make_enum justification_tbl
  let menu_direction_type = _make_enum menu_direction_type_tbl
  let message_type = _make_enum message_type_tbl
  let movement_step = _make_enum movement_step_tbl
  let orientation = _make_enum orientation_tbl
  let pack_type = _make_enum pack_type_tbl
  let position_type = _make_enum position_type_tbl
  let relief_style = _make_enum relief_style_tbl
  let scroll_step = _make_enum scroll_step_tbl
  let scroll_type = _make_enum scroll_type_tbl
  let selection_mode = _make_enum selection_mode_tbl
  let shadow_type = _make_enum shadow_type_tbl
  let state_type = _make_enum state_type_tbl
  let toolbar_style = _make_enum toolbar_style_tbl
  let wrap_mode = _make_enum wrap_mode_tbl
  let sort_type = _make_enum sort_type_tbl
  let pack_direction = _make_enum pack_direction_tbl
  let print_pages = _make_enum print_pages_tbl
  let page_set = _make_enum page_set_tbl
  let number_up_layout = _make_enum number_up_layout_tbl
  let page_orientation = _make_enum page_orientation_tbl
  let print_quality = _make_enum print_quality_tbl
  let print_duplex = _make_enum print_duplex_tbl
  let gtk_unit = _make_enum gtk_unit_tbl
  let tree_view_grid_lines = _make_enum tree_view_grid_lines_tbl
  let drag_result = _make_enum drag_result_tbl
  let size_group_mode = _make_enum size_group_mode_tbl
  let size_request_mode = _make_enum size_request_mode_tbl
  let scrollable_policy = _make_enum scrollable_policy_tbl
  let state_flag = Gobject.Data.flags state_flag_tbl
  let region_flag = Gobject.Data.flags region_flag_tbl
  let junction_sides = _make_enum junction_sides_tbl
  let border_style = _make_enum border_style_tbl
  let level_bar_mode = _make_enum level_bar_mode_tbl
  let input_purpose = _make_enum input_purpose_tbl
  let input_hints = _make_enum input_hints_tbl
  let propagation_phase = _make_enum propagation_phase_tbl
  let event_sequence_state = _make_enum event_sequence_state_tbl
  let pan_direction = _make_enum pan_direction_tbl
  let text_window_type = _make_enum text_window_type_tbl
  let text_view_layer = _make_enum text_view_layer_tbl
  let text_extend_selection = _make_enum text_extend_selection_tbl
  let text_search_flag = _make_enum text_search_flag_tbl
  let toolbar_space_style = _make_enum toolbar_space_style_tbl
  let spin_button_update_policy = _make_enum spin_button_update_policy_tbl
  let spin_type = _make_enum spin_type_tbl
  let accel_flag = _make_enum accel_flag_tbl
  let button_box_style = _make_enum button_box_style_tbl
  let calendar_display_options = _make_enum calendar_display_options_tbl
  let resize_mode = _make_enum resize_mode_tbl
  let dest_defaults = _make_enum dest_defaults_tbl
  let target_flags = _make_enum target_flags_tbl
  let corner_type = _make_enum corner_type_tbl
  let policy_type = _make_enum policy_type_tbl
  let tree_model_flags = _make_enum tree_model_flags_tbl
  let tree_view_drop_position = _make_enum tree_view_drop_position_tbl
  let tree_view_column_sizing = _make_enum tree_view_column_sizing_tbl
  let cell_renderer_state = _make_enum cell_renderer_state_tbl
  let cell_renderer_mode = _make_enum cell_renderer_mode_tbl
  let cell_renderer_accel_mode = _make_enum cell_renderer_accel_mode_tbl
  let buttons_type = _make_enum buttons_type_tbl
  let dialog_flag = _make_enum dialog_flag_tbl
  let response = _make_enum response_tbl
  let widget_help_type = _make_enum widget_help_type_tbl
  let window_position = _make_enum window_position_tbl
  let window_type = _make_enum window_type_tbl
  let image_type = _make_enum image_type_tbl
  let file_chooser_action = _make_enum file_chooser_action_tbl
  let file_chooser_confirmation = _make_enum file_chooser_confirmation_tbl
  let file_chooser_errot = _make_enum file_chooser_errot_tbl
  let file_filter_flags = _make_enum file_filter_flags_tbl
  let ui_manager_item_type = _make_enum ui_manager_item_type_tbl
  let assistant_page_type = _make_enum assistant_page_type_tbl
  let entry_icon_position = _make_enum entry_icon_position_tbl
end
