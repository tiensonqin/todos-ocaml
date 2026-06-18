open! Core
open UIKit
open Runtime
open Objc

module Apple = Bonsai_apple
module App = Bonsai_apple_uikit.App
module Store = Todos.Todo_store

type todo_section =
  { title : string
  ; todos : Store.todo list
  }

let mounted_apps = ref []
let window = ref None
let root_controller = ref None
let table_view = ref None
let table_views = ref []
let search_query = ref ""
let last_selected_tab = ref None
let store = ref (Store.demo ())
let todos_cache = ref (Store.all !store)
let retained_objects = ref []
let retained_blocks = ref []

let nsstring value = new_string value

let flexible_size_mask = _UIViewAutoresizingFlexibleWidth lor _UIViewAutoresizingFlexibleHeight

let zero_rect = CoreGraphics.CGRect.make ~x:0. ~y:0. ~width:0. ~height:0.
let table_tag = 1001

let nsarray values =
  let array = NSMutableArray.self |> NSMutableArrayClass.arrayWithCapacity (List.length values) in
  List.iter values ~f:(fun value -> NSMutableArray.addObject value array);
  array
;;

let retain_object object_ = retained_objects := Obj.repr object_ :: !retained_objects
let retain_block block = retained_blocks := Obj.repr block :: !retained_blocks

let refresh_cache () = todos_cache := Store.all !store

let make_view_controller_provider controller =
  let block =
    Block.make ~args:Objc_type.[ id ] ~return:Objc_type.id (fun _block _tab -> controller)
  in
  retain_block block;
  block
;;

let make_tab ~title ~icon ~identifier controller =
  let image = UIImage.self |> UIImageClass.systemImageNamed (nsstring icon) in
  let provider = make_view_controller_provider controller in
  let tab =
    msg_send
      ~self:(get_class "UITab" |> alloc)
      ~cmd:(selector "initWithTitle:image:identifier:viewControllerProvider:")
      ~typ:(id @-> id @-> id @-> (ptr void) @-> returning id)
      (nsstring title)
      image
      (nsstring identifier)
      provider
  in
  retain_object tab;
  tab
;;

let make_search_tab controller =
  let provider = make_view_controller_provider controller in
  let tab =
    msg_send
      ~self:(get_class "UISearchTab" |> alloc)
      ~cmd:(selector "initWithViewControllerProvider:")
      ~typ:((ptr void) @-> returning id)
      provider
  in
  msg_send
    ~self:tab
    ~cmd:(selector "setAutomaticallyActivatesSearch:")
    ~typ:(bool @-> returning void)
    true;
  retain_object tab;
  tab
;;

let set_tabs tab_controller tabs =
  msg_send
    ~self:tab_controller
    ~cmd:(selector "setTabs:animated:")
    ~typ:(id @-> bool @-> returning void)
    (nsarray tabs)
    false
;;

let set_selected_tab tab_controller tab =
  msg_send
    ~self:tab_controller
    ~cmd:(selector "setSelectedTab:")
    ~typ:(id @-> returning void)
    tab
;;

let selected_tab tab_controller =
  msg_send ~self:tab_controller ~cmd:(selector "selectedTab") ~typ:(returning id)
;;

let string_of_nsstring value = if is_nil value then "" else NSString._UTF8String value

let tab_identifier tab =
  msg_send ~self:tab ~cmd:(selector "identifier") ~typ:(returning id)
  |> string_of_nsstring
;;

let date_formatter format =
  let formatter = NSDateFormatter.self |> alloc |> NSDateFormatter.init in
  NSDateFormatter.setDateFormat (nsstring format) formatter;
  formatter
;;

let format_date ~format date =
  let formatter = date_formatter format in
  NSDateFormatter.stringFromDate date formatter |> string_of_nsstring
;;

let parse_date ~format text =
  let formatter = date_formatter format in
  NSDateFormatter.dateFromString (nsstring text) formatter
;;

let alert_text_at alert index =
  let fields = UIAlertController.textFields alert in
  if is_nil fields
  then ""
  else (
    let field_count = NSArray.count fields in
    if index >= field_count
    then ""
    else (
      let field = NSArray.objectAtIndex index fields in
      if is_nil field then "" else UITextField.text field |> string_of_nsstring))
;;

let system_font size =
  msg_send
    ~self:(get_class "UIFont")
    ~cmd:(selector "systemFontOfSize:")
    ~typ:(double @-> returning id)
    size
;;

let bold_system_font size =
  msg_send
    ~self:(get_class "UIFont")
    ~cmd:(selector "boldSystemFontOfSize:")
    ~typ:(double @-> returning id)
    size
;;

let attributed_string value =
  msg_send
    ~self:(NSMutableAttributedString.self |> alloc)
    ~cmd:(selector "initWithString:")
    ~typ:(id @-> returning id)
    (nsstring value)
;;

let strikethrough value =
  let attributed = attributed_string value in
  let range =
    NSRange.init ~location:(ULLong.of_int 0) ~length:(ULLong.of_int (String.length value)) ()
  in
  NSMutableAttributedString.addAttribute
    _NSStrikethroughStyleAttributeName
    ~value:(NSNumberClass.numberWithInt 1 NSNumber.self)
    ~range
    attributed;
  NSMutableAttributedString.addAttribute
    _NSStrikethroughColorAttributeName
    ~value:(UIColorClass.secondaryLabelColor UIColor.self)
    ~range
    attributed;
  attributed
;;

let take n values =
  let rec loop n values acc =
    match n, values with
    | 0, _ | _, [] -> List.rev acc, values
    | n, value :: rest -> loop (n - 1) rest (value :: acc)
  in
  loop n values []
;;

let normalized value = value |> String.strip |> String.lowercase

let string_contains ~substring value =
  let value_length = String.length value in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > value_length
    then false
    else if String.equal (String.sub value ~pos:index ~len:substring_length) substring
    then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0
;;

let cached_todos ~query =
  match normalized query with
  | "" -> !todos_cache
  | query ->
    !todos_cache
    |> List.filter ~f:(fun todo ->
      string_contains (normalized todo.Store.title) ~substring:query)
;;

type table_mode =
  | Dashboard
  | Upcoming
  | Search

let sections_for ~mode ~query () =
  let todos = cached_todos ~query |> List.rev in
  let active, completed = List.partition_tf todos ~f:(fun todo -> not todo.Store.completed) in
  let today, upcoming = take 3 active in
  let sections =
    match mode with
    | Dashboard -> [ "Today", today; "Upcoming", upcoming; "Completed", completed ]
    | Upcoming -> [ "Upcoming", upcoming ]
    | Search -> [ "Today", today; "Upcoming", upcoming; "Completed", completed ]
  in
  sections
  |> List.filter_map ~f:(fun (title, todos) ->
    match todos with
    | [] -> None
    | todos -> Some { title; todos })
;;

let todo_at ~mode ~query index_path =
  let section_index = NSIndexPath.section index_path in
  let row_index = NSIndexPath.row index_path in
  sections_for ~mode ~query ()
  |> Fn.flip List.nth section_index
  |> Option.bind ~f:(fun section -> List.nth section.todos row_index)
;;

let reload_table () = List.iter !table_views ~f:UITableView.reloadData

let reload_table_animated () =
  let animate table =
    let animations =
      Block.make ~args:Objc_type.[] ~return:Objc_type.void (fun _block ->
        UITableView.reloadData table)
    in
    retain_block animations;
    UIViewClass.transitionWithView
      table
      ~duration:0.22
      ~options:
        (_UIViewAnimationOptionTransitionCrossDissolve
         lor _UIViewAnimationOptionAllowUserInteraction
         lor _UIViewAnimationOptionBeginFromCurrentState)
      ~animations
      ~completion:null
      UIView.self
  in
  match !table_views with
  | [] -> ()
  | tables -> List.iter tables ~f:animate
;;

let action_handler f =
  let block =
    Block.make ~args:Objc_type.[ id ] ~return:Objc_type.void (fun _block _sender -> f ())
  in
  retain_block block;
  UIActionClass.actionWithHandler block UIAction.self
;;

let alert_action ~title ~style f =
  let block =
    Block.make ~args:Objc_type.[ id ] ~return:Objc_type.void (fun _block _action -> f ())
  in
  retain_block block;
  UIAlertActionClass.actionWithTitle (nsstring title) ~style ~handler:block UIAlertAction.self
;;

let present_editor ?todo () =
  match !root_controller with
  | None -> ()
  | Some controller ->
    let title, action_title, initial_title, initial_date, initial_time =
      match todo with
      | None -> "New Task", "Add", "", "Today", ""
      | Some todo -> "Edit Task", "Save", todo.Store.title, todo.Store.date, todo.Store.time
    in
    let alert =
      UIAlertControllerClass.alertControllerWithTitle
        (nsstring title)
        ~message:nil
        ~preferredStyle:_UIAlertControllerStyleAlert
        UIAlertController.self
    in
    let add_text_field ~placeholder ~text =
      let configure_field =
        Block.make
          ~args:Objc_type.[ id ]
          ~return:Objc_type.void
          (fun _block field ->
             UITextField.setPlaceholder (nsstring placeholder) field;
             UITextField.setText (nsstring text) field;
             UITextField.setClearButtonMode _UITextFieldViewModeWhileEditing field)
      in
      retain_block configure_field;
      UIAlertController.addTextFieldWithConfigurationHandler configure_field alert
    in
    add_text_field ~placeholder:"Task title" ~text:initial_title;
    let add_picker_field ~placeholder ~text ~mode ~format =
      let configure_field =
        Block.make
          ~args:Objc_type.[ id ]
          ~return:Objc_type.void
          (fun _block field ->
             let picker =
               UIDatePicker.self
               |> alloc
               |> UIDatePicker.initWithFrame
                    (CoreGraphics.CGRect.make ~x:0. ~y:0. ~width:0. ~height:216.)
             in
             retain_object picker;
             UIDatePicker.setDatePickerMode mode picker;
             UIDatePicker.setPreferredDatePickerStyle _UIDatePickerStyleWheels picker;
             let parsed = parse_date ~format text in
             if not (is_nil parsed) then UIDatePicker.setDate parsed picker;
             UITextField.setPlaceholder (nsstring placeholder) field;
             UITextField.setText (nsstring text) field;
             UITextField.setInputView picker field;
             UITextField.setClearButtonMode _UITextFieldViewModeNever field;
             UIControl.addAction
               (action_handler (fun () ->
                  UIDatePicker.date picker
                  |> format_date ~format
                  |> nsstring
                  |> fun value -> UITextField.setText value field))
               ~forControlEvents:_UIControlEventValueChanged
               picker)
      in
      retain_block configure_field;
      UIAlertController.addTextFieldWithConfigurationHandler configure_field alert
    in
    add_picker_field
      ~placeholder:"Date"
      ~text:initial_date
      ~mode:_UIDatePickerModeDate
      ~format:"MMM d";
    add_picker_field
      ~placeholder:"Time"
      ~text:initial_time
      ~mode:_UIDatePickerModeTime
      ~format:"h:mm a";
    let save =
      alert_action ~title:action_title ~style:_UIAlertActionStyleDefault (fun () ->
        let title = alert_text_at alert 0 in
        let date = alert_text_at alert 1 in
        let time = alert_text_at alert 2 in
        store
        := (match todo with
            | None -> Store.add !store ~title ~date ~time
            | Some todo -> Store.rename !store ~id:todo.Store.id ~title ~date ~time);
        refresh_cache ();
        reload_table ())
    in
    let cancel =
      alert_action ~title:"Cancel" ~style:_UIAlertActionStyleCancel (fun () -> ())
    in
    UIAlertController.addAction cancel alert;
    UIAlertController.addAction save alert;
    UIViewController.presentViewController alert ~animated:true ~completion:null controller
;;

let install_tab_delegate tab_controller =
  let class_name = "TodosTabDelegate" ^ Int.to_string (Oo.id (object end)) in
  let _ =
    Class.define
      class_name
      ~superclass:NSObject.self
      ~methods:
        [ (Define.method_spec
             ~cmd:(selector "tabBarController:shouldSelectTab:")
             ~typ:(id @-> id @-> returning bool)
             ~enc:"c32@0:8@16@24"
           @@ fun self _cmd _tab_controller tab ->
           if String.equal (tab_identifier tab) "add"
           then (
             let previous_tab =
               match !last_selected_tab with
               | Some tab when not (is_nil tab) -> tab
               | _ -> selected_tab tab_controller
             in
             last_selected_tab := Some previous_tab;
             present_editor ();
             NSObject.performSelector3
               (selector "restoreSelectedTab:")
               ~withObject:tab_controller
               ~afterDelay:0.
               self;
             false)
           else true)
        ; (Define.method_spec
             ~cmd:(selector "tabBarController:didSelectTab:previousTab:")
             ~typ:(id @-> id @-> id @-> returning void)
             ~enc:"v40@0:8@16@24@32"
           @@ fun _self _cmd _tab_controller selected_tab _previous_tab ->
           if not (String.equal (tab_identifier selected_tab) "add")
           then last_selected_tab := Some selected_tab)
        ; (Define.method_spec
             ~cmd:(selector "restoreSelectedTab:")
             ~typ:(id @-> returning void)
             ~enc:"v32@0:8@16"
           @@ fun _self _cmd tab_controller ->
           match !last_selected_tab with
           | Some tab when not (is_nil tab) -> set_selected_tab tab_controller tab
           | _ -> ())
        ]
  in
  let delegate_ = Objc.get_class class_name |> alloc |> init in
  retain_object delegate_;
  UITabBarController.setDelegate delegate_ tab_controller
;;

let make_row_action ~title ~style ~color f =
  let block =
    Block.make
      ~args:Objc_type.[ id; id ]
      ~return:Objc_type.void
      (fun _block _action index_path -> f index_path)
  in
  retain_block block;
  let action =
    UITableViewRowActionClass.rowActionWithStyle
      style
      ~title:(nsstring title)
      ~handler:block
      UITableViewRowAction.self
  in
  UITableViewRowAction.setBackgroundColor color action;
  action
;;

let todo_metadata todo =
  match String.strip todo.Store.time, String.strip todo.Store.date with
  | time, _ when not (String.is_empty time) -> time
  | _, date -> date
;;

let should_show_section_header ~mode title =
  match mode, title with
  | Upcoming, "Upcoming" -> false
  | Dashboard, "Today" | Dashboard, "Upcoming" | Dashboard, "Completed" -> true
  | Search, "Today" -> false
  | _ -> true
;;

let configure_cell cell todo =
  UITableViewCell.setAccessoryType _UITableViewCellAccessoryNone cell;
  UITableViewCell.setSelectionStyle _UITableViewCellSelectionStyleDefault cell;
  let content = UITableViewCell.contentView cell in
  let toggle =
    UIButton.self |> UIButtonClass.buttonWithType _UIButtonTypeSystem
  in
  UIView.setFrame (CoreGraphics.CGRect.make ~x:14. ~y:14. ~width:30. ~height:30.) toggle;
  let toggle_image =
    UIImageClass.systemImageNamed
      (nsstring
         (if todo.Store.completed then "checkmark.circle.fill" else "circle"))
      UIImage.self
  in
  UIButton.setImage toggle_image ~forState:_UIControlStateNormal toggle;
  UIButton.setTintColor
    (if todo.completed
     then UIColorClass.systemGreenColor UIColor.self
     else UIColorClass.systemGray3Color UIColor.self)
    toggle;
  let toggle_action =
    action_handler (fun () ->
      store := Store.toggle !store ~id:todo.Store.id;
      refresh_cache ();
      reload_table_animated ())
  in
  UIControl.addAction toggle_action ~forControlEvents:_UIControlEventTouchUpInside toggle;
  let title_label =
    UILabel.self
    |> alloc
    |> UILabel.initWithFrame (CoreGraphics.CGRect.make ~x:56. ~y:16. ~width:210. ~height:26.)
  in
  if todo.completed
  then UILabel.setAttributedText (strikethrough todo.Store.title) title_label
  else UILabel.setText (nsstring todo.Store.title) title_label;
  UILabel.setFont (system_font 16.5) title_label;
  UILabel.setNumberOfLines 1 title_label;
  UILabel.setTextColor
    (if todo.completed
     then UIColorClass.secondaryLabelColor UIColor.self
     else UIColorClass.labelColor UIColor.self)
    title_label;
  let metadata_label =
    UILabel.self
    |> alloc
    |> UILabel.initWithFrame (CoreGraphics.CGRect.make ~x:274. ~y:18. ~width:78. ~height:22.)
  in
  let metadata = todo_metadata todo in
  UILabel.setText (nsstring metadata) metadata_label;
  UILabel.setFont (system_font 13.) metadata_label;
  UILabel.setTextAlignment _UITextAlignmentRight metadata_label;
  UILabel.setTextColor (UIColorClass.secondaryLabelColor UIColor.self) metadata_label;
  UIView.addSubview toggle content;
  UIView.addSubview title_label content;
  UIView.addSubview metadata_label content;
  cell
;;

let make_table_data_source ~mode ~query () =
  let class_name = "TodosTableDataSource" ^ Int.to_string (Oo.id (object end)) in
  let _ =
    Class.define
      class_name
      ~superclass:NSObject.self
      ~methods:
        [ (UITableViewControllerMethods.numberOfSectionsInTableView'
           @@ fun _self _cmd _table ->
           sections_for ~mode ~query:(query ()) () |> List.length |> LLong.of_int)
        ; (UITableViewControllerMethods.tableView'numberOfRowsInSection'
           @@ fun _self _cmd _table section ->
           sections_for ~mode ~query:(query ()) ()
           |> Fn.flip List.nth (LLong.to_int section)
           |> Option.value_map ~default:0 ~f:(fun section -> List.length section.todos)
           |> LLong.of_int)
        ; (UITableViewControllerMethods.tableView'titleForHeaderInSection'
           @@ fun _self _cmd _table section ->
           sections_for ~mode ~query:(query ()) ()
           |> Fn.flip List.nth (LLong.to_int section)
           |> Option.value_map ~default:nil ~f:(fun section ->
             if not (should_show_section_header ~mode section.title)
             then nil
             else nsstring (sprintf "%s  %d" section.title (List.length section.todos))))
        ; (UITableViewControllerMethods.tableView'cellForRowAtIndexPath'
           @@ fun _self _cmd _table index_path ->
           let cell =
             UITableViewCell.self
             |> alloc
             |> UITableViewCell.initWithStyle
                  _UITableViewCellStyleSubtitle
                  ~reuseIdentifier:(nsstring "TodoCell")
           in
           let query = query () in
           match todo_at ~mode ~query index_path with
           | None -> cell
           | Some todo -> configure_cell cell todo)
        ; (UITableViewDelegate.tableView'heightForRowAtIndexPath'
           @@ fun _self _cmd _table _index_path -> 58.)
        ; (UITableViewDelegate.tableView'didSelectRowAtIndexPath'
           @@ fun _self _cmd table index_path ->
           if Option.is_some (todo_at ~mode ~query:(query ()) index_path)
           then UITableView.deselectRowAtIndexPath index_path ~animated:true table)
        ; (UITableViewDelegate.tableView'editActionsForRowAtIndexPath'
           @@ fun _self _cmd _table index_path ->
           match todo_at ~mode ~query:(query ()) index_path with
           | None -> nil
           | Some todo ->
             let edit =
               make_row_action
                 ~title:"Edit"
                 ~style:_UITableViewRowActionStyleNormal
                 ~color:(UIColorClass.systemBlueColor UIColor.self)
                 (fun _index_path -> present_editor ~todo ())
             in
             let delete =
               make_row_action
                 ~title:"Delete"
                 ~style:_UITableViewRowActionStyleDestructive
                 ~color:(UIColorClass.systemRedColor UIColor.self)
                 (fun _index_path ->
                    store := Store.delete !store ~id:todo.Store.id;
                    refresh_cache ();
                    reload_table ())
            in
            nsarray [ delete; edit ])
        ]
  in
  Objc.get_class class_name |> alloc |> init
;;

let make_header_view () =
  let header =
    UIView.self
    |> alloc
    |> UIView.initWithFrame (CoreGraphics.CGRect.make ~x:0. ~y:0. ~width:390. ~height:112.)
  in
  UIView.setBackgroundColor (UIColorClass.clearColor UIColor.self) header;
  let title =
    UILabel.self
    |> alloc
    |> UILabel.initWithFrame (CoreGraphics.CGRect.make ~x:20. ~y:30. ~width:320. ~height:34.)
  in
  UILabel.setText (nsstring "Good morning") title;
  UILabel.setFont (bold_system_font 28.) title;
  UILabel.setTextColor (UIColorClass.labelColor UIColor.self) title;
  let subtitle =
    UILabel.self
    |> alloc
    |> UILabel.initWithFrame (CoreGraphics.CGRect.make ~x:20. ~y:66. ~width:320. ~height:24.)
  in
  UILabel.setText (nsstring "Let's get things done.") subtitle;
  UILabel.setFont (system_font 17.) subtitle;
  UILabel.setTextColor (UIColorClass.secondaryLabelColor UIColor.self) subtitle;
  UIView.addSubview title header;
  UIView.addSubview subtitle header;
  header
;;

let install_search_controller controller =
  let search_controller =
    UISearchController.self |> alloc |> UISearchController.initWithSearchResultsController nil
  in
  retain_object search_controller;
  UISearchController.setObscuresBackgroundDuringPresentation false search_controller;
  UISearchController.setHidesNavigationBarDuringPresentation false search_controller;
  UISearchController.setAutomaticallyShowsCancelButton true search_controller;
  let search_bar = UISearchController.searchBar search_controller in
  UISearchBar.setPlaceholder (nsstring "Search") search_bar;
  UISearchBar.setSearchBarStyle _UISearchBarStyleMinimal search_bar;
  let class_name = "TodosNativeSearchDelegate" ^ Int.to_string (Oo.id (object end)) in
  let _ =
    Class.define
      class_name
      ~superclass:NSObject.self
      ~methods:
        [ (UISearchBarDelegate.searchBar'textDidChange'
           @@ fun _self _cmd _search_bar text ->
           search_query := string_of_nsstring text;
           reload_table ())
        ; (UISearchBarDelegate.searchBarCancelButtonClicked'
           @@ fun _self _cmd _search_bar ->
           search_query := "";
           reload_table ())
        ]
  in
  let delegate_ = Objc.get_class class_name |> alloc |> init in
  retain_object delegate_;
  UISearchBar.setDelegate delegate_ search_bar;
  let navigation_item = UIViewController.navigationItem controller in
  UINavigationItem.setSearchController search_controller navigation_item;
  UINavigationItem.setHidesSearchBarWhenScrolling true navigation_item
;;

let layout_table_view self =
  let bounds = UIView.bounds self in
  let size = CoreGraphics.CGRect.size bounds in
  let width = CoreGraphics.CGSize.width size in
  let height = CoreGraphics.CGSize.height size in
  let table = UIView.viewWithTag table_tag self in
  if not (is_nil table)
  then UIView.setFrame (CoreGraphics.CGRect.make ~x:0. ~y:0. ~width ~height) table
;;

let install_table_view ~mode ~query ?(show_header = false) self =
  if is_nil (UIView.viewWithTag table_tag self)
  then (
    UIView.setBackgroundColor (UIColorClass.systemGroupedBackgroundColor UIColor.self) self;
    let table =
      UITableView.self
      |> alloc
      |> UITableView.initWithFrame' zero_rect ~style:_UITableViewStyleInsetGrouped
    in
    table_view := Some table;
    UIView.setTag table_tag table;
    UIView.setAutoresizingMask flexible_size_mask table;
    UITableView.setBackgroundColor (UIColorClass.systemGroupedBackgroundColor UIColor.self) table;
    UITableView.setShowsVerticalScrollIndicator false table;
    UITableView.setRowHeight 58. table;
    UITableView.setEstimatedRowHeight 58. table;
    UITableView.setSectionHeaderTopPadding 8. table;
    UITableView.setContentInset
      (UIEdgeInsets.init ~top:0. ~left:0. ~bottom:118. ~right:0.)
      table;
    if show_header then UITableView.setTableHeaderView (make_header_view ()) table;
    let data_source = make_table_data_source ~mode ~query () in
    retain_object data_source;
    UITableView.setDataSource data_source table;
    UITableView.setDelegate data_source table;
    UIView.addSubview table self;
    table_views := table :: !table_views;
    layout_table_view self);
  self
;;

let register_table_view ~class_name ~mode ~query ~show_header =
  let _ =
    Class.define
      class_name
      ~superclass:UIView.self
      ~methods:
        [ (UIViewMethods.didMoveToSuperview
           @@ fun self _cmd ->
           ignore (install_table_view ~mode ~query ~show_header self))
        ; (UIViewMethods.layoutSubviews @@ fun self _cmd -> layout_table_view self)
        ]
  in
  ()
;;

let register_views () =
  register_table_view
    ~class_name:"TodosDashboardView"
    ~mode:Dashboard
    ~query:(fun () -> "")
    ~show_header:true;
  register_table_view
    ~class_name:"TodosUpcomingView"
    ~mode:Upcoming
    ~query:(fun () -> "")
    ~show_header:false;
  register_table_view
    ~class_name:"TodosSearchView"
    ~mode:Search
    ~query:(fun () -> !search_query)
    ~show_header:false
;;

let component _graph = Bonsai.return (Apple.custom_view ~kind:"TodosDashboardView" ())

let install_tab_item ~title ~icon controller =
  let title = nsstring title in
  let image = UIImage.self |> UIImageClass.systemImageNamed (nsstring icon) in
  let item =
    UITabBarItem.self |> alloc |> UITabBarItem.initWithTitle title ~image ~selectedImage:nil
  in
  UIViewController.setTitle title controller;
  UIViewController.setTabBarItem item controller
;;

let make_table_controller ~title ~icon ~class_name ~screen_bounds =
  let controller = UIViewController.self |> alloc |> init in
  let view = Objc.get_class class_name |> alloc |> UIView.initWithFrame screen_bounds in
  UIView.setAutoresizingMask flexible_size_mask view;
  UIViewController.setView view controller;
  UIViewController.setTitle (nsstring title) controller;
  let navigation =
    UINavigationController.self
    |> alloc
    |> UINavigationController.initWithRootViewController controller
  in
  install_tab_item ~title ~icon navigation;
  controller, navigation
;;

let install_root_view ~time_source app_delegate _cmd _application _launch_options =
  register_views ();
  let screen_bounds = UIScreen.self |> UIScreenClass.mainScreen |> UIScreen.bounds in
  let background_color = UIColor.self |> UIColorClass.systemGroupedBackgroundColor in
  let win = UIWindow.self |> alloc |> UIWindow.initWithFrame screen_bounds in
  UIView.setBackgroundColor background_color win;
  table_views := [];
  let app = App.create ~time_source component in
  App.flush_and_render app;
  mounted_apps := [ app ];
  let tab_controller = UITabBarController.self |> alloc |> init in
  (match App.view app with
   | None -> ()
   | Some root ->
     let root_view = Bonsai_apple_uikit.native root in
     let controller = Bonsai_apple_uikit.controller root in
     UIView.setAutoresizingMask flexible_size_mask root_view;
     UIView.setBackgroundColor background_color root_view;
     let upcoming_controller, _upcoming_navigation =
       make_table_controller
         ~title:"Upcoming"
         ~icon:"calendar"
         ~class_name:"TodosUpcomingView"
         ~screen_bounds
     in
     let search_controller, search_navigation =
       make_table_controller
         ~title:"Search"
         ~icon:"magnifyingglass"
         ~class_name:"TodosSearchView"
         ~screen_bounds
     in
     install_search_controller search_controller;
     let today_tab =
       make_tab
         ~title:"Today"
         ~icon:"sun.max"
         ~identifier:"today"
         controller
     in
     let upcoming_tab =
       make_tab
         ~title:"Upcoming"
         ~icon:"calendar"
         ~identifier:"upcoming"
         upcoming_controller
     in
     let add_controller = UIViewController.self |> alloc |> init in
     let add_tab =
       make_tab ~title:"Add" ~icon:"plus" ~identifier:"add" add_controller
     in
     let search_tab = make_search_tab search_navigation in
     set_tabs tab_controller [ today_tab; upcoming_tab; add_tab; search_tab ];
     set_selected_tab tab_controller today_tab;
     last_selected_tab := Some today_tab;
     root_controller := Some tab_controller;
     install_tab_delegate tab_controller;
     UIWindow.setRootViewController tab_controller win);
  UIWindow.makeKeyAndVisible win;
  window := Some win;
  ignore app_delegate;
  true
;;

let main ~time_source =
  let _ =
    Class.define
      "TodosAppDelegate"
      ~superclass:UIResponder.self
      ~methods:
        [ (UIApplicationDelegate.application'didFinishLaunchingWithOptions'
           @@ install_root_view ~time_source)
        ]
  in
  _UIApplicationMain
    0
    (Objc.from_voidp Objc.string Objc.null)
    nil
    (new_string "TodosAppDelegate")
  |> exit
;;

let () = main ~time_source:(Bonsai.Time_source.create ~start:Time_ns.epoch)
