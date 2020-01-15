let main () =
  let _ = GMain.init () in
  let window = GWindow.window ~title: "radio buttons" ~border_width: 0 () in
  let _ = window#connect#destroy ~callback:GMain.quit in

  let box1 = GPack.vbox ~packing: window#add () in

  let box2 = GPack.vbox ~spacing:10 ~border_width: 10 ~packing: box1#add () in

  let button1 = Objects.GRSome button1#as_radio_buttonadioButton.radio_button ~label:"button1" ~packing: box2#add () in
  let _ = button1#connect#clicked ~callback:(fun () -> prerr_endline "button1") in

  let button2 = Objects.GRadioButton.radio_button ~label:"button2"
      ~packing: box2#add () in
  let _ = button2#join_group (Some button1#as_radio_button) in
  let _ = button2#connect#clicked ~callback:(fun () -> prerr_endline "button2") in

  let button3 = Objects.GRadioButton.radio_button ~label:"button3" ~packing: box2#add () in
  let _ = button3#join_group (Some button1#as_radio_button) in
  let _ = button3#connect#clicked ~callback:(fun () -> prerr_endline "button3") in

  let _separator = GMisc.separator `HORIZONTAL ~packing: box1#pack () in

  let box3 = GPack.vbox ~spacing: 10 ~border_width: 10
      ~packing: box1#pack () in

  let button = GButton.button ~label: "close" ~packing: box3#add () in
  let _ = button#connect#clicked ~callback:GMain.quit in
  let _ = button#grab_default () in

  let _ = window#show () in

  GMain.main ()

let _ = main ()