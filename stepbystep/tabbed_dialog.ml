open Js_of_ocaml
open Js_of_ocaml_tyxml
open Utils

let jd_start = ref 2451544.5
let jd_stop = ref 2451544.5
let txtdate_start = ref ""
let txtdate_stop = ref ""
let txttime_start = ref ""
let txttime_stop = ref ""
let mybody = ref "Sun"
let name = ref ""
let sequence = ref ""
let discoverer = ref ""

(* Get current date and tomorrow's date *)
let today = Unix.time()
let tomorrow = today +. 86400.0
    
let confirm_my_button _ = fun _ ->
  true

let handle_julian_date txtdate jd selected_date =
	  let yr,mon,dy = Scanf.sscanf selected_date "%d-%d-%d" (fun yr mon dy -> yr,mon,dy) in
          let element = Js_of_ocaml.Dom_html.getElementById "date-message" in
	  let jd_frac = !jd -. 0.5 -. floor (!jd -. 0.5) in
	  txtdate := selected_date;
	  jd := Altaz.computeTheJulianDay true yr mon dy +. jd_frac;
	  if !jd < !jd_start then jd_start := !jd;
	  if !jd > !jd_stop then jd_stop := !jd;
          set_static_text element ("Julian Date Start: "^string_of_float !jd_start^", Stop: "^ string_of_float !jd_stop);
          true

let handle_julian_time txttime jd selected_time =          
	  let hr,min = Scanf.sscanf selected_time "%d:%d" (fun hr min -> hr,min) in
          let element = Js_of_ocaml.Dom_html.getElementById "date-message" in
	  txttime := selected_time;
	  jd := floor (!jd -. 0.5) +. 0.5 +. float_of_int(hr*3600+min*60) /. 86400.0;
	  if !jd < !jd_start then jd_start := !jd;
	  if !jd > !jd_stop then jd_stop := !jd;
          set_static_text element ("Julian Date Start: "^string_of_float !jd_start^", Stop: "^ string_of_float !jd_stop);
          true

let handle_startend_date_change txtdate jd ev =
  Js.Opt.case (ev##.target)
    (fun () -> false)
    (fun target ->
      let input = Dom_html.CoerceTo.input target in
      Js.Opt.case input
        (fun () -> false)
        (fun input -> handle_julian_date txtdate jd (Js.to_string (input##.value)))
    )

let handle_startend_time_change txttime jd ev =
  Js.Opt.case (ev##.target)
    (fun () -> false)
    (fun target ->
      let input = Dom_html.CoerceTo.input target in
      Js.Opt.case input
        (fun () -> false)
        (fun input -> handle_julian_time txttime jd (Js.to_string (input##.value)))
    )

let create_date_picker () =
  let open Tyxml_js.Html in
  let message_div = div ~a:[a_id "date-message"; a_style "padding-top: 15px; font-size: 16px; color: #333; display: none;"] [txt ""] in

  let start_date_label = label ~a:[a_label_for "start-date-picker"] [txt "Start Date: "] in
  let start_date_input = input ~a:[
    a_id "start-date-picker"; 
    a_input_type `Date;
    a_value (format_date today);  (* Set default to current date *)
    a_oninput (handle_startend_date_change txtdate_start jd_start)
  ] () in
  
  let start_time_label = label ~a:[a_label_for "start-time-picker"] [txt "Start Time: "] in
  let start_time_input = input ~a:[
    a_id "start-time-picker"; 
    a_input_type `Time;
    a_value (format_time today);  (* Set default to current date *)
    a_oninput (handle_startend_time_change txttime_start jd_start)
  ] () in
  
  let end_date_label = label ~a:[a_label_for "end-date-picker"] [txt "End Date: "] in
  let end_date_input = input ~a:[
    a_id "end-date-picker"; 
    a_input_type `Date;
    a_value (format_date tomorrow);  (* Set default to current date *)
   a_oninput (handle_startend_date_change txtdate_stop jd_stop)
  ] () in
  
  let end_time_label = label ~a:[a_label_for "end-time-picker"] [txt "End Time: "] in
  let end_time_input = input ~a:[
    a_id "end-time-picker"; 
    a_input_type `Time;
    a_value (format_time tomorrow);  (* Set default to current date *)
    a_oninput (handle_startend_time_change txttime_stop jd_stop)
  ] () in
  
  div [
    start_date_label; br (); start_date_input; 
    br (); 
    start_time_label; br (); start_time_input;
    br (); 
    end_date_label; br (); end_date_input; 
    br (); 
    end_time_label; br (); end_time_input;
  message_div
  ]

let tz_local () =
    let dummy = (Js.Unsafe.obj [||]) in
    let intl = Js.Unsafe.global##.Intl in
    let date = intl##DateTimeFormat(dummy) in
    let options = date##resolvedOptions(dummy) in
    let tz = options##.timeZone in
    Js.to_string tz

let create_color_picker () =
  let open Tyxml_js.Html in
  let label = label ~a:[a_label_for "color-picker"] [txt "Pick a color: "] in
  let input = input ~a:[a_id "color-picker"; a_input_type `Color] () in
  div [label; br (); input]
