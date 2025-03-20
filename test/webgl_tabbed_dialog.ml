open Js_of_ocaml
open Js_of_ocaml.Js
open Js_of_ocaml_tyxml
open Utils

let tab_content = ref []

(* Tab selection logic *)
let select_tab tab_id =
  let tabs = Dom_html.document##getElementsByClassName (Js.string "tab-content") in
  for i = 0 to tabs##.length - 1 do
    let tab = Js.Opt.get (tabs##item i) (fun () -> assert false) in
    tab##.style##.display := string "none"
  done;
  
  let selected_tab = Dom_html.getElementById tab_id in
  selected_tab##.style##.display := string "block";
  
  let tab_buttons = Dom_html.document##getElementsByClassName (string "tab-button") in
  for i = 0 to tab_buttons##.length - 1 do
    let btn = Js.Opt.get (tab_buttons##item i) (fun () -> assert false) in
    btn##.classList##remove (string "active")
  done;
  
  let active_button = Dom_html.getElementById (tab_id ^ "-btn") in
  active_button##.classList##add (string "active");
  true

(* Create tabs interface *)
let create_tabbed_interface tabs =
  let open Tyxml_js.Html in
  
  (* Tab buttons *)
  let tab_buttons = List.map (fun (id, title, _, _) ->
    button ~a:[
      a_id (id ^ "-btn");
      a_class ["tab-button"];
      a_onclick (fun _ -> select_tab id)
    ] [txt title]
  ) tabs in
  
  (* Tab button container *)
  let tab_nav = div ~a:[a_class ["tab-nav"]] tab_buttons in
  
  (* Tab content *)
  let tab_contents = List.map (fun (id, _, content, is_active) ->
    div ~a:[
      a_id id;
      a_class ["tab-content"];
      a_style (if is_active then "display: block;" else "display: none;")
    ] [content]
  ) tabs in
  
  (* Return the full tabbed interface *)
  div ~a:[a_class ["tabbed-dialog"]] (tab_nav :: tab_contents)

(* Initialize the application *)
let init () =
  let open Tyxml_js.Html in
  
  (* Add styles *)
  let style_el = Dom_html.createStyle Dom_html.document in
  Dom.appendChild style_el (Dom_html.document##createTextNode (string "
    .tabbed-dialog {
      font-family: Arial, sans-serif;
      max-width: 900px;
      margin: 0 auto;
      padding: 20px;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
      border-radius: 8px;
    }
    .tab-nav {
      display: flex;
      border-bottom: 1px solid #ccc;
      margin-bottom: 20px;
    }
    .tab-button {
      padding: 10px 20px;
      background: none;
      border: none;
      border-bottom: 3px solid transparent;
      cursor: pointer;
      font-size: 16px;
      outline: none;
      transition: all 0.3s;
    }
    .tab-button:hover {
      background-color: #f5f5f5;
    }
    .tab-button.active {
      border-bottom: 3px solid #4a90e2;
      font-weight: bold;
    }
    .tab-content {
      padding: 10px;
    }
    h2 {
      color: #333;
    }
  "));
  Dom.appendChild (Dom_html.document##.head) style_el;
  
  (* Create tab content *)
  let date_picker = Tabbed_dialog.create_date_picker () in
  let color_picker = Tabbed_dialog.create_color_picker () in
  let webgl_tests = Webgl_tests_tabbed.create_webgl_tests_tab () in
  
  (* Define tabs *)
  let tabs = [
    ("date-tab", "Date Selection", date_picker, true);
    ("color-tab", "Color Selection", color_picker, false);
    ("webgl-tab", "WebGL Tests", webgl_tests, false);
  ] in
  
  (* Store tab content for reference *)
  tab_content := tabs;
  
  (* Create header *)
  let header = h1 ~a:[a_style "text-align: center; color: #333;"] [txt "Astronomical Tools"] in
  
  (* Create the tabbed interface *)
  let tabbed_interface = create_tabbed_interface tabs in
  
  (* Create the main container *)
  let main_container = div [
    header;
    tabbed_interface;
  ] in
  
  (* Get the app container and append the main container *)
  let app_container = Dom_html.getElementById "app" in
  let main_dom = Tyxml_js.To_dom.of_div main_container in
  Dom.appendChild app_container main_dom;
  
  (* Initialize the first tab *)
  select_tab "date-tab"

(* Initialize the application when the DOM is loaded *)
let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> bool (init ()))
