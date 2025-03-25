open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js_of_ocaml_lwt
open Lwt
open Js
open Utils
open Geolocate

(* Utility functions *)
let error f = Printf.ksprintf (fun s -> Console.console##error (string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Console.console##log (string s)) f
let check_error (gl:WebGL.renderingContext Js.t) = 
  let err = gl##getError in
  if err <> gl##._NO_ERROR_ then begin
    if err = gl##._INVALID_ENUM_ then
      debug "WebGL error: INVALID_ENUM"
    else if err = gl##._INVALID_VALUE_ then
      debug "WebGL error: INVALID_VALUE"
    else if err = gl##._INVALID_OPERATION_ then
      debug "WebGL error: INVALID_OPERATION"
    else if err = gl##._OUT_OF_MEMORY_ then
      debug "WebGL error: OUT_OF_MEMORY"
    else if err = gl##._CONTEXT_LOST_WEBGL_ then
      debug "WebGL error: CONTEXT_LOST_WEBGL"
    else if err = gl##._INVALID_FRAMEBUFFER_OPERATION_ then
      debug "WebGL error: INVALID_FRAMEBUFFER_OPERATION"
    else
      debug "Unknown WebGL error code";
    false
  end else true

(* Float32Array helper *)
let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

(* 3D Projection Matrix operations *)
module Proj3D = struct
  type t = float array
  
  let scale x y z : t = [|x;0.;0.;0.; 0.;y;0.;0.; 0.;0.;z;0.; 0.;0.;0.;1.|]
  
  let translate x y z : t = [|1.;0.;0.;0.; 0.;1.;0.;0.; 0.;0.;1.;0.; x;y;z;1.|]
  
  let rotate_x t : t =
    [|1.;0.;0.;0.; 0.;cos t;sin t;0.; 0.;-.sin t;cos t;0.; 0.;0.;0.;1.|]
  
  let rotate_y t : t =
    [|cos t;0.;-.sin t;0.; 0.;1.;0.;0.; sin t;0.;cos t;0.; 0.;0.;0.;1.|]

  let rotate_z t : t =
    [|cos t;sin t;0.;0.; -.sin t;cos t;0.;0.; 0.;0.;1.;0.; 0.;0.;0.;1.|]
  
  let c i j = (i*4)+j
  
  let o i = i/4, i mod 4
  
  let mult m1 m2 =
    let v p =
      let i,j = o p in
      (m1.(c i 0) *. m2.(c 0 j)) +.
      (m1.(c i 1) *. m2.(c 1 j)) +.
      (m1.(c i 2) *. m2.(c 2 j)) +.
      (m1.(c i 3) *. m2.(c 3 j))
    in
    Array.init 16 v
  
  let array m = float32array m

  (* Debug helper *)
  let debug_matrix m =
    debug "Matrix: [";
    for i = 0 to 3 do
      debug "  [%f, %f, %f, %f]" m.(i*4) m.(i*4+1) m.(i*4+2) m.(i*4+3);
    done;
    debug "]";
end

(* Generate a sphere for the celestial globe *)
let generate_sphere radius segments =
  debug "Generating sphere with radius %f and %d segments" radius segments;
  let vertices = ref [] in
  let normals = ref [] in
  let texcoords = ref [] in
  let indices = ref [] in
  
  (* Generate vertices, normals, and texture coordinates *)
  for i = 0 to segments do
    let theta = float_of_int i *. Float.pi /. float_of_int segments in
    let sin_theta = sin theta in
    let cos_theta = cos theta in
    
    for j = 0 to segments * 2 do
      let phi = float_of_int j *. 2. *. Float.pi /. float_of_int (segments * 2) in
      let sin_phi = sin phi in
      let cos_phi = cos phi in
      
      (* Vertex position *)
      let x = cos_phi *. sin_theta *. radius in
      let y = cos_theta *. radius in
      let z = sin_phi *. sin_theta *. radius in
      vertices := x :: y :: z :: !vertices;
      
      (* Normal - for a sphere, normal = normalized position *)
      let length = sqrt (x *. x +. y *. y +. z *. z) in
      normals := (x /. length) :: (y /. length) :: (z /. length) :: !normals;
      
      (* Texture coordinates - map longitude/latitude to u/v *)
      (* Flip the texture coordinates to match astronomy images *)
      let u = 1.0 -. (float_of_int j /. float_of_int (segments * 2)) in
      let v = float_of_int i /. float_of_int segments in
      texcoords := u :: v :: !texcoords;
    done;
  done;
  
  (* Generate indices for triangles *)
  for i = 0 to segments - 1 do
    for j = 0 to (segments * 2) - 1 do
      let first = i * (segments * 2 + 1) + j in
      let second = first + (segments * 2 + 1) in
      
      (* First triangle *)
      indices := first :: second :: (first + 1) :: !indices;
      
      (* Second triangle *)
      indices := (second) :: (second + 1) :: (first + 1) :: !indices;
    done;
  done;
  
  (* Reverse the lists because we added elements in reverse order *)
  let vertices = List.rev !vertices in
  let normals = List.rev !normals in
  let texcoords = List.rev !texcoords in
  let indices = List.rev !indices in
  
  debug "Generated sphere with %d vertices, %d normals, %d texture coordinates, %d indices" 
    (List.length vertices / 3) (List.length normals / 3) (List.length texcoords / 2) (List.length indices);
  
  (* Check a few vertices to make sure they're correct *)
  if List.length vertices > 0 then begin
    let first_vertex = Array.of_list (List.filteri (fun i _ -> i < 3) vertices) in
    debug "First vertex: (%f, %f, %f)" first_vertex.(0) first_vertex.(1) first_vertex.(2);
  end;
  
  (* Convert to typed arrays *)
  let vertices_array = float32array (Array.of_list vertices) in
  let normals_array = float32array (Array.of_list normals) in
  let texcoords_array = float32array (Array.of_list texcoords) in
  
  (* Convert indices to Uint16Array for drawElements *)
  let indices_array = new%js Typed_array.uint16Array (List.length indices) in
  List.iteri (fun i v -> Typed_array.set indices_array i v) indices;
  
  (vertices_array, normals_array, texcoords_array, indices_array)

(* Create a procedural texture for the celestial sphere *)
let create_star_texture (gl:WebGL.renderingContext Js.t) size =
  debug "Creating star texture with size %d" size;
  let texture = gl##createTexture in
  gl##bindTexture gl##._TEXTURE_2D_ texture;
  ignore (check_error gl);
  
  (* Create star field pattern *)
  let data = new%js Typed_array.uint8Array (size * size * 4) in
  
  (* Fill with dark blue background *)
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let pos = (i * size + j) * 4 in
      Typed_array.set data pos 0;      (* R *)
      Typed_array.set data (pos + 1) 0; (* G *)
      Typed_array.set data (pos + 2) 30; (* B *)
      Typed_array.set data (pos + 3) 255; (* A *)
    done;
  done;
  
  (* Add some stars *)
  let add_star x y brightness size =
    let brightness = int_of_float (brightness *. 255.) in
    let center_x = int_of_float (x *. float_of_int size) in
    let center_y = int_of_float (y *. float_of_int size) in
    let radius = int_of_float (float_of_int size *. 0.005) in
    
    for i = max 0 (center_y - radius) to min (size - 1) (center_y + radius) do
      for j = max 0 (center_x - radius) to min (size - 1) (center_x + radius) do
        let dx = j - center_x in
        let dy = i - center_y in
        let distance = sqrt (float_of_int (dx * dx + dy * dy)) in
        if distance < float_of_int radius then
          let factor = (1.0 -. distance /. float_of_int radius) *. 0.8 in
          let pos = (i * size + j) * 4 in
          let current_r = Js.Optdef.get (Typed_array.get data pos) (fun () -> 0) in
          let b = int_of_float (float_of_int brightness *. factor) in
          Typed_array.set data pos (min 255 (current_r + b));
          Typed_array.set data (pos + 1) (min 255 (current_r + b));
          Typed_array.set data (pos + 2) (min 255 (current_r + b));
        done;
    done;
  in
  
  (* Generate random stars *)
  Random.self_init();
  for _ = 1 to 1000 do
    add_star (Random.float 1.0) (Random.float 1.0) (0.5 +. Random.float 0.5) size;
  done;
  
  (* Upload the texture data *)
  gl##texImage2D_fromView gl##._TEXTURE_2D_ 0 gl##._RGBA size size 0 gl##._RGBA gl##._UNSIGNED_BYTE_ data;
  ignore (check_error gl);
  
  (* Set texture parameters *)
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MAG_FILTER_ gl##._LINEAR;
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MIN_FILTER_ gl##._LINEAR;
  gl##generateMipmap gl##._TEXTURE_2D_;
  ignore (check_error gl);
  
  debug "Star texture created successfully";
  texture

(* Initialize canvas and WebGL context *)
let init_canvas canvas_id =
  debug "Initializing canvas with ID: %s" canvas_id;
  let canvas = Opt.get
    (Opt.bind (Dom_html.document##getElementById (string canvas_id))
       Dom_html.CoerceTo.canvas)
    (fun () -> error "can't find canvas element %s" canvas_id) in
  
  debug "Canvas dimensions: %d x %d" canvas##.width canvas##.height;
  
  let gl = Opt.get
    (WebGL.getContext canvas)
    (fun () -> error "can't initialise webgl context") in
  
  debug "WebGL context created successfully";
  
  (* Log WebGL capabilities *)
  debug "WebGL Version: %s" (to_string (gl##getParameter gl##._VERSION));
  debug "WebGL Vendor: %s" (to_string (gl##getParameter gl##._VENDOR));
  debug "WebGL Renderer: %s" (to_string (gl##getParameter gl##._RENDERER));
  debug "Max Texture Size: %d" (gl##getParameter gl##._MAX_TEXTURE_SIZE_);
  
  canvas, gl

(* Shader loading and program creation *)
let create_program (gl : WebGL.renderingContext t) vert_src frag_src =
  debug "Creating WebGL program";
  let load_shader shader text =
    gl##shaderSource shader text;
    gl##compileShader shader;
    if not (to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_)) then
      error "Shader compilation error: %s" (to_string (gl##getShaderInfoLog shader))
  in

  let vertexShader = gl##createShader gl##._VERTEX_SHADER_ in
  let fragmentShader = gl##createShader gl##._FRAGMENT_SHADER_ in
  
  debug "Compiling vertex shader";
  load_shader vertexShader vert_src;
  
  debug "Compiling fragment shader";
  load_shader fragmentShader frag_src;
  
  let prog = gl##createProgram in
  gl##attachShader prog vertexShader;
  gl##attachShader prog fragmentShader;
  gl##linkProgram prog;
  
  if not (to_bool (gl##getProgramParameter prog gl##._LINK_STATUS_)) then
    error "Program linking error: %s" (to_string (gl##getProgramInfoLog prog));
  
  debug "WebGL program created and linked successfully";
  prog

(* Main celestial globe animation loop *)
let start_celestial_globe canvas_id info_div : 'a Lwt.t =
  debug "Starting celestial globe with canvas ID: %s" canvas_id;
  
  (* Initialize canvas and WebGL context *)
  let _canvas, gl = init_canvas canvas_id in
  
  (* Using a more visible clear color for debugging *)
  gl##clearColor (Js.float 0.1) (Js.float 0.1) (Js.float 0.2) (Js.float 1.0);
  
  (* Create shaders and program *)
  let vert_shader = string "
    attribute vec3 a_position;
    attribute vec3 a_normal;
    attribute vec2 a_texcoord;
    
    uniform mat4 u_proj;
    
    varying vec3 v_normal;
    varying vec2 v_texcoord;
    
    void main() {
      gl_Position = u_proj * vec4(a_position, 1.0);
      v_normal = a_normal;
      v_texcoord = a_texcoord;
    }
  " in
  let frag_shader_bright = string "
    precision mediump float;

    varying vec3 v_normal;
    varying vec2 v_texcoord;

    uniform vec3 u_lightPos;
    uniform vec3 u_ambientLight;
    uniform sampler2D u_texture;

    void main() {
      // Use a bright color for debugging
      gl_FragColor = vec4(1.0, 0.5, 0.0, 1.0); // Bright orange
    }
  " in  
  let frag_shader = string "
    precision mediump float;
    
    varying vec3 v_normal;
    varying vec2 v_texcoord;
    
    uniform vec3 u_lightPos;
    uniform vec3 u_ambientLight;
    uniform sampler2D u_texture;
    
    void main() {
      vec3 normal = normalize(v_normal);
      vec3 lightDir = normalize(u_lightPos);
      float diffuse = max(dot(normal, lightDir), 0.0);
      
      vec4 texColor = texture2D(u_texture, v_texcoord);
      vec3 color = texColor.rgb * (u_ambientLight + diffuse);
      
      gl_FragColor = vec4(color, texColor.a);
    }
  " in
  
  let prog = create_program gl vert_shader frag_shader_bright in
  gl##useProgram prog;
  if not (check_error gl) then
    debug "Error after setting program";
  
  (* Enable depth test *)
  gl##enable gl##._DEPTH_TEST_;
  gl##depthFunc gl##._LESS;
  if not (check_error gl) then
    debug "Error after enabling depth test";
  
  (* Get attribute and uniform locations *)
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  let norm_attr = gl##getAttribLocation prog (string "a_normal") in
  let texcoord_attr = gl##getAttribLocation prog (string "a_texcoord") in
  
  debug "Attribute locations - position: %d, normal: %d, texcoord: %d" 
    pos_attr norm_attr texcoord_attr;
  
  let proj_loc = gl##getUniformLocation prog (string "u_proj") in
  let lightPos_loc = gl##getUniformLocation prog (string "u_lightPos") in
  let ambientLight_loc = gl##getUniformLocation prog (string "u_ambientLight") in
  let texture_loc = gl##getUniformLocation prog (string "u_texture") in
(*
  debug "Uniform locations - projection: %b, lightPos: %b, ambientLight: %b, texture: %b" 
    (not (proj_loc == Js.null)) (not (lightPos_loc == Js.null)) 
    (not (ambientLight_loc == Js.null)) (not (texture_loc == Js.null));
*)    
  (* Set lighting parameters *)
  let lightPos = float32array [| 3.; 0.; -1. |] in
  let ambientLight = float32array [| 0.2; 0.2; 0.2 |] in
  gl##uniform3fv_typed lightPos_loc lightPos;
  gl##uniform3fv_typed ambientLight_loc ambientLight;
  if not (check_error gl) then
    debug "Error after setting lighting uniforms";
  
  (* Generate sphere geometry *)
  let (vertices, normals, texcoords, indices) = generate_sphere 1.0 32 in
  
  debug "Generated geometry - vertices: %d, indices: %d" 
    (vertices##.length / 3) indices##.length;
  
  (* Set up position attribute *)
  let pos_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ pos_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
  gl##enableVertexAttribArray pos_attr;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  if not (check_error gl) then
    debug "Error after setting up position attribute";
  
  (* Set up normal attribute *)
  let norm_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ norm_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ normals gl##._STATIC_DRAW_;
  gl##enableVertexAttribArray norm_attr;
  gl##vertexAttribPointer norm_attr 3 gl##._FLOAT _false 0 0;
  if not (check_error gl) then
    debug "Error after setting up normal attribute";
  
  (* Set up texture coordinate attribute *)
  let texcoord_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ texcoord_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ texcoords gl##._STATIC_DRAW_;
  gl##enableVertexAttribArray texcoord_attr;
  gl##vertexAttribPointer texcoord_attr 2 gl##._FLOAT _false 0 0;
  if not (check_error gl) then
    debug "Error after setting up texcoord attribute";
  
  (* Set up index buffer for drawElements *)
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  if not (check_error gl) then
    debug "Error after setting up index buffer";
  
  (* Create a procedural texture for the globe *)
  let texture = create_star_texture gl 512 in
  
  (* FPS counter *)
  let fps_text = Dom_html.document##createTextNode (string "Initializing...") in
  (match Dom_html.getElementById_opt "fps" with
  | Some span -> Dom.appendChild span fps_text
  | None -> debug "FPS element not found");
  
  (* Initial transformation matrix *)
  let pi = Float.pi in
  
  (* Variables for mouse interaction *)
  let mouse_down = ref false in
  let last_mouse_x = ref 0 in
  let last_mouse_y = ref 0 in
  let rotation_x = ref 0.0 in
  let rotation_y = ref 0.0 in
  
  (* Setup mouse event handlers *)
  let setup_mouse_handlers canvas =
    canvas##.onmousedown := Dom_html.handler (fun e ->
      mouse_down := true;
      last_mouse_x := int_of_float (Js.to_float e##.clientX);
      last_mouse_y := int_of_float (Js.to_float e##.clientY);
      Js._true);
      
    canvas##.onmouseup := Dom_html.handler (fun _ ->
      mouse_down := false;
      Js._true);
      
    canvas##.onmousemove := Dom_html.handler (fun e ->
      if !mouse_down then begin
        let new_x = int_of_float (Js.to_float e##.clientX) in
        let new_y = int_of_float (Js.to_float e##.clientY) in
        let delta_x = float_of_int (new_x - !last_mouse_x) *. 0.01 in
        let delta_y = float_of_int (new_y - !last_mouse_y) *. 0.01 in
        rotation_y := !rotation_y +. delta_x;
        rotation_x := !rotation_x +. delta_y;
        last_mouse_x := new_x;
        last_mouse_y := new_y;
      end;
      Js._true);
  in
  
  (* Initialize canvas for WebGL context *)
  let canvas_element = Dom_html.getElementById canvas_id in
  if Js.Opt.test (some canvas_element) then
    let canvas_opt = Dom_html.CoerceTo.canvas canvas_element in
    if Js.Opt.test canvas_opt then
      let canvas = Js.Opt.get canvas_opt (fun () -> assert false) in
      setup_mouse_handlers canvas
    else
      error "element is not a canvas"
  else
    error "can't find canvas element %s" canvas_id;
  
  (* Adjust projection based on geolocation *)
  let lat = latitude () in
  let long = longitude () in
  
  debug "Using geolocation: Lat %.2f°, Long %.2f°" lat long;
  
  (* Convert latitude and longitude to initial rotation *)
  let lat_rad = lat *. pi /. 180.0 in
  let long_rad = long *. pi /. 180.0 in
  
  (* Set initial rotation to point to user's location *)
  rotation_x := lat_rad;
  rotation_y := -. long_rad;
  
  debug "Initial rotation: x=%.2f, y=%.2f" !rotation_x !rotation_y;
  
  (* Update info div with location *)
  let location_info = Printf.sprintf "Globe centered at: Lat %.2f°, Long %.2f°" lat long in
  set_static_text info_div location_info;
  
  let last_draw = ref (Js.to_float (new%js date_now)##getTime) in
  let draw_times = Queue.create () in
  
  let rec animation_loop _ =
    (* Activate texture *)
    gl##activeTexture gl##._TEXTURE0;
    gl##bindTexture gl##._TEXTURE_2D_ texture;
    gl##uniform1i texture_loc 0;
    if not (check_error gl) then
      debug "Error after binding texture";
    
    (* Update rotation matrix *)
    let base_mat = Proj3D.(mult (scale 0.8 0.8 0.8) (translate 0. 0. (-2.5))) in
    let rotation_matrix = 
      Proj3D.(mult (rotate_x !rotation_x)
                   (rotate_y !rotation_y)) in
    let mat = Proj3D.mult base_mat rotation_matrix in
    
    (* Debug matrix for first few frames *)
    if Queue.length draw_times < 3 then
      Proj3D.debug_matrix mat;
    
    gl##uniformMatrix4fv_typed proj_loc _false (Proj3D.array mat);
    if not (check_error gl) then
      debug "Error after setting projection matrix";
    
    (* Clear canvas and draw *)
    gl##clear (gl##._DEPTH_BUFFER_BIT_ lor gl##._COLOR_BUFFER_BIT_);
    
    if Queue.length draw_times < 3 then
      debug "Drawing %d elements" indices##.length;
    
    gl##drawElements gl##._TRIANGLES indices##.length gl##._UNSIGNED_SHORT_ 0;

    ignore (check_error gl);
    
    (* Calculate FPS *)
    let now = Js.to_float (new%js date_now)##getTime in
    Queue.push (now -. !last_draw) draw_times;
    last_draw := now;
    if Queue.length draw_times > 30 then ignore (Queue.pop draw_times);
    let fps = 1000.0 /. (Queue.fold ( +. ) 0. draw_times /.
              float_of_int (Queue.length draw_times)) in
    fps_text##.data := string (Printf.sprintf "%.1f FPS" fps);
    
    (* Continue animation loop *)
    Lwt_js.sleep 0.016 >>= animation_loop
  in
  
  debug "Starting animation loop";
  (* Start animation loop *)
  animation_loop ()

(* Create the geolocation tab with celestial globe *)
let create_geolocation_tab () =
  let open Tyxml_js.Html in
  
  debug "Creating geolocation tab";
  
  (* Create a canvas for the celestial globe *)
  let canvas = canvas ~a:[
    a_id "globe-canvas";
    a_width 400;
    a_height 400;
    a_style "border: 1px solid #000; display: block; margin: 10px auto; cursor: move;"
  ] [] in
  
  (* Create location info section *)
  let location_info = div ~a:[
    a_id "globe-location-info";
    a_style "margin: 10px; padding: 10px; background-color: #e9f5e9; border: 1px solid #ddd; border-radius: 4px; text-align: center;"
  ] [txt "Location data loading..."] in
  
  (* Create FPS counter *)
  let fps_counter = div ~a:[
    a_id "fps";
    a_style "margin: 10px; padding: 5px; background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; text-align: center;"
  ] [txt "Initializing..."] in
  
  (* Instructions *)
  let instructions = div ~a:[
    a_style "margin: 10px; padding: 10px; background-color: #f0f0f0; border: 1px solid #ddd; border-radius: 4px;"
  ] [
    p [txt "This celestial globe shows a star field centered at your current location."];
    p [txt "Click and drag to rotate the globe."];
    p [txt "The globe will initially point to your geolocation coordinates."]
  ] in
  
  (* Control buttons *)
  let detect_location_button = button ~a:[
    a_style "margin: 10px; padding: 8px 15px; background-color: #4CAF50; color: white; border: none; border-radius: 4px; cursor: pointer;";
    a_onclick (fun _ -> 
      Geo.geo (fun () -> 
        let info_div = Dom_html.getElementById "globe-location-info" in
        let lat = latitude () in
        let long = longitude () in
        let location_info = Printf.sprintf "Globe centered at: Lat %.2f°, Long %.2f°" lat long in
        set_static_text info_div location_info;
        Lwt.return_unit); 
      true)
  ] [txt "Refresh Location"] in
  
  (* Add debug button *)
  let debug_button = button ~a:[
    a_style "margin: 10px; padding: 8px 15px; background-color: #4285F4; color: white; border: none; border-radius: 4px; cursor: pointer;";
    a_onclick (fun _ -> 
      debug "Debug button clicked";
      debug "Canvas element exists: %b" (Dom_html.getElementById_opt "globe-canvas" <> None);
      debug "FPS element exists: %b" (Dom_html.getElementById_opt "fps" <> None);
      debug "Location info element exists: %b" (Dom_html.getElementById_opt "globe-location-info" <> None);
      
      let lat = latitude () in
      let long = longitude () in
      debug "Current latitude: %.2f, longitude: %.2f" lat long;
      true)
  ] [txt "Debug Info"] in
  
  (* Assemble the tab content *)
  let tab_content = div [
    h2 ~a:[a_style "text-align: center;"] [txt "Celestial Globe"];
    location_info;
    canvas;
    fps_counter;
    div ~a:[a_style "display: flex; justify-content: center; gap: 10px;"] [
      detect_location_button;
      debug_button
    ];
    instructions
  ] in
  
  debug "Geolocation tab created, setting up initialization";
  
  (* Initialize the celestial globe after the DOM is loaded *)
  let _ = Lwt.async (fun () ->
    let open Lwt.Infix in
    debug "Waiting for DOM before initializing globe";
    Lwt_js.yield () >>= fun () ->
    debug "DOM yielded, starting celestial globe";
    start_celestial_globe "globe-canvas" (Dom_html.getElementById "globe-location-info")
  ) in
  
  tab_content
