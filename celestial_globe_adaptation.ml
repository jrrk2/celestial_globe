open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js

(* Utility functions from the original demo *)
let error f = Printf.ksprintf (fun s -> Console.console##error (string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Console.console##log (string s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (string s); failwith s) f
let check_error gl = if gl##getError <> gl##._NO_ERROR_ then error "WebGL error"

(* Canvas and WebGL context initialization *)
let init_canvas canvas_id =
  let canvas = Opt.get
    (Opt.bind (Dom_html.document##getElementById (string canvas_id))
       Dom_html.CoerceTo.canvas)
    (fun () -> error "can't find canvas element %s" canvas_id) in
  let gl = Opt.get
    (try WebGL.getContext canvas with _ -> null)
    (fun () -> alert "can't initialise webgl context") in
  canvas, gl

(* Shader loading *)
let load_shader (gl : WebGL.renderingContext t) shader text =
  gl##shaderSource shader text;
  gl##compileShader shader;
  if not (to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_)) then
    error "An error occurred compiling the shaders: \n%s\n%s"
      (to_string text)
      (to_string (gl##getShaderInfoLog shader))

let create_program (gl : WebGL.renderingContext t) vert_src frag_src =
  let vertexShader = gl##createShader gl##._VERTEX_SHADER_ in
  let fragmentShader = gl##createShader gl##._FRAGMENT_SHADER_ in
  load_shader gl vertexShader vert_src;
  load_shader gl fragmentShader frag_src;
  let prog = gl##createProgram in
  gl##attachShader prog vertexShader;
  gl##attachShader prog fragmentShader;
  gl##linkProgram prog;
  if not (to_bool (gl##getProgramParameter prog gl##._LINK_STATUS_)) then
    error "Unable to link the shader program.";
  prog

let get_source src_id =
  let script = Opt.get
    (Opt.bind (Dom_html.document##getElementById (string src_id))
       Dom_html.CoerceTo.script)
    (fun () -> error "can't find script element %s" src_id) in
  script##.text

(* TypedArray helpers *)
let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

(* 3D Projection Matrix operations - from original demo *)
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
end
let generate_triangle () =
  let vertices = float32array [| 
    0.0; 0.5; 0.0;    (* Top *)
    -0.5; -0.5; 0.0;  (* Bottom left *)
    0.5; -0.5; 0.0    (* Bottom right *)
  |] in
  let normals = float32array [|
    0.0; 0.0; 1.0;
    0.0; 0.0; 1.0;
    0.0; 0.0; 1.0
  |] in
  let texcoords = float32array [|
    0.5; 0.0;
    0.0; 1.0;
    1.0; 1.0
  |] in
  let indices = new%js Typed_array.uint16Array 3 in
  Typed_array.set indices 0 0;
  Typed_array.set indices 1 1;
  Typed_array.set indices 2 2;
  (vertices, normals, texcoords, indices)

(* Generate a sphere for the celestial globe *)
let generate_sphere radius segments =
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
  
  (* Convert to typed arrays *)
  let vertices_array = float32array (Array.of_list vertices) in
  let normals_array = float32array (Array.of_list normals) in
  let texcoords_array = float32array (Array.of_list texcoords) in
  
  (* Convert indices to Uint16Array for drawElements *)
  let indices_array = new%js Typed_array.uint16Array (List.length indices) in
  List.iteri (fun i v -> Typed_array.set indices_array i v) indices;
  
  (vertices_array, normals_array, texcoords_array, indices_array)

(* Create a procedural texture for testing *)
let create_checkerboard_texture (gl:WebGL.renderingContext Js.t) size =
  let texture = gl##createTexture in
  gl##bindTexture gl##._TEXTURE_2D_ texture;
  
  (* Create checkerboard pattern *)
  let data = new%js Typed_array.uint8Array (size * size * 4) in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let pos = (i * size + j) * 4 in
      let is_blue = (i / 8 + j / 8) mod 2 = 0 in
      let r = if is_blue then 30 else 70 in
      let g = if is_blue then 60 else 150 in
      let b = if is_blue then 180 else 220 in
      Typed_array.set data pos r;
      Typed_array.set data (pos + 1) g;
      Typed_array.set data (pos + 2) b;
      Typed_array.set data (pos + 3) 255;
    done;
  done;
  
  (* Upload the texture data *)
  gl##texImage2D_fromView gl##._TEXTURE_2D_ 0 gl##._RGBA size size 0 gl##._RGBA gl##._UNSIGNED_BYTE_ data;
  
  (* Set texture parameters *)
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MAG_FILTER_ gl##._LINEAR;
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MIN_FILTER_ gl##._LINEAR;
  gl##generateMipmap gl##._TEXTURE_2D_;
  
  texture

(* Main function to start the celestial globe *)
let start () =
  (* Initialize canvas and WebGL context *)
  let _canvas, gl = init_canvas "canvas" in
  
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
  
  let prog = create_program gl vert_shader frag_shader in
  gl##useProgram prog;
  check_error gl;
  
  (* Enable depth test *)
  gl##enable gl##._DEPTH_TEST_;
  gl##depthFunc gl##._LESS;
  
  (* Get attribute and uniform locations *)
  let proj_loc = gl##getUniformLocation prog (string "u_proj") in
  let lightPos_loc = gl##getUniformLocation prog (string "u_lightPos") in
  let ambientLight_loc = gl##getUniformLocation prog (string "u_ambientLight") in
  let texture_loc = gl##getUniformLocation prog (string "u_texture") in
  
  (* Set lighting parameters *)
  let lightPos = float32array [| 3.; 0.; -1. |] in
  let ambientLight = float32array [| 0.2; 0.2; 0.2 |] in
  gl##uniform3fv_typed lightPos_loc lightPos;
  gl##uniform3fv_typed ambientLight_loc ambientLight;
  
  (* Generate sphere geometry *)
  let (vertices, normals, texcoords, indices) = generate_triangle () in
  
  (* Set up position attribute *)
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  gl##enableVertexAttribArray pos_attr;
  let pos_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ pos_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  
  (* Set up normal attribute *)
  let norm_attr = gl##getAttribLocation prog (string "a_normal") in
  gl##enableVertexAttribArray norm_attr;
  let norm_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ norm_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ normals gl##._STATIC_DRAW_;
  gl##vertexAttribPointer norm_attr 3 gl##._FLOAT _false 0 0;
  
  (* Set up texture coordinate attribute *)
  let texcoord_attr = gl##getAttribLocation prog (string "a_texcoord") in
  gl##enableVertexAttribArray texcoord_attr;
  let texcoord_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ texcoord_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ texcoords gl##._STATIC_DRAW_;
  gl##vertexAttribPointer texcoord_attr 2 gl##._FLOAT _false 0 0;
  
  (* Set up index buffer for drawElements *)
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  
  (* Create a procedural texture for the globe *)
  let texture = create_checkerboard_texture gl 256 in
  
  (* FPS counter *)
  let fps_text = Dom_html.document##createTextNode (string "loading") in
  Opt.iter (Opt.bind (Dom_html.document##getElementById (string "fps"))
              Dom_html.CoerceTo.element)
    (fun span -> Dom.appendChild span fps_text);
  
  let pi = 4. *. atan 1. in
  
  (* Initial transformation matrix *)
  let mat = Proj3D.(mult (rotate_x (pi /. 8.)) 
                   (mult (scale 0.5 0.5 0.5) 
                         (translate 0. 0. (-5.)))) in
  
  (* Animation loop *)
  let get_time () = Js.to_float (new%js date_now)##getTime in
  let last_draw = ref (get_time ()) in
  let draw_times = Queue.create () in
  
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
  
  let canvas = Opt.get
    (Opt.bind (Dom_html.document##getElementById (string "canvas"))
       Dom_html.CoerceTo.canvas)
    (fun () -> error "can't find canvas element") in
  setup_mouse_handlers canvas;
  
  let rec animation_loop _ =
    (* Activate texture *)
    gl##activeTexture gl##._TEXTURE0;
    gl##bindTexture gl##._TEXTURE_2D_ texture;
    gl##uniform1i texture_loc 0;
    
    (* Update rotation matrix *)
    let rotation_matrix = 
      Proj3D.(mult (rotate_x !rotation_x)
                   (rotate_y !rotation_y)) in
    let mat' = Proj3D.mult mat rotation_matrix in
    
    gl##uniformMatrix4fv_typed proj_loc _false (Proj3D.array mat');
    
    (* Clear canvas and draw *)
    gl##clear (gl##._DEPTH_BUFFER_BIT_ lor gl##._COLOR_BUFFER_BIT_);
    gl##drawElements gl##._TRIANGLES indices##.length gl##._UNSIGNED_SHORT_ 0;
    check_error gl;
    
    (* Calculate FPS *)
    let now = get_time () in
    Queue.push (now -. !last_draw) draw_times;
    last_draw := now;
    if Queue.length draw_times > 50 then ignore (Queue.pop draw_times);
    let fps = 1. /. Queue.fold ( +. ) 0. draw_times *. 
              float_of_int (Queue.length draw_times) *. 1000. in
    fps_text##.data := string (Printf.sprintf "%.1f FPS" fps);
    
    Lwt_js.sleep 0.016 >>= animation_loop
  in
  
  animation_loop ()
  
(* Main entry point *)
let () =
  Lwt.async (fun () ->
    Lwt.catch
      (fun () ->
        debug "Starting celestial globe";
        start ())
      (fun exn ->
        error "Uncaught exception: %s" (Printexc.to_string exn);
        )
  )
