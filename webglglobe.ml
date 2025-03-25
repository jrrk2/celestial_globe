open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js

let error f = Printf.ksprintf (fun s -> Console.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Console.console##log (Js.string s)) f

let check_error gl = if gl##getError <> gl##._NO_ERROR_ then error "WebGL error"

let init_canvas canvas_id =
  let canvas =
    Opt.get
      (Opt.bind
         (Dom_html.document##getElementById (string canvas_id))
         Dom_html.CoerceTo.canvas)
      (fun () -> error "can't find canvas element %s" canvas_id)
  in
  let gl =
    Opt.get
      (try WebGL.getContext canvas with _ -> null)
      (fun () -> error "can't initialize webgl context")
  in
  canvas, gl

let create_program (gl:WebGL.renderingContext Js.t) vert_src frag_src =
  let compile_shader shader_type src =
    let shader = gl##createShader shader_type in
    gl##shaderSource shader src;
    gl##compileShader shader;
    if not (to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_)) then
      error "Shader compilation error: %s" (to_string (gl##getShaderInfoLog shader));
    shader
  in
  
  let vert_shader = compile_shader gl##._VERTEX_SHADER_ vert_src in
  let frag_shader = compile_shader gl##._FRAGMENT_SHADER_ frag_src in
  
  let prog = gl##createProgram in
  gl##attachShader prog vert_shader;
  gl##attachShader prog frag_shader;
  gl##linkProgram prog;
  
  if not (to_bool (gl##getProgramParameter prog gl##._LINK_STATUS_)) then
    error "Program linking error: %s" (to_string (gl##getProgramInfoLog prog));
  
  prog
(*
let get_source src_id =
  let script =
    Opt.get
      (Opt.bind
         (Dom_html.document##getElementById (string src_id))
         Dom_html.CoerceTo.script)
      (fun () -> error "can't find script element %s" src_id)
  in
  script##.text
*)
let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

(* Generate a sphere for the globe *)
let generate_sphere radius segments =
  let vertices = ref [] in
  let normals = ref [] in
  let indices = ref [] in
  
  (* Generate vertices *)
  for i = 0 to segments do
    let theta = float_of_int i *. Float.pi /. float_of_int segments in
    let sin_theta = sin theta in
    let cos_theta = cos theta in
    
    for j = 0 to segments * 2 do
      let phi = float_of_int j *. 2. *. Float.pi /. float_of_int (segments * 2) in
      let sin_phi = sin phi in
      let cos_phi = cos phi in
      
      (* Position *)
      let x = cos_phi *. sin_theta *. radius in
      let y = cos_theta *. radius in
      let z = sin_phi *. sin_theta *. radius in
      vertices := x :: y :: z :: !vertices;
      
      (* Normal (normalized position for sphere) *)
      normals := (x /. radius) :: (y /. radius) :: (z /. radius) :: !normals;
    done;
  done;
  
  (* Generate indices *)
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
  
  (* Reverse lists because we added elements in reverse order *)
  let vertices = List.rev !vertices in
  let normals = List.rev !normals in
  let indices = List.rev !indices in
  
  debug "Generated sphere with %d vertices and %d indices" 
    (List.length vertices / 3) (List.length indices);
  
  (* Convert to typed arrays *)
  let vertices_array = float32array (Array.of_list vertices) in
  let normals_array = float32array (Array.of_list normals) in
  
  (* Convert indices to Uint16Array *)
  let indices_array = new%js Typed_array.uint16Array (List.length indices) in
  List.iteri (fun i v -> Typed_array.set indices_array i v) indices;
  
  vertices_array, normals_array, indices_array

let start () =
  debug "Starting globe animation";
  
  (* Get FPS counter element *)
  let fps_text = Dom_html.document##createTextNode (string "Loading...") in
  Opt.iter
    (Opt.bind (Dom_html.document##getElementById (string "fps")) Dom_html.CoerceTo.element)
    (fun span -> Dom.appendChild span fps_text);
  
  (* Initialize canvas and WebGL *)
  let (_canvas, gl) = init_canvas "canvas" in
  
  (* Clear color: dark blue *)
  gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.2) (Js.float 1.0);
  
  (* Create shaders and program *)
  let vert_shader = string "
    attribute vec3 a_position;
    attribute vec3 a_normal;
    
    uniform mat4 u_matrix;
    
    varying vec3 v_normal;
    varying vec3 v_position;
    
    void main() {
      vec4 position = u_matrix * vec4(a_position, 1.0);
      gl_Position = position;
      
      v_normal = a_normal;
      v_position = a_position;
    }
  " in
  
  let frag_shader = string "
    precision mediump float;
    
    varying vec3 v_normal;
    varying vec3 v_position;
    
    uniform vec3 u_lightDir;
    
    void main() {
      // Normalize the normal
      vec3 normal = normalize(v_normal);
      
      // Dot product with light direction gives diffuse lighting
      float light = max(dot(normal, u_lightDir), 0.0);
      
      // Base color: light gray
      vec3 baseColor = vec3(0.8, 0.8, 0.8);
      
      // Add a red stripe along the equator
      if (abs(v_position.y) < 0.1) {
        baseColor = vec3(1.0, 0.0, 0.0);
      }
      
      // Add a blue vertical stripe
      if (abs(v_position.x) < 0.1) {
        baseColor = vec3(0.0, 0.0, 1.0);
      }
      
      // Add a green horizontal stripe
      if (abs(v_position.z) < 0.1) {
        baseColor = vec3(0.0, 1.0, 0.0);
      }
      
      // Apply lighting with ambient component
      vec3 color = baseColor * (0.3 + 0.7 * light);
      
      gl_FragColor = vec4(color, 1.0);
    }
  " in
  
  let prog = create_program gl vert_shader frag_shader in
  gl##useProgram prog;
  
  (* Enable depth testing *)
  gl##enable gl##._DEPTH_TEST_;
  gl##depthFunc gl##._LESS;
  
  (* Set up uniforms *)
  let matrix_loc = gl##getUniformLocation prog (string "u_matrix") in
  let light_dir_loc = gl##getUniformLocation prog (string "u_lightDir") in
  
  (* Set light direction *)
  let light_dir = float32array [| 0.5; 0.7; 1.0 |] in
  gl##uniform3fv_typed light_dir_loc light_dir;
  
  (* Generate sphere *)
  debug "Generating sphere...";
  let (positions, normals, indices) = generate_sphere 0.7 32 in
  
  (* Set up position attribute *)
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  gl##enableVertexAttribArray pos_attr;
  let pos_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ pos_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ positions gl##._STATIC_DRAW_;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  
  (* Set up normal attribute *)
  let norm_attr = gl##getAttribLocation prog (string "a_normal") in
  gl##enableVertexAttribArray norm_attr;
  let norm_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ norm_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ normals gl##._STATIC_DRAW_;
  gl##vertexAttribPointer norm_attr 3 gl##._FLOAT _false 0 0;
  
  (* Set up index buffer *)
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  
  (* Animation state *)
  let get_time () = Js.to_float (new%js date_now)##getTime in
  let last_draw = ref (get_time ()) in
  let draw_times = Queue.create () in
  (*
  (* Simple perspective matrix *)
  let perspective = [|
    1.0; 0.0; 0.0; 0.0;
    0.0; 1.0; 0.0; 0.0;
    0.0; 0.0; 1.0; 0.0;
    0.0; 0.0; -2.0; 1.0  (* Move back on Z axis *)
  |] in
  *)
  (* Animation loop *)
  let rec animate () =
    (* Get current time for animation *)
    let t = Js.to_float (new%js date_now)##getTime /. 1000. in
    
    (* Create rotation matrix - simple Y-axis rotation *)
    let c = cos t in
    let s = sin t in
    let rotation = [|
      c; 0.0; s; 0.0;
      0.0; 1.0; 0.0; 0.0;
      (-.s); 0.0; c; 0.0;
      0.0; 0.0; 0.0; 1.0
    |] in
    
    (* Set the matrix uniform *)
    gl##uniformMatrix4fv_typed matrix_loc _false (float32array rotation);
    
    (* Clear the canvas *)
    gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
    
    (* Draw the sphere *)
    gl##drawElements gl##._TRIANGLES indices##.length gl##._UNSIGNED_SHORT_ 0;
    
    (* Check for GL errors *)
    check_error gl;
    
    (* Calculate FPS *)
    let now = get_time () in
    Queue.push (now -. !last_draw) draw_times;
    last_draw := now;
    if Queue.length draw_times > 50 then ignore (Queue.pop draw_times);
    let fps =
      1.0 /.
      (Queue.fold ( +. ) 0.0 draw_times /. float_of_int (Queue.length draw_times)) *.
      1000.0
    in
    fps_text##.data := string (Printf.sprintf "%.1f" fps);
    
    (* Continue animation *)
    Lwt_js.sleep 0.016 >>= animate
  in
  
  (* Start animation loop *)
  animate ()

let () =
  debug "Initializing globe application";
  try
    Lwt.async start
  with e ->
    error "Uncaught exception: %s" (Printexc.to_string e)
