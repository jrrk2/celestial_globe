open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js

(* Utility functions *)
let error f = Printf.ksprintf (fun s -> Console.console##error (string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Console.console##log (string s)) f
let alert f = Printf.ksprintf (fun s -> Dom_html.window##alert (string s); failwith s) f
let check_error (gl:WebGL.renderingContext Js.t) = 
  let err = gl##getError in
  if err <> gl##._NO_ERROR_ then begin
    if err = gl##._INVALID_ENUM_ then
      error "WebGL invalid enum"
    else if err = gl##._INVALID_VALUE_ then
      error "WebGL invalid value"
    else if err = gl##._INVALID_OPERATION_ then
      error "WebGL invalid operation"
    else if err = gl##._OUT_OF_MEMORY_ then
      error "WebGL out of memory"
    else if err = gl##._CONTEXT_LOST_WEBGL_ then
      error "WebGL context lost"
    else if err = gl##._INVALID_FRAMEBUFFER_OPERATION_ then
      error "WebGL invalid framebuffer operation"
    else
      error "Unknown WebGL error code";
    false
  end else
    true

(* Float32Array helper *)
let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

let test_1_canvas_creation () =
  debug "Test 1: Canvas Creation";
  let canvas_opt = Opt.bind 
    (Dom_html.document##getElementById(string "canvas")) 
    Dom_html.CoerceTo.canvas in
  
  if Opt.test canvas_opt then begin
    let canvas = Opt.get canvas_opt (fun () -> assert false) in
    debug "Canvas found, dimensions: %d×%d" 
      canvas##.width
      canvas##.height;
    true
  end else begin
    error "Canvas element not found";
    false
  end

let test_2_webgl_context () =
  debug "Test 2: WebGL Context";
  let canvas_opt = Opt.bind 
    (Dom_html.document##getElementById(string "canvas")) 
    Dom_html.CoerceTo.canvas in
  
  if not (Opt.test canvas_opt) then begin
    error "Could not convert to canvas element";
    false
  end else begin
    let canvas = Opt.get canvas_opt (fun () -> assert false) in
    let gl_opt = WebGL.getContext canvas in
    
    if gl_opt == null then begin
      error "WebGL context creation failed";
      false
    end else begin
      let gl = Opt.get gl_opt (fun () -> assert false) in
      debug "WebGL context created successfully";
      debug "WebGL version: %s" (to_string (gl##getParameter gl##._VERSION));
      debug "WebGL vendor: %s" (to_string (gl##getParameter gl##._VENDOR));
      debug "WebGL renderer: %s" (to_string (gl##getParameter gl##._RENDERER));
      debug "Max vertex attributes: %d" (gl##getParameter gl##._MAX_VERTEX_ATTRIBS_);
      true
    end
  end

let test_3_clear_canvas () =
  debug "Test 3: Clear Canvas";
  let canvas_opt = Opt.bind 
    (Dom_html.document##getElementById(string "canvas")) 
    Dom_html.CoerceTo.canvas in
  
  if not (Opt.test canvas_opt) then
    false
  else begin
    let canvas = Opt.get canvas_opt (fun () -> assert false) in
    let gl_opt = WebGL.getContext canvas in
    
    if gl_opt == null then
      false
    else begin
     let gl = Opt.get gl_opt (fun () -> assert false) in
      debug "Setting clear color to red";
      gl##clearColor (Js.float 1.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0); (* Bright red *)
      gl##clear (gl##._COLOR_BUFFER_BIT_);
      check_error gl
    end
  end

let test_4_shader_compilation (gl:WebGL.renderingContext Js.t) =
  debug "Test 4: Shader Compilation";
  
  let vertex_shader_src = string "
    attribute vec2 aPosition;
    void main() {
      gl_Position = vec4(aPosition, 0.0, 1.0);
    }
  " in
  
  let fragment_shader_src = string "
    precision mediump float;
    void main() {
      gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0); // Green
    }
  " in
  
  let compile_shader source shader_type =
    let shader = gl##createShader shader_type in
    gl##shaderSource shader source;
    gl##compileShader shader;
    
    let success = gl##getShaderParameter shader gl##._COMPILE_STATUS_ in
    if not (to_bool success) then begin
      let info = gl##getShaderInfoLog shader in
      error "Shader compilation error: %s" (to_string info);
      Js.null
    end else begin
      debug "Shader compiled successfully";
      Js.some shader
    end
  in
  
  let vs = compile_shader vertex_shader_src gl##._VERTEX_SHADER_ in
  let fs = compile_shader fragment_shader_src gl##._FRAGMENT_SHADER_ in
  
  if vs == null || fs == null then begin
    debug "Shader compilation failed";
    false
  end else begin
    debug "Both shaders compiled successfully";
    true
  end

let test_5_program_creation (gl:WebGL.renderingContext Js.t) =
  debug "Test 5: Program Creation and Linking";
  
  let vertex_shader_src = string "
    attribute vec2 aPosition;
    void main() {
      gl_Position = vec4(aPosition, 0.0, 1.0);
    }
  " in
  
  let fragment_shader_src = string "
    precision mediump float;
    void main() {
      gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0); // Green
    }
  " in
  
  let create_program vs_src fs_src =
    let vs = gl##createShader gl##._VERTEX_SHADER_ in
    gl##shaderSource vs vs_src;
    gl##compileShader vs;
    
    let fs = gl##createShader gl##._FRAGMENT_SHADER_ in
    gl##shaderSource fs fs_src;
    gl##compileShader fs;
    
    let program = gl##createProgram in
    gl##attachShader program vs;
    gl##attachShader program fs;
    gl##linkProgram program;
    
    let success = gl##getProgramParameter program gl##._LINK_STATUS_ in
    if not (to_bool success) then begin
      let info = gl##getProgramInfoLog program in
      error "Program linking error: %s" (to_string info);
      Js.null
    end else begin
      debug "Program linked successfully";
      Js.some program
    end
  in
  
  let program = create_program vertex_shader_src fragment_shader_src in
  
  if program == null then begin
    debug "Program creation failed";
    false
  end else begin
    let program = Opt.get program (fun () -> assert false) in
    gl##useProgram program;
    let loc = gl##getAttribLocation program (string "aPosition") in
    debug "aPosition attribute location: %d" loc;
    true
  end

let test_6_buffer_creation (gl:WebGL.renderingContext Js.t) =
  debug "Test 6: Buffer Creation and Binding";
  
  let buffer = gl##createBuffer in
(*
  if buffer == Js.null then begin
    error "Buffer creation failed";
    false
  end else *) begin
    gl##bindBuffer gl##._ARRAY_BUFFER_ buffer;
    
    let vertices = float32array [|
      -0.5; -0.5;  (* Bottom left *)
      0.5; -0.5;   (* Bottom right *)
      0.0;  0.5    (* Top *)
    |] in
    
    gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
    if not (check_error gl) then
      false
    else begin
      debug "Buffer created and data loaded successfully";
      true
    end
  end

let test_7_render_triangle (gl:WebGL.renderingContext Js.t) =
  debug "Test 7: Render Triangle";
  
  (* Create and link program *)
  let vertex_shader_src = string "
    attribute vec2 aPosition;
    void main() {
      gl_Position = vec4(aPosition, 0.0, 1.0);
    }
  " in
  
  let fragment_shader_src = string "
    precision mediump float;
    void main() {
      gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0); // Green
    }
  " in
  
  let vs = gl##createShader gl##._VERTEX_SHADER_ in
  gl##shaderSource vs vertex_shader_src;
  gl##compileShader vs;
  
  let fs = gl##createShader gl##._FRAGMENT_SHADER_ in
  gl##shaderSource fs fragment_shader_src;
  gl##compileShader fs;
  
  let program = gl##createProgram in
  gl##attachShader program vs;
  gl##attachShader program fs;
  gl##linkProgram program;
  gl##useProgram program;
  
  (* Create buffer with vertices *)
  let buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ buffer;
  
  let vertices = float32array [|
    -0.5; -0.5;  (* Bottom left *)
    0.5; -0.5;   (* Bottom right *)
    0.0;  0.5    (* Top *)
  |] in
  
  gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
  
  (* Get attribute location and enable it *)
  let pos_attr = gl##getAttribLocation program (string "aPosition") in
  debug "Position attribute location: %d" pos_attr;
  
  if pos_attr < 0 then begin
    error "Could not find position attribute";
    false
  end else begin
    gl##enableVertexAttribArray pos_attr;
    gl##vertexAttribPointer pos_attr 2 gl##._FLOAT _false 0 0;
    
    (* Draw triangle *)
    gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0); (* Black *)
    gl##clear (gl##._COLOR_BUFFER_BIT_);
    gl##drawArrays gl##._TRIANGLES 0 3;
    
    if not (check_error gl) then begin
      false
    end else begin
      debug "Triangle rendered (should be green)";
      true
    end
  end

let test_8_render_with_indices (gl:WebGL.renderingContext Js.t) =
  debug "Test 8: Render with Indices";
  
  (* Create and link program *)
  let vertex_shader_src = string "
    attribute vec2 aPosition;
    void main() {
      gl_Position = vec4(aPosition, 0.0, 1.0);
    }
  " in
  
  let fragment_shader_src = string "
    precision mediump float;
    void main() {
      gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0); // Magenta
    }
  " in
  
  let program = gl##createProgram in
  
  let vs = gl##createShader gl##._VERTEX_SHADER_ in
  gl##shaderSource vs vertex_shader_src;
  gl##compileShader vs;
  gl##attachShader program vs;
  
  let fs = gl##createShader gl##._FRAGMENT_SHADER_ in
  gl##shaderSource fs fragment_shader_src;
  gl##compileShader fs;
  gl##attachShader program fs;
  
  gl##linkProgram program;
  gl##useProgram program;
  
  (* Create buffer with vertices *)
  let vertices = float32array [|
    -0.5; -0.5;  (* Bottom left *)
    0.5; -0.5;   (* Bottom right *)
    0.5;  0.5;   (* Top right *)
    -0.5;  0.5   (* Top left *)
  |] in
  
  let vertex_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ vertex_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
  
  (* Create index buffer *)
  let indices = new%js Typed_array.uint16Array 6 in
  List.iteri (fun ix itm -> Typed_array.set indices ix itm) (* First triangle: bottom-left *)
    [ 0; 1; 2; 0; 2; 3];
  
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  
  (* Get attribute location and enable it *)
  let pos_attr = gl##getAttribLocation program (string "aPosition") in
  gl##enableVertexAttribArray pos_attr;
  gl##vertexAttribPointer pos_attr 2 gl##._FLOAT _false 0 0;

  debug "Drawing %d triangles, using %d indices, with %d vertices"
  (indices##.length / 3)
  indices##.length
  (vertices##.length / 2); (* Divide by 2 for (x,y) pairs *)
  
  (* Draw with indices *)
  gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0); (* Black *)
  gl##clear (gl##._COLOR_BUFFER_BIT_);
  gl##drawElements gl##._TRIANGLES 6 gl##._UNSIGNED_SHORT_ 0;
  
  if not (check_error gl) then begin
    false
  end else begin
    debug "Square rendered using indices (should be magenta)";
    true
  end

let test_9_3d_cube (gl:WebGL.renderingContext Js.t) =
  debug "Test 9: 3D Cube with Perspective";
  
  (* Create and link program *)
  let vertex_shader_src = string "
    attribute vec3 aPosition;
    attribute vec3 aColor;
    uniform mat4 uProjection;
    uniform mat4 uModelView;
    varying vec3 vColor;
    
    void main() {
      gl_Position = uProjection * uModelView * vec4(aPosition, 1.0);
      vColor = aColor;
    }
  " in
  
  let fragment_shader_src = string "
    precision mediump float;
    varying vec3 vColor;
    
    void main() {
      gl_FragColor = vec4(vColor, 1.0);
    }
  " in
  
  let program = gl##createProgram in
  
  let vs = gl##createShader gl##._VERTEX_SHADER_ in
  gl##shaderSource vs vertex_shader_src;
  gl##compileShader vs;
  let vs_success = gl##getShaderParameter vs gl##._COMPILE_STATUS_ in
  if not (to_bool vs_success) then
    debug "Vertex shader compilation error: %s" (to_string (gl##getShaderInfoLog vs));
  gl##attachShader program vs;
  
  let fs = gl##createShader gl##._FRAGMENT_SHADER_ in
  gl##shaderSource fs fragment_shader_src;
  gl##compileShader fs;
  let fs_success = gl##getShaderParameter fs gl##._COMPILE_STATUS_ in
  if not (to_bool fs_success) then
    debug "Fragment shader compilation error: %s" (to_string (gl##getShaderInfoLog fs));
  gl##attachShader program fs;
  
  gl##linkProgram program;
  let link_success = gl##getProgramParameter program gl##._LINK_STATUS_ in
  if not (to_bool link_success) then
    debug "Program linking error: %s" (to_string (gl##getProgramInfoLog program));
  gl##useProgram program;
  
  (* Create perspective matrix *)
  let perspective aspect near far fov_degrees =
    let fov_radians = fov_degrees *. Float.pi /. 180.0 in
    let f = Float.tan (Float.pi *. 0.5 -. 0.5 *. fov_radians) in
    let range_inv = 1.0 /. (near -. far) in
    
    [|
      f /. aspect; 0.0; 0.0; 0.0;
      0.0; f; 0.0; 0.0;
      0.0; 0.0; (near +. far) *. range_inv; -1.0;
      0.0; 0.0; near *. far *. range_inv *. 2.0; 0.0
    |]
  in
  
  (* Create model-view matrix *)
  let model_view angle =
    let c = Float.cos angle in
    let s = Float.sin angle in
    
    [|
      c; 0.0; -.s; 0.0;
      0.0; 1.0; 0.0; 0.0;
      s; 0.0; c; 0.0;
      0.0; 0.0; -5.0; 1.0
    |]
  in
  
  (* Upload matrices *)
  let proj_loc = gl##getUniformLocation program (string "uProjection") in
  let mv_loc = gl##getUniformLocation program (string "uModelView") in
  
  let canvas_opt = Opt.bind 
    (Dom_html.document##getElementById(string "canvas")) 
    Dom_html.CoerceTo.canvas in
    
  if not (Opt.test canvas_opt) then begin
    error "Could not find canvas for 3D test";
    false
  end else begin
    let canvas = Opt.get canvas_opt (fun () -> assert false) in
    let aspect = (float_of_int canvas##.width) /. float_of_int (canvas##.height) in
    
    let projection = perspective aspect 0.1 100.0 45.0 in
    let model_view = model_view (Float.pi /. 4.0) in
    
    gl##uniformMatrix4fv_typed proj_loc _false (float32array projection);
    gl##uniformMatrix4fv_typed mv_loc _false (float32array model_view);
    
    (* Create cube vertices *)
    let vertices = float32array [|
      (* Front face *)
      -1.0; -1.0;  1.0;
       1.0; -1.0;  1.0;
       1.0;  1.0;  1.0;
      -1.0;  1.0;  1.0;
      
      (* Back face *)
      -1.0; -1.0; -1.0;
      -1.0;  1.0; -1.0;
       1.0;  1.0; -1.0;
       1.0; -1.0; -1.0;
      
      (* Top face *)
      -1.0;  1.0; -1.0;
      -1.0;  1.0;  1.0;
       1.0;  1.0;  1.0;
       1.0;  1.0; -1.0;
      
      (* Bottom face *)
      -1.0; -1.0; -1.0;
       1.0; -1.0; -1.0;
       1.0; -1.0;  1.0;
      -1.0; -1.0;  1.0;
      
      (* Right face *)
       1.0; -1.0; -1.0;
       1.0;  1.0; -1.0;
       1.0;  1.0;  1.0;
       1.0; -1.0;  1.0;
      
      (* Left face *)
      -1.0; -1.0; -1.0;
      -1.0; -1.0;  1.0;
      -1.0;  1.0;  1.0;
      -1.0;  1.0; -1.0
    |] in
    
    (* Create colors for each vertex *)
    let colors = float32array [|
      (* Front face: red *)
      1.0; 0.0; 0.0;
      1.0; 0.0; 0.0;
      1.0; 0.0; 0.0;
      1.0; 0.0; 0.0;
      
      (* Back face: green *)
      0.0; 1.0; 0.0;
      0.0; 1.0; 0.0;
      0.0; 1.0; 0.0;
      0.0; 1.0; 0.0;
      
      (* Top face: blue *)
      0.0; 0.0; 1.0;
      0.0; 0.0; 1.0;
      0.0; 0.0; 1.0;
      0.0; 0.0; 1.0;
      
      (* Bottom face: yellow *)
      1.0; 1.0; 0.0;
      1.0; 1.0; 0.0;
      1.0; 1.0; 0.0;
      1.0; 1.0; 0.0;
      
      (* Right face: magenta *)
      1.0; 0.0; 1.0;
      1.0; 0.0; 1.0;
      1.0; 0.0; 1.0;
      1.0; 0.0; 1.0;
      
      (* Left face: cyan *)
      0.0; 1.0; 1.0;
      0.0; 1.0; 1.0;
      0.0; 1.0; 1.0;
      0.0; 1.0; 1.0
    |] in
    
    (* Create indices for the cube *)
    let indices = new%js Typed_array.uint16Array 36 in
    let set_face_indices base i0 i1 i2 i3 =
      let idx = base * 6 in
      (* First triangle of face *)
      List.iteri (fun ix itm -> Typed_array.set indices (idx + ix) itm) [i0;i1;i2;i0;i2;i3];
    in
    
    (* Set indices for each face (6 faces) *)
    set_face_indices 0 0 1 2 3;   (* front *)
    set_face_indices 1 4 5 6 7;   (* back *)
    set_face_indices 2 8 9 10 11; (* top *)
    set_face_indices 3 12 13 14 15; (* bottom *)
    set_face_indices 4 16 17 18 19; (* right *)
    set_face_indices 5 20 21 22 23; (* left *)
    
    (* Create and bind vertex buffer *)
    let vertex_buffer = gl##createBuffer in
    gl##bindBuffer gl##._ARRAY_BUFFER_ vertex_buffer;
    gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
    
    (* Create and bind color buffer *)
    let color_buffer = gl##createBuffer in
    gl##bindBuffer gl##._ARRAY_BUFFER_ color_buffer;
    gl##bufferData gl##._ARRAY_BUFFER_ colors gl##._STATIC_DRAW_;
    
    (* Create and bind index buffer *)
    let index_buffer = gl##createBuffer in
    gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
    gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
    
    (* Set up vertex attributes *)
    let pos_attr = gl##getAttribLocation program (string "aPosition") in
    let color_attr = gl##getAttribLocation program (string "aColor") in
    
    gl##bindBuffer gl##._ARRAY_BUFFER_ vertex_buffer;
    gl##enableVertexAttribArray pos_attr;
    gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
    
    gl##bindBuffer gl##._ARRAY_BUFFER_ color_buffer;
    gl##enableVertexAttribArray color_attr;
    gl##vertexAttribPointer color_attr 3 gl##._FLOAT _false 0 0;
    
    (* Enable depth testing *)
    gl##enable gl##._DEPTH_TEST_;
    gl##depthFunc gl##._LEQUAL;
    
    (* Draw the cube *)
    gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0); (* Black *)
    gl##clearDepth (Js.float 1.0);
    gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
    
    gl##drawElements gl##._TRIANGLES 36 gl##._UNSIGNED_SHORT_ 0;
    
    if not (check_error gl) then begin
      false
    end else begin
      debug "3D Cube rendered with perspective";
      true
    end
  end

let run_tests () =
  debug "=== WebGL Diagnostic Tests ===";
  debug "Browser: %s" (to_string Dom_html.window##.navigator##.userAgent);
  debug "Date: %s" (to_string (new%js Js.date_now)##toLocaleString);
  
  (* Test canvas creation *)
  if not (test_1_canvas_creation ()) then
    error "Canvas test failed";
  
  (* Test WebGL context *)
  if not (test_2_webgl_context ()) then
    error "WebGL context test failed";
  
  (* Get WebGL context for subsequent tests *)
  let canvas_opt = Opt.bind 
    (Dom_html.document##getElementById(string "canvas")) 
    Dom_html.CoerceTo.canvas in
  
  if not (Opt.test canvas_opt) then begin
    error "Could not get canvas for WebGL tests";
    Lwt.return_unit
  end else begin
    let canvas = Opt.get canvas_opt (fun () -> assert false) in
    let gl_opt = WebGL.getContext canvas in
    
    if gl_opt == null then begin
      error "Could not get WebGL context for tests";
      Lwt.return_unit
    end else begin
      let gl = Opt.get gl_opt (fun () -> assert false) in
      
      (* Clear canvas *)
      if not (test_3_clear_canvas ()) then
        error "Clear canvas test failed";
      
      (* Test shader compilation *)
      if not (test_4_shader_compilation gl) then
        error "Shader compilation test failed";
      
      (* Test program creation *)
      if not (test_5_program_creation gl) then
        error "Program creation test failed";
      
      (* Test buffer creation *)
      if not (test_6_buffer_creation gl) then
        error "Buffer creation test failed";
      
      (* Test basic triangle rendering *)
      if not (test_7_render_triangle gl) then
        error "Triangle rendering test failed";

      (* Test indexed rendering *)
      if not (test_8_render_with_indices gl) then
        error "Indexed rendering test failed";

      (* Test 3D cube with perspective *)
      if not (test_9_3d_cube gl) then
        error "3D cube test failed";

      debug "===== Tests complete =====";
      Lwt.return_unit
    end
  end

let () =
  Lwt.async (fun () ->
    Lwt.catch
      (fun () -> run_tests ())
      (fun exn ->
        error "Uncaught exception: %s" (Printexc.to_string exn);
        Lwt.return_unit)
  )
