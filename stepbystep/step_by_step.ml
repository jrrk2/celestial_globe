open Js_of_ocaml
open Js_of_ocaml_tyxml
open Js

(* Utility functions *)
let error f = Printf.ksprintf (fun s -> Console.console##error (string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Console.console##log (string s)) f
let check_error gl = 
  let err = gl##getError in
  if err <> gl##._NO_ERROR_ then begin
    let err_msg = match err with
      | _ when err = gl##._INVALID_ENUM_ -> "INVALID_ENUM"
      | _ when err = gl##._INVALID_VALUE_ -> "INVALID_VALUE"
      | _ when err = gl##._INVALID_OPERATION_ -> "INVALID_OPERATION"
      | _ when err = gl##._OUT_OF_MEMORY_ -> "OUT_OF_MEMORY"
      | _ when err = gl##._CONTEXT_LOST_WEBGL_ -> "CONTEXT_LOST_WEBGL"
      | _ when err = gl##._INVALID_FRAMEBUFFER_OPERATION_ -> "INVALID_FRAMEBUFFER_OPERATION"
      | _ -> "UNKNOWN ERROR"
    in
    debug "WebGL error: %s" err_msg;
    false
  end else true

(* Helpers for floating point arrays *)
let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

(* Initialize canvas and get WebGL context *)
let init_webgl canvas_id =
  debug "Step 1: Getting canvas and WebGL context";
  
  let canvas_opt = Dom_html.getElementById_opt canvas_id in
  if canvas_opt = None then
    error "Canvas element '%s' not found" canvas_id;
  
  let canvas_el = Option.get canvas_opt in
  let canvas_opt = Dom_html.CoerceTo.canvas canvas_el in
  
  if not (Js.Opt.test canvas_opt) then
    error "Element '%s' is not a canvas" canvas_id;
  
  let canvas = Js.Opt.get canvas_opt (fun () -> assert false) in
  debug "Canvas dimensions: %dx%d" canvas##.width canvas##.height;
  
  let gl_opt = WebGL.getContext canvas in
  if not (Js.Opt.test gl_opt) then
    error "Could not create WebGL context";
  
  let gl = Js.Opt.get gl_opt (fun () -> assert false) in
  debug "WebGL context created successfully";
  canvas, gl

(* Create and compile a shader *)
let create_shader (gl:WebGL.renderingContext Js.t) shader_type source =
  debug "Step 2: Creating %s shader" 
    (if shader_type = gl##._VERTEX_SHADER_ then "vertex" else "fragment");
  
  let shader = gl##createShader shader_type in
  gl##shaderSource shader source;
  gl##compileShader shader;
  
  if not (to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_)) then begin
    let info = gl##getShaderInfoLog shader in
    error "Shader compilation error: %s" (to_string info);
  end;
  
  debug "Shader compilation successful";
  shader

(* Create a WebGL program from shaders *)
let create_program (gl:WebGL.renderingContext Js.t) vert_shader frag_shader =
  debug "Step 3: Creating and linking program";
  
  let prog = gl##createProgram in
  gl##attachShader prog vert_shader;
  gl##attachShader prog frag_shader;
  gl##linkProgram prog;
  
  if not (to_bool (gl##getProgramParameter prog gl##._LINK_STATUS_)) then begin
    let info = gl##getProgramInfoLog prog in
    error "Program linking error: %s" (to_string info);
  end;
  
  debug "Program linked successfully";
  prog

(* Test 1: Clear the canvas with a color *)
let test_clear_canvas canvas_id =
  debug "=== Test 1: Clear Canvas ===";
  let (_canvas, gl) = init_webgl canvas_id in
  
  debug "Setting clear color to blue";
  gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.8) (Js.float 1.0); (* Blue *)
  gl##clear (gl##._COLOR_BUFFER_BIT_);
  
  (* Update the result div *)
  let result_div = Dom_html.getElementById "step-test-result" in
  result_div##.textContent := Js.some (string "Test 1 complete: Canvas cleared with blue color");
  
  check_error gl

(* Test 2: Draw a simple triangle *)
let test_triangle canvas_id =
  debug "=== Test 2: Draw Triangle ===";
  let (_canvas, gl) = init_webgl canvas_id in
  
  (* Simple vertex and fragment shaders *)
  let vert_src = string "
    attribute vec3 a_position;
    
    void main() {
      gl_Position = vec4(a_position, 1.0);
    }
  " in
  
  let frag_src = string "
    precision mediump float;
    
    void main() {
      gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);  // Red
    }
  " in
  
  (* Create shaders and program *)
  let vert_shader = create_shader gl gl##._VERTEX_SHADER_ vert_src in
  let frag_shader = create_shader gl gl##._FRAGMENT_SHADER_ frag_src in
  let prog = create_program gl vert_shader frag_shader in
  
  gl##useProgram prog;
  
  (* Create triangle geometry *)
  let vertices = float32array [|
    -0.5; -0.5; 0.0;  (* Bottom left *)
     0.5; -0.5; 0.0;  (* Bottom right *)
     0.0;  0.5; 0.0;  (* Top *)
  |] in
  
  let buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ vertices gl##._STATIC_DRAW_;
  
  (* Set up attribute *)
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  debug "Position attribute location: %d" pos_attr;
  gl##enableVertexAttribArray pos_attr;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  
  (* Clear and draw *)
  gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0); (* Black *)
  gl##clear (gl##._COLOR_BUFFER_BIT_);
  gl##drawArrays gl##._TRIANGLES 0 3;
  
  (* Update the result div *)
  let result_div = Dom_html.getElementById "step-test-result" in
  result_div##.textContent := Js.some (string "Test 2 complete: Red triangle drawn on black background");
  
  check_error gl

(* Test 3: Draw a colored square using indices *)
let test_colored_square canvas_id =
  debug "=== Test 3: Draw Colored Square with Indices ===";
  let (_canvas, gl) = init_webgl canvas_id in
  
  (* Vertex and fragment shaders with color attribute *)
  let vert_src = string "
    attribute vec3 a_position;
    attribute vec3 a_color;
    
    varying vec3 v_color;
    
    void main() {
      gl_Position = vec4(a_position, 1.0);
      v_color = a_color;
    }
  " in
  
  let frag_src = string "
    precision mediump float;
    
    varying vec3 v_color;
    
    void main() {
      gl_FragColor = vec4(v_color, 1.0);
    }
  " in
  
  (* Create shaders and program *)
  let vert_shader = create_shader gl gl##._VERTEX_SHADER_ vert_src in
  let frag_shader = create_shader gl gl##._FRAGMENT_SHADER_ frag_src in
  let prog = create_program gl vert_shader frag_shader in
  
  gl##useProgram prog;
  
  (* Create square geometry *)
  let vertices = float32array [|
    -0.5; -0.5; 0.0;  (* Bottom left *)
     0.5; -0.5; 0.0;  (* Bottom right *)
     0.5;  0.5; 0.0;  (* Top right *)
    -0.5;  0.5; 0.0;  (* Top left *)
  |] in
  
  let colors = float32array [|
    1.0; 0.0; 0.0;  (* Red *)
    0.0; 1.0; 0.0;  (* Green *)
    0.0; 0.0; 1.0;  (* Blue *)
    1.0; 1.0; 0.0;  (* Yellow *)
  |] in
  
  (* Create indices for two triangles *)
  let indices = new%js Typed_array.uint16Array 6 in
  Typed_array.set indices 0 0;  (* First triangle *)
  Typed_array.set indices 1 1;
  Typed_array.set indices 2 2;
  Typed_array.set indices 3 0;  (* Second triangle *)
  Typed_array.set indices 4 2;
  Typed_array.set indices 5 3;
  
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
  
  (* Set up position attribute *)
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  gl##bindBuffer gl##._ARRAY_BUFFER_ vertex_buffer;
  gl##enableVertexAttribArray pos_attr;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  
  (* Set up color attribute *)
  let color_attr = gl##getAttribLocation prog (string "a_color") in
  gl##bindBuffer gl##._ARRAY_BUFFER_ color_buffer;
  gl##enableVertexAttribArray color_attr;
  gl##vertexAttribPointer color_attr 3 gl##._FLOAT _false 0 0;
  
  (* Clear and draw *)
  gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0); (* Black *)
  gl##clear (gl##._COLOR_BUFFER_BIT_);
  gl##drawElements gl##._TRIANGLES 6 gl##._UNSIGNED_SHORT_ 0;
  
  (* Update the result div *)
  let result_div = Dom_html.getElementById "step-test-result" in
  result_div##.textContent := Js.some (string "Test 3 complete: Colored square drawn using indices");
  
  check_error gl

(* Matrix helper functions *)
module Matrix = struct
  (* Perspective projection matrix *)
  let perspective fovy_deg aspect near far =
    let fovy_rad = fovy_deg *. Float.pi /. 180.0 in
    let f = 1.0 /. tan (fovy_rad /. 2.0) in
    let nf = 1.0 /. (near -. far) in
    
    [|
      f /. aspect; 0.0; 0.0; 0.0;
      0.0; f; 0.0; 0.0;
      0.0; 0.0; (far+.near) *. nf; -1.0;
      0.0; 0.0; 2.0 *. far *. near *. nf; 0.0
    |]
  
  (* Translation matrix *)
  let translation x y z =
    [|
      1.0; 0.0; 0.0; 0.0;
      0.0; 1.0; 0.0; 0.0;
      0.0; 0.0; 1.0; 0.0;
      x; y; z; 1.0
    |]
  
  (* Simple rotation around Y axis *)
  let rotation_y angle =
    let c = cos angle in
    let s = sin angle in
    [|
      c; 0.0; (-.s); 0.0;
      0.0; 1.0; 0.0; 0.0;
      s; 0.0; c; 0.0;
      0.0; 0.0; 0.0; 1.0
    |]
  
  (* Matrix multiplication *)
  let multiply a b =
    let result = Array.make 16 0.0 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        let idx = i * 4 + j in
        for k = 0 to 3 do
          result.(idx) <- result.(idx) +. a.(i * 4 + k) *. b.(k * 4 + j);
        done;
      done;
    done;
    result
end

(* Test 4: Draw a 3D cube with perspective - No Animation *)
let test_3d_cube canvas_id =
  try
    debug "=== Test 4: Draw 3D Cube (No Animation) ===";
    let (canvas, gl) = init_webgl canvas_id in
    
    (* Vertex and fragment shaders with matrices *)
    let vert_src = string "
      attribute vec3 a_position;
      attribute vec3 a_color;
      
      uniform mat4 u_matrix;
      
      varying vec3 v_color;
      
      void main() {
        gl_Position = u_matrix * vec4(a_position, 1.0);
        v_color = a_color;
      }
    " in
    
    let frag_src = string "
      precision mediump float;
      
      varying vec3 v_color;
      
      void main() {
        gl_FragColor = vec4(v_color, 1.0);
      }
    " in
    
    (* Create shaders and program *)
    let vert_shader = create_shader gl gl##._VERTEX_SHADER_ vert_src in
    let frag_shader = create_shader gl gl##._FRAGMENT_SHADER_ frag_src in
    let prog = create_program gl vert_shader frag_shader in
    
    gl##useProgram prog;
    
    (* Create cube geometry *)
    let vertices = float32array [|
      (* Front face *)
      -0.5; -0.5;  0.5;
       0.5; -0.5;  0.5;
       0.5;  0.5;  0.5;
      -0.5;  0.5;  0.5;
      
      (* Back face *)
      -0.5; -0.5; -0.5;
       0.5; -0.5; -0.5;
       0.5;  0.5; -0.5;
      -0.5;  0.5; -0.5;
      
      (* Top face *)
      -0.5;  0.5; -0.5;
       0.5;  0.5; -0.5;
       0.5;  0.5;  0.5;
      -0.5;  0.5;  0.5;
      
      (* Bottom face *)
      -0.5; -0.5; -0.5;
       0.5; -0.5; -0.5;
       0.5; -0.5;  0.5;
      -0.5; -0.5;  0.5;
      
      (* Right face *)
       0.5; -0.5; -0.5;
       0.5;  0.5; -0.5;
       0.5;  0.5;  0.5;
       0.5; -0.5;  0.5;
      
      (* Left face *)
      -0.5; -0.5; -0.5;
      -0.5;  0.5; -0.5;
      -0.5;  0.5;  0.5;
      -0.5; -0.5;  0.5;
    |] in
    
    (* Colors for each face *)
    let colors = float32array [|
      (* Front face: red *)
      1.0; 0.0; 0.0;  1.0; 0.0; 0.0;  1.0; 0.0; 0.0;  1.0; 0.0; 0.0;
      (* Back face: green *)
      0.0; 1.0; 0.0;  0.0; 1.0; 0.0;  0.0; 1.0; 0.0;  0.0; 1.0; 0.0;
      (* Top face: blue *)
      0.0; 0.0; 1.0;  0.0; 0.0; 1.0;  0.0; 0.0; 1.0;  0.0; 0.0; 1.0;
      (* Bottom face: yellow *)
      1.0; 1.0; 0.0;  1.0; 1.0; 0.0;  1.0; 1.0; 0.0;  1.0; 1.0; 0.0;
      (* Right face: purple *)
      1.0; 0.0; 1.0;  1.0; 0.0; 1.0;  1.0; 0.0; 1.0;  1.0; 0.0; 1.0;
      (* Left face: cyan *)
      0.0; 1.0; 1.0;  0.0; 1.0; 1.0;  0.0; 1.0; 1.0;  0.0; 1.0; 1.0;
    |] in
    
    (* Indices for the cube (6 faces, 2 triangles per face, 3 vertices per triangle) *)
    let indices = new%js Typed_array.uint16Array 36 in
    
    (* Manually set indices for each face to avoid helper function complexity *)
    (* Front face *)
    Typed_array.set indices 0 0;  Typed_array.set indices 1 1;  Typed_array.set indices 2 2;
    Typed_array.set indices 3 0;  Typed_array.set indices 4 2;  Typed_array.set indices 5 3;
    
    (* Back face *)
    Typed_array.set indices 6 4;  Typed_array.set indices 7 5;  Typed_array.set indices 8 6;
    Typed_array.set indices 9 4;  Typed_array.set indices 10 6;  Typed_array.set indices 11 7;
    
    (* Top face *)
    Typed_array.set indices 12 8;  Typed_array.set indices 13 9;  Typed_array.set indices 14 10;
    Typed_array.set indices 15 8;  Typed_array.set indices 16 10;  Typed_array.set indices 17 11;
    
    (* Bottom face *)
    Typed_array.set indices 18 12;  Typed_array.set indices 19 13;  Typed_array.set indices 20 14;
    Typed_array.set indices 21 12;  Typed_array.set indices 22 14;  Typed_array.set indices 23 15;
    
    (* Right face *)
    Typed_array.set indices 24 16;  Typed_array.set indices 25 17;  Typed_array.set indices 26 18;
    Typed_array.set indices 27 16;  Typed_array.set indices 28 18;  Typed_array.set indices 29 19;
    
    (* Left face *)
    Typed_array.set indices 30 20;  Typed_array.set indices 31 21;  Typed_array.set indices 32 22;
    Typed_array.set indices 33 20;  Typed_array.set indices 34 22;  Typed_array.set indices 35 23;
    
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
    
    (* Set up position attribute *)
    let pos_attr = gl##getAttribLocation prog (string "a_position") in
    gl##bindBuffer gl##._ARRAY_BUFFER_ vertex_buffer;
    gl##enableVertexAttribArray pos_attr;
    gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
    
    (* Set up color attribute *)
    let color_attr = gl##getAttribLocation prog (string "a_color") in
    gl##bindBuffer gl##._ARRAY_BUFFER_ color_buffer;
    gl##enableVertexAttribArray color_attr;
    gl##vertexAttribPointer color_attr 3 gl##._FLOAT _false 0 0;
    
    (* Get uniform location *)
    let matrix_location = gl##getUniformLocation prog (string "u_matrix") in
    
    (* Enable depth testing *)
    gl##enable gl##._DEPTH_TEST_;
    
    (* Update the result div *)
    let result_div = Dom_html.getElementById "step-test-result" in
    result_div##.textContent := Js.some (string "Test 4 complete: Statically rendered 3D cube");
    
    (* Create a simple combined matrix (perspective * rotation * translation) *)
    let combined_matrix = [|
      0.8; 0.0; 0.0; 0.0;
      0.0; 0.8; 0.0; 0.0;
      0.0; 0.0; 0.8; -1.0;
      0.0; 0.0; -3.0; 1.0
    |] in
    
    (* Set the matrix uniform *)
    gl##uniformMatrix4fv_typed matrix_location _false (float32array combined_matrix);
    
    (* Clear and draw *)
    gl##viewport 0 0 canvas##.width canvas##.height;
    gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0);
    gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
    
    (* Draw the cube *)
    gl##drawElements gl##._TRIANGLES 36 gl##._UNSIGNED_SHORT_ 0;
    
    if check_error gl then
      debug "Cube rendered successfully"
    else
      debug "Error rendering cube";

    true
  with
  | e -> 
    debug "Error in test_3d_cube: %s" (Printexc.to_string e);
    let result_div = Dom_html.getElementById "step-test-result" in
    result_div##.textContent := Js.some (string ("Test 4 failed: " ^ Printexc.to_string e));
    false
(* Test 5: Simple sphere with flat color *)
let test_simple_sphere canvas_id =
  debug "=== Test 5: Draw Simple Sphere ===";
  let (canvas, gl) = init_webgl canvas_id in
  
  (* Vertex and fragment shaders with matrices and lighting *)
  let vert_src = string "
    attribute vec3 a_position;
    attribute vec3 a_normal;
    
    uniform mat4 u_matrix;
    
    varying vec3 v_normal;
    
    void main() {
      gl_Position = u_matrix * vec4(a_position, 1.0);
      v_normal = a_normal;
    }
  " in
  
  let frag_src = string "
    precision mediump float;
    
    varying vec3 v_normal;
    
    uniform vec3 u_lightDir;
    
    void main() {
      vec3 normal = normalize(v_normal);
      float light = max(dot(normal, u_lightDir), 0.0);
      vec3 color = vec3(1.0, 0.5, 0.0); // Orange base color
      gl_FragColor = vec4(color * (0.3 + 0.7 * light), 1.0);
    }
  " in
  
  (* Create shaders and program *)
  let vert_shader = create_shader gl gl##._VERTEX_SHADER_ vert_src in
  let frag_shader = create_shader gl gl##._FRAGMENT_SHADER_ frag_src in
  let prog = create_program gl vert_shader frag_shader in
  
  gl##useProgram prog;
  
  (* Generate sphere *)
  let generate_sphere radius segments =
    debug "Generating sphere with radius %f and %d segments" radius segments;
    
    let positions = ref [] in
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
        positions := x :: y :: z :: !positions;
        
        (* Normal (normalized position for sphere) *)
        let length = sqrt (x *. x +. y *. y +. z *. z) in
        let nx = x /. length in
        let ny = y /. length in
        let nz = z /. length in
        normals := nx :: ny :: nz :: !normals;
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
    let positions = List.rev !positions in
    let normals = List.rev !normals in
    let indices = List.rev !indices in
    
    debug "Generated sphere with %d vertices and %d indices" 
      (List.length positions / 3) (List.length indices);
    
    (* Convert to typed arrays *)
    let positions_array = float32array (Array.of_list positions) in
    let normals_array = float32array (Array.of_list normals) in
    
    (* Convert indices to Uint16Array *)
    let indices_array = new%js Typed_array.uint16Array (List.length indices) in
    List.iteri (fun i v -> Typed_array.set indices_array i v) indices;
    
    positions_array, normals_array, indices_array
  in
  
  (* Generate sphere with 16 segments *)
  let (positions, normals, indices) = generate_sphere 1.0 16 in
  
  (* Create and bind vertex buffer *)
  let position_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ position_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ positions gl##._STATIC_DRAW_;
  
  (* Create and bind normal buffer *)
  let normal_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ normal_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ normals gl##._STATIC_DRAW_;
  
  (* Create and bind index buffer *)
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  
  (* Set up position attribute *)
  let pos_attr = gl##getAttribLocation prog (string "a_position") in
  gl##bindBuffer gl##._ARRAY_BUFFER_ position_buffer;
  gl##enableVertexAttribArray pos_attr;
  gl##vertexAttribPointer pos_attr 3 gl##._FLOAT _false 0 0;
  
  (* Set up normal attribute *)
  let normal_attr = gl##getAttribLocation prog (string "a_normal") in
  gl##bindBuffer gl##._ARRAY_BUFFER_ normal_buffer;
  gl##enableVertexAttribArray normal_attr;
  gl##vertexAttribPointer normal_attr 3 gl##._FLOAT _false 0 0;
  
  (* Get uniform locations *)
  let matrix_location = gl##getUniformLocation prog (string "u_matrix") in
  let light_dir_location = gl##getUniformLocation prog (string "u_lightDir") in
  
  (* Set light direction *)
  let light_dir = float32array [| 0.5; 0.7; 1.0 |] in
  gl##uniform3fv_typed light_dir_location light_dir;
  
  (* Enable depth testing *)
  gl##enable gl##._DEPTH_TEST_;
  
  (* Update the result div *)
  let result_div = Dom_html.getElementById "step-test-result" in
  result_div##.textContent := Js.some (string "Test 5 running: Animated sphere with lighting");
  
  (* Create matrices *)
  let aspect = float_of_int canvas##.width /. float_of_int canvas##.height in
  let projection = Matrix.perspective 45.0 aspect 0.1 100.0 in
  let view = Matrix.translation 0.0 0.0 (-3.0) in
  
  (* Animation function *)
  let angle = ref 0.0 in
  
  let rec animate _ =
    (* Update angle *)
    angle := !angle +. 0.01;
    
    (* Create model matrix with rotation *)
    let model = Matrix.rotation_y !angle in
    
    (* Combine matrices *)
    let matrix = Matrix.multiply projection (Matrix.multiply view model) in
    
    (* Set uniform *)
    gl##uniformMatrix4fv_typed matrix_location _false (float32array matrix);
    
    (* Clear and draw *)
    gl##viewport 0 0 canvas##.width canvas##.height;
    gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.0) (Js.float 1.0);
    gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
    gl##drawElements gl##._TRIANGLES indices##.length gl##._UNSIGNED_SHORT_ 0;
    
    (* Continue animation *)
    ignore (Dom_html.window##requestAnimationFrame(Js.wrap_callback animate));
  in
  
  (* Start animation *)
  ignore (Dom_html.window##requestAnimationFrame(Js.wrap_callback animate));
  true

(* Create the WebGL test tab with buttons for each test *)
let create_webgl_tests_tab () =
  let open Tyxml_js.Html in
  
  (* Create a canvas for WebGL rendering *)
  let canvas = canvas ~a:[
    a_id "step-by-step-canvas";
    a_width 400;
    a_height 400;
    a_style "border: 1px solid #000; display: block; margin: 10px auto;"
  ] [] in
  
  (* Create test buttons *)
  let test_buttons = [
    button ~a:[
      a_style "margin: 5px; padding: 8px 12px; background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 4px;";
      a_onclick (fun _ -> test_clear_canvas "step-by-step-canvas")
    ] [txt "Test 1: Clear Canvas"];
    
    button ~a:[
      a_style "margin: 5px; padding: 8px 12px; background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 4px;";
      a_onclick (fun _ -> test_triangle "step-by-step-canvas")
    ] [txt "Test 2: Draw Triangle"];
    
    button ~a:[
      a_style "margin: 5px; padding: 8px 12px; background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 4px;";
      a_onclick (fun _ -> test_colored_square "step-by-step-canvas")
    ] [txt "Test 3: Colored Square"];
    
    button ~a:[
      a_style "margin: 5px; padding: 8px 12px; background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 4px;";
      a_onclick (fun _ -> test_3d_cube "step-by-step-canvas")
    ] [txt "Test 4: 3D Cube"];
    
    button ~a:[
      a_style "margin: 5px; padding: 8px 12px; background-color: #f0f0f0; border: 1px solid #ccc; border-radius: 4px;";
      a_onclick (fun _ -> test_simple_sphere "step-by-step-canvas")
    ] [txt "Test 5: Simple Sphere"];
  ] in
  
  (* Create button container *)
  let button_container = div ~a:[
    a_style "display: flex; flex-wrap: wrap; justify-content: center; margin: 10px 0;"
  ] test_buttons in
  
  (* Create result div to show status *)
  let result_div = div ~a:[
    a_id "step-test-result";
    a_style "margin: 10px; padding: 10px; min-height: 50px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 4px;"
  ] [txt "Click a test button above to run WebGL tests"] in
  
  (* Create info div for instructions *)
  let info_div = div ~a:[
    a_style "margin: 10px; padding: 10px; background-color: #e9f5e9; border: 1px solid #ddd; border-radius: 4px;"
  ] [
    p [txt "These tests progressively build WebGL functionality from simple to complex:"];
    ul [
      li [txt "Test 1: Clear the canvas with a color"];
      li [txt "Test 2: Draw a simple triangle"];
      li [txt "Test 3: Draw a colored square using indices"];
      li [txt "Test 4: Draw a rotating 3D cube with perspective"];
      li [txt "Test 5: Draw a rotating sphere with lighting"]
    ];
    p [txt "Check the browser console for detailed log messages."]
  ] in
  
  (* Assemble the tab content *)
  div [
    h2 ~a:[a_style "text-align: center;"] [txt "WebGL Step-by-Step Tests"];
    info_div;
    canvas;
    button_container;
    result_div
  ]
