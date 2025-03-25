open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt
open Js

(* Log entry type *)
type log_entry = {
  timestamp: float;
  level: string; (* "debug" or "error" *)
  message: string;
}

(* Celestial coordinates type for texture mapping *)
type celestial_coords = {
  ra: float;  (* Right Ascension in degrees (0-360) *)
  dec: float; (* Declination in degrees (-90 to +90) *)
  size: float; (* Angular size in degrees *)
}

(* Extended texture type with celestial coordinates *)
type texture_info = {
  texture: WebGL.texture Js.t;
  coords: celestial_coords;
  url: string;
}

let error f = Printf.ksprintf (fun s -> Console.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf (fun s -> Console.console##log (Js.string s)) f

let check_error gl = if gl##getError <> gl##._NO_ERROR_ then error "WebGL error"

(* Helper function to convert WebGL error codes to readable strings *)
let gl_error_to_string (gl:WebGL.renderingContext Js.t) error_code =
  if error_code = gl##._NO_ERROR_ then "NO_ERROR"
  else if error_code = gl##._INVALID_ENUM_ then "INVALID_ENUM"
  else if error_code = gl##._INVALID_VALUE_ then "INVALID_VALUE"
  else if error_code = gl##._INVALID_OPERATION_ then "INVALID_OPERATION"
  else if error_code = gl##._INVALID_FRAMEBUFFER_OPERATION_ then "INVALID_FRAMEBUFFER_OPERATION"
  else if error_code = gl##._OUT_OF_MEMORY_ then "OUT_OF_MEMORY"
  else if error_code = gl##._CONTEXT_LOST_WEBGL_ then "CONTEXT_LOST_WEBGL"
  else "UNKNOWN_ERROR"

(* Convert RA/DEC to 3D coordinates on unit sphere *)
let celestial_to_cartesian ra_deg dec_deg =
  (* Convert to radians *)
  let ra = ra_deg *. Float.pi /. 180.0 in
  let dec = dec_deg *. Float.pi /. 180.0 in
  
  (* Calculate 3D position (x,y,z) *)
  let cos_dec = cos dec in
  
  (* Note: Coordinate system conversion - RA increases eastward *)
  let x = cos_dec *. cos ra in
  let y = sin dec in
  let z = cos_dec *. (-1.0) *. sin ra in
  
  (x, y, z)

(* Check if a point on the sphere is within the angular distance of a celestial position *)
let is_within_region sphere_point celestial_pos angular_radius_deg =
  let (sx, sy, sz) = sphere_point in
  let (cx, cy, cz) = celestial_to_cartesian celestial_pos.ra celestial_pos.dec in
  
  (* Calculate dot product *)
  let dot_product = sx *. cx +. sy *. cy +. sz *. cz in
  
  (* Cosine of the angle between the vectors *)
  let cos_angle = max (-1.0) (min dot_product 1.0) in
  
  (* Convert to angle in degrees *)
  let angle_deg = acos cos_angle *. 180.0 /. Float.pi in
  
  (* Check if within angular radius *)
  angle_deg <= angular_radius_deg
(* Modified fragment shader that crops textures without yellow highlights *)
let create_celestial_fragment_shader num_textures =
  let texture_uniform_declarations = 
    String.concat "\n" (List.init num_textures (fun i ->
      Printf.sprintf "uniform sampler2D u_texture%d;" i
    ))
  in
  
  let coord_uniform_declarations =
    String.concat "\n" (List.init num_textures (fun i ->
      Printf.sprintf "uniform vec3 u_texPos%d; // x=RA, y=DEC, z=size in degrees" i
    ))
  in
  
  let texture_sampling_code =
    if num_textures = 0 then
      "vec3 textureColor = vec3(0.8, 0.8, 0.8);"
    else
      String.concat "\n" [
        "vec3 textureColor = vec3(0.0, 0.0, 0.0);";
        "bool found_texture = false;";
        String.concat "\n" (List.init num_textures (fun i ->
          Printf.sprintf "
          if (!found_texture) {
            // Calculate angle between fragment position and texture center
            vec3 texDir%d = vec3(
              cos(u_texPos%d.y * 3.14159 / 180.0) * cos(u_texPos%d.x * 3.14159 / 180.0),
              sin(u_texPos%d.y * 3.14159 / 180.0),
              cos(u_texPos%d.y * 3.14159 / 180.0) * (-1.0) * sin(u_texPos%d.x * 3.14159 / 180.0)
            );
            
            float dotProduct%d = dot(normalize(v_position), texDir%d);
            float angleDeg%d = acos(clamp(dotProduct%d, -1.0, 1.0)) * 180.0 / 3.14159;
            
            if (angleDeg%d <= u_texPos%d.z) {
              // We're inside this texture's region
              
              // Calculate normalized coordinates within the circular region
              // Get perpendicular direction for calculating image projection
              vec3 north = vec3(0.0, 1.0, 0.0);
              vec3 perpDir = normalize(cross(texDir%d, north));
              vec3 perpDir2 = normalize(cross(perpDir, texDir%d));
              
              // Project onto the plane
              float xOffset = dot(normalize(v_position), perpDir);
              float yOffset = dot(normalize(v_position), perpDir2);
              
              // Scale to -1 to 1 range based on the angular size
              xOffset = xOffset / sin(u_texPos%d.z * 3.14159 / 180.0);
              yOffset = yOffset / sin(u_texPos%d.z * 3.14159 / 180.0);
              
              // Convert to 0 to 1 texture coordinates
              // But use only the center portion of the texture (crop)
              float u = (xOffset + 1.0) * 0.5;
              float v = (yOffset + 1.0) * 0.5;
              
              // Remap to center crop
              u = 0.3 + (u * 0.4); // Use middle 40%% horizontally (30%% to 70%%)
              v = 0.3 + (v * 0.4); // Use middle 40%% vertically (30%% to 70%%)
              
              // Sample texture
              textureColor = texture2D(u_texture%d, vec2(u, v)).rgb;
              
              found_texture = true;
            }
          }" i i i i i i i i i i i i i i i i i
        ));
        "if (!found_texture) {";
        "  // Use background star field";
        "  textureColor = vec3(0.0, 0.0, 0.1);";
        "  ";
        "  // Add stars based on position";
        "  float starValue = fract(sin(dot(normalize(v_position).xy, vec2(12.9898, 78.233))) * 43758.5453);";
        "  if (starValue > 0.995) {";
        "    float starBrightness = (starValue - 0.995) * 200.0;";
        "    textureColor = vec3(starBrightness);";
        "  }";
        "}";
      ]
  in
  
  Printf.sprintf "
    precision mediump float;
    
    varying vec3 v_normal;
    varying vec2 v_texcoord;
    varying vec3 v_position;
    
    uniform vec3 u_lightDir;
    uniform bool u_useTexture;
    
    %s
    
    %s
    
    void main() {
      // Normalize the normal
      vec3 normal = normalize(v_normal);
      
      // Dot product with light direction gives diffuse lighting
      float light = max(dot(normal, u_lightDir), 0.0) * 0.7 + 0.3;
      
      vec3 color;
      
      if (u_useTexture) {
        %s
        
        // Apply simple lighting
        color = textureColor * light;
      } else {
        // Base color: dark blue for night sky
        vec3 baseColor = vec3(0.0, 0.0, 0.1);
        
        // Add grid lines
        if (abs(mod(v_position.x, 0.2)) < 0.005 || abs(mod(v_position.y, 0.2)) < 0.005 || abs(mod(v_position.z, 0.2)) < 0.005) {
          baseColor = vec3(0.3, 0.3, 0.3);
        }
        
        // Apply lighting
        color = baseColor * light;
      }
      
      gl_FragColor = vec4(color, 1.0);
    }
  " texture_uniform_declarations coord_uniform_declarations texture_sampling_code

let messier_objects = 
   [
     (* Cluster 16 *)
     ("Cluster 16", 173.64, 54.52, 4.29, 16, "cluster_16_M109_M108.jpg");
     (* Cluster 15 *)
     ("Cluster 15", 23.34, 60.66, 0.50, 15, "cluster_15_M103.jpg");
     (* Cluster 13 *)
     ("Cluster 13", 40.67, -0.01, 0.50, 13, "cluster_13_M77.jpg");
     (* Cluster 10 *)
     ("Cluster 10", 181.02, 18.15, 34.02, 10, "cluster_10_M66_M65_M63_M61.jpg");
     (* Cluster 5 *)
     ("Cluster 5", 13.15, 46.61, 23.70, 5, "cluster_5_M52_M34_M32_M31.jpg");
     (* Cluster 4 *)
     ("Cluster 4", 299.90, 22.72, 0.50, 4, "cluster_4_M27.jpg");
     (* Cluster 2 *)
     ("Cluster 2", 221.08, 41.95, 27.92, 2, "cluster_2_M102_M51_M13_M3.jpg");
     (* Cluster 1 *)
     ("Cluster 1", 87.01, 28.26, 8.34, 1, "cluster_1_M37_M36_M35_M1.jpg");
     (* Cluster 3 *)
     ("Cluster 3", 274.94, -14.98, 1.46, 3, "cluster_3_M17_M16.jpg");
     (* Cluster 6 *)
     ("Cluster 6", 75.23, 20.01, 27.44, 6, "cluster_6_M78_M45_M38.jpg");
     (* Cluster 7 *)
     ("Cluster 7", 162.95, 62.96, 14.48, 7, "cluster_7_M97_M82_M81_M40.jpg");
     (* Cluster 8 *)
     ("Cluster 8", 96.12, -11.56, 22.92, 8, "cluster_8_M46_M43_M42_M41.jpg");
     (* Cluster 9 *)
     ("Cluster 9", 118.33, -2.23, 29.64, 9, "cluster_9_M50_M48_M47_M44.jpg");
     (* Cluster 11 *)
     ("Cluster 11", 152.02, 12.03, 22.51, 11, "cluster_11_M105_M95_M67.jpg");
     (* Cluster 12 *)
     ("Cluster 12", 19.96, 36.35, 25.08, 12, "cluster_12_M110_M76_M74.jpg");
     (* Cluster 14 *)
     ("Cluster 14", 185.23, 31.56, 18.90, 14, "cluster_14_M106_M100.jpg")
   ]

(* Helper to get DOM localStorage object *)
let local_storage () =
  Js.Optdef.get Dom_html.window##.localStorage
    (fun () -> failwith "localStorage not available")

(* LocalStorage key for texture logs *)
let texture_log_key = "webgl_texture_logs"

(* Helper to deserialize JSON to log entries *)
let deserialize_logs json_str =
  try
    let json_array = Js._JSON##parse (Js.string json_str) in
    let arr = Js.to_array json_array in
    
    Array.map (fun obj ->
      let timestamp = Js.Unsafe.get obj (Js.string "timestamp") in
      let timestamp_float = Js.float_of_number (Js.Unsafe.coerce timestamp) in
      
      let level_js = Js.Unsafe.get obj (Js.string "level") in
      let level = Js.to_string (Js.Unsafe.coerce level_js) in
      
      let message_js = Js.Unsafe.get obj (Js.string "message") in
      let message = Js.to_string (Js.Unsafe.coerce message_js) in
      
      { timestamp = timestamp_float; level; message }
    ) arr
  with _ -> [||]

(* Load logs from localStorage *)
let load_logs () =
  let storage = local_storage () in
  match Js.Opt.to_option (storage##getItem (Js.string texture_log_key)) with
  | None -> [||]
  | Some json_str -> deserialize_logs (Js.to_string json_str)

(* Helper to serialize log entries to JSON *)
let serialize_logs logs =
  let entries_json = Array.map (fun entry ->
    let obj = Js.Unsafe.obj [|
      ("timestamp", Js.Unsafe.inject (Js.number_of_float entry.timestamp));
      ("level", Js.Unsafe.inject (Js.string entry.level));
      ("message", Js.Unsafe.inject (Js.string entry.message))
    |] in
    obj
  ) logs in
  
  let json_string = Js.to_string (Js._JSON##stringify (Js.array entries_json)) in
  json_string

(* Save logs to localStorage *)
let save_logs logs =
  let json_str = serialize_logs logs in
  let storage = local_storage () in
  storage##setItem (Js.string texture_log_key) (Js.string json_str)

(* Format timestamp for display *)
let format_timestamp ts =
  let date = new%js Js.date_fromTimeValue (Js.float ts) in
  let hours = date##getHours in
  let minutes = date##getMinutes in
  let seconds = date##getSeconds in
  let ms = date##getMilliseconds in
  Printf.sprintf "%02d:%02d:%02d.%03d" hours minutes seconds ms

(* Add a new log entry *)
let add_log_entry level message =
  let logs = load_logs () in
  let entry = {
    timestamp = Js.to_float (new%js date_now)##getTime;
    level;
    message;
  } in
  
  (* Add new entry *)
  let new_logs = Array.append logs [|entry|] in
  
  (* Limit to last 1000 entries *)
  let limited_logs = 
    if Array.length new_logs > 1000 then
      Array.sub new_logs (Array.length new_logs - 1000) 1000
    else
      new_logs
  in
  
  save_logs limited_logs;
  entry

(* Refresh the log display with current localStorage entries *)
and refresh_log_display container =
  (* Find or create log content div *)
  let content_id = "texture-log-content" in
  let content = 
    match Opt.to_option (Dom_html.document##getElementById (string content_id)) with
    | Some content -> content
    | None -> 
        let new_content = Dom_html.createDiv Dom_html.document in
        new_content##.id := string content_id;
        Dom.appendChild container new_content;
        new_content
  in
  
  (* Clear existing content *)
  content##.innerHTML := string "";
  
  (* Get logs from localStorage *)
  let logs = load_logs () in
  
  (* Add each log entry to display *)
  Array.iter (fun entry ->
    (* Create log entry *)
    let log_div = Dom_html.createDiv Dom_html.document in
    
    (* Style based on type *)
    let style = log_div##.style in
    style##.borderLeft := string (
      match entry.level with
      | "debug" -> "3px solid #2196F3"
      | "error" -> "3px solid #F44336"
      | _ -> "3px solid #9E9E9E"
    );
    style##.paddingLeft := string "5px";
    style##.marginBottom := string "3px";
    
    (* Set content *)
    let time_display = format_timestamp entry.timestamp in
    log_div##.innerHTML := string (Printf.sprintf "[%s] %s" time_display entry.message);
    
    (* Append to container *)
    Dom.appendChild content log_div
  ) logs;
  
  (* Scroll to bottom *)
  content##.scrollTop := Js.float (float_of_int (content##.scrollHeight))

(* Log a message to both console and localStorage *)
let log_texture_msg msg_type msg =
  (* First log to console *)
  (match msg_type with
  | `Debug -> debug "%s" msg
  | `Error -> error "%s" msg);
  
  (* Then add to localStorage *)
  let level = match msg_type with
    | `Debug -> "debug"
    | `Error -> "error"
  in
  let _ = add_log_entry level msg in
  
  (* Then update display if it exists *)
  match Opt.to_option (Dom_html.document##getElementById (string "texture-log-container")) with
  | None -> () (* Container doesn't exist *)
  | Some container -> refresh_log_display container

(* Enhanced load_texture function with localStorage logging *)
let load_texture (gl:WebGL.renderingContext Js.t) url =
  log_texture_msg `Debug (Printf.sprintf "Texture loading started: %s" url);
  let texture = gl##createTexture in
  let image = Dom_html.createImg Dom_html.document in
  
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Created WebGL texture object" url);
  gl##bindTexture gl##._TEXTURE_2D_ texture;
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Bound to TEXTURE_2D target" url);
  
  (* Use a temporary 1x1 pixel while loading *)
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Setting up temporary 1x1 placeholder texture" url);
  let tmp = new%js Typed_array.uint8Array 4 in
  Array.iteri (fun i v -> Typed_array.set tmp i v) [|255; 0; 255; 255|];
  
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Uploading placeholder pixel to GPU" url);
  gl##texImage2D_fromView gl##._TEXTURE_2D_ 0 gl##._RGBA 1 1 0 gl##._RGBA gl##._UNSIGNED_BYTE_ 
    (Js.Unsafe.coerce tmp);
  
  let error_code = gl##getError in
  if error_code <> gl##._NO_ERROR_ then begin
    let error_msg = Printf.sprintf "Texture %s: WebGL error after setting placeholder: %s" 
      url (gl_error_to_string gl error_code) in
    log_texture_msg `Error error_msg
  end;
  
  (* Setup load callback *)
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Setting up onload handler" url);
  image##.onload := Dom_html.handler (fun _ ->
    log_texture_msg `Debug (Printf.sprintf "Texture %s: Image loaded successfully, dimensions: %dx%d" 
      url image##.width image##.height);
    
    gl##bindTexture gl##._TEXTURE_2D_ texture;
    log_texture_msg `Debug (Printf.sprintf "Texture %s: Bound texture before uploading image data" url);
    
    (try
      gl##texImage2D_fromImage gl##._TEXTURE_2D_ 0 gl##._RGBA gl##._RGBA gl##._UNSIGNED_BYTE_ image;
      
      let upload_error = gl##getError in
      if upload_error <> gl##._NO_ERROR_ then begin
        let error_msg = Printf.sprintf "Texture %s: WebGL error after uploading image: %s" 
          url (gl_error_to_string gl upload_error) in
        log_texture_msg `Error error_msg
      end else
        log_texture_msg `Debug (Printf.sprintf "Texture %s: Successfully uploaded image to GPU" url);
      
      (* Check if the image is a power of 2 in both dimensions *)
      let width = image##.width in
      let height = image##.height in
      let is_power_of_2 n = (n land (n - 1)) = 0 in
      
      log_texture_msg `Debug (Printf.sprintf "Texture %s: Dimensions power-of-2 check: width=%b, height=%b" 
        url (is_power_of_2 width) (is_power_of_2 height));
      
      if is_power_of_2 width && is_power_of_2 height then begin
        log_texture_msg `Debug (Printf.sprintf "Texture %s: Power-of-2 texture, generating mipmaps" url);
        gl##generateMipmap gl##._TEXTURE_2D_;
        gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MIN_FILTER_ gl##._LINEAR_MIPMAP_LINEAR_;
        log_texture_msg `Debug (Printf.sprintf "Texture %s: Set up LINEAR_MIPMAP_LINEAR filtering" url);
      end else begin
        (* For non-power-of-2 textures, disable mipmapping and set wrapping to clamp *)
        log_texture_msg `Debug (Printf.sprintf "Texture %s: Non-power-of-2 texture, setting up appropriate parameters" url);
        gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_S_ gl##._CLAMP_TO_EDGE_;
        gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_T_ gl##._CLAMP_TO_EDGE_;
        gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MIN_FILTER_ gl##._LINEAR;
        log_texture_msg `Debug (Printf.sprintf "Texture %s: Set up CLAMP_TO_EDGE wrapping and LINEAR filtering" url);
      end;
      
      let param_error = gl##getError in
      if param_error <> gl##._NO_ERROR_ then begin
        let error_msg = Printf.sprintf "Texture %s: WebGL error after setting texture parameters: %s" 
          url (gl_error_to_string gl param_error) in
        log_texture_msg `Error error_msg
      end else
        log_texture_msg `Debug (Printf.sprintf "Texture %s: Successfully set up all texture parameters" url);
    with e ->
      let error_msg = Printf.sprintf "Texture %s: Exception during image upload: %s" 
        url (Printexc.to_string e) in
      log_texture_msg `Error error_msg);
    
    Js._false
  );
  
  (* Setup error handler *)
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Setting up onerror handler" url);
  image##.onerror := Dom_html.handler (fun _ ->
    let error_msg = Printf.sprintf "Texture %s: Image loading failed" url in
    log_texture_msg `Error error_msg;
    error "Failed to load texture: %s" url
  );
  
  (* Add abort handler *)
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Setting up onabort handler" url);
  image##.onabort := Dom_html.handler (fun _ ->
    let error_msg = Printf.sprintf "Texture %s: Image loading aborted" url in
    log_texture_msg `Error error_msg;
    error "Texture loading aborted: %s" url
  );
  
  (* Start loading *)
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Setting image.src to start loading" url);
  image##.src := string url;
  log_texture_msg `Debug (Printf.sprintf "Texture %s: Loading initiated" url);
  
  texture

(* Create a texture with celestial coordinates *)
let create_texture_with_coords gl url ra dec size =
  let texture_info = {
    texture = load_texture gl url;
    coords = { ra; dec; size };
    url;
  } in
  log_texture_msg `Debug (Printf.sprintf "Created texture for %s at RA=%.2f° DEC=%.2f° (size=%.2f°)" 
    url ra dec size);
  texture_info

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

let float32array a =
  let array = new%js Typed_array.float32Array (Array.length a) in
  Array.iteri (fun i v -> Typed_array.set array i (Js.float v)) a;
  array

(* Generate a sphere for the globe, now with texture coordinates *)
let generate_sphere radius segments =
  let vertices = ref [] in
  let normals = ref [] in
  let texcoords = ref [] in  (* Added for texture coordinates *)
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
      
      (* Texture coordinates *)
      let u = 1.0 -. (float_of_int j /. float_of_int (segments * 2)) in
      let v = 1.0 -. (float_of_int i /. float_of_int segments) in
      texcoords := u :: v :: !texcoords;
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
  let texcoords = List.rev !texcoords in  (* Added for texture coordinates *)
  let indices = List.rev !indices in
  
  debug "Generated sphere with %d vertices and %d indices" 
    (List.length vertices / 3) (List.length indices);
  
  (* Convert to typed arrays *)
  let vertices_array = float32array (Array.of_list vertices) in
  let normals_array = float32array (Array.of_list normals) in
  let texcoords_array = float32array (Array.of_list texcoords) in  (* Added for texture coordinates *)
  
  (* Convert indices to Uint16Array *)
  let indices_array = new%js Typed_array.uint16Array (List.length indices) in
  List.iteri (fun i v -> Typed_array.set indices_array i v) indices;
  
  vertices_array, normals_array, texcoords_array, indices_array  (* Return texture coordinates as well *)

(* Updated start_celestial_globe function to use cluster filenames *)

let start_celestial_globe texture_base_url =
  debug "Starting celestial globe...";
  
  (* Get FPS counter element *)
  let fps_text = Dom_html.document##createTextNode (string "Loading...") in
  Opt.iter
    (Opt.bind (Dom_html.document##getElementById (string "fps")) Dom_html.CoerceTo.element)
    (fun span -> Dom.appendChild span fps_text);
  
  (* Initialize canvas and WebGL *)
  let (_canvas, gl) = init_canvas "canvas" in
  
  (* Clear color: black for night sky *)
  gl##clearColor (Js.float 0.0) (Js.float 0.0) (Js.float 0.1) (Js.float 1.0);
  
  (* Create shaders and program *)
  let vert_shader = string "
    attribute vec3 a_position;
    attribute vec3 a_normal;
    attribute vec2 a_texcoord;
    
    uniform mat4 u_matrix;
    
    varying vec3 v_normal;
    varying vec2 v_texcoord;
    varying vec3 v_position;
    
    void main() {
      vec4 position = u_matrix * vec4(a_position, 1.0);
      gl_Position = position;
      
      v_normal = a_normal;
      v_texcoord = a_texcoord;
      v_position = a_position; // Use the raw position for celestial mapping
    }
  " in
  
  (* Create fragment shader using our generator function *)
  let num_textures = List.length messier_objects in
  let frag_shader = string (create_celestial_fragment_shader num_textures) in
  
  let prog = create_program gl vert_shader frag_shader in
  gl##useProgram prog;
  
  (* Enable depth testing *)
  gl##enable gl##._DEPTH_TEST_;
  gl##depthFunc gl##._LESS;
  
  (* Set up uniforms *)
  let matrix_loc = gl##getUniformLocation prog (string "u_matrix") in
  let light_dir_loc = gl##getUniformLocation prog (string "u_lightDir") in
  let use_texture_loc = gl##getUniformLocation prog (string "u_useTexture") in
  
  (* Get texture uniforms *)
  let texture_locs = List.init num_textures (fun i ->
    gl##getUniformLocation prog (string (Printf.sprintf "u_texture%d" i))
  ) in
  
  (* Get texture position uniforms *)
  let texture_pos_locs = List.init num_textures (fun i ->
    gl##getUniformLocation prog (string (Printf.sprintf "u_texPos%d" i))
  ) in
  
  (* Set light direction - slightly offset from viewing direction *)
  let light_dir = float32array [| 0.5; 0.7; 1.0 |] in
  gl##uniform3fv_typed light_dir_loc light_dir;
  
  (* Load textures with celestial coordinates - UPDATED FOR CLUSTER FILENAMES *)
  log_texture_msg `Debug "Loading celestial textures...";
  
  (* Magnification factor to make objects more visible - reduced for clusters *)
  let magnification = 1.0 in
  
  let texture_infos = List.mapi (fun i (name, ra, dec, size, id, filename) ->
    (* For clusters, use the full filename provided in the messier_objects list *)
    let url = Printf.sprintf "%s/%s" texture_base_url filename in
    
    let magnified_size = size *. magnification in
    log_texture_msg `Debug (Printf.sprintf "Loading texture for %s at RA=%.2f DEC=%.2f, size=%.2f degrees (magnified to %.2f)" 
      name ra dec size magnified_size);
    create_texture_with_coords gl url ra dec magnified_size
  ) messier_objects in
  
  (* Generate sphere *)
  debug "Generating celestial sphere...";
  let (positions, normals, texcoords, indices) = generate_sphere 0.9 48 in
  
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
  
  (* Set up texture coordinate attribute *)
  let texcoord_attr = gl##getAttribLocation prog (string "a_texcoord") in
  gl##enableVertexAttribArray texcoord_attr;
  let texcoord_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ texcoord_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ texcoords gl##._STATIC_DRAW_;
  gl##vertexAttribPointer texcoord_attr 2 gl##._FLOAT _false 0 0;
  
  (* Set up index buffer *)
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  
  (* Animation state *)
  let get_time () = Js.to_float (new%js date_now)##getTime in
  let last_draw = ref (get_time ()) in
  let draw_times = Queue.create () in
  let use_texture = ref true in  (* Flag to toggle texture *)
  
  (* Set up toggle button for texture *)
  Opt.iter
    (Dom_html.document##getElementById (string "toggle-texture"))
    (fun button ->
      button##.onclick := Dom_html.handler (fun _ ->
        use_texture := not !use_texture;
        debug "Textures %s" (if !use_texture then "enabled" else "disabled");
        Js._false
      )
    );
  
  (* Add rotation controls *)
  let rotation_speed = ref 0.2 in (* degrees per second *)
  let rotation_x = ref 0.0 in
  let rotation_y = ref 0.0 in
  
  (* Create rotation control buttons *)
  let create_control_button id label callback =
    Opt.iter
      (Dom_html.document##getElementById (string id))
      (fun button ->
        button##.textContent := Js.some (string label);
        button##.onclick := Dom_html.handler (fun _ ->
          callback ();
          Js._false
        )
      )
  in
  
  (* Setup rotation controls (if they exist in your HTML) *)
  create_control_button "rotate-up" "↑" (fun () -> rotation_x := !rotation_x +. 5.0);
  create_control_button "rotate-down" "↓" (fun () -> rotation_x := !rotation_x -. 5.0);
  create_control_button "rotate-left" "←" (fun () -> rotation_y := !rotation_y -. 5.0);
  create_control_button "rotate-right" "→" (fun () -> rotation_y := !rotation_y +. 5.0);
  create_control_button "reset-rotation" "Reset" (fun () -> 
    rotation_x := 0.0;
    rotation_y := 0.0
  );
  
  (* Animation loop *)
  let rec animate () =
    (* Get current time for animation *)
    let t = get_time () /. 1000.0 in
    
    (* Auto-rotate slowly if set *)
    if !rotation_speed <> 0.0 then
      rotation_y := !rotation_y +. !rotation_speed *. 0.016;

    if false then debug "Rotation: x=%.2f, y=%.2f" !rotation_x !rotation_y;
      
    (* Create rotation matrix based on rotation_x and rotation_y *)
    let rx = !rotation_x *. Float.pi /. 180.0 in
    let ry = !rotation_y *. Float.pi /. 180.0 in
    
    let cx = cos rx in
    let sx = sin rx in
    let cy = cos ry in
    let sy = sin ry in
    
    let rotation = [|
      cos !rotation_y; 0.0; sin !rotation_y; 0.0;
      0.0; 1.0; 0.0; 0.0;
      -. sin !rotation_y; 0.0; cos !rotation_y; 0.0;
      0.0; 0.0; 0.0; 1.0
    |] in

    (* Set the matrix uniform *)
    gl##uniformMatrix4fv_typed matrix_loc _false (float32array rotation);
    
    (* Set texture uniforms *)
    List.iteri (fun i texture_info ->
      (* Set texture sampler *)
      gl##activeTexture (gl##._TEXTURE0 + i);
      gl##bindTexture gl##._TEXTURE_2D_ texture_info.texture;
      gl##uniform1i (List.nth texture_locs i) i;
      
      (* Set texture position (ra, dec, size) *)
      let c = texture_info.coords in
      gl##uniform3f (List.nth texture_pos_locs i) 
        (Js.float c.ra) (Js.float c.dec) (Js.float c.size);
    ) texture_infos;
    
    (* Set texture toggle uniform *)
    gl##uniform1i use_texture_loc (if !use_texture then 1 else 0);
    
    (* Clear the canvas *)
    gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
    
    (* Draw the sphere *)
    gl##drawElements gl##._TRIANGLES indices##.length gl##._UNSIGNED_SHORT_ 0;
    
    (* Check for GL errors *)
    let error_code = gl##getError in
    if error_code <> gl##._NO_ERROR_ then begin
      let error_msg = Printf.sprintf "WebGL error in animation loop: %s" 
        (gl_error_to_string gl error_code) in
      log_texture_msg `Error error_msg
    end;
    
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
    fps_text##.data := string (Printf.sprintf "%.1f FPS" fps);
    
    (* Continue animation *)
    Lwt_js.sleep 0.016 >>= animate
  in
  
  (* Start animation loop *)
  animate ()

(* Create log container in a non-interfering position *)
let setup_log_container () =
  let container_id = "texture-log-container" in
  
  (* Check if container already exists *)
  let container = 
    match Opt.to_option (Dom_html.document##getElementById (string container_id)) with
    | Some container -> container
    | None ->
        (* Create container *)
        let new_container = Dom_html.createDiv Dom_html.document in
        new_container##.id := string container_id;
        
        (* Style the container - position to the right side instead of bottom *)
        let style = new_container##.style in
        style##.position := string "fixed";
        style##.top := string "10px";         (* Top of page instead of bottom *)
        style##.right := string "10px";       (* Right side instead of left *)
        style##.width := string "400px";      (* Fixed width *)
        style##.maxHeight := string "90vh";   (* Almost full height *)
        style##.overflowY := string "auto";
        style##.backgroundColor := string "rgba(0,0,0,0.7)";
        style##.color := string "white";
        style##.fontFamily := string "monospace";
        style##.fontSize := string "10px";    (* Smaller font *)
        style##.padding := string "10px";
        style##.zIndex := string "1000";
        style##.border := string "1px solid #444";
        style##.borderRadius := string "4px";
        
        (* Append container to body *)
        Dom.appendChild Dom_html.document##.body new_container;
        new_container
  in
  
  (* Clear existing content *)
  container##.innerHTML := string "";
  
  (* Add header with title and controls *)
  let header = Dom_html.createDiv Dom_html.document in
  let header_style = header##.style in
  header_style##.position := string "sticky";
  header_style##.top := string "0";
  header_style##.backgroundColor := string "rgba(30,30,30,0.9)";
  header_style##.padding := string "5px";
  header_style##.marginBottom := string "5px";
  header_style##.display := string "flex";
  header_style##.borderBottom := string "1px solid #555";
  
  (* Add title *)
  let title = Dom_html.createDiv Dom_html.document in
  title##.textContent := Js.some (string "Texture Loading Log");
  let title_style = title##.style in
  title_style##.fontWeight := string "bold";
  
  (* Create buttons container *)
  let buttons = Dom_html.createDiv Dom_html.document in
  
  (* Create minimize button *)
  let minimize_button = Dom_html.createButton Dom_html.document in
  minimize_button##.textContent := Js.some (string "_");
  minimize_button##.title := string "Minimize";
  minimize_button##.onclick := Dom_html.handler (fun _ ->
    (* Toggle visibility of log content *)
    let content = Opt.get (Dom_html.document##getElementById (string "texture-log-content")) (fun () -> assert false) in
    if content##.style##.display = string "none" then begin
      content##.style##.display := string "block";
      minimize_button##.textContent := Js.some (string "_");
    end else begin
      content##.style##.display := string "none";
      minimize_button##.textContent := Js.some (string "□");
    end;
    Js._false
  );
  
  (* Style minimize button *)
  let min_btn_style = minimize_button##.style in
  min_btn_style##.width := string "24px";
  min_btn_style##.marginRight := string "5px";
  min_btn_style##.cursor := string "pointer";
  
  (* Create a clear button *)
  let clear_button = Dom_html.createButton Dom_html.document in
  clear_button##.textContent := Js.some (string "Clear");
  clear_button##.title := string "Clear log";
  clear_button##.onclick := Dom_html.handler (fun _ ->
    (* Clear logs from localStorage *)
    save_logs [||];
    (* Update display *)
    refresh_log_display container;
    Js._false
  );
  
  (* Style clear button *)
  let clear_btn_style = clear_button##.style in
  clear_btn_style##.cursor := string "pointer";
  
  (* Add buttons to buttons container *)
  Dom.appendChild buttons minimize_button;
  Dom.appendChild buttons clear_button;
  
  (* Add title and buttons to header *)
  Dom.appendChild header title;
  Dom.appendChild header buttons;
  
  (* Add header to container *)
  Dom.appendChild container header;
  
  (* Add log content container *)
  let log_content = Dom_html.createDiv Dom_html.document in
  log_content##.id := string "texture-log-content";
  Dom.appendChild container log_content;
  
  container

(* Add this to your setup code *)
let setup_texture_debugging () =
  let container = setup_log_container () in
  refresh_log_display container

(* LocalStorage-based texture logging system *)
    
let start texture_url =
  debug "Starting globe animation with texture: %s" texture_url;
  
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
    attribute vec2 a_texcoord;
    
    uniform mat4 u_matrix;
    
    varying vec3 v_normal;
    varying vec2 v_texcoord;
    varying vec3 v_position;
    
    void main() {
      vec4 position = u_matrix * vec4(a_position, 1.0);
      gl_Position = position;
      
      v_normal = a_normal;
      v_texcoord = a_texcoord;
      v_position = a_position;
    }
  " in
  
  let frag_shader = string "
    precision mediump float;
    
    varying vec3 v_normal;
    varying vec2 v_texcoord;
    varying vec3 v_position;
    
    uniform vec3 u_lightDir;
    uniform sampler2D u_texture;
    uniform bool u_useTexture;
    
    void main() {
      // Normalize the normal
      vec3 normal = normalize(v_normal);
      
      // Dot product with light direction gives diffuse lighting
      float light = max(dot(normal, u_lightDir), 0.0);
      
      vec3 color;
      
      if (u_useTexture) {
        // Use texture for the base color
        vec3 textureColor = texture2D(u_texture, v_texcoord).rgb;
        color = textureColor * (0.3 + 0.7 * light); // Apply lighting with ambient component
      } else {
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
        color = baseColor * (0.3 + 0.7 * light);
      }
      
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
  let texture_loc = gl##getUniformLocation prog (string "u_texture") in
  let use_texture_loc = gl##getUniformLocation prog (string "u_useTexture") in
  
  (* Set light direction *)
  let light_dir = float32array [| 0.5; 0.7; 1.0 |] in
  gl##uniform3fv_typed light_dir_loc light_dir;
  
  (* Load texture *)
  let texture = load_texture gl texture_url in
  
  (* Generate sphere *)
  debug "Generating sphere...";
  let (positions, normals, texcoords, indices) = generate_sphere 0.7 32 in
  
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
  
  (* Set up texture coordinate attribute *)
  let texcoord_attr = gl##getAttribLocation prog (string "a_texcoord") in
  gl##enableVertexAttribArray texcoord_attr;
  let texcoord_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ texcoord_buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ texcoords gl##._STATIC_DRAW_;
  gl##vertexAttribPointer texcoord_attr 2 gl##._FLOAT _false 0 0;
  
  (* Set up index buffer *)
  let index_buffer = gl##createBuffer in
  gl##bindBuffer gl##._ELEMENT_ARRAY_BUFFER_ index_buffer;
  gl##bufferData gl##._ELEMENT_ARRAY_BUFFER_ indices gl##._STATIC_DRAW_;
  
  (* Animation state *)
  let get_time () = Js.to_float (new%js date_now)##getTime in
  let last_draw = ref (get_time ()) in
  let draw_times = Queue.create () in
  let use_texture = ref true in  (* Flag to toggle texture *)
  
  (* Set up toggle button for texture *)
  Opt.iter
    (Dom_html.document##getElementById (string "toggle-texture"))
    (fun button ->
      button##.onclick := Dom_html.handler (fun _ ->
        use_texture := not !use_texture;
        debug "Texture %s" (if !use_texture then "enabled" else "disabled");
        Js._false
      )
    );
  
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
    
    (* Set texture uniforms *)
    gl##activeTexture gl##._TEXTURE0;
    gl##bindTexture gl##._TEXTURE_2D_ texture;
    gl##uniform1i texture_loc 0;
    gl##uniform1i use_texture_loc (if !use_texture then 1 else 0);
    
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
  debug "Initializing webgldemo";
  try
    setup_texture_debugging ();

    (* Choose which mode to start in *)
    let globe_mode = "celestial" in (* or "standard" *)
    
    match globe_mode with
    | "standard" ->
        debug "Calling standard globe";
        Lwt.async (fun () -> start "http://localhost:9000/earth.jpg")
    | "celestial" ->
        debug "Calling celestial globe";
        let texture_base_url = "http://localhost:9000/atlas" in
        debug "Using texture base URL: %s" texture_base_url;
        Lwt.async (fun () -> start_celestial_globe texture_base_url)
    | _ ->
        error "Unknown globe mode: %s" globe_mode
  with e ->
    error "Uncaught exception: %s" (Printexc.to_string e)
