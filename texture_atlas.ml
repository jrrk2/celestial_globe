(* texture_atlas.ml - Module for generating and managing texture atlases *)

type atlas_entry = {
  obj_id: int;          (* Messier object ID *)
  obj_name: string;     (* Messier object name (e.g., "M31") *)
  x_offset: float;      (* X position in atlas (0.0 - 1.0) *)
  y_offset: float;      (* Y position in atlas (0.0 - 1.0) *)
  width: float;         (* Width in atlas (0.0 - 1.0) *)
  height: float;        (* Height in atlas (0.0 - 1.0) *)
}

type texture_atlas = {
  cluster_id: int;             (* ID of the cluster this atlas represents *)
  center_ra: float;            (* RA of cluster center in degrees *)
  center_dec: float;           (* Dec of cluster center in degrees *)
  radius: float;               (* Radius of the cluster in degrees *)
  entries: atlas_entry list;   (* Entries within this atlas *)
  url: string;                 (* URL to the combined texture *)
}

(* Generate entries for a 2x2 texture atlas *)
let generate_2x2_atlas_entries objects =
  (* We'll organize objects in a 2x2 grid *)
  let positions = [
    (0.0, 0.0, 0.5, 0.5);   (* top-left: x, y, width, height *)
    (0.5, 0.0, 0.5, 0.5);   (* top-right *)
    (0.0, 0.5, 0.5, 0.5);   (* bottom-left *)
    (0.5, 0.5, 0.5, 0.5);   (* bottom-right *)
  ] in
  
  (* Pair objects with positions *)
  let rec pair_objects_with_positions objs positions entries =
    match objs, positions with
    | [], _ -> entries
    | _, [] -> entries (* No more positions available *)
    | (name, id) :: rest_objs, (x, y, w, h) :: rest_pos ->
        let entry = {
          obj_id = id;
          obj_name = name;
          x_offset = x;
          y_offset = y;
          width = w;
          height = h;
        } in
        pair_objects_with_positions rest_objs rest_pos (entry :: entries)
  in
  
  List.rev (pair_objects_with_positions objects positions [])

(* Generate a texture atlas for a cluster *)
let generate_atlas cluster_id center_ra center_dec radius objects base_url =
  (* Filter objects to only those that are in the imaged list *)
  let imaged_objects = List.filter (fun (name, _) -> 
    List.mem name Messier_data.imaged
  ) objects in
  
  (* Limit to first 4 (for 2x2 grid) *)
  let limited_objects = 
    if List.length imaged_objects > 4 then
      List.filteri (fun i _ -> i < 4) imaged_objects
    else
      imaged_objects
  in
  
  (* Generate the atlas entries *)
  let entries = generate_2x2_atlas_entries limited_objects in
  
  (* Create the URL for the combined texture *)
  (* In a real implementation, you'd need server-side image processing to 
     actually create these combined textures. For this example, we'll assume
     a URL pattern that indicates the cluster and contained objects. *)
  let objects_str = String.concat "_" (List.map (fun (name, _) -> name) limited_objects) in
  let url = Printf.sprintf "%s/cluster_%d_%s.jpg" base_url cluster_id objects_str in
  
  {
    cluster_id;
    center_ra;
    center_dec;
    radius;
    entries;
    url;
  }

(* Generate all texture atlases from messier clusters *)
let generate_atlases clusters base_url =
  List.map (fun (id, ra, dec, radius, objects) ->
    generate_atlas id ra dec radius objects base_url
  ) clusters
