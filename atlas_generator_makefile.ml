(* atlas_generator_makefile.ml - Generate a Makefile for texture atlas creation *)

open Messier_data
open Texture_clustering
open Texture_atlas

(* Function to generate a Makefile for creating texture atlases *)
let generate_makefile source_dir output_dir base_url max_distance max_per_cluster =
  let oc = open_out "Makefile.atlas" in
  let temp_dir = "$(OUTPUT_DIR)/temp" in
  
  (* Helper to generate targets for each atlas *)
  let generate_atlas_target atlas =
    let objects_str = String.concat "_" (List.map (fun e -> e.obj_name) atlas.entries) in
    let output_filename = Printf.sprintf "cluster_%d_%s.jpg" atlas.cluster_id objects_str in
    let output_path = Filename.concat output_dir output_filename in
    
    (* Source files for dependencies *)
    let source_files = List.map (fun e -> 
      Filename.concat source_dir (Printf.sprintf "%s.jpg" e.obj_name)
    ) atlas.entries in
    
    (* Create the target rule *)
    Printf.fprintf oc "%s: %s\n" 
      output_path 
      (String.concat " " source_files);
    
    (* Generate temporary cropped files for each entry *)
    let temp_files = List.map (fun entry ->
      let obj = match Messier_data.find_by_id entry.obj_id with
        | Some obj -> obj
        | None -> failwith (Printf.sprintf "Could not find Messier object with ID %d" entry.obj_id)
      in
      
      (* Get size information from the messier data *)
      let (width_arcmin, height_arcmin) = obj.size_arcmin in
      let aspect_ratio = width_arcmin /. height_arcmin in
      
      (* Calculate output size maintaining aspect ratio *)
      let (width, height) = 
        if aspect_ratio >= 1.0 then
          (512, int_of_float (512.0 /. aspect_ratio))
        else
          (int_of_float (512.0 *. aspect_ratio), 512)
      in
      
      (* Create temporary file path *)
      let temp_file = Printf.sprintf "%s/%s_cropped.jpg" temp_dir entry.obj_name in
      
      (* Add rule to create the temporary file *)
      Printf.fprintf oc "%s: %s %s\n" 
        temp_file
        (Filename.concat source_dir (Printf.sprintf "%s.jpg" entry.obj_name))
        temp_dir;
      
      (* Add margin (10%% on each side) *)
      let margin_factor = 1.2 in  (* 20% total margin (10% on each side) *)
      
      Printf.fprintf oc "\t@echo \"Processing %s (%.1f x %.1f arcmin)\"\n" 
        entry.obj_name width_arcmin height_arcmin;
      
      (* Add preprocessing command - crop to proper aspect ratio with margin *)
      Printf.fprintf oc "\t@magick %s -gravity center -extent %dx%d \\\n" 
        (Filename.concat source_dir (Printf.sprintf "%s.jpg" entry.obj_name))
        (int_of_float (float_of_int width *. margin_factor))
        (int_of_float (float_of_int height *. margin_factor));
      
      (* Add border for visibility *)
      Printf.fprintf oc "\t\t-bordercolor black -border 2 \\\n";
      
      (* Resize to final size *)
      Printf.fprintf oc "\t\t-resize %dx%d %s\n\n"
        width height
        temp_file;
      
      temp_file
    ) atlas.entries in
    
    (* Make the output dependent on the temp files *)
    Printf.fprintf oc "%s: %s\n" 
      output_path
      (String.concat " " temp_files);
    
    (* Determine the montage command based on the number of entries *)
    let _ = match atlas.entries with
    | [e1] ->
        Printf.fprintf oc "\t@echo \"Creating atlas for cluster %d (%s)\"\n" 
          atlas.cluster_id e1.obj_name;
        Printf.fprintf oc "\t@magick %s -resize 512x512 %s\n"
          (List.hd temp_files)
          output_path
    | [e1; e2] ->
        Printf.fprintf oc "\t@echo \"Creating atlas for cluster %d (%s, %s)\"\n" 
          atlas.cluster_id e1.obj_name e2.obj_name;
        Printf.fprintf oc "\t@montage %s %s -tile 2x1 -geometry 512x512+0+0 -background black %s\n"
          (List.nth temp_files 0)
          (List.nth temp_files 1)
          output_path
    | [e1; e2; e3] ->
        Printf.fprintf oc "\t@echo \"Creating atlas for cluster %d (%s, %s, %s)\"\n" 
          atlas.cluster_id e1.obj_name e2.obj_name e3.obj_name;
        Printf.fprintf oc "\t@montage %s %s %s null: -tile 2x2 -geometry 512x512+0+0 -background black %s\n"
          (List.nth temp_files 0)
          (List.nth temp_files 1)
          (List.nth temp_files 2)
          output_path
    | [e1; e2; e3; e4] ->
        Printf.fprintf oc "\t@echo \"Creating atlas for cluster %d (%s, %s, %s, %s)\"\n" 
          atlas.cluster_id e1.obj_name e2.obj_name e3.obj_name e4.obj_name;
        Printf.fprintf oc "\t@montage %s %s %s %s -tile 2x2 -geometry 512x512+0+0 -background black %s\n"
          (List.nth temp_files 0)
          (List.nth temp_files 1)
          (List.nth temp_files 2)
          (List.nth temp_files 3)
          output_path
    | _ ->
        Printf.fprintf oc "\t@echo \"ERROR: Invalid number of entries for cluster %d\"\n" 
          atlas.cluster_id
    in
    
    (* Add the output path to the list of atlas paths *)
    output_path
  in
  
  (* Generate the cluster objects *)
  let objects_with_images = List.filter_map (fun obj ->
    if List.mem obj.name imaged then
      Some obj
    else
      None
  ) catalog in
  
  Printf.printf "Found %d Messier objects with images\n" (List.length objects_with_images);
  
  let clusters = cluster_objects objects_with_images max_distance max_per_cluster in
  Printf.printf "Generated %d clusters\n" (List.length clusters);
  
  (* Generate atlases *)
  let atlas_generator (cluster:object_cluster) =
    let objects = List.map (fun obj -> (obj.name, obj.id)) cluster.objects in
    generate_atlas cluster.id cluster.center_ra cluster.center_dec cluster.radius objects base_url
  in
  
  let atlases = List.map atlas_generator clusters in
  
  (* Write the Makefile header *)
  Printf.fprintf oc "# Makefile for generating Messier texture atlases\n";
  Printf.fprintf oc "# Generated on %s\n\n" (
    let tm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" 
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec
  );
  
  (* Output directories *)
  Printf.fprintf oc "OUTPUT_DIR = %s\n" output_dir;
  Printf.fprintf oc "SOURCE_DIR = %s\n\n" source_dir;

  Printf.fprintf oc "default: all\n\n";
  (* Create temporary directory target if it doesn't exist yet *)
  if not (List.exists (fun line -> String.trim line = temp_dir ^ ":") 
	 (String.split_on_char '\n' (Buffer.contents (Buffer.create 0)))) then
    Printf.fprintf oc "%s:\n\t@mkdir -p %s\n\n" temp_dir temp_dir;
      
  (* Create output directory targets *)
  Printf.fprintf oc "$(OUTPUT_DIR):\n";
  Printf.fprintf oc "\t@mkdir -p $(OUTPUT_DIR)\n\n";
  
  (* Generate the all target *)
  let atlas_paths = List.map generate_atlas_target atlases in
  Printf.fprintf oc ".PHONY: all clean\n\n";
  Printf.fprintf oc "all: $(OUTPUT_DIR) %s\n\n" (String.concat " " atlas_paths);
  
  (* Clean target *)
  Printf.fprintf oc "clean:\n";
  Printf.fprintf oc "\t@echo \"Cleaning atlas images\"\n";
  Printf.fprintf oc "\t@rm -f %s\n" (String.concat " " atlas_paths);
  Printf.fprintf oc "\t@echo \"Cleaning temporary files\"\n";
  Printf.fprintf oc "\t@rm -rf $(OUTPUT_DIR)/temp\n\n";
  
  (* Generate the updated messier_objects code *)
  Printf.fprintf oc "# Code to update messier_objects in webgldemo.ml:\n#\n";
  Printf.fprintf oc "# let messier_objects =\\\n";
  Printf.fprintf oc "   [\\\n";
  List.iteri (fun i atlas ->
   (* Generate the filename based on atlas entries *)
    let objects_str = String.concat "_" (List.map (fun e -> e.obj_name) atlas.entries) in
    let filename = Printf.sprintf "cluster_%d_%s.jpg" atlas.cluster_id objects_str in
    Printf.fprintf oc "     (* Cluster %d *)\\\n" atlas.cluster_id;
    Printf.fprintf oc "     (\"Cluster %d\", %.2f, %.2f, %.2f, %d, \"%s\")%s\\\n"
      atlas.cluster_id
      atlas.center_ra
      atlas.center_dec
      atlas.radius
      atlas.cluster_id
      filename
      (if i < List.length atlases - 1 then ";" else "")
  ) atlases;
  Printf.fprintf oc "   ]\n";
  
  close_out oc;
  Printf.printf "Makefile.atlas generated successfully\n"

(* Main function with cluster parameters *)
let () =
  let source_dir = ref "messier" in
  let output_dir = ref "atlas" in
  let max_distance = ref 40.0 in
  let max_per_cluster = ref 4 in
  let base_url = ref "" in
  
  let speclist = [
    ("--source-dir", Arg.Set_string source_dir, "Directory containing source Messier images");
    ("--output-dir", Arg.Set_string output_dir, "Directory for output atlas images");
    ("--max-distance", Arg.Set_float max_distance, "Maximum angular distance in degrees for clustering");
    ("--max-per-cluster", Arg.Set_int max_per_cluster, "Maximum objects per cluster");
    ("--base-url", Arg.Set_string base_url, "Base URL for texture atlases (default: same as output-dir)");
  ] in
  
  let usage_msg = "Usage: atlas_generator_makefile [options]" in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  
  (* If base_url is empty, use output_dir *)
  let actual_base_url = if !base_url = "" then !output_dir else !base_url in
  
  generate_makefile !source_dir !output_dir actual_base_url !max_distance !max_per_cluster
