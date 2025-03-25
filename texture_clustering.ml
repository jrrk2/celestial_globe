(* texture_clustering.ml - New module for clustering celestial objects *)

open Messier_data

(* Cluster of celestial objects that are near each other *)
type object_cluster = {
  objects: messier_object list;
  center_ra: float;  (* Average RA of the cluster in degrees *)
  center_dec: float; (* Average DEC of the cluster in degrees *)
  radius: float;     (* Radius needed to encompass all objects in the cluster *)
  id: int;           (* Unique identifier for the cluster *)
}

(* Angular distance between two celestial coordinates in degrees *)
let angular_distance (ra1, dec1) (ra2, dec2) =
  (* Convert to radians *)
  let ra1_rad = ra1 *. Float.pi /. 180.0 in
  let dec1_rad = dec1 *. Float.pi /. 180.0 in
  let ra2_rad = ra2 *. Float.pi /. 180.0 in
  let dec2_rad = dec2 *. Float.pi /. 180.0 in
  
  (* Calculate the angular distance using the Haversine formula *)
  let d_ra = ra2_rad -. ra1_rad in
  let d_dec = dec2_rad -. dec1_rad in
  let a = (sin (d_dec /. 2.0)) ** 2.0 +. 
          cos dec1_rad *. cos dec2_rad *. (sin (d_ra /. 2.0)) ** 2.0 in
  let c = 2.0 *. atan2 (sqrt a) (sqrt (1.0 -. a)) in
  
  (* Convert back to degrees *)
  c *. 180.0 /. Float.pi

(* Check if an object can be added to a cluster within the maximum radius *)
let can_add_to_cluster cluster obj max_radius =
  (* RA needs special handling because it's circular (0-360 degrees) *)
  let ra_to_degrees ra_hours = ra_hours *. 15.0 in
  
  let obj_ra = ra_to_degrees obj.ra_hours in
  let obj_dec = obj.dec_degrees in
  
  List.fold_left (fun can_add cluster_obj ->
    if not can_add then false
    else
      let cluster_ra = ra_to_degrees cluster_obj.ra_hours in
      let cluster_dec = cluster_obj.dec_degrees in
      angular_distance (obj_ra, obj_dec) (cluster_ra, cluster_dec) <= max_radius
  ) true cluster.objects

(* Update a cluster's center and radius when adding a new object *)
let update_cluster_properties cluster obj =
  (* RA needs special handling because it's circular (0-360 degrees) *)
  let ra_to_degrees ra_hours = ra_hours *. 15.0 in
  
  let obj_ra = ra_to_degrees obj.ra_hours in
  let obj_dec = obj.dec_degrees in
  
  (* Calculate new center (average of all coordinates) *)
  let all_objects = obj :: cluster.objects in
  let all_ras = List.map (fun o -> ra_to_degrees o.ra_hours) all_objects in
  let all_decs = List.map (fun o -> o.dec_degrees) all_objects in
  
  (* Simple average for dec *)
  let avg_dec = List.fold_left (+.) 0.0 all_decs /. float_of_int (List.length all_decs) in
  
  (* For RA, we need to handle wrap-around at 0/360 degrees *)
  let avg_ra =
    let sin_sum = List.fold_left (fun acc ra -> acc +. sin (ra *. Float.pi /. 180.0)) 0.0 all_ras in
    let cos_sum = List.fold_left (fun acc ra -> acc +. cos (ra *. Float.pi /. 180.0)) 0.0 all_ras in
    let n = float_of_int (List.length all_ras) in
    atan2 (sin_sum /. n) (cos_sum /. n) *. 180.0 /. Float.pi in
  
  (* Ensure RA is in 0-360 range *)
  let avg_ra = if avg_ra < 0.0 then avg_ra +. 360.0 else avg_ra in
  
  (* Calculate new radius (maximum distance from center to any object) *)
  let radius = List.fold_left (fun max_dist o ->
    let o_ra = ra_to_degrees o.ra_hours in
    let o_dec = o.dec_degrees in
    let dist = angular_distance (avg_ra, avg_dec) (o_ra, o_dec) in
    max max_dist dist
  ) 0.0 all_objects in
  
  (* Add a small buffer to ensure all objects fit *)
  let radius_with_buffer = radius *. 1.2 in
  
  { cluster with 
    objects = all_objects;
    center_ra = avg_ra;
    center_dec = avg_dec;
    radius = radius_with_buffer 
  }

(* Generate clusters from a list of Messier objects *)
let cluster_objects objects max_distance max_objects_per_cluster =
  let rec build_clusters remaining_objects current_clusters next_cluster_id =
    match remaining_objects with
    | [] -> current_clusters
    | obj :: rest ->
        (* Try to add to an existing cluster *)
        let found_cluster, updated_clusters = 
          List.fold_left (fun (found, clusters) cluster ->
            if found then (true, cluster :: clusters)
            else if List.length cluster.objects < max_objects_per_cluster && 
                    can_add_to_cluster cluster obj max_distance then
              let updated_cluster = update_cluster_properties cluster obj in
              (true, updated_cluster :: clusters)
            else
              (found, cluster :: clusters)
          ) (false, []) current_clusters
        in
        
        if found_cluster then
          (* Continue with remaining objects *)
          build_clusters rest updated_clusters next_cluster_id
        else
          (* Create a new cluster with just this object *)
          let new_cluster = {
            objects = [obj];
            center_ra = obj.ra_hours *. 15.0;  (* Convert hours to degrees *)
            center_dec = obj.dec_degrees;
            radius = 0.5;  (* Initial small radius *)
            id = next_cluster_id
          } in
          build_clusters rest (new_cluster :: current_clusters) (next_cluster_id + 1)
  in
  
  build_clusters objects [] 1

(* Convert a cluster to a simplified representation for the rendering system *)
let cluster_to_render_info cluster =
  (* Count how many objects have images *)
  let imaged_count = List.fold_left (fun count obj ->
    if List.mem obj.name imaged then count + 1 else count
  ) 0 cluster.objects in
  
  (* Only return clusters that have at least one imaged object *)
  if imaged_count > 0 then
    Some (cluster.id, cluster.center_ra, cluster.center_dec, cluster.radius, 
         List.map (fun obj -> (obj.name, obj.id)) cluster.objects)
  else
    None

(* Main function to generate clusters from the Messier catalog *)
let generate_messier_clusters ?(max_distance=10.0) ?(max_per_cluster=4) () =
  let clusters = cluster_objects Messier_data.catalog max_distance max_per_cluster in
  (* Filter out clusters with no imaged objects and convert to render info *)
  List.filter_map cluster_to_render_info clusters
