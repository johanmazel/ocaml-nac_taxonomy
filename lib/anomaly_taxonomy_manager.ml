
open Printf

open Map_ext_instantiations

exception Anomaly_signature_file_not_found;;

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Anomaly_taxonomy]: %s@." s)
      else
  ignore
    )
    fmt

type node_data = 
  (Event_type.t 
   * Anomaly_signature_type.t
   * int 
   * Anomaly_signature_matching_mode.t
   * Anomaly_signature.t)
[@@deriving compare]

type node_path = node_data list
[@@deriving compare]

(* TODO: add path/level/matching mode string map *)
type t =
  {
    anomaly_taxonomy_all : Anomaly_taxonomy.t;

    anomaly_taxonomy : Anomaly_taxonomy.t;
    normal_taxonomy : Anomaly_taxonomy.t;
    unknown_taxonomy : Anomaly_taxonomy.t;

    path_hashtable : (Anomaly_signature.t, node_path) Hashtbl.t;
  }

let new_t
    anomaly_taxonomy_all

    anomaly_taxonomy
    normal_taxonomy
    unknown_taxonomy

    path_hashtable
    =
  {
    anomaly_taxonomy_all;

    anomaly_taxonomy;
    normal_taxonomy;
    unknown_taxonomy;

    path_hashtable;
  }

let of_anomaly_taxonomy
    anomaly_taxonomy
  =
  (
    let anomaly_subtree, normal_subtree, unknown_subtree =
      Anomaly_taxonomy.generate_subtrees
        anomaly_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree
    in

    let hashtable =
      Anomaly_taxonomy.generate_height_hashtable 
        anomaly_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree 
    in

    let anomaly_anomaly_taxonomy =
      Anomaly_taxonomy.of_ptree
        anomaly_subtree
        hashtable
    in
    let normal_taxonomy =
      Anomaly_taxonomy.of_ptree
        normal_subtree
        hashtable
    in
    let unknwon_taxonomy =
      Anomaly_taxonomy.of_ptree
        unknown_subtree
        hashtable
    in

    new_t
      anomaly_taxonomy

      anomaly_anomaly_taxonomy
      normal_taxonomy
      unknwon_taxonomy

      anomaly_taxonomy.Anomaly_taxonomy.path_hashtable
  )

let find_biggest_level
    anomaly_string
    network_attributes
    anomaly_taxonomy_node_data_list
  =
  let fourple_list_sorted =
    List.sort
      (fun (_, _, level_1, _, _) (_, _, level_2, _, _) -> compare level_1 level_2)
      anomaly_taxonomy_node_data_list
  in

  let rev_list = List.rev fourple_list_sorted in

  let (event_type_1, anomaly_signature_type_1, level_1, anomaly_signature_matching_mode_1, anomaly_signature_1) = List.hd rev_list in

  if List.length anomaly_taxonomy_node_data_list > 1 then
    let (event_type_2, anomaly_signature_type_2, level_2, anomaly_signature_matching_mode_2, anomaly_signature_2) = List.hd (List.tl rev_list) in

    if level_1 <= level_2 then
      (
        print_endline
          (sprintf
             "Anomaly_taxonomy: find_biggest_level: several signatures with the same level:\n%s\nfor:\n%s\n%s"
             (List_ext.to_string
                ~sep: "\n"
                (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) ->
                   sprintf
                     "%s %d %s"
                     (Anomaly_signature_type.to_string anomaly_signature_type)
                     level
                     anomaly_signature.Anomaly_signature.name
                )
                fourple_list_sorted
             )
             anomaly_string
             (Network_traffic_attributes.to_string network_attributes)
          );

        assert(false);
      );

    (event_type_1, anomaly_signature_type_1, level_1, anomaly_signature_matching_mode_1, anomaly_signature_1)
  else
    (event_type_1, anomaly_signature_type_1, level_1, anomaly_signature_matching_mode_1, anomaly_signature_1)

let match_network_attributes
    use_anomaly_signature_matching_mode
    anomaly_signature_matching_mode_to_compare
    network_traffic_attributes
    network_traffic_values
    anomaly_taxonomy
  =
  (* debug "match_network_attributes: call"; *)

  let reverse_int_map =
    if use_anomaly_signature_matching_mode then
      (
        (* let reverse_int_map = *)
        (*   Ptree.fold *)
        (*     (fun *)
        (*       (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature) *)
        (*       anomaly_signature_ptree_list *)
        (*       int_map_acc *)
        (*     -> *)
        (*       ( *)
        (*   if anomaly_signature_matching_mode_to_compare = anomaly_signature_matching_mode then *)
        (*     (   *)
        (*       let result = *)
        (*         Network_attribute_signature_comparator.compare *)
        (*     network_attributes *)
        (*     anomaly_signature *)
        (*       in *)

        (*       if result then *)
        (*         let node_data = event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature in *)

        (*         try *)
        (*     ( *)
        (*       let (_, _, height_found, _,anomaly_signature_found) = *)
        (*         Reverse_int_map.find *)
        (*           height *)
        (*           int_map_acc *)
        (*       in *)

        (*       print_endline *)
        (*         (sprintf *)
        (*            "Anomaly_taxonomy: match_network_attributes: current is matching: %d - %s" *)
        (*            height *)
        (*            (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
        (*         ); *)

        (*       print_endline *)
        (*         (sprintf *)
        (*            "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height in map:\n%d - %s\n\n%s" *)
        (*               height_found *)
        (*            (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
        (*            (Reverse_int_map.to_string *)
        (*         ~sep: "\n" *)
        (*         ~display_key: false *)
        (*         (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) ->  *)
        (*           sprintf "%d - %s" level (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
        (*         ) *)
        (*         int_map_acc *)
        (*            ) *)
        (*         ); *)
        (*       assert(false); *)
        (*     ) *)
        (*         with *)
        (*         | Not_found -> *)
        (*     ( *)
        (*       Reverse_int_map.add *)
        (*         height *)
        (*         node_data *)
        (*         int_map_acc *)
        (*     ) *)
        (*       else *)
        (*         int_map_acc *)
        (*     ) *)
        (*   else *)
        (*     int_map_acc *)
        (*       )     *)
        (*     ) *)
        (*     Reverse_int_map.empty *)
        (*     ptree *)
        (* in *)

        let node_data_list =
          Hashtbl.find
            anomaly_taxonomy.Anomaly_taxonomy.anomaly_signature_matching_mode_node_data_list_hashtable
            anomaly_signature_matching_mode_to_compare
        in

        let reverse_int_map =
          List.fold_left
            (fun
              rev_int_map_acc
              (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature)
              ->
                (
                  let result =
                    Network_attribute_signature_comparator.compare
                      network_traffic_attributes
                      network_traffic_values
                      anomaly_signature
                  in

                  if result then
                    let node_data = event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature in

                    try
                      (
                        let (_, _, height_found, _,anomaly_signature_found) =
                          Reverse_int_map.find
                            height
                            rev_int_map_acc
                        in

                        print_endline
                          (sprintf
                             "Anomaly_taxonomy: match_network_attributes: current is matching: %d - %s"
                             height
                             (Anomaly_signature.to_string anomaly_signature)
                          );

                        print_endline
                          (sprintf
                             "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height in map:\n%d - %s\n\n%s"
                             height_found
                             (Anomaly_signature.to_string anomaly_signature)
                             (Reverse_int_map.to_string
                                ~sep: "\n"
                                (* ~display_key: false *)
                                (fun _ (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) ->
                                   sprintf "%d - %s" level (Anomaly_signature.to_string anomaly_signature)
                                )
                                rev_int_map_acc
                             )
                          );
                        assert(false);
                      )
                    with
                    | Not_found ->
                      (
                        Reverse_int_map.add
                          height
                          node_data
                          rev_int_map_acc
                      )

                  else
                    rev_int_map_acc
                )
            )
            Reverse_int_map.empty
            node_data_list
        in

        reverse_int_map
      )
    else
      (
        let reverse_int_map =
          Ptree.fold
            (fun
              (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature)
              anomaly_signature_ptree_list
              int_map_acc
              ->
                (
                  let result =
                    Network_attribute_signature_comparator.compare
                      network_traffic_attributes
                      network_traffic_values
                      anomaly_signature
                  in

                  if result then
                    (* print_endline *)
                    (*   (sprintf *)
                    (*      "Anomaly_taxonomy: match_network_attributes: matched signature : %d - %s" *)
                    (*      height *)
                    (*      (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
                    (*   ); *)

                    try
                      (
                        let (_, _, height_found, _,anomaly_signature_found) =
                          Reverse_int_map.find
                            height
                            int_map_acc
                        in

                        print_endline
                          (sprintf
                             "Anomaly_taxonomy: match_network_attributes: current is matching: %d - %s"
                             height
                             (Anomaly_signature.to_string anomaly_signature)
                          );

                        print_endline
                          (sprintf
                             "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height in map:\n%d - %s\nLeaves:\n%s\n\nInt_map:\n%s"

                             height_found
                             (Anomaly_signature.to_string anomaly_signature_found)

                             (List_ext.to_string
                                ~sep: "\n"
                                (fun ptree ->
                                   match ptree with
                                   | Ptree.Empty -> "empty"
                                   | Ptree.Node ((event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature), list) ->
                                     sprintf "%d - %s" height (Anomaly_signature.to_string anomaly_signature)
                                )
                                anomaly_signature_ptree_list
                             )

                             (Reverse_int_map.to_string
                                ~sep: "\n"
                                (* ~display_key: false *)
                                (fun _ (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature) ->
                                   sprintf "%d - %s" height (Anomaly_signature.to_string anomaly_signature)
                                )
                                int_map_acc
                             )
                          );
                        assert(false);
                      )
                    with
                    | Not_found ->
                      (
                        let node_data = event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature in

                        Reverse_int_map.add
                          height
                          node_data
                          int_map_acc
                      )
                  else
                    int_map_acc
                )
            )
            Reverse_int_map.empty
            anomaly_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree
        in

        reverse_int_map
      )
  in

  let result =
    match Reverse_int_map.cardinal reverse_int_map with
    | 0 -> None
    | _ -> Some reverse_int_map
  in

  (* debug "match_network_attributes: end"; *)

  result

let verify_node_data_list
    node_data_list
    t
  =
  (
    (* let node_dada_list_sorted = *)
    (*   List.sort *)
    (*   (fun (_, _, height_1, _, _) (_, _, height_2, _, _) -> *)
    (*     compare height_1 height_2 *)
    (*   ) *)
    (*   node_dada_list *)
    (* in *)

    (* debug *)
    (*   "verify_ple_list: paths:\n%s" *)
    (*   (List_ext.to_string *)
    (*    ~sep: "\n\n" *)
    (*    (fun node_list -> *)
    (*      (List_ext.to_string *)
    (*         ~sep: "\n" *)
    (*         (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> *)
    (*     sprintf "%d - %s" level (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
    (*         ) *)
    (*         node_list *)
    (*      ) *)
    (*    ) *)
    (*    t.paths *)
    (*   ); *)

    (* let node_dada_list_sorted_rev = List.rev node_dada_list_sorted in *)

    let (bigghest_height, (_, _, biggest_height,_, highest_anomaly_signature)) = (Reverse_int_map.min_binding node_data_list) in
    (* let biggest_node_data = Int_map.find highest_height node_dada_list in *)

    let other_node_data_list = Reverse_int_map.remove bigghest_height node_data_list in

    (* let (_, _, biggest_height,_, highest_anomaly_signature) = highest_node_data in *)

    (* Verifying if height is consecutive *)
    let (_, height_is_consecutive) =
      Reverse_int_map.fold
        (fun height node (previous_node_height, bool_acc) ->
           (
             let previous_node_height_to_compare = previous_node_height - 1 in

             let (_, _, height,_, _) = node in

             let bool = previous_node_height_to_compare = height in

             (previous_node_height_to_compare, bool_acc && bool)
           )
        )
        other_node_data_list
        (biggest_height, true)
    in

    if height_is_consecutive = false then
      (
        print_endline
          (sprintf
             "Anomaly_taxonomy: verify_node_data_list: height is not consistent in node_data_list:\n%s"
             (Reverse_int_map.to_string
                ~sep: "\n"
                (* ~display_key: false *)
                (fun _ (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
                   sprintf "%d - %s" level (Anomaly_signature.to_string anomaly_signature)
                )
                node_data_list
             )
          );
        assert(false);
      );

    let highest_node_path =
      Hashtbl.find
        t.path_hashtable
        highest_anomaly_signature
    in

    let (_, path_consistent) =
      Reverse_int_map.fold
        (fun height other_node (previous_node_path, bool_acc) ->
           (
             let previous_node_path_without_last_node = 
               List.rev
                 (List.tl
                    (List.rev previous_node_path)
                 )
             in

             let (_, _, _,_, other_anomaly_signature) = other_node in
             let other_node_path =
               Hashtbl.find
                 t.path_hashtable
                 other_anomaly_signature
             in

             let compare_result =
               compare
                 previous_node_path_without_last_node
                 other_node_path
               =
               0
             in

             let bool = bool_acc && compare_result in

             (previous_node_path_without_last_node, bool)
           )
        )
        other_node_data_list
        (highest_node_path, true)
    in

    if path_consistent = false then
      (
        print_endline
          (sprintf
             "Anomaly_taxonomy: verify_node_data_list: path not consistent in node_data_list:\n%s"
             (Reverse_int_map.to_string
                ~sep: "\n"
                (* ~display_key: false *)
                (fun _ (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
                   sprintf "%d - %s" level (Anomaly_signature.to_string anomaly_signature)
                )
                node_data_list
             )
          );
        assert(false);
      );

    (* assert(List.length ple_list_sorted = 1); *)
  )

let classify
    t
    anomaly_string
    network_traffic_attributes
    network_traffic_values
  =
  debug "classify: call";

  assert(
    match t.anomaly_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree with
    | Ptree.Empty -> false
    | Ptree.Node _ -> true
  );

  debug "classify: searching in anomaly";
  let anomaly_int_map_option =
    Anomaly_taxonomy.match_network_attributes
      false
      Anomaly_signature_matching_mode.None
      network_traffic_attributes
      network_traffic_values
      t.anomaly_taxonomy
  in

  let node_dada_list =
    match anomaly_int_map_option with
    | Some anomaly_node_data_list ->
      (
        if Reverse_int_map.cardinal anomaly_node_data_list > 1 then
          (
            verify_node_data_list 
              anomaly_node_data_list
              t;
          );

        anomaly_node_data_list
      )
    | None ->
      (
        debug "classify: searching in normal";

        assert(
          match t.normal_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree with 
          | Ptree.Empty -> false
          | Ptree.Node _ -> true
        );

        let normal_node_data_list_option =
          Anomaly_taxonomy.match_network_attributes
            false
            Anomaly_signature_matching_mode.None
            network_traffic_attributes
            network_traffic_values
            t.normal_taxonomy
        in

        match normal_node_data_list_option with
        | Some normal_node_data_list ->
          (
            if Reverse_int_map.cardinal normal_node_data_list > 1 then
              (
                verify_node_data_list  
                  normal_node_data_list
                  t;
              );

            normal_node_data_list
          )
        | None ->
          (
            debug "classify: searching in unknown";

            assert(
              match t.unknown_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree with 
              | Ptree.Empty -> false
              | Ptree.Node _ -> true
            );

            let unknown_node_data_list_option =
              Anomaly_taxonomy.match_network_attributes
                false
                Anomaly_signature_matching_mode.None
                network_traffic_attributes
                network_traffic_values
                t.unknown_taxonomy
            in

            match unknown_node_data_list_option with
            | Some unknown_node_data_list ->
              unknown_node_data_list
            | None ->
              (
                failwith "Anomaly_taxonomy: classify_all: no match found in anomaly, normal or unknown";
              )
          )
      )
  in

  let event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature =
    find_biggest_level
      anomaly_string
      network_traffic_attributes
      (List.map snd (Reverse_int_map.bindings node_dada_list))
  in

  debug "classify: end";

  anomaly_signature

let classify_option
    t
    anomaly_string
    anomaly_signature_matching_mode_to_compare
    network_traffic_attributes
    network_traffic_values
  =
  debug "classify_option: call";

  assert(
    match t.anomaly_taxonomy.Anomaly_taxonomy.anomaly_signature_ptree with 
    | Ptree.Empty -> false
    | Ptree.Node _ -> true
  );

  debug "classify_option: searching in anomaly";
  let anomaly_node_data_list_option =
    Anomaly_taxonomy.match_network_attributes
      true
      anomaly_signature_matching_mode_to_compare
      network_traffic_attributes
      network_traffic_values
      t.anomaly_taxonomy
  in

  let anomaly_signature_option =
    match anomaly_node_data_list_option with
    | Some anomaly_node_data_list ->
      (
        if Reverse_int_map.cardinal anomaly_node_data_list > 1 then
          (
            verify_node_data_list 
              anomaly_node_data_list
              t;
          );

        let _, _, _, _, anomaly_signature =
          Anomaly_taxonomy.find_biggest_level
            anomaly_string
            network_traffic_attributes
            (* network_traffic_values *)
            (List.map snd (Reverse_int_map.bindings anomaly_node_data_list))
        in

        Some anomaly_signature
      )
    | None -> None
  in

  debug "classify_option: end";

  anomaly_signature_option
