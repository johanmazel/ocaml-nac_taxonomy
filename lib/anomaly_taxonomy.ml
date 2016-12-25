
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Sexplib.Std
open Bin_prot.Std

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
[@@deriving compare, sexp, bin_io]

type node_path = node_data list
[@@deriving compare, sexp, bin_io]

(* TODO: add path/level/matching mode string map *)
type t =
  {
    anomaly_signature_ptree : node_data Ptree.t;

    anomaly_subtree : node_data Ptree.t;
    normal_subtree : node_data Ptree.t;
    unknown_subtree : node_data Ptree.t;

    anomaly_signature_matching_mode_node_data_list_hashtable : (Anomaly_signature_matching_mode.t, node_data list) Hashtbl.t;
    anomaly_signature_level_hashtable : (Anomaly_signature.t, int) Hashtbl.t;
    paths : node_path list;
    path_hashtable : (Anomaly_signature.t, node_path) Hashtbl.t;
  }
[@@deriving sexp]

let new_t
    anomaly_signature_ptree

    anomaly_subtree
    normal_subtree
    unknown_subtree

    anomaly_signature_matching_mode_node_data_list_hashtable
    anomaly_signature_level_hashtable
    paths
    path_hashtable
  =
  {
    anomaly_signature_ptree;

    anomaly_subtree;
    normal_subtree;
    unknown_subtree;

    anomaly_signature_matching_mode_node_data_list_hashtable;
    anomaly_signature_level_hashtable;
    paths;
    path_hashtable;
  }

let iter
    f
    t
    =
  Ptree.iter
    f
    t.anomaly_signature_ptree

let fold
    f
    acc
    t
    =
  Ptree.fold
    f
    acc
    t.anomaly_signature_ptree

let fold_level
    f
    acc
    t
  =
  Ptree.fold_level
    f
    acc
    t.anomaly_signature_ptree

let to_string_signature_short_name_list
    t
    =
  let anomaly_signature_short_name_list =
    fold
      (fun
        (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature)
        anomaly_signature_ptree_list
        anomaly_signature_short_name_list
      ->        
        match anomaly_signature_type with
        | Anomaly_signature_type.Category ->
                anomaly_signature_short_name_list
        | Anomaly_signature_type.Standard ->
          (Anomaly_signature.to_short_name
             anomaly_signature)
          :: 
            anomaly_signature_short_name_list
        | Anomaly_signature_type.Default ->
          (Anomaly_signature.to_short_name
             anomaly_signature)
          :: 
            anomaly_signature_short_name_list
      )
      []
      t
  in

  Batteries.List.rev anomaly_signature_short_name_list

let generate_anomaly_signature_height_hashtable ptree =
  let anomaly_signature_height_hashtable =
    Ptree.fold
      (fun 
        (_, _, height, _, anomaly_signature)
        _ (* node_list *)
        hashtable_acc
      ->
        try
          (
            let height_found =
              Hashtbl.find
                hashtable_acc 
                anomaly_signature 
            in

            print_endline
              (sprintf
                 "Anomaly_taxonomy: generate_anomaly_signature_height_hashtable: possible duplicate anomaly in taxonomy (height %d and %d):\n%s"
                 height
                 height_found
                 (Anomaly_signature.to_string anomaly_signature)
              );

            assert(false);
          )
        with 
        | Not_found ->
          (
            Hashtbl.add
              hashtable_acc
              anomaly_signature
              height;

            hashtable_acc
          )
      )
      (Hashtbl.create 0)
      ptree
  in

  anomaly_signature_height_hashtable

let generate_subtrees ptree =
  debug "generate_subtrees: call";

  debug "generate_subtrees: building anomaly subtree";
  let anomaly_subtree =
    try
      Ptree.extract_subtree
        (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
           Anomaly_signature.to_string anomaly_signature      
        )
        (fun (event_type, _, _, _, anomaly_signature) ->
           (* Event_type.compare event_type Event_type.Anomaly = 0 *)
           anomaly_signature.Anomaly_signature.name = "anomaly"
        )
        ptree
    with
    | Ptree.Not_found ->
      Ptree.Empty
  in

  debug "generate_subtrees: building normal subtree";
  let normal_subtree =
    try
      Ptree.extract_subtree
        (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
           Anomaly_signature.to_string anomaly_signature      
        )
        (fun (event_type, _, _, _, anomaly_signature) ->
           (* Event_type.compare event_type Event_type.Normal = 0 *)
           anomaly_signature.Anomaly_signature.name = "normal"
        )
        ptree
    with
    | Ptree.Not_found ->
      Ptree.Empty
  in

  debug "generate_subtrees: building unknown subtree";
  let unknown_subtree =
    try
      Ptree.extract_subtree
        (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
           Anomaly_signature.to_string anomaly_signature      
        )
        (fun (event_type, _, _, _, anomaly_signature) ->
           (* Event_type.compare event_type Event_type.Normal = 0 *)
           anomaly_signature.Anomaly_signature.name = "unknown"
        )
        ptree
    with
    | Ptree.Not_found ->
      Ptree.Empty
  in

  debug "generate_subtrees: end";

  (anomaly_subtree, normal_subtree, unknown_subtree)

let of_ptrees
    ptree 
    anomaly_subtree
    normal_subtree
    unknown_subtree
    anomaly_signature_height_hashtable 
  =
  (
    (* let anomaly_signature_height_hashtable = *)
    (*   generate_anomaly_signature_height_hashtable *)
    (*         ptree *)
    (* in *)

    let paths = Ptree.paths ptree in

    let path_hashtable =
      List.fold_left
        (fun hashtable_acc node_list ->
           let (_, _, height, _, anomaly_signature) = List.hd (List.rev node_list) in

           (* try *)
           (*   ( *)
           (*     let height_found = *)
           (*         Hashtbl.find *)
           (*           hashtable_acc  *)
           (*           anomaly_signature  *)
           (*     in *)

           (*     print_endline *)
           (*         (sprintf *)
           (*            "Anomaly_taxonomy: of_ptree: possible duplicate anomaly in taxonomy (height %d and %d):\n%s" *)
           (*            height *)
           (*            height_found *)
           (*            (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
           (*         ); *)

           (*     assert(false); *)
           (*   ) *)
           (* with  *)
           (* | Not_found -> *)
           (*   ( *)
           (*     Hashtbl.add *)
           (*         hashtable_acc *)
           (*         anomaly_signature *)
           (*         node_list; *)

           (*     hashtable_acc *)
           (*   ) *)

           Hashtbl.add
             hashtable_acc
             anomaly_signature
             node_list;

           hashtable_acc
        )
        (Hashtbl.create 0)
        paths
    in

    let anomaly_signature_matching_mode_node_data_list_hashtable =
      Ptree.fold
        (fun
          (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature)
          _
          hashtable_acc
          ->
            let node_data = (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature) in

            try
              let node_data_list =
                Hashtbl.find
                  hashtable_acc
                  anomaly_signature_matching_mode
              in

              Hashtbl.replace
                hashtable_acc
                anomaly_signature_matching_mode
                (List.append node_data_list [ node_data ]);

              hashtable_acc
            with
            | Not_found ->
              Hashtbl.add
                hashtable_acc
                anomaly_signature_matching_mode
                [ node_data ];

              hashtable_acc
        )
        (Hashtbl.create 0)
        ptree
    in
    let anomaly_signature_matching_mode_node_data_list_hashtable =
      List.fold_left
        (fun hashtable_acc anomaly_signature_matching_mode ->
           try
             let _node_data_list_found =
               Hashtbl.find
                 hashtable_acc
                 anomaly_signature_matching_mode
             in
             hashtable_acc
           with
           | Not_found ->
             Hashtbl.add
               hashtable_acc
               anomaly_signature_matching_mode
               [];
             hashtable_acc
        )
        anomaly_signature_matching_mode_node_data_list_hashtable
        (Anomaly_signature_matching_mode.to_list ())
    in

    let t =
      new_t
        ptree

        anomaly_subtree
        normal_subtree
        unknown_subtree

        anomaly_signature_matching_mode_node_data_list_hashtable
        anomaly_signature_height_hashtable
        paths
        path_hashtable
    in

    t
  )

let extract_subset
    anomaly_name
    t
  =
  debug "extract_subset: call";

  let subtree =
    try
      Ptree.extract_subtree
        (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
           Anomaly_signature.to_string anomaly_signature      
        )
        (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> 
           String.compare anomaly_name anomaly_signature.Anomaly_signature.name = 0)
        t.anomaly_signature_ptree
    with
    | Ptree.Not_found ->
      print_endline
        (sprintf
           "Anomaly_taxonomy: extract_subset: could not find tree for %s"
           anomaly_name
        );
      assert(false)
  in

  debug "extract_subset: end";

  let anomaly_signature_height_hashtable =
    generate_anomaly_signature_height_hashtable
      subtree
  in

  (* let paths = Ptree.paths subtree in *)

  let anomaly_subtree, normal_subtree, unknown_subtree =
    generate_subtrees
      subtree
  in

  debug "extract_subset: end";

  of_ptrees
    subtree

    anomaly_subtree
    normal_subtree
    unknown_subtree

    anomaly_signature_height_hashtable

let of_ptree
    ptree
    hashtable
  =
  (
    let anomaly_subtree, normal_subtree, unknown_subtree =
      generate_subtrees
        ptree
    in

    of_ptrees
      ptree
      anomaly_subtree
      normal_subtree
      unknown_subtree
      hashtable
  )

let generate_height_hashtable
    ptree
  =
  (
    let hashtable =
      Ptree.fold
        (fun 
          (event_type, anomaly_signature_type, height, matching_mode, anomaly_signature)
          _
          hashtable_acc
          ->
            try
              (
                let height_found =
                  Hashtbl.find
                    hashtable_acc
                    anomaly_signature 
                in

                print_endline
                  (sprintf
                     "Anomaly_taxonomy: of_string_tuple_ptree: possible duplicate anomaly in taxonomy (height %d and %d):\n%s"
                     height
                     height_found
                     (Anomaly_signature.to_string anomaly_signature)
                  );

                assert(false);
              )
            with 
            | Not_found ->
              (
                Hashtbl.add
                  hashtable_acc
                  anomaly_signature
                  height;

                hashtable_acc
              )
        )
        (Hashtbl.create 0)
        ptree
    in

    hashtable
  )

let generate_height_ptree
    ptree
  =
  (
    let ptree =
      Ptree.map
        (fun (event_type, anomaly_signature_type, matching_mode, anomaly_signature) ->
           let height_option =
             try
               Ptree.node_height
                 (fun (_, _, _, anomaly_signature_to_test) ->
                    (Anomaly_signature.to_string anomaly_signature_to_test)
                 )
                 (fun (_, _, _, anomaly_signature_to_test) ->
                    let result =
                      Anomaly_signature.compare
                        anomaly_signature_to_test
                        anomaly_signature
                      =
                      0
                    in

                    (* print_endline *)
                    (*   (sprintf  *)
                    (*      "Anomaly_taxonomy: of_string_tuple_ptree: %b:\n%s\n%s" *)
                    (*      result *)
                    (*      (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
                    (*      (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature_to_test) *)
                    (*   ); *)

                    result
                 )
                 ptree
             with
             | Ptree.Several_node_found (node_1_string, node_2_string) ->
               (
                 print_endline
                   (sprintf
                      "Anomaly_taxonomy: of_string_tuple_ptree: several_node_found for:\n%s\n\n%s\n%s"
                      (Anomaly_signature.to_string anomaly_signature)
                      node_1_string
                      node_2_string
                   );

                 assert(false)
               )
           in

           let height =
             match height_option with
             | None -> 
               ( 
                 print_endline
                   (sprintf 
                      "Anomaly_taxonomy: of_string_tuple_ptree: could not find height for:\n%s"
                      (Anomaly_signature.to_string anomaly_signature)
                   );
                 assert(false)
               )
             | Some height -> height
           in

           (* ignore( *)
           (*   try *)
           (*     ( *)
           (*         let height_found = *)
           (*           Hashtbl.find *)
           (*             hashtable *)
           (*             anomaly_signature  *)
           (*         in *)

           (*         print_endline *)
           (*           (sprintf *)
           (*              "Anomaly_taxonomy: of_string_tuple_ptree: possible duplicate anomaly in taxonomy (height %d and %d):\n%s" *)
           (*              height *)
           (*              height_found *)
           (*              (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
           (*           ); *)

           (*         assert(false); *)
           (*     ) *)
           (*   with  *)
           (*   | Not_found -> *)
           (*     ( *)
           (*         Hashtbl.add *)
           (*           hashtable *)
           (*           anomaly_signature *)
           (*           height; *)
           (*     ) *)
           (* ); *)

           (
             event_type
             ,
             anomaly_signature_type
             ,
             height
             , 
             matching_mode
             ,
             anomaly_signature
           )
        )
        ptree
    in

    ptree
  )

let of_string_tuple_ptree
    global_feature_container
    global_metric_container
    taxonomy_filepath
  =
  (
    debug "of_string_tuple_ptree: call";    

    let string_triple_ptree = Parser_instantiations.Ptree_parser_utils.parse taxonomy_filepath in

    debug
      "of_string_tuple_ptree: taxonomy size: %d"
      (Ptree.size string_triple_ptree)
    ;    

    let taxonomy_path = Filename.dirname taxonomy_filepath in

    let local_filepath_list = Unix_utils.walk_directory_rec taxonomy_path  ".*" in

    (* debug *)
    (*   "of_string_tuple_ptree: local_filepath_list:\n%s" *)
    (*   (List_ext.to_string *)
    (*          ~sep: "\n" *)
    (*          (fun string -> string) *)
    (*          local_filepath_list *)
    (*   ); *)

    let anomaly_name_path_tuple_list =
      List.map
        (fun local_filepath ->
           let anomaly_name = Filename.basename local_filepath in
           (* let local_path = Filename.dirname local_filepath in *)

           (anomaly_name, local_filepath)
        )
        local_filepath_list
    in

    let anomaly_name_path_string_map =
      String_map.of_list
        anomaly_name_path_tuple_list
    in

    (* debug *)
    (*   "of_string_tuple_ptree: string_map:\n%s" *)
    (*   (String_map.to_string *)
    (*          ~sep: "\n" *)
    (*          (fun anomaly_name path -> sprintf "%s: %s" anomaly_name path) *)
    (*          anomaly_name_path_string_map *)
    (*   ); *)

    (* String_graph.string_dot_output                      *)
    (*         graph                                             *)
    (*         "taxonomy_graph.dot";                             *)

    (* let anomaly_signature_path = Filename.dirname taxonomy_filepath in *)

    (* TODO: check if anomaly signature are actually consistent across nodes (father and children) *)

    debug "of_string_tuple_ptree: checking files";    

    (* Ptree.iter *)
    (*   (fun (meta, matching_mode, label) -> *)
    (*         let file_path = anomaly_signature_path ^ "/" ^ label in *)
    (*         if Sys.file_exists file_path = false then *)
    (*           ( *)
    (*             print_endline *)
    (*               (sprintf *)
    (*                  "Anomaly_taxonomy: of_string_ptree: cannot find file: %s" *)
    (*                  file_path *)
    (*               ); *)

    (*             raise Anomaly_signature_file_not_found *)
    (*           ) *)
    (*   ) *)
    (*   string_triple_ptree; *)

    (* debug "of_string_tuple_ptree: building ptree";   *)

    (* let anomaly_signature_ptree = *)
    (*   Ptree.map *)
    (*         (fun (meta, matching_mode, label) -> *)
    (*           let file_path = anomaly_signature_path ^ "/" ^ label in *)

    (*           ( *)
    (*             Anomaly_signature_type.of_string meta *)
    (*               , *)
    (*             0 *)
    (*               , *)
    (*             Anomaly_signature_matching_mode.of_string matching_mode *)
    (*               , *)
    (*             Anomaly_signature.of_filename *)
    (*               global_feature_container *)
    (*               global_metric_container *)
    (*               file_path *)
    (*           ) *)
    (*         ) *)
    (*         string_triple_ptree *)
    (* in *)

    (* TODO: DO THIS *)

    let anomaly_signature_type_matching_mode_anomaly_signature_tuple_ptree =
      Ptree.map
        (fun (event_type, meta, matching_mode, label) ->

           if String_map.mem label anomaly_name_path_string_map = false then
             (
               print_endline
                 (sprintf
                    "[Anomaly_taxonomy]: of_string_tuple_ptree: can not find anomaly signature for file for %s among:\n%s"
                    label
                    (String_map.to_string
                       ~sep: "\n"
                       (fun anomaly_name path -> sprintf "%s: %s" anomaly_name path)
                       anomaly_name_path_string_map
                    )
                 );
               assert(false);
             );

           (* let file_path = anomaly_signature_path ^ "/" ^ label in *)
           let file_path = String_map.find label anomaly_name_path_string_map in

           (* debug *)
           (*   "of_string_tuple_ptree: parsing %s from: %s" *)
           (*   label *)
           (*   file_path *)
           (* ; *)

           let anomaly_signature_type=
             Anomaly_signature_type.of_string
               meta
           in

           let anomaly_signature_matching_mode =
             Anomaly_signature_matching_mode.of_string 
               matching_mode
           in
           let anomaly_signature =
             Anomaly_signature.of_filename
               global_feature_container
               global_metric_container
               file_path
           in

           let empty_signature = 
             Anomaly_signature.is_empty
               anomaly_signature
           in

           let anomaly_signature_matching_mode_coherent_with_anomaly_signature =
             match anomaly_signature_type with
             | Anomaly_signature_type.Category -> empty_signature
             | Anomaly_signature_type.Standard -> if empty_signature then false else true
             | Anomaly_signature_type.Default -> if empty_signature then false else true
           in

           if anomaly_signature_matching_mode_coherent_with_anomaly_signature = false then
             (
               print_endline
                 (sprintf
                    "Anomaly_taxonomy: of_string_tuple_ptree: incoherence between anomaly_signature_type (%s) and signature (empty: %b):\n%s"
                    (Anomaly_signature_type.to_string anomaly_signature_type)
                    empty_signature
                    (Anomaly_signature.to_string anomaly_signature)
                 );
               assert(false);
             );

           (
             Event_type.of_string event_type
             ,
             anomaly_signature_type
             ,
             anomaly_signature_matching_mode
             ,
             anomaly_signature
           )
        )
        string_triple_ptree
    in

    let ptree =
      generate_height_ptree
        anomaly_signature_type_matching_mode_anomaly_signature_tuple_ptree
    in
    let hashtable = generate_height_hashtable ptree in

    debug "of_string_tuple_ptree: end";  

    let t =
      of_ptree
        ptree
        hashtable
    in

    t
  )

let find_biggest_level
    anomaly_string
    network_attributes
    anomaly_signature_type_level_anomaly_signature_fourple_list
    =
  let fourple_list_sorted =
    List.sort
      (fun (_, _, level_1, _, _) (_, _, level_2, _, _) -> compare level_1 level_2)
      anomaly_signature_type_level_anomaly_signature_fourple_list
  in

  let rev_list = List.rev fourple_list_sorted in

  let (event_type_1, anomaly_signature_type_1, level_1, anomaly_signature_matching_mode_1, anomaly_signature_1) = List.hd rev_list in

  if List.length anomaly_signature_type_level_anomaly_signature_fourple_list > 1 then
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

    (* ptree *)
    t
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
        (*         if anomaly_signature_matching_mode_to_compare = anomaly_signature_matching_mode then *)
        (*           (   *)
        (*             let result = *)
        (*               Network_attribute_signature_comparator.compare *)
        (*                 network_attributes *)
        (*                 anomaly_signature *)
        (*             in *)

        (*             if result then *)
        (*               let node_data = event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature in *)

        (*               try *)
        (*                 ( *)
        (*                   let (_, _, height_found, _,anomaly_signature_found) = *)
        (*                     Reverse_int_map.find *)
        (*                       height *)
        (*                       int_map_acc *)
        (*                   in *)

        (*                   print_endline *)
        (*                     (sprintf *)
        (*                        "Anomaly_taxonomy: match_network_attributes: current is matching: %d - %s" *)
        (*                        height *)
        (*                        (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
        (*                     ); *)

        (*                   print_endline *)
        (*                     (sprintf *)
        (*                        "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height in map:\n%d - %s\n\n%s" *)
        (*                           height_found *)
        (*                        (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
        (*                        (Reverse_int_map.to_string *)
        (*                           ~sep: "\n" *)
        (*                           ~display_key: false *)
        (*                           (fun (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) ->  *)
        (*                             sprintf "%d - %s" level (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
        (*                           ) *)
        (*                           int_map_acc *)
        (*                        ) *)
        (* ); *)
        (*                   assert(false); *)
        (*                 ) *)
        (*               with *)
        (*               | Not_found -> *)
        (*                 ( *)
        (*                   Reverse_int_map.add *)
        (*                     height *)
        (*                     node_data *)
        (*                     int_map_acc *)
        (*                 ) *)
        (*             else *)
        (*               int_map_acc *)
        (*           ) *)
        (*         else *)
        (*           int_map_acc *)
        (*       )                 *)
        (*     ) *)
        (*     Reverse_int_map.empty *)
        (*     ptree *)
        (* in *)

        let node_data_list =
          Hashtbl.find
            t.anomaly_signature_matching_mode_node_data_list_hashtable
            anomaly_signature_matching_mode_to_compare
        in

        let reverse_int_map =
          List.fold_left
            (fun
              rev_int_map_acc
              (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature)
              ->
                (
                  if anomaly_signature_matching_mode_to_compare = anomaly_signature_matching_mode then
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
                                 "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height (%d) in map:\nOther signature for same height:\n%s\n\nInt_map:\n%s"

                                 height_found

                                 (Anomaly_signature.to_string anomaly_signature_found)

                                 (Reverse_int_map.to_string
                                    ~sep: "\n"
                                    (* ~display_key: false *)
                                    (fun _ (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature) ->
                                       sprintf "%d - %s" height (Anomaly_signature.to_string anomaly_signature)
                                    )
                                    rev_int_map_acc
                                 )
                              );

                            (* print_endline *)
                            (*   (sprintf *)
                            (*      "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height in map:\n%d - %s\n\n%s" *)
                            (*      height_found *)
                            (*      (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
                            (*      (Reverse_int_map.to_string *)
                            (*           ~sep: "\n" *)
                            (* (\* ~display_key: false *\) *)
                            (*           (fun _ (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, anomaly_signature) -> *)
                            (*             sprintf "%d - %s" level (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
                            (*           ) *)
                            (*           rev_int_map_acc *)
                            (*      ) *)
                            (* ); *)
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
              rev_int_map_acc
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
                    (*         (sprintf *)
                    (*            "Anomaly_taxonomy: match_network_attributes: matched signature : %d - %s" *)
                    (*            height *)
                    (*            (Anomaly_signature.to_string To_string_mode.Normal anomaly_signature) *)
                    (*         ); *)

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
                             "Anomaly_taxonomy: match_network_attributes: another anomaly signature found for this height (%d) in map:\nLeaves of found:\n%s\nOther signature for same height:\n%s\nInt_map:\n%s\n\nNetwork_attributes:\n%s"

                             height_found

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

                             (Anomaly_signature.to_string anomaly_signature_found)

                             (Reverse_int_map.to_string
                                ~sep: "\n"
                                (* ~display_key: false *)
                                (fun _ (event_type, anomaly_signature_type, height, anomaly_signature_matching_mode, anomaly_signature) ->
                                   sprintf "%d - %s" height (Anomaly_signature.to_string anomaly_signature)
                                )
                                rev_int_map_acc
                             )

                             (Network_traffic_attributes.to_string network_traffic_attributes)
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
                          rev_int_map_acc
                      )
                  else
                    rev_int_map_acc
                )
            )
            Reverse_int_map.empty
            t.anomaly_signature_ptree
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
    let (bigghest_height, (_, _, biggest_height,_, highest_anomaly_signature)) = Reverse_int_map.min_binding node_data_list in

    let other_node_data_list = Reverse_int_map.remove bigghest_height node_data_list in

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
