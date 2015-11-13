
open Printf



module A = BatArray
module L = BatList

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Taxonomy_verifier]: %s@." s)
      else
  ignore
    )
    fmt

let find_closest_strings_in_string_list
    string_list
    string
  =
  let first_string = List.hd string_list in
  let first_string_distance = 
    String_distance.levenshtein_distance 
      first_string 
      string
  in

  let (overall_closest_string, overall_closest_distance) =
    List.fold_left
      (fun (closest_string_list, closest_distance) string_to_test ->
         let current_distance =
           String_distance.levenshtein_distance
             string_to_test
             string
         in

         if current_distance > closest_distance then
           (closest_string_list, closest_distance)
         else
           (
             if current_distance < closest_distance then
               ([ string_to_test ], current_distance)
             else
               (List.append [ string_to_test ] closest_string_list, current_distance)
           )
      )
      ([ first_string ], first_string_distance)
      (List.tl string_list)
  in

  overall_closest_string

let check_feature
    feature_name_list
    anomaly_signature_name
    
    feature_name_container
    feature
  =
  (
    let feature_indice = feature.Feature.indice in
    let feature_name = feature.Feature.name in

    let result =
      try
        (
          let short_name_found =
            Feature_name_container.find_name
              feature_name_container
              feature_indice
          in

          if String.compare short_name_found feature_name = 0 then
            true
          else
            (
              (* TODO: add suggestion of name by string distance *)
              print_endline
                (sprintf 
                   "verify_taxonomy: in %s unconsistent feature name for indice %d %s ; reference name is %s"
                   anomaly_signature_name
                   feature_indice
                   feature_name
                   short_name_found
                );

              let closest_string_list =
                find_closest_strings_in_string_list
                  feature_name_list
                  feature_name
              in

              let closest_string_indice_tuple_list =
                List.map
                  (fun name ->
                     (* (name, String_map.find name feature_name_indice_string_map) *)
                     (name,
                      Feature_name_container.find_indice
                        feature_name_container
                        name
                     )
                  )
                  closest_string_list
              in

              print_endline
                (sprintf 
                   "verify_taxonomy: did you mean %s ?"
                   (List_ext.to_string
                      (fun (name, indice) -> sprintf "%d %s" indice name)
                      closest_string_indice_tuple_list)
                );

              assert(List.length closest_string_indice_tuple_list > 0);

              let closest_string_indice_tuple = 
                List.hd
                  closest_string_indice_tuple_list
              in

              print_endline
                (sprintf 
                   "verify_taxonomy: sed command suggested:\nsed -i 's/%d %s/%d %s/g' $1/*"
                   feature_indice
                   feature_name
                   (snd closest_string_indice_tuple)
                   (fst closest_string_indice_tuple)
                );

              false
            )
        )
      with
      | Not_found ->
        (
          print_endline 
            (sprintf
               "verify_taxonomy: in %s could not find feature for indice %d ; name is %s" 
               anomaly_signature_name
               feature_indice
               feature_name
            );
          let closest_string_list =
            find_closest_strings_in_string_list
              feature_name_list
              feature_name
          in

          let closest_string_indice_tuple_list =
            List.map
              (fun name ->
                 (name,
                  (* String_map.find name feature_name_indice_string_map *)
                  Feature_name_container.find_indice
                    feature_name_container
                    name
                 )
              )
              closest_string_list
          in

          print_endline
            (sprintf 
               "verify_taxonomy: did you mean %s ?"
               (List_ext.to_string
                  (fun (name, indice) -> sprintf "%d %s" indice name)
                  closest_string_indice_tuple_list)
            );

          assert(List.length closest_string_indice_tuple_list > 0);

          let closest_string_indice_tuple = 
            List.hd
              closest_string_indice_tuple_list
          in

          print_endline
            (sprintf 
               "verify_taxonomy: sed command suggested:\nsed -i 's/%d %s/%d %s/g' $1/*"
               feature_indice
               feature_name
               (snd closest_string_indice_tuple)
               (fst closest_string_indice_tuple)
            );

          false
        )
    in

    result
  )

let verify_taxonomy
    anomaly_taxonomy
  =
  (
    debug "verify_taxonomy: call";

    debug "verify_taxonomy: data structures for feature names and indices";

    (* let feature_indice_name_tuple_array = Network_traffic_attributes.to_indice_name_tuple_array () in *)
    (* let feature_indice_name_int_map = *)
    (*   Array.fold_left *)
    (*     (fun acc (indice, short_name) -> *)
    (*        Int_map.add *)
    (*          indice *)
    (*          short_name *)
    (*          acc *)
    (*     ) *)
    (*     Int_map.empty *)
    (*     feature_indice_name_tuple_array *)
    (* in *)
    (* let feature_name_indice_string_map = *)
    (*   Array.fold_left *)
    (*     (fun acc (indice, short_name) -> *)
    (*        String_map.add *)
    (*          short_name *)
    (*          indice *)
    (*          acc *)
    (*     ) *)
    (*     String_map.empty *)
    (*     feature_indice_name_tuple_array *)
    (* in *)
    (* let feature_name_list = *)
    (*   Array.to_list *)
    (*     (Array.map *)
    (*        snd *)
    (*        feature_indice_name_tuple_array) *)
    (* in *)

    let attribute_indice_name_tuple_array =
      Network_traffic_attributes.to_indice_name_tuple_array
        ()
    in
    let attribute_name_list =
      A.to_list
        (A.map
           snd
           attribute_indice_name_tuple_array
        )
    in
    let attribute_feature_name_container =
      Feature_name_container.of_indice_name_tuple_array
        attribute_indice_name_tuple_array
    in

    (* let metric_indice_name_tuple_array = *)
    (*   Network_traffic_values.to_indice_name_tuple_array *)
    (*     () *)
    (* in *)
    (* let metric_indice_name_int_map = *)
    (*   Array.fold_left *)
    (*     (fun acc (indice, short_name) -> *)
    (*        Int_map.add *)
    (*          indice *)
    (*          short_name *)
    (*          acc *)
    (*     ) *)
    (*     Int_map.empty *)
    (*     metric_indice_name_tuple_array *)
    (* in *)
    let value_indice_name_tuple_array =
      Network_traffic_values.to_indice_name_tuple_array
        ()
    in
    let _value_name_list =
      A.to_list
        (A.map
           snd
           value_indice_name_tuple_array
        )
    in
    let value_feature_name_container =
      Feature_name_container.of_indice_name_tuple_array
        value_indice_name_tuple_array
    in

    debug "verify_taxonomy: checking";

    let indice_check =
      Anomaly_taxonomy.fold
        (fun 
          (event_type, anomaly_signature_type, level, anomaly_signature_matching_mode, (anomaly_signature : Anomaly_signature.t))
          anomaly_signature_ptree_list_
          acc
          ->
            (* debug *)
            (*   "verify_taxonomy: signature:\n%s" *)
            (*   (Anomaly_signature.to_string To_string_mode.Normal anomaly_taxonomy) *)
            (* ; *)

            let anomaly_signature_valid =
              match anomaly_signature_type with
              | Anomaly_signature_type.Category ->
                true
              | Anomaly_signature_type.Standard ->
                (
                  Anomaly_signature.fold_wo_acc
                    (fun rule ->
                       match rule with
                       | Rule.Attribute attribute_rule ->
                         let feature = attribute_rule.Attribute_rule.feature in

                         check_feature
                           attribute_name_list 
                           anomaly_signature.Anomaly_signature.name
                           attribute_feature_name_container
                           feature
                       | Rule.Attribute_arithmetic_expression feature_arithmetic_expression_rule ->
                         (
                           Attribute_arithmetic_expression_rule.fold
                             (fun feature ->
                                check_feature
                                  attribute_name_list 
                                  anomaly_signature.Anomaly_signature.name
                                  attribute_feature_name_container
                                  feature
                             )
                             (fun _ b1 b2 -> b1 && b2)
                             feature_arithmetic_expression_rule
                         )
                       (* TODO: add use of check_feature *)
                       | Rule.Value metric_rule ->
                         let feature = metric_rule.Value_rule.feature in

                         let feature_indice = feature.Feature.indice in
                         let feature_name = feature.Feature.name in

                         let result =
                           try
                             (
                               let short_name_found =
                                 Feature_name_container.find_name
                                   value_feature_name_container
                                   feature_indice
                               in

                               if String.compare short_name_found feature_name = 0 then
                                 true
                               else
                                 (
                                   (* TODO: add suggestion of name by string distance *)
                                   print_endline
                                     (sprintf 
                                        "verify_taxonomy: in %s unconsistent metric name for indice %d: rule (%s) ref (%s)"
                                        anomaly_signature.Anomaly_signature.name
                                        feature_indice
                                        feature_name
                                        short_name_found
                                     );
                                   false
                                 )
                             )
                           with
                           | Not_found ->
                             print_endline 
                               (sprintf
                                  "verify_taxonomy: in %s could not find metric for indice %d"
                                  anomaly_signature.Anomaly_signature.name
                                  feature_indice
                               );
                             false
                         in

                         result
                    )
                    (fun boolean_operator bool_list ->
                       match List.length bool_list with
                       | 0 ->
                         true
                       | 1 ->
                         List.hd bool_list
                       | _ ->
                         List.fold_left
                           (fun acc bool ->
                              acc && bool
                           )
                           (List.hd bool_list)
                           (List.tl bool_list)      
                    )
                    anomaly_signature
                )
              | Anomaly_signature_type.Default ->
                true
            in

            anomaly_signature_valid && acc
        )
        true
        anomaly_taxonomy
    in

    debug "verify_taxonomy: call";

    indice_check
  )
