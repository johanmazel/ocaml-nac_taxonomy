
let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Network_attribute_signature_comparator]: %s@." s)
      else
  ignore
    )
    fmt

let compare
    network_traffic_attributes
    network_traffic_values
    anomaly_signature
  =
  (
    let result =
      Anomaly_signature.fold_wo_acc
        (fun rule ->
           match rule with
           | Rule.Attribute attribute_rule ->
             let feature = attribute_rule.Attribute_rule.feature in

             let feature_indice = feature.Feature.indice in
             let attribute_value =
               Network_traffic_attributes.find_indice
                 network_traffic_attributes
                 feature_indice
             in

             let result =
               Attribute_rule.test_value
                 attribute_rule
                 attribute_value
             in

             result
           | Rule.Attribute_arithmetic_expression feature_arithmetic_expression_rule ->
             (
               let value =
                 Attribute_arithmetic_expression_rule.fold
                   (fun feature ->
                      let attribute_value =
                        Network_traffic_attributes.find_indice
                          network_traffic_attributes
                          feature.Feature.indice
                      in

                      attribute_value
                   )
                   (fun ao f1 f2 -> Arithmetic_operator.apply f1 f2 ao)
                   feature_arithmetic_expression_rule
               in


               let result =
                 Attribute_arithmetic_expression_rule.test_value
                   feature_arithmetic_expression_rule
                   value
               in

               result
             )
           | Rule.Value value_rule ->
             let feature = value_rule.Value_rule.feature in

             let feature_indice = feature.Feature.indice in
             let feature_value =
               Network_traffic_values.find_indice
                 network_traffic_values
                 feature_indice
             in

             let result =
               Value_rule.test_value
                 value_rule
                 feature_value
             in
             result
        )
        (fun boolean_operator bool_list ->
           match List.length bool_list with
           | 0 ->
             false
           | 1 ->
             List.hd bool_list
           | _ ->
             List.fold_left
               (fun acc bool ->
                  Boolean_operator.apply boolean_operator acc bool
               )
               (List.hd bool_list)
               (List.tl bool_list)

        )
        anomaly_signature
    in

    result
  )
