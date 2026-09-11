st_network_standard_processsing = function(sf_data, input_directed){
  sf_data %>% 
    as_sfnetwork(directed = input_directed) %>%
    activate("edges") %>%
    mutate(weight = as.numeric(edge_length())) %>%
    mutate(index_edge = row_number()) %>%
    activate("nodes") %>%
    mutate(node_index = row_number()) %>% 
    morph(to_linegraph) %>%
    mutate(group_edges = group_components()) %>% 
    unmorph() %>% 
    activate("edges") %>%
    group_by(group_edges) %>% 
    mutate(group_edges_count = n()) %>% 
    # mutate(group_edges_pctmain = mean(str_detect(highway, "prim|secon|tertia|trunk|motor"))) %>% 
    mutate(group_edges_pctmain = sum(str_detect(highway, "prim|secon|tertia|trunk|motor")*weight)) %>% 
    mutate(node_pair = str_glue("{pmin(to, from)}_{pmax(to, from)}")) %>% 
    group_by(node_pair) %>% 
    mutate(node_pair_count = n()) %>%
    ungroup()
}