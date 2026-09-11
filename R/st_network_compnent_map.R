st_network_compnent_map = function(
    comp_obj, jitter = 0, 
    edge_color = "index_edge", edge_lwd = 5
    ,node_color = "index_node", node_cex = 3){
  
  temp_map_node = comp_obj$network_nodes %>% 
    mutate(index_node = row_number()) %>%
    mapview(z = node_color, cex = node_cex, layer.name = "Node Layer")
  
  temp_map_edge = comp_obj$network_edges %>% 
    mutate(index_edge = row_number()) %>%
    st_jitter(jitter) %>% 
    mapview(z = edge_color, lwd = edge_lwd, layer.name = "Edge Layer", alpha = .8)
  
  temp_map_node + temp_map_edge
}