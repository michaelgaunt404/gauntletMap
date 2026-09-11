st_network_extract_comonents = function(net_object){
  network_edges = net_object %>% st_as_sf("edges") 
  network_nodes = net_object %>% st_as_sf("nodes")
  
  return(
    list(
      network_edges = network_edges
      ,network_nodes = network_nodes
    )
  )
}