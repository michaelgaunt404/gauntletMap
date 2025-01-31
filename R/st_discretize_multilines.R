#' Discretize Multilines in a Roadway Network
#'
#' This function processes a spatial object representing a roadway network, breaking it into discrete segments for analysis. It identifies contiguous sections within named corridors, orders links within these sections, and further segments them into 10-meter long units. The function outputs an ordered and segmented representation of the network and summary statistics for each corridor.
#'
#' @param network_object A spatial object of class `sf` representing the roadway network. It should contain a geometry column and attributes such as corridor names.
#' @param section_length_threshold Numeric. A length threshold in meters. Sections shorter than this threshold will be removed from processing.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{temp_segemented_links_seg_ordered}}{An `sf` object with the segmented network, including attributes for section IDs, segment order, and lengths.}
#'     \item{\code{cor_section_summary}}{A data frame summarizing each corridor, including total lengths, number of sections, and distance metrics.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies contiguous sections of links within named corridors.
#' 2. Removes sections shorter than the specified length threshold.
#' 3. Orders links within each contiguous section for consistency.
#' 4. Breaks links into 10-meter long segments and reorders them.
#' 5. Computes summary statistics for each corridor, such as total length, shortest and longest sections, and distances between sections.
#'
#' Note: This function assumes the input network is projected in a CRS (e.g., EPSG:32610) that supports distance measurement in meters.
#'
#' @importFrom data.table last
#' @importFrom dplyr across arrange bind_rows filter group_by group_map mutate rename row_number select summarise ungroup
#' @importFrom gauntlet dgt0
#' @importFrom igraph components graph_from_adj_list
#' @importFrom lwgeom st_split
#' @importFrom magrittr %>%
#' @importFrom purrr reduce
#' @importFrom sf st_buffer st_collection_extract st_combine st_distance st_drop_geometry st_length st_nearest_points st_touches st_transform
#' @importFrom stringr str_glue
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a sample network object
#' library(sf)
#' network <- st_read("path_to_network_file.shp")
#'
#' # Apply the function
#' result <- st_discretize_multilines(network, section_length_threshold = 100)
#'
#' # Access the results
#' segmented_links <- result$temp_segemented_links_seg_ordered
#' section_summary <- result$cor_section_summary
#' }
st_discretize_multilines = function(network_object, section_length_threshold, input_crs, segmentation_length){

  #code_notes
  {
    #mg_note: having trouble figuring out i this object can/should be able to handle directions
    #currently it can handle directional
    #this really should not have to handle directionality
    #in theorey a unique corrdior/multiline should be sent to this object
    #i have removed all directions
    #note mg_20241218
    #previous note was shit, enhancing
    #I believe that I took a large amount of code from original HIN network and put into this function - compare to process_network_segmentation
    #--This part function performs all of the network link segmentation tasks
    #--I cant remember exactly but I beleive that this function naturally handles both bi-/uni-directional links
    #--i dont think that it explicitly looks for anything with re/ directions
    #----but the idea is that the directions are not continuous and would be treated as different
    #update: this code can be made to process bi-directional links but it currently does not
    #------- I checked process_network_segmentation_freeway.r
    #------- --it maps over a glue of name and direction so everything is effectively uni-directional

    #NOTE!!!
    #this function is shared across two repos PSRC_HIN and SR99
    #it should be pushed over to gauntlet map but until then it lives in two repos
    #before push to gauntlet map - both repos should chekc for most recent

    #sec: identify contiguous network sections for corridor
    #---- process identifies links that touch each other
    #---- gives them unique ID and makes basic attributes
    #---- performs operation for any number of directions it has in flag_direction
  }

  # browser()
  temp_igraph = network_object %>%
    # dplyr::group_by(flag_direction_1 = flag_direction) %>%
    # dplyr::group_map(~{
    #   data = .x
    #
    #   data %>%
    dplyr::mutate(id_section =  network_object %>%
                    sf::st_touches() %>%
                    igraph::graph_from_adj_list() %>%
                    igraph::components() %>%
                    .[["membership"]] %>%
                    as.character()) %>%
    dplyr::group_by(id_section) %>%
    dplyr::mutate(section_length = sum(sf::st_length(geometry)) %>%
                    as.numeric(units = "meters")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flag_section_removal = section_length < section_length_threshold) %>%
    mutate(across(c("id_section", "section_length"), as.numeric))

  # }) %>%
  # purrr::reduce(bind_rows)


  #sec: makes continuous segment and NN statistics=============================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #calculates the smallest distance between each corridor section
  section_distance_matrix = temp_igraph %>%
    dplyr::group_by(id_section) %>%
    dplyr::summarise() %>%
    sf::st_distance() %>%
    .[lower.tri(., diag = F)] %>%
    as.numeric()

  temp_igraph_summary = temp_igraph %>%
    dplyr::select(id_section, section_length) %>%
    sf::st_drop_geometry() %>%
    unique()

  #mk: data frame detailing relationship/stats of contiguous sections
  cor_section_summary = data.frame(
    street_name = unique(network_object$full_name)
    ,num_secs = max(temp_igraph$id_section)
    ,ttl_length_sections = sum(temp_igraph_summary$section_length) %>% gauntlet::dgt0()
    ,shortest = min(temp_igraph_summary$section_length) %>% gauntlet::dgt0()
    ,longest = max(temp_igraph_summary$section_length) %>% gauntlet::dgt0()
    ,length_mean = mean(temp_igraph_summary$section_length) %>% gauntlet::dgt0()
    ,length_med = median(temp_igraph_summary$section_length) %>% gauntlet::dgt0()
    ,dist_max = max(section_distance_matrix, na.rm = T) %>% gauntlet::dgt0()
    ,dist_min = min(section_distance_matrix, na.rm = T) %>% gauntlet::dgt0()
    ,dist_mean = mean(section_distance_matrix, na.rm = T) %>% gauntlet::dgt0()
    ,dist_med = median(section_distance_matrix, na.rm = T) %>% gauntlet::dgt0())

  cor_section_summary = dplyr::bind_rows(
    cor_section_summary %>%  dplyr::mutate(units = "meters")
    ,cor_section_summary %>%
      dplyr::mutate(
        dplyr::across(ttl_length_sections:dist_med, ~round(.x/1609.34, 3))
        ,units = "miles"))

  #sec: section_reordering======================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #for each group section arrange links in touching order
  temp_cor_order = Sys.time()
  temp_igraph_reorded = temp_igraph %>%
    dplyr::group_by(id_section_1 = id_section) %>%
    dplyr::group_map(~{
      x = .x
      # browser()
      #when only one link the st_touches makes an empty list which fails

      adjacency <- sf::st_touches(x, x)
      wrk_item <- min(which(sapply(adjacency, length) == 1))
      check_inf = is.infinite(wrk_item)

      if (!check_inf){
        index_vec = c(wrk_item)

        for (i in seq_along(adjacency)-1) {
          next_index = data.table::last(index_vec)
          next_index_pot = adjacency[next_index][[1]]
          index_vec = c(index_vec, next_index_pot[!next_index_pot %in% index_vec])
        }

        temp_pro = x %>%
          dplyr::mutate(cor_link_ordr = dplyr::row_number()) %>%
          .[index_vec,] %>%
          dplyr::mutate(cor_link_ordr_pro = dplyr::row_number())
      } else {
        message("Empty st_touches matric detected....")
        temp_pro = x %>%
          dplyr::mutate(cor_link_ordr = 1) %>%
          dplyr::mutate(cor_link_ordr_pro = 1)
      }

      return(temp_pro)

    }) %>%
    purrr::reduce(bind_rows)

  message(stringr::str_glue("{max(as.numeric(temp_igraph_reorded$id_section))} contiguous links have been detected..."))

  #sec: segmentation============================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp_cor_segmentation = Sys.time()
  message(stringr::str_glue("Finished initial processing at {temp_cor_segmentation}\nStarting segmentation of links...."))
  temp_segemented_links = temp_igraph_reorded %>%
    # .[12,] %>%
    dplyr::filter(!is.na(shape_id)) %>% #had to add this
    dplyr::group_by(shape_id_1 = shape_id
                    ,full_name_1 = full_name) %>%
    dplyr::group_map(~{

      tryCatch({
        # browser()

        temp_sf_object = .x
        temp_shape_id = unique(temp_sf_object$shape_id)
        temp_shape_length = round(as.numeric(sf::st_length(temp_sf_object$geometry)), 1)
        temp_shape_street_name = unique(temp_sf_object$full_name)

        # browser()
        message(stringr::str_glue("Processing: {unique(temp_shape_id)} -- {temp_shape_length} meters"))

        if (temp_shape_length > 12){
          #need to skip if length less than 10
          temp_points = st_line_sample_to_points_pro(
            sf_object = temp_sf_object
            ,samp_dist = segmentation_length) %>%
            # dplyr::mutate(raw_link_length = sf::st_length(geometry)) %>%
            sf::st_transform(crs = input_crs) %>%
            sf::st_as_sf() %>%
            dplyr::mutate(shape_id = temp_shape_id) %>%
            dplyr::rename(index_point = index) %>%
            dplyr::arrange(index_point)

          nrst = sf::st_nearest_points(temp_points, temp_sf_object)
          buf_all = sf::st_combine(sf::st_buffer(nrst,0.1))
          parts_all = sf::st_collection_extract(
            lwgeom::st_split(
              temp_sf_object$geometry
              ,buf_all),"LINESTRING")

        } else {
          parts_all = temp_sf_object %>%
            sf::st_as_sfc()
        }

        parts_all_pro = sf::st_as_sf(
          data.frame(
            # street_name = temp_shape_street_name
            index_link = 1:length(parts_all)
            ,geometry = parts_all
          )
        ) %>%
          dplyr::mutate(length = sf::st_length(geometry) %>% as.numeric()) %>%
          dplyr::group_by(flag_group = (length > 2)) %>%
          dplyr::mutate(flag_group_index = dplyr::row_number()) %>%
          dplyr::group_by(flag_group_index) %>%
          dplyr::summarise() %>%
          dplyr::mutate(length = sf::st_length(geometry) %>% as.numeric())  %>%
          dplyr::rename(link_seg_ordr = flag_group_index) %>%
          dplyr::mutate(link_seg_ordr_pct = round(link_seg_ordr/max(link_seg_ordr), 2)) %>%
          dplyr::mutate(
            shape_id = temp_shape_id
            ,bearing = temp_sf_object$bearing
            ,id_section  = temp_sf_object$id_section
            ,cor_link_ordr  = temp_sf_object$cor_link_ordr
            ,cor_link_ordr_pro = temp_sf_object$cor_link_ordr_pro
          )

      },  error = function(e) {
        error_message <- paste("An error occurred while querying the trip table:\n", e$message)

        return(NA)
      })

      return(parts_all_pro)

    }) %>%
    purrr::reduce(bind_rows) %>%
    dplyr::select(shape_id, id_section, cor_link_ordr, cor_link_ordr_pro
                  ,link_seg_ordr, link_seg_ordr_pct, bearing, length, geometry) %>%
    dplyr::arrange(id_section, cor_link_ordr_pro)

  #reorders links again
  temp_cor_seg_reorder = Sys.time()
  temp_segemented_links_seg_ordered = temp_segemented_links %>%
    dplyr::group_by(id_section_1 = id_section) %>%
    dplyr::group_map(~{
      x = .x
      adjacency <- sf::st_touches(x, x)
      wrk_item <- min(which(sapply(adjacency, length) == 1))
      index_vec = c(wrk_item)

      for (i in seq_along(adjacency)-1) {
        next_index = data.table::last(index_vec)
        next_index_pot = adjacency[next_index][[1]]
        index_vec = c(index_vec, next_index_pot[!next_index_pot %in% index_vec])
      }

      temp_pro = x %>%
        dplyr::mutate(cor_seg_ordr = dplyr::row_number()) %>%
        .[index_vec,] %>%
        dplyr::mutate(cor_seg_ordr_pro = dplyr::row_number()
                      ,cor_seg_ordr_pro_pct = round(cor_seg_ordr_pro/max(cor_seg_ordr_pro), 2))

      return(temp_pro)

    }) %>%
    purrr::reduce(bind_rows) %>%
    merge(
      temp_igraph_reorded %>%
        sf::st_drop_geometry() %>%
        dplyr::select(shape_id, flag_direction )
    )

  return(
    list(
      temp_segemented_links_seg_ordered = temp_segemented_links_seg_ordered
      ,cor_section_summary = cor_section_summary)
  )
}
