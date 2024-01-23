### Add OTM metadat ###

#' Add OTM metadata
#'
#' Adds metadata (e.g., location, orientation, microhabitat characterstis) to an existing OTM dataset
#'
#' @param otm_data An OTM data frame generated using the `rnp_otm_data.R` function.
#' @param otm_metadata An OTM metadata data frame with an otm_id column to match with `otm_data`
#'
#' @return An OTM data frame with metadata included
#'
#' @export

add_otm_metadata <- function(otm_data, otm_metadata){

  # merge processed OTM data with OTM metadata by OTM identity
  otm_merged <- merge(otm_data, otm_metadata, by = "otm_id")

  # remove columns that have not processed correctly
  otm_merged <- otm_merged %>% filter(!is.na(latitude))

  return(otm_merged)

}
