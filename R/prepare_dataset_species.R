#' Read the species groups and save them to git and the results database
#' @inheritParams n2khelper::odbc_connect
#' @export
#' @importFrom n2khelper odbc_get_multi_id connect_result
#' @importFrom RODBC odbcClose
#' @importFrom n2kanalysis mark_obsolete_dataset
prepare_dataset_species <- function(develop = TRUE){
  
  import.date <- Sys.time()
  species.list <- read_specieslist(develop = TRUE)
  
  channel <- connect_result(develop = develop)
  species.id <- odbc_get_multi_id(
    data = species.list$Species,
    id.field = "ID", merge.field = "ExternalCode", table = "Species",
    channel = channel, create = TRUE
  )
  species.id <- merge(species.id, species.list$Species[, c("ExternalCode", "DutchName")])
  colnames(species.id) <- gsub("^ID$", "SpeciesID", colnames(species.id))
  
  species.group.species <- merge(
    species.list$Speciesgroup,
    species.id[, c("ExternalCode", "SpeciesID")]
  )
  species.group.species$ExternalCode <- NULL
  
  species.group <- rbind(
    data.frame(
      Description = species.id$DutchName,
      SchemeID = scheme_id(develop = develop),
      Combined = 0,
      SpeciesID = species.id$SpeciesID
    ),
    cbind(
      species.group.species,
      Combined = 1
    )
  )
  
  database.id <- odbc_get_multi_id(
    data = unique(species.group[, c("Description", "SchemeID")]),
    id.field = "ID", merge.field = c("Description", "SchemeID"), table = "SpeciesGroup",
    channel = channel, create = TRUE
  )
  species.group <- merge(database.id, species.group)
  colnames(species.group) <- gsub("^ID$", "SpeciesGroupID", colnames(species.group))
  
  species.group.species <- species.group[
    order(species.group$SpeciesGroupID, species.group$SpeciesID),
    c("SpeciesGroupID", "SpeciesID")
  ]
  
  species.group <- unique(species.group[, c("SpeciesGroupID", "Combined")])
  species.group <- species.group[
    order(species.group$SpeciesGroupID),
    c("SpeciesGroupID", "Combined")
  ]
  
  
  
  species.group.sha <- write_delim_git(
    x = species.group, file = "speciesgroup.txt", path = "abv"
  )
  species.group.species.sha <- write_delim_git(
    x = species.group.species, file = "speciesgroupspecies.txt", path = "abv"
  )
  dataset <- data.frame(
    FileName = c("speciesgroup.txt", "speciesgroupspecies.txt"),
    PathName = "abv",
    Fingerprint = c(species.group.sha, species.group.species.sha),
    ImportDate = import.date,
    Obsolete = FALSE
  )
  database.id <- odbc_get_multi_id(
    data = dataset,
    id.field = "ID", merge.field = c("FileName", "PathName", "Fingerprint"), 
    table = "Dataset", 
    channel = channel, create = TRUE
  )  
  odbcClose(channel)
  mark_obsolete_dataset(develop = develop)
  
  species.group.species <- merge(
    species.group.species,
    species.group
  )
  species.group.species <- species.group.species[
    !species.group.species$Combined, 
    c("SpeciesGroupID", "SpeciesID")
  ]
  species <- merge(
    species.id[, c("SpeciesID", "ExternalCode")],
    species.group.species
  )
  
  return(species[, c("ExternalCode", "SpeciesGroupID")])
}
