#' Retrieve the id for a field in a table
#' @inheritParams prepare_dataset
#' @param table_name The name of the table.
#' @param field_name The name of the field.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom git2rdata read_vc update_metadata write_vc
get_field_id <- function(repo, table_name, field_name) {
  assert_that(
    inherits(repo, "git_repository"), is.string(table_name),
    is.string(field_name)
  )

  existing_db <- try(read_vc("database_id", root = repo), silent = TRUE)
  # what to do when table doesn't exist
  if (inherits(existing_db, "try-error")) {
    write_vc(
      x = data.frame(id = 1L, table = table_name, field = field_name),
      file = "database_id", root = repo, sorting = c("table", "field"),
      stage = TRUE
    )
    update_metadata(
      file = "database_id", root = repo, stage = TRUE, name = "database_id",
      title = "Database id for tables and fields",
      description =
        "This table describes to which table and field an id refers.",
      field_description = c(
        id = "Unique identifier", table = "Name of the table",
        field = "Name of the field"
      )
    )
    return(1L)
  }

  # find table and field
  current <- existing_db$table == table_name & existing_db$field == field_name
  if (any(current)) {
    # return known id
    return(existing_db$id[current])
  }

  # add new id and return it
  current_max <- max(existing_db$id)
  write_vc(
    file = "database_id", root = repo, stage = TRUE,
    x = rbind(
      existing_db,
      data.frame(
        id = current_max + 1L, table = table_name, field = field_name
      )
    )
  )
  update_metadata(
    file = "database_id", root = repo, stage = TRUE, name = "database_id",
    title = "Database id for tables and fields",
    description =
      "This table describes to which table and field an id refers.",
    field_description = c(
      id = "Unique identifier", table = "Name of the table",
      field = "Name of the field"
    )
  )
  return(current_max + 1L)
}
