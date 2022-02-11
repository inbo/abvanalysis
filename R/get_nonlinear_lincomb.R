#' Calculate the matrix for linear combinations of strata
#' @param dataset the raw dataset.
#' @param stratum_weights a dataframe with stratum weights.
#' @param time_var variable indicating the time.
#' @param label_var variable containing the label for every `time_var`.
#' @param stratum_var variable containing the stratum information.
#' @export
#' @importFrom assertthat assert_that has_name is.string
#' @importFrom dplyr %>% arrange distinct filter inner_join mutate row_number select transmute
#' @importFrom purrr map
#' @importFrom rlang .data !!
#' @importFrom stats setNames
#' @importFrom tidyr  expand_grid pivot_longer pivot_wider
get_nonlinear_lincomb <- function(
    dataset, stratum_weights, time_var, label_var, stratum_var
) {
  assert_that(is.string(time_var), is.string(stratum_var), is.string(label_var))
  assert_that(
    inherits(dataset, "data.frame"), has_name(dataset, time_var),
    has_name(dataset, label_var), has_name(dataset, stratum_var)
  )
  assert_that(
    inherits(stratum_weights, "data.frame"),
    has_name(stratum_weights, "weight"), has_name(stratum_weights, stratum_var)
  )
  start <- min(dataset[[label_var]])
  end <- max(dataset[[label_var]])
  seq(start, end, by = ifelse(time_var == "cycle", 3, 1)) %>%
    length() -> n_total

  dataset %>%
    arrange(!!stratum_var, !!time_var, !!label_var) %>%
    inner_join(stratum_weights, by = stratum_var) %>%
    arrange(!!stratum_var, !!time_var) %>%
    mutate(
      id = row_number() %>%
        sprintf(fmt = "%04i")
    ) -> base_weights
  base_weights %>%
    pivot_wider(
      id_cols = label_var, names_from = .data$id, values_from = .data$weight,
      values_fill = 0
    ) -> year_effect
  dataset %>%
    select(!!time_var, label = !!label_var) %>%
    distinct() %>%
    arrange(!!time_var) -> time_steps
  strata <- sort(unique(dataset[[stratum_var]]))
  expand_grid(
    from = time_steps[[time_var]], to = time_steps[[time_var]],
    stratum = strata, to_s = strata
  ) %>%
    filter(.data$from < .data$to, .data$stratum == .data$to_s) %>%
    select(-.data$to_s) %>%
    inner_join(time_steps, by = c(from = time_var)) %>%
    mutate(rowname = paste0("index_", .data$label, "_")) %>%
    select(-.data$label) %>%
    inner_join(time_steps, by = c(to = time_var)) %>%
    mutate(rowname = paste0(.data$rowname, .data$label)) %>%
    select(-.data$label) %>%
    pivot_longer(
      c("from", "to"), names_to = "direction", values_to = time_var
    ) %>%
    inner_join(stratum_weights, by = stratum_var) %>%
    transmute(
      id = interaction(.data[[time_var]], .data[[stratum_var]]) %>%
        as.integer() %>%
        sprintf(fmt = "%04i"),
      .data$rowname,
      weight = ifelse(.data$direction == "to", 1, -1) * .data$weight
    ) %>%
    arrange(.data$id, .data$rowname) %>%
    pivot_wider(
      names_from = .data$id, values_from = .data$weight, values_fill = 0
    ) -> indices
  rbind(
    moving_trend(
      n_total = n_total, start = start,
      n_window = ifelse(time_var == "cycle", 2, 6)
    ),
    moving_trend(
      n_total = n_total, start = start,
      n_window = ifelse(time_var == "cycle", 4, 12)
    ),
    moving_trend(n_total = n_total, n_window = n_total, start = start)
  ) %>%
    map(.x = stratum_weights$weight, .f = `*`) %>%
    do.call(what = "cbind") -> trend_coef
  select(year_effect, -.data$label) %>%
    bind_rows(select(indices, -.data$rowname)) %>%
    as.matrix() %>%
    `row.names<-`(
      c(sprintf("estimate_%i", year_effect$label), indices$rowname)
    ) %>%
    rbind(trend_coef) %>%
    list() %>%
    setNames(time_var) %>%
    c(list(
      `(Intercept)` = c(
        rep(1, nrow(year_effect)), rep(0, nrow(indices) + nrow(trend_coef))
      )
    ))
}
