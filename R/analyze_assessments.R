#' Title
#'
#' @param file
#'
#' @returns
#'
#' @export
#' @examples
analyze_assessments <- function(
  file = system.file(
    "extdata/assessments.yaml",
    package = "hoaTools"
  )
) {
  library(yaml)
  library(dplyr)

  # ------------------------------
  # Load YAML
  # ------------------------------
  data <- yaml::read_yaml(file)

  # ------------------------------
  # Convert 'units' to a dataframe
  # ------------------------------
  units_df <- dplyr::bind_rows(data$units)

  # ------------------------------
  # Identify year columns
  # ------------------------------
  year_cols <- names(units_df)[grepl("^\\d{4}$", names(units_df))]
  year_cols <- sort(year_cols)

  # ------------------------------
  # Compute percentage increases
  # ------------------------------
  pct_increases <- units_df %>%
    select(unit_size, dplyr::all_of(year_cols))

  for (i in seq_len(length(year_cols) - 1)) {
    from_year <- year_cols[i]
    to_year <- year_cols[i + 1]

    col_name <- paste0("pct_", from_year, "_to_", to_year)

    pct_increases[[col_name]] <- ((pct_increases[[to_year]] -
      pct_increases[[from_year]]) /
      pct_increases[[from_year]]) *
      100
  }

  pct_cols <- names(pct_increases)[grepl("^pct_", names(pct_increases))]

  results <- pct_increases %>%
    select(unit_size, dplyr::all_of(pct_cols))

  # ------------------------------
  # Summary statistics
  # ------------------------------
  summary_stats <- tibble()

  for (col in pct_cols) {
    summary_stats <- bind_rows(
      summary_stats,
      tibble(
        Period = gsub("pct_", "", col) %>% gsub("_to_", " to ", .),
        Mean = mean(results[[col]], na.rm = TRUE),
        StDev = sd(results[[col]], na.rm = TRUE),
        Min = min(results[[col]], na.rm = TRUE),
        Max = max(results[[col]], na.rm = TRUE)
      )
    )
  }

  # ------------------------------
  # Return as list
  # ------------------------------
  return(list(
    units_df = units_df,
    results = results,
    summary_stats = summary_stats
  ))
}
