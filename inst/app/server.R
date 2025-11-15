library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(yaml)

# Define server logic
server <- function(input, output, session) {
  # Load and parse data
  assessment_data <- reactive({
    # Default data
    default_data <- list(
      units = list(
        list(
          unit_size = "1 bed 1 bath",
          no_of_unit = 60,
          "2020" = 236.77,
          "2023" = 251.16,
          "2024" = 275.77368
        ),
        list(
          unit_size = "2 bed 1 bath",
          no_of_unit = 92,
          "2020" = 312.26,
          "2023" = 331.22,
          "2024" = 363.67956
        ),
        list(
          unit_size = "2 bed 2 bath",
          no_of_unit = 20,
          "2020" = 340.50,
          "2023" = 361.17,
          "2024" = 396.56466
        ),
        list(
          unit_size = "2 bed 2 bath with den",
          no_of_unit = 2,
          "2020" = 387.57,
          "2023" = 411.13,
          "2024" = 451.42074
        ),
        list(
          unit_size = "3 bed 2 bath",
          no_of_unit = 6,
          "2020" = 387.57,
          "2023" = 411.13,
          "2024" = 451.42074
        )
      )
    )

    # If file uploaded, use that instead
    if (!is.null(input$yaml_file)) {
      data <- yaml::read_yaml(input$yaml_file$datapath)
    } else {
      data <- default_data
    }

    # Convert to dataframe
    units_df <- bind_rows(data$units)

    # Identify year columns
    year_cols <- names(units_df)[grepl("^\\d{4}$", names(units_df))]
    year_cols <- sort(year_cols)

    # Update unit type choices
    unit_choices <- c(
      "All Units" = "all",
      setNames(units_df$unit_size, units_df$unit_size)
    )
    updateSelectInput(session, "unit_type", choices = unit_choices)

    list(units_df = units_df, year_cols = year_cols)
  })

  # Calculate percentage increases
  percent_increases <- reactive({
    data <- assessment_data()
    units_df <- data$units_df
    year_cols <- data$year_cols

    results <- units_df %>%
      select(unit_size, all_of(year_cols))

    increase_cols <- list()

    for (i in seq_len(length(year_cols) - 1)) {
      from_year <- year_cols[i]
      to_year <- year_cols[i + 1]

      years_diff <- as.numeric(to_year) - as.numeric(from_year)

      col_name <- paste0("pct_", from_year, "_to_", to_year)
      annual_col_name <- paste0("annual_", from_year, "_to_", to_year)

      # Total percentage increase
      results[[col_name]] <- ((results[[to_year]] - results[[from_year]]) /
        results[[from_year]]) *
        100

      # Annualized percentage increase
      results[[annual_col_name]] <- results[[col_name]] / years_diff

      increase_cols[[paste(from_year, "to", to_year)]] <- annual_col_name
    }

    list(results = results, increase_cols = increase_cols)
  })

  # Calculate overall statistics
  overall_stats <- reactive({
    pct_data <- percent_increases()
    results <- pct_data$results
    annual_cols <- unlist(pct_data$increase_cols)

    all_rates <- results %>%
      select(all_of(annual_cols)) %>%
      pivot_longer(everything(), names_to = "period", values_to = "rate") %>%
      pull(rate)

    list(
      mean = mean(all_rates, na.rm = TRUE),
      sd = sd(all_rates, na.rm = TRUE),
      min = min(all_rates, na.rm = TRUE),
      max = max(all_rates, na.rm = TRUE)
    )
  })

  # Get effective rate (manual or calculated)
  effective_rate <- reactive({
    if (!is.null(input$manual_rate) && !is.na(input$manual_rate)) {
      input$manual_rate
    } else {
      overall_stats()$mean
    }
  })

  # Reset to calculated average
  observeEvent(input$reset_rate, {
    updateNumericInput(session, "manual_rate", value = NA)
  })

  # Filter data by unit type
  filtered_data <- reactive({
    pct_data <- percent_increases()
    results <- pct_data$results

    if (input$unit_type != "all") {
      results <- results %>% filter(unit_size == input$unit_type)
    }

    list(results = results, increase_cols = pct_data$increase_cols)
  })

  # Output: Statistics summary
  output$stats_summary <- renderText({
    stats <- overall_stats()
    rate <- effective_rate()

    paste0(
      "Calculated Average: ",
      round(stats$mean, 2),
      "%\n",
      "Std Deviation: ",
      round(stats$sd, 2),
      "%\n",
      "Min: ",
      round(stats$min, 2),
      "%\n",
      "Max: ",
      round(stats$max, 2),
      "%\n\n",
      "Current Rate Used: ",
      round(rate, 2),
      "%"
    )
  })

  # Output: Percent increase plot
  output$percent_plot <- renderPlot({
    filtered <- filtered_data()
    results <- filtered$results
    increase_cols <- filtered$increase_cols

    # Prepare data for plotting
    plot_data <- results %>%
      select(unit_size, all_of(unlist(increase_cols))) %>%
      pivot_longer(
        -unit_size,
        names_to = "period",
        values_to = "annual_increase"
      ) %>%
      mutate(
        period_label = gsub("annual_", "", period) %>%
          gsub("_to_", " to ", .),
        year_mid = sapply(
          strsplit(gsub("annual_", "", period), "_to_"),
          function(x) mean(as.numeric(x))
        )
      )

    avg_rate <- effective_rate()

    ggplot(
      plot_data,
      aes(x = year_mid, y = annual_increase, color = unit_size)
    ) +
      geom_point(size = 4) +
      geom_hline(
        yintercept = avg_rate,
        linetype = "dashed",
        color = "black",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = mean(plot_data$year_mid),
        y = avg_rate + 0.5,
        label = paste0("Avg: ", round(avg_rate, 2), "%"),
        color = "black",
        fontface = "bold"
      ) +
      labs(
        title = "Annual Percentage Increase by Period",
        x = "Year (midpoint of period)",
        y = "Annual Increase (%)",
        color = "Unit Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # Output: Increase table
  output$increase_table <- renderTable(
    {
      pct_data <- percent_increases()
      results <- pct_data$results
      increase_cols <- pct_data$increase_cols

      # Calculate summary statistics for each period
      summary_df <- data.frame()

      for (period_name in names(increase_cols)) {
        col_name <- increase_cols[[period_name]]
        summary_df <- bind_rows(
          summary_df,
          data.frame(
            Period = period_name,
            Mean = mean(results[[col_name]], na.rm = TRUE),
            StDev = sd(results[[col_name]], na.rm = TRUE),
            Min = min(results[[col_name]], na.rm = TRUE),
            Max = max(results[[col_name]], na.rm = TRUE)
          )
        )
      }

      summary_df
    },
    digits = 2
  )

  # Output: Forecast plot
  output$forecast_plot <- renderPlot({
    data <- assessment_data()
    units_df <- data$units_df
    year_cols <- data$year_cols

    rate <- effective_rate() / 100

    # Filter by unit type if needed
    if (input$unit_type != "all") {
      units_df <- units_df %>% filter(unit_size == input$unit_type)
    }

    # Create forecast data
    forecast_data <- data.frame()

    for (i in 1:nrow(units_df)) {
      unit <- units_df[i, ]

      # Historical data
      for (year_col in year_cols) {
        forecast_data <- bind_rows(
          forecast_data,
          data.frame(
            unit_size = unit$unit_size,
            year = as.numeric(year_col),
            value = as.numeric(unit[[year_col]]),
            type = "Historical"
          )
        )
      }

      # Forecast data
      last_year <- as.numeric(year_cols[length(year_cols)])
      last_value <- as.numeric(unit[[year_cols[length(year_cols)]]])

      for (j in 1:5) {
        forecast_year <- last_year + j
        forecast_value <- last_value * ((1 + rate)^j)

        forecast_data <- bind_rows(
          forecast_data,
          data.frame(
            unit_size = unit$unit_size,
            year = forecast_year,
            value = forecast_value,
            type = "Forecast"
          )
        )
      }
    }

    ggplot(forecast_data, aes(x = year, y = value, color = unit_size)) +
      geom_point(aes(shape = type), size = 3) +
      geom_line(
        data = forecast_data %>% filter(type == "Historical"),
        linewidth = 1
      ) +
      geom_line(
        data = forecast_data %>% filter(type == "Forecast"),
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_shape_manual(values = c("Historical" = 16, "Forecast" = 1)) +
      labs(
        title = paste0(
          "Assessment Forecast (Rate: ",
          round(effective_rate(), 2),
          "%)"
        ),
        x = "Year",
        y = "Monthly Assessment ($)",
        color = "Unit Type",
        shape = "Data Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::dollar_format())
  })

  # Output: Forecast table
  output$forecast_table <- renderTable(
    {
      data <- assessment_data()
      units_df <- data$units_df
      year_cols <- data$year_cols

      rate <- effective_rate() / 100

      # Filter by unit type if needed
      if (input$unit_type != "all") {
        units_df <- units_df %>% filter(unit_size == input$unit_type)
      }

      # Create forecast table
      last_year <- as.numeric(year_cols[length(year_cols)])
      forecast_years <- (last_year + 1):(last_year + 5)

      forecast_table <- units_df %>%
        select(unit_size, last_of(all_of(year_cols)))

      colnames(forecast_table)[2] <- as.character(last_year)

      for (i in 1:5) {
        year <- last_year + i
        col_name <- as.character(year)
        forecast_table[[col_name]] <- forecast_table[[as.character(
          last_year
        )]] *
          ((1 + rate)^i)
      }

      forecast_table
    },
    digits = 2
  )

  # Output: Raw data table
  output$raw_data_table <- renderTable(
    {
      assessment_data()$units_df
    },
    digits = 2
  )
}
