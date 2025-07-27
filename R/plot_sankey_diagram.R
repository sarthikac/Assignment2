#' Plot a Sankey Diagram with Square-Like Ribbons and Optional Gaps
#'
#' Creates a block-style Sankey diagram from stroke risk factor data.
#' Rectangles represent proportions; connectors align perfectly across years.
#' You can toggle vertical gaps and horizontal connector spacing.
#'
#' @param data A data frame like stroke_data, with risk factors in rows and years in columns.
#' @param vertical_gap Logical. If TRUE, applies a default vertical gap of 0.05. If FALSE, no vertical gap.
#' @param horizontal_gap Logical. If TRUE, connectors leave 0.05 of white space on each end between them and rectangles.
#' @param min_ribbon_size Numeric. Minimum ribbon size for display (default = 0.06).
#' @return A ggplot2 object showing the Sankey diagram.
#' @import ggplot2 ggforce dplyr tidyr
#' @export
plot_sankey_diagram <- function(data,
                                vertical_gap = FALSE,
                                horizontal_gap = FALSE,
                                min_ribbon_size = 0.06) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)

  # --- Define spacing settings ---
  # Controls the amount of vertical space between rectangles and the white gap at connector edges.
  vertical_gap_value <- ifelse(vertical_gap, 0.05, 0)
  gap_width <- 0.05  # Width of the white space (horizontal gap) between rectangles and connectors.

  # --- Clean and prepare the dataset ---
  # Removes unnecessary rows (like text artifacts or headers) and converts proportion columns to numeric.
  clean_data <- data %>%
    filter(
      !is.na(`Risk Factors for Stroke in Blacks`),
      !`Risk Factors for Stroke in Blacks` %in% c(
        "Raise the Platform", "Website URL", "data source:",
        "Find more:", "E90E50", "E90E50fx"
      )
    ) %>%
    mutate(across(-1, as.numeric))

  # --- Reshape the dataset into long format ---
  # Converts the wide table (years as columns) into long format with Year and Proportion columns.
  df_long <- clean_data %>%
    pivot_longer(cols = -1, names_to = "Year", values_to = "Proportion") %>%
    filter(Proportion > 0)

  df_long$Year <- as.numeric(as.character(df_long$Year))

  # --- Layout constants for the plot ---
  # Defines spacing along the x-axis (global shift) and the width of the stacked rectangles.
  global_shift <- 1.0
  rect_width <- 1.8

  # --- Ensure minimum ribbon display size ---
  # Prevents very small proportions from disappearing by setting a lower bound.
  df_long <- df_long %>%
    mutate(display_prop = ifelse(Proportion < min_ribbon_size, min_ribbon_size, Proportion))

  # --- Calculate rectangle positions ---
  # For each year, calculate ymin/ymax for each rectangle and where it should be drawn on the x-axis.
  df_display <- df_long %>%
    group_by(Year) %>%
    arrange(display_prop, .by_group = TRUE) %>%
    mutate(
      gap_increment = row_number() - 1,
      ymin_rect = cumsum(lag(display_prop, default = 0)) + vertical_gap_value * gap_increment,
      ymax_rect = ymin_rect + display_prop,
      ymid_rect = (ymin_rect + ymax_rect) / 2,
      xmin = Year - rect_width/2 + global_shift,
      xmax = Year + rect_width/2 + global_shift
    ) %>%
    ungroup()

  # --- Connector position calculations ---
  # Determines the vertical span for each connector, then lines up the “from” and “to” coordinates between years.
  df_connectors <- df_display %>%
    mutate(
      ymin_conn = ymid_rect - (Proportion / 2),
      ymax_conn = ymid_rect + (Proportion / 2)
    ) %>%
    group_by(`Risk Factors for Stroke in Blacks`) %>%
    arrange(Year) %>%
    mutate(
      ymin_next = lead(ymin_conn),
      ymax_next = lead(ymax_conn),
      xmin_next = lead(xmin),
      xmax_next = lead(xmax),
      Year_next = lead(Year)
    ) %>%
    filter(!is.na(Year_next)) %>%
    ungroup()

  # --- Build polygons for connectors ---
  # Creates a set of polygon coordinates that link rectangles from one year to the next.
  connector_polygons <- df_connectors %>%
    rowwise() %>%
    mutate(group_id = paste(`Risk Factors for Stroke in Blacks`, Year, sep = "_")) %>%
    do({
      data.frame(
        RiskFactor = .$`Risk Factors for Stroke in Blacks`,
        group = .$group_id,
        x = c(.$xmax, .$xmax,
              .$xmin_next, .$xmin_next),
        y = c(.$ymin_conn, .$ymax_conn, .$ymax_next, .$ymin_next)
      )
    }) %>%
    ungroup()

  # --- Optional white mask rectangles for gaps ---
  # If horizontal gaps are enabled, these white rectangles cover the connector edges where they meet the rectangles.
  gap_masks <- NULL
  if (horizontal_gap) {
    gap_masks <- bind_rows(
      # Left edges
      df_display %>%
        transmute(
          xmin = xmin - 0.0001,
          xmax = xmin + gap_width,
          ymin = ymin_rect,
          ymax = ymax_rect
        ),
      # Right edges
      df_display %>%
        transmute(
          xmin = xmax - gap_width,
          xmax = xmax + 0.0001,
          ymin = ymin_rect,
          ymax = ymax_rect
        )
    )
  }

  # --- Define color palette for the plot ---
  stroke_colors <- c(
    "Hypertension" = "#8fbc8f",
    "Diabetes" = "#ee5c42",
    "Smoking" = "#83c6d9",
    "Hypercholesterolemia" = "#e77d22",
    "Obesity" = "#005596"
  )

  # --- Determine label positions for the first year ---
  # Used to place risk factor labels on the left-hand side of the diagram.
  first_year <- min(df_display$Year)
  label_positions <- df_display %>%
    filter(Year == first_year) %>%
    mutate(x_label = first_year - (rect_width * 0.8) + global_shift)

  # --- Assemble the plot ---
  # Add connectors, rectangles, optional white masks, text labels, and styling.
  p <- ggplot() +
    # Connectors
    geom_polygon(
      data = connector_polygons,
      aes(x = x, y = y, group = group, fill = RiskFactor),
      color = NA
    ) +
    # Rectangles
    geom_rect(
      data = df_display,
      aes(xmin = xmin, xmax = xmax,
          ymin = ymin_rect, ymax = ymax_rect,
          fill = `Risk Factors for Stroke in Blacks`),
      color = NA
    )

  # If horizontal gaps are enabled, draw white masks over connector edges.
  if (horizontal_gap) {
    p <- p +
      geom_rect(
        data = gap_masks,
        aes(xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax),
        fill = "white", color = "white"
      )
  }

  # Add proportion labels inside rectangles.
  p <- p +
    geom_text(
      data = df_display,
      aes(x = (xmin + xmax) / 2,
          y = ymid_rect,
          label = sprintf("%.2f", Proportion)),
      color = "white", fontface = "bold", size = 3.4
    ) +
    # Add risk factor labels on the left side.
    geom_text(
      data = label_positions,
      aes(x = x_label, y = ymid_rect,
          label = `Risk Factors for Stroke in Blacks`),
      hjust = 1, size = 3.8, color = "black"
    ) +
    # Apply color scheme and axis formatting.
    scale_fill_manual(values = stroke_colors) +
    scale_x_continuous(
      breaks = unique(df_display$Year) + global_shift,
      labels = unique(df_display$Year)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.margin = margin(10, 10, 10, 70),
      legend.position = "none"
    ) +
    labs(title = "Risk Factors for Stroke in Blacks") +
    coord_fixed(ratio = 8, clip = "off")

  return(p)
}
