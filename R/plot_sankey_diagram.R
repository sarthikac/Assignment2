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

  # --- Gap settings ---
  vertical_gap_value <- ifelse(vertical_gap, 0.05, 0)
  gap_width <- 0.05  # width of white space when horizontal_gap = TRUE

  # --- Clean Data ---
  clean_data <- data %>%
    filter(
      !is.na(`Risk Factors for Stroke in Blacks`),
      !`Risk Factors for Stroke in Blacks` %in% c(
        "Raise the Platform", "Website URL", "data source:",
        "Find more:", "E90E50", "E90E50fx"
      )
    ) %>%
    mutate(across(-1, as.numeric))

  # --- Reshape ---
  df_long <- clean_data %>%
    pivot_longer(cols = -1, names_to = "Year", values_to = "Proportion") %>%
    filter(Proportion > 0)

  df_long$Year <- as.numeric(as.character(df_long$Year))

  # --- Layout constants ---
  global_shift <- 1.0
  rect_width <- 1.8

  # --- Minimum display size for rectangles ---
  df_long <- df_long %>%
    mutate(display_prop = ifelse(Proportion < min_ribbon_size, min_ribbon_size, Proportion))

  # --- Rectangle positions ---
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

  # --- Connector positions ---
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

  # --- WHITE MASK rectangles (to carve the gap) ---
  gap_masks <- NULL
  if (horizontal_gap) {
    gap_masks <- bind_rows(
      # LEFT EDGE masks
      df_display %>%
        transmute(
          xmin = xmin - 0.0001,
          xmax = xmin + gap_width,
          ymin = ymin_rect,
          ymax = ymax_rect
        ),
      # RIGHT EDGE masks
      df_display %>%
        transmute(
          xmin = xmax - gap_width,
          xmax = xmax + 0.0001,
          ymin = ymin_rect,
          ymax = ymax_rect
        )
    )
  }

  # --- Colors ---
  stroke_colors <- c(
    "Hypertension" = "#8fbc8f",
    "Diabetes" = "#ee5c42",
    "Smoking" = "#83c6d9",
    "Hypercholesterolemia" = "#e77d22",
    "Obesity" = "#005596"
  )

  # --- Label positions ---
  first_year <- min(df_display$Year)
  label_positions <- df_display %>%
    filter(Year == first_year) %>%
    mutate(x_label = first_year - (rect_width * 0.8) + global_shift)

  # --- Plot ---
  p <- ggplot() +
    # 1️⃣ Draw connectors (full width)
    geom_polygon(
      data = connector_polygons,
      aes(x = x, y = y, group = group, fill = RiskFactor),
      color = NA
    ) +
    # 2️⃣ Draw rectangles (full width)
    geom_rect(
      data = df_display,
      aes(xmin = xmin, xmax = xmax,
          ymin = ymin_rect, ymax = ymax_rect,
          fill = `Risk Factors for Stroke in Blacks`),
      color = NA
    )

  # 3️⃣ Draw white masks on top if gap is enabled
  if (horizontal_gap) {
    p <- p +
      geom_rect(
        data = gap_masks,
        aes(xmin = xmin, xmax = xmax,
            ymin = ymin, ymax = ymax),
        fill = "white", color = "white"
      )
  }

  # 4️⃣ Add labels
  p <- p +
    geom_text(
      data = df_display,
      aes(x = (xmin + xmax) / 2,
          y = ymid_rect,
          label = sprintf("%.2f", Proportion)),
      color = "white", fontface = "bold", size = 3.4
    ) +
    geom_text(
      data = label_positions,
      aes(x = x_label, y = ymid_rect,
          label = `Risk Factors for Stroke in Blacks`),
      hjust = 1, size = 3.8, color = "black"
    ) +
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
