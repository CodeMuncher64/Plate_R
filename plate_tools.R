library(tidyverse)
library(ggplot2)

plate_simple <- function(dataframe) {
  dataframe |>
    mutate(
      label = do.call(
        paste,
        c(as.list(pick(3:last_col())), sep = "\n")
      )
    ) |>
    ggplot(aes(
      x = factor(col),
      y = factor(row, levels = rev(LETTERS[1:8]))
    )) +
    geom_text(aes(label = label), size = 2.8, lineheight = 0.9) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 12, face = "bold")
    )
}

plate_3cols <- function(dataframe){
  dataframe <- janitor::clean_names(dataframe)                          
  if (names(dataframe)[1] %in% c('X', 'x')) dataframe[[1]] <- NULL
  vars <- names(dataframe[3:ncol(dataframe)])
  vars <- if ('well' == vars[1]|'wells' == vars[1]) vars[-1] else vars
  while (length(vars) < 3) {
    name <- paste0("empty", length(vars) + 1)
    dataframe[[name]] <- NA
    vars <- c(vars, name)
  }
  dataframe <- dataframe |> 
    mutate(label = if_else(is.na(.data[[vars[1]]]), '', paste(.data[[vars[1]]],.data[[vars[2]]],.data[[vars[3]]], sep = "\n")),
           !!vars[1] :=  factor(.data[[vars[1]]], ordered = TRUE),
           !!vars[2] :=  factor(.data[[vars[2]]], ordered = TRUE),
           !!vars[3] :=  factor(.data[[vars[3]]], ordered = TRUE),
           row = fct_rev(factor(row, order = TRUE))
    )
  ggplot(dataframe, aes(
    x = factor(col),
    y = row
  ))+
    geom_tile(aes_string(fill = vars[1], alpha = vars[3]), color = 'black', linewidth = 0.15) + scale_fill_brewer(palette = "Set2", na.value = "white", na.translate = FALSE) +
    scale_alpha_discrete(na.value = 0.5, na.translate = FALSE)+
    geom_text(aes_string(label = 'label', color = vars[2]), size = 2.8, lineheight = 0.8, fontface = "bold") +
    scale_color_brewer(palette = "Set1", na.value = 'black', na.translate = FALSE)+
    coord_fixed() +
    labs(x = NULL, y = NULL, fill = vars[1], alpha = vars[3]) +
    theme_minimal() +
    scale_x_discrete(limits = 1:12, position = "top") +
    theme(panel.grid = element_blank(),
          axis.text  = element_text(size = 12, face = "bold")
    )
}
