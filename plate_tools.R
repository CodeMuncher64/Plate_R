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
  janitor::clean_names(dataframe)
  vars <- names(dataframe[3:ncol(dataframe)])
  vars <- if ('well' == vars[1]|'wells' == vars[1]) vars[-1] else vars
  print(vars)
  dataframe <- mutate(dataframe, label = do.call(paste, c(plate[vars], sep = "\n")
  ))
  ggplot(dataframe, aes(
    x = factor(col),
    y = factor(row, levels = rev(LETTERS[1:8]))
  ))+
    geom_tile(aes_string(fill = vars[1], alpha = vars[3]), color = 'black', linewidth = 0.15) + scale_fill_brewer(palette = "Set2", na.value = "white", na.translate = FALSE) +
    geom_text(aes_string(label = 'label', color = vars[2]), size = 2.8, lineheight = 0.8, fontface = "bold") +
    scale_color_brewer(palette = "Set1")+
    #scale_alpha_continuous(range = c(0.2, 1), breaks = 0:3, na.value = 1) +
    coord_fixed() +
    labs(x = NULL, y = NULL, fill = vars[1], alpha = vars[3]) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    theme(panel.grid = element_blank(),
          axis.text  = element_text(size = 12, face = "bold")
    )
}