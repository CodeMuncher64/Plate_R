library(tidyverse)

plate <- expand.grid(row = LETTERS[1:8], col = 1:12, stringsAsFactors = FALSE)
MOI1 = 1
MOI2 = 2
MOI3 = 3
POS = 0
plate$well <- paste0(plate$row, plate$col)
plate$cell <- case_when(
  plate$row %in% LETTERS[1:8] & plate$col %in% 1:3 ~ "VERO",
  plate$row %in% LETTERS[1:8] & plate$col %in% 4:6 ~ "123EF",
  plate$row %in% LETTERS[1:8] & plate$col %in% 7:9 ~ "31EF",
  
)
plate$treatment <- case_when(
  plate$row %in% c('A','C','E') & plate$col %in% 1:9 ~ "LP",
  plate$row %in% c('B','D','F') & plate$col %in% 1:9 ~ "MG",
  plate$row == "G" & plate$col %in% 1:9 ~ "NT", 
  plate$row == "H" & plate$col %in% 1:9 ~ "POS" 
)
plate$MOI <- case_when(
  plate$row %in% LETTERS[1:6] & plate$col %in% c(1,4,7) ~ MOI1,
  plate$row %in% LETTERS[1:6] & plate$col %in% c(2,5,8) ~ MOI2,
  plate$row %in% LETTERS[1:6] & plate$col %in% c(3,6,9) ~ MOI3,
  plate$row %in% LETTERS[7] & plate$col %in% 1:9 ~ 0,
  plate$row %in% LETTERS[8] & plate$col %in% 1:9 ~ POS
)
plate = mutate(plate,
       cell_density <- case_when(
         row %in% LETTERS[1:6]
       ))
