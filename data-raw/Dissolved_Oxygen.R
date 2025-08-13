library(dplyr)

Dissolved_Oxygen <- readr::read_delim(file = here::here("data-raw/DO.txt"), delim = "|" ) %>%
  mutate(`End Date` = as.Date(`End Date`, "%m/%d/%Y")) %>%
  group_by(`Station ID`, `End Date`, `Parameter Code`, `Parameter Description`) %>%
  summarise(Average_DO = mean(Value),
            Min_DO = min(Value)) %>%
  ungroup() %>%
  rename(Station_ID = `Station ID`,
         Date = `End Date`,
         Param_Code = `Parameter Code`,
         Param_Desc = `Parameter Description`)

usethis::use_data(Dissolved_Oxygen, overwrite = TRUE)


Entero <- readr::read_delim(file = here::here("data-raw/Entero.txt"), delim = "|" ) |> 
  mutate(`End Date` = as.Date(`End Date`, "%m/%d/%Y"),
         `Station ID` = as.factor(`Station ID`)) |> 
  select(Station_ID = `Station ID`,
         Date = `End Date`,
         Param_Code = `Parameter Code`,
         Param_Desc = `Parameter Description`,
         Value = Value)

usethis::use_data(Entero, overwrite = TRUE)
