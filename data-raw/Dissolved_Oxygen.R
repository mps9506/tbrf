library(dplyr)

Dissolved_Oxygen <- readr::read_delim(file = here::here("data/DO.txt"), delim = "|" ) %>%
  mutate(`End Date` = as.Date(`End Date`, "%m/%d/%Y")) %>%
  group_by(`Station ID`, `End Date`, `Parameter Code`, `Parameter Description`) %>%
  summarise(Average_DO = mean(Value),
            Min_DO = min(Value)) %>%
  rename(Station_ID = `Station ID`,
         Date = `End Date`,
         Param_Code = `Parameter Code`,
         Param_Desc = `Parameter Description`)

devtools::use_data(Dissolved_Oxygen)
