calculate_degrees_of_freedom <- function(x
                                         , df_nhanes)
{
  print(x)
  
  subset_nhanes <- df_nhanes %>%
    select(x
           , "SDMVSTRA"
           , "SDMVPSU"
           , "SDDSRVYR") %>%
    na.omit(.)
  # View(subset_nhanes)
  
  subset_psu_strata <- subset_nhanes %>%
    select("SDMVSTRA"
           , "SDMVPSU") %>%
    unique(.)
  # View(subset_psu_strata)
  
  num_psu <- nrow(subset_psu_strata)
  # print(num_psu)
  
  num_strata <- subset_psu_strata %>%
    pull(SDMVSTRA) %>%
    unique(.) %>%
    length(.)
  # print(num_strata)
  
  degrees_of_freedom <- num_psu - num_strata
  # print(degrees_of_freedom)
  
  df_stats <- data.frame(chemical_codename_use = x
                         , degrees_of_freedom = degrees_of_freedom
                         , num_psu = num_psu
                         , num_strata = num_strata)
  return(df_stats)
  
}