get_counts_for_cu_zn  <- function(df_inclusion_criteria_stats
                                  , nhanes_subset
                                  , df_weights
                                  , demographics)
{
  nhanes_subset <- left_join(nhanes_subset
                             , df_weights)

  #############################################################################################################
  ################################################## for copper ###############################################
  #############################################################################################################
  
  subset_cu <- nhanes_subset %>%
    select(SEQN
           , SDDSRVYR
           , LBXSCU
           , all_of(demographics)
           , WTMEC2YR) %>%
    na.omit(.)
  # print(colnames(subset_cu))
  
  unique_cycles <- unique(subset_cu$SDDSRVYR)
  # print(unique_cycles)
  
  cycle_length <- length(unique_cycles)
  
  subset_cu <- subset_cu %>%
    mutate(adjusted_weight = (1/cycle_length)*WTMEC2YR)
  
  #get measurement counts for copper
  total_counts <- length(subset_cu$LBXSCU)
  miss_cu <- sum(is.na(subset_cu$LBXSCU))
  total_cu <- total_counts - miss_cu
  # print(total_cu)
  
  total_weights <- sum(subset_cu$adjusted_weight)
  # print(total_weights)
  
  subset_psu_strata <- subset_cu %>%
    select("SDMVSTRA"
           , "SDMVPSU") %>%
    unique(.)
  
  num_psu <- nrow(subset_psu_strata)
  # print(num_psu)
  
  num_strata <- subset_psu_strata %>%
    pull(SDMVSTRA) %>%
    unique(.) %>%
    length(.)
  # print(num_strata)
  
  degrees_of_freedom <- num_psu - num_strata
  
  df_inclusion_criteria_stats <- df_inclusion_criteria_stats %>%
    add_row(chemical_codename_use = "LBXSCU"
            , chemical_name = "Serum Copper (ug/dL)"
            , comment_codename = NA
            , above_num_people_weighted = total_weights
            , above_percentage_weighted = 100     
            , below_num_people_weighted = 0
            , below_percentage_weighted = 0    
            , total_number_people_weighted = total_weights
            , above_num_people_unweighted = total_cu
            , above_percentage_unweighted = 100   
            , below_num_people_unweighted = 0
            , below_percentage_unweighted = 0
            , total_number_people_unweighted = total_cu
            , degrees_of_freedom = degrees_of_freedom
            , num_psu = num_psu
            , num_strata = num_strata
            , chem_family = "Metals"
            , chem_family_shortened = "Metals"
            )
  
  #############################################################################################################
  ################################################## for zinc ###############################################
  #############################################################################################################
  
  subset_zn <- nhanes_subset %>%
    select(SEQN
           , SDDSRVYR
           , LBXSZN
           , all_of(demographics)
           , WTMEC2YR) %>%
    na.omit(.)
  # print(colnames(subset_zn))
  
  unique_cycles <- unique(subset_zn$SDDSRVYR)
  # print(unique_cycles)
  
  cycle_length <- length(unique_cycles)
  
  subset_zn <- subset_zn %>%
    mutate(adjusted_weight = (1/cycle_length)*WTMEC2YR)
  
  #get measurement counts for copper
  total_counts <- length(subset_zn$LBXSZN)
  miss_zn <- sum(is.na(subset_zn$LBXSZN))
  total_zn <- total_counts - miss_zn
  # print(total_zn)
  
  total_weights <- sum(subset_zn$adjusted_weight)
  # print(total_weights)
  
  subset_psu_strata <- subset_zn %>%
    select("SDMVSTRA"
           , "SDMVPSU") %>%
    unique(.)
  
  num_psu <- nrow(subset_psu_strata)
  # print(num_psu)
  
  num_strata <- subset_psu_strata %>%
    pull(SDMVSTRA) %>%
    unique(.) %>%
    length(.)
  # print(num_strata)
  
  degrees_of_freedom <- num_psu - num_strata
  
  df_inclusion_criteria_stats <- df_inclusion_criteria_stats %>%
    add_row(chemical_codename_use = "LBXSZN"
            , chemical_name = "Serum Zinc (ug/dL)"
            , comment_codename = NA
            , above_num_people_weighted = total_weights
            , above_percentage_weighted = 100     
            , below_num_people_weighted = 0
            , below_percentage_weighted = 0    
            , total_number_people_weighted = total_weights
            , above_num_people_unweighted = total_zn
            , above_percentage_unweighted = 100   
            , below_num_people_unweighted = 0
            , below_percentage_unweighted = 0
            , total_number_people_unweighted = total_zn
            , degrees_of_freedom = degrees_of_freedom
            , num_psu = num_psu
            , num_strata = num_strata
            , chem_family = "Metals"
            , chem_family_shortened = "Metals"
    )
  
  return(df_inclusion_criteria_stats)
}