#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#########################################  Adjusted Linear Regression  ########################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function runs the adjusted linear regression for all chemicals and all immune measures
#          
# Inputs: long_nhanes_subset - long dataframe containing complete demographic and cognitive data for each
#                              participant
#         conversion - dataframe of chemical names, codenames, and families
#
# Outputs: model_stats_adjusted - dataframe of linear regression outputs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

run_linear_regression <- function(long_nhanes_subset,
                                  conversion,
                                  weights_dataset,
                                  nhanes_subset)
{
  library(tidyverse)
  
  #TEMPORARY
  # long_nhanes_subset <- long_nhanes_subset_dataset
  # conversion <- use_these_chems
  
  long_nhanes_subset$chemical_codename <- as.character(long_nhanes_subset$chemical_codename)
  
  long_nhanes_subset <- long_nhanes_subset %>%
   mutate(chem_copy = chemical_codename) 
  
  

  #run linear regressions
  # print("run reg")
  df_regressions_i <- long_nhanes_subset %>%
    # filter(chemical_codename %in% c("LBXBPB")) %>%
    # filter(celltype_codename %in% c("LBXLYPCT")) %>%
    group_by(celltype_codename, chemical_codename) %>%
    do(run_if_else_glm_weighted(.,
                                weights_dataset,
                                nhanes_subset)) %>% #the dot is each group by chunk - pulling from chemical_immune_chunk
    ungroup(.)
  # View(df_regressions_i)
  
  #############################################################################################################
  ############################################## RENAME CHEMICALS #############################################
  #############################################################################################################
  
  conversion_subset <- conversion %>%
    dplyr::select(chemical_codename_use,
                  chem_family,
                  chemical_name) %>%
    distinct(chemical_codename_use, .keep_all = TRUE) %>%
    rename(chemical_codename = chemical_codename_use)

  #merge in the chemical_names
  df_regressions_names <- left_join(df_regressions_i, conversion_subset, by = "chemical_codename")

  df_regressions_names <- df_regressions_names %>%
    mutate(immune_measure = case_when(celltype_codename == "LBXLYPCT" ~ "Lymphocytes (%)",
                                      celltype_codename == "LBXNEPCT" ~ "Neutrophils (%)",
                                      celltype_codename == "LBXMOPCT" ~ "Monocytes (%)",
                                      celltype_codename == "LBXBAPCT" ~  "Basophils (%)",
                                      celltype_codename == "LBXEOPCT" ~ "Eosinophils (%)",
                                      celltype_codename == "LBXWBCSI" ~ "WBC (1000 cells/uL)",
                                      celltype_codename == "LBXRBCSI" ~ "RBC (million cells/uL)",
                                      celltype_codename == "LBXMCVSI" ~ "Mean Corpuscular Volume (fL)"))
  
  #############################################################################################################
  ###################################### ADD CONFIDENCE INTERVALS AND FDR #####################################
  #############################################################################################################

  #calculate the unscaled confidence intervals
  z_score <- 1.96
  model_stats_CI_unscaled <- df_regressions_names %>%
    mutate(lower.CI = estimate - (z_score*std.error),
           upper.CI = estimate + (z_score*std.error)) %>%
    group_by(celltype_codename) %>%
    mutate(FDR = p.adjust(p.value, method = "fdr")) %>%
    ungroup()

  model_stats_adjusted <- model_stats_CI_unscaled %>%
    dplyr::select(chemical_name,
                  chemical_codename,
                  chem_family,
                  immune_measure,
                  celltype_codename,
                  term,
                  estimate,
                  std.error,
                  lower.CI,
                  upper.CI,
                  statistic,
                  p.value,
                  FDR)
  # View(model_stats_adjusted)


  return(model_stats_adjusted)
}