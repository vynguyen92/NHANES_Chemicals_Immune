find_problematic_pesticides <- function(df_master_chemicals)
{
  df_master_prob_pesticides <- df_master_chemicals %>%
    filter(weight_codename == "WTSPP4YR")
  
  chems_not_problematic <- df_master_prob_pesticides %>%
    filter(remove_cycle_changes_in_LOD == 1) %>%
    pull(chemical_codename_use) %>%
    unique(.)
  # print(chems_not_problematic)
  
  df_master_prob_pesticides <- df_master_prob_pesticides %>%
    filter(!(chemical_codename_use %in% chems_not_problematic))
  
  pesticides_with_1_cycle <- df_master_prob_pesticides %>%
    group_by(chemical_codename_use) %>%
    summarise(count = n()) %>%
    ungroup(.) %>%
    filter(count == 1) %>%
    pull(chemical_codename_use)
  # print(pesticides_with_1_cycle)
  
  df_master_prob_pesticides <- df_master_prob_pesticides %>%
    filter(!(chemical_codename_use %in% pesticides_with_1_cycle))
  # View(df_master_prob_pesticides)
  
  return(df_master_prob_pesticides)
}