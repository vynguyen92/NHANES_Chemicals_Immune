calculate_weighted_detection_frequency <- function(x
                                                   , df_comments
                                                   , df_weights
                                                   , df_chem_master)
{
  print(x)
  
  chemical_codename <- df_chem_master %>%
    filter(comments_codename_use == x) %>%
    pull(chemical_codename_use) %>%
    unique(.)
  # print(chemical_codename)
  
  if(length(chemical_codename) > 1)
  {
    chemical_codename <- chemical_codename[grepl("LA$|L$", chemical_codename)]
  }

  # If chemical is measured in urine, create a label to include urinary creatinine in the subsetting
  if(str_detect(chemical_codename, "^LB") == FALSE)
  {
    urinary_measurement_tag <- TRUE
  } else {
    urinary_measurement_tag <- FALSE
  }
  # print(urinary_measurement_tag)
  
  weight_codename <- paste("WT_"
                           , chemical_codename
                           , sep = "")
  # print(weight_codename)
  

  if(!(weight_codename %in% colnames(df_weights)))
  {
    print(x)
    print(chemical_codename)
    print(weight_codename)
  }

  if(urinary_measurement_tag == TRUE)
  {
    subset_comments <- df_comments %>%
      select("SEQN"
             , x
             , "URXUCR"
             , "SDDSRVYR") %>%
      mutate(URXUCR = ifelse(URXUCR == 0, NA, URXUCR)) %>%
      left_join(.
                , df_weights %>%
                  select("SEQN"
                         , weight_codename)
                , by = "SEQN") %>%
      na.omit(.)

  } else {

    subset_comments <- df_comments %>%
      select("SEQN"
             , x
             , "SDDSRVYR") %>%
      left_join(.
                , df_weights %>%
                  select("SEQN"
                         , weight_codename)
                , by = "SEQN") %>%
      na.omit(.)
  }
  # View(subset_comments %>%
  #        unique(.))
  print(dim(subset_comments))

  index_x <- which(colnames(subset_comments) == x)

  colnames(subset_comments)[index_x] <- "comments"
  # View(subset_comments)

  index_weights <- which(colnames(subset_comments) == weight_codename)

  colnames(subset_comments)[index_weights] <- "unadjusted_weights"

  unique_cycles <- unique(subset_comments$SDDSRVYR)
  print(unique_cycles)

  cycle_length <- length(unique_cycles)
  print(cycle_length)
  
  df_problematic_pesticides <- find_problematic_pesticides(df_chem_master)
  # View(df_problematic_pesticides)
  
  if(all(c("1", "2") %in% unique_cycles) == TRUE)
  {
    if(x %in% df_problematic_pesticides$comments_codename_use)
    {
      
      cycle_cat_id <- "1 or 2"
      
    } else {
      
      cycle_cat_id <- "1 and 2"
      
    }
    
  } else if(all(c("1", "2") %in% unique_cycles) == FALSE 
            & "1" %in% unique_cycles 
            | "2" %in% unique_cycles) 
  {

    cycle_cat_id <- "1 or 2"

  } else {

    cycle_cat_id <- "other"
  }
  print(cycle_cat_id)
  
  subset_comments <- subset_comments %>%
    mutate(cycle_cat_id = cycle_cat_id) %>%
    mutate(adjusted_weights = case_when(SDDSRVYR %in% c(1,2) & cycle_cat_id == "1 and 2" ~ ((2/cycle_length)*(unadjusted_weights))
                                        , SDDSRVYR %in% c(1,2) & cycle_cat_id == "1 or 2" ~ ((1/cycle_length)*(unadjusted_weights))
                                        , SDDSRVYR %in% c(3:10) ~ (1/cycle_length)*(unadjusted_weights)))
  # View(subset_comments)

  total_people <- sum(subset_comments$adjusted_weights)
  # print(total_people)

  total_participants <- nrow(subset_comments)
  # print(total_participants)

  subset_stats_unweighted <- subset_comments %>%
    group_by(comments) %>%
    summarise(num_people = n()) %>%
    ungroup(.) %>%
    mutate(relative_to_lod = ifelse(comments %in% c(0,2)
                                    , "above"
                                    , "below")) %>%
    group_by(relative_to_lod) %>%
    summarise(num_people = sum(num_people)) %>%
    ungroup(.) %>%
    mutate(percentage = num_people/total_participants*100) %>%
    pivot_longer(!relative_to_lod
                 , names_to = "stat"
                 , values_to = "values") %>%
    mutate(comment_codename = x) %>%
    mutate(stats = paste(relative_to_lod
                         , stat
                         , "unweighted"
                         , sep = "_")) %>%
    select(comment_codename
           , stats
           , values) %>%
    pivot_wider(names_from = stats
                , values_from = values) %>%
    mutate(total_number_people_unweighted = total_participants)
  # print(subset_stats_unweighted)

  subset_stats_weighted <- subset_comments %>%
    group_by(comments) %>%
    summarise(num_people = sum(adjusted_weights)) %>%
    ungroup(.) %>%
    mutate(relative_to_lod = ifelse(comments %in% c(0,2)
                                    , "above"
                                    , "below")) %>%
    group_by(relative_to_lod) %>%
    summarise(num_people = sum(num_people)) %>%
    ungroup(.) %>%
    mutate(percentage = num_people/total_people*100) %>%
    pivot_longer(!relative_to_lod
                 , names_to = "stat"
                 , values_to = "values") %>%
    mutate(comment_codename = x) %>%
    mutate(stats = paste(relative_to_lod
                         , stat
                         , "weighted"
                         , sep = "_")) %>%
    select(comment_codename
           , stats
           , values) %>%
    pivot_wider(names_from = stats
                , values_from = values) %>%
    mutate(total_number_people_weighted = total_people)
  # print(wide_subset_stats_weighted)

  final_stats <- full_join(subset_stats_weighted
                           , subset_stats_unweighted)
  # View(final_stats)

  return(final_stats)
}