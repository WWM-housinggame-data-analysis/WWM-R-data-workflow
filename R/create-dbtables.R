# R/create-dbtables.R
# ---------------------------------------------------------------
# Set defaults ----
# ---------------------------------------------------------------

## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "check-df-cols.R")))


# ---------------------------------------------------------------
# Functions ----
# ---------------------------------------------------------------

## Create personalmeasure_cumulative_df 
create_personalmeasure_cumulative_df <- function(pm_df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable PLAYER_CODE_COL not found in R/constants.R" = exists(deparse(substitute(PLAYER_CODE_COL))),
            "Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_NUMBER_COL))),
            "Default variable CALCULATED_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COSTS_COL))),
            "Default variable CALCULATED_COSTS_PERSONAL_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COSTS_PERSONAL_COL))),
            "Default variable COST_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(COST_HOUSE_COL))),
            "Default variable PERSONAL_HOUSE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(PERSONAL_HOUSE_DIFFCOL))),
            "Default variable CUMULATIVE_COSTS_PERSONAL_COL not found in R/constants.R" = exists(deparse(substitute(CUMULATIVE_COSTS_PERSONAL_COL))),
            "Default variable CUMULATIVE_PERSONAL_HOUSE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(CUMULATIVE_PERSONAL_HOUSE_DIFFCOL)))
  )
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(pm_df, c(CALCULATED_COSTS_COL, COST_HOUSE_COL))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  #check_char_cols(pm_df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  check_df_cols(pm_df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  
  ## calculate the cumulative of the personal measures to compare it against the cost of house measures bought
  pmc_df <- pm_df |>
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    dplyr::arrange(dplyr::across(tidyselect::all_of(c(PLAYER_CODE_COL, ROUND_NUMBER_COL)))) |>   
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(PLAYER_CODE_COL, ROUND_NUMBER_COL)))) |>
    
    #add up CALCULATED_COSTS_PERSONAL_COL within each round for each player and keep COST_HOUSE_COL value
    #summarise(!!CALCULATED_COSTS_PERSONAL_COL := sum(.data[[CALCULATED_COSTS_PERSONAL_COL]]),
    #          !!COST_HOUSE_COL := first(.data[[COST_HOUSE_COL]]),
    dplyr::summarise(!!CALCULATED_COSTS_PERSONAL_COL := sum(.data[[CALCULATED_COSTS_COL]]),
              !!TOTAL_BOUGHT_COL := dplyr::first(.data[[COST_HOUSE_COL]]),
              .groups = "drop"
    ) |>
    
    #ensure cumulative totals are calculated separately for each player
    dplyr::mutate(
      !!PERSONAL_HOUSE_DIFFCOL := .data[[CALCULATED_COSTS_PERSONAL_COL]] - .data[[TOTAL_BOUGHT_COL]]
    ) |>
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    dplyr::group_by(.data[[PLAYER_CODE_COL]]) |>
    dplyr::arrange(.data[[ROUND_NUMBER_COL]]) |>
    
    # compute the running total across rounds
    # mutate(
    #   !!CUMULATIVE_COSTS_PERSONAL_COL     := cumsum(CALCULATED_COSTS_PERSONAL_COL),
    #   !!CUMULATIVE_PERSONAL_HOUSE_DIFFCOL := cumsum(.data[[CALCULATED_COSTS_PERSONAL_COL]] - .data[[COST_HOUSE_COL]])
    
    dplyr::mutate(
      !!CUMULATIVE_COSTS_PERSONAL_COL     := cumsum(.data[[CALCULATED_COSTS_PERSONAL_COL]]),
      !!CUMULATIVE_PERSONAL_HOUSE_DIFFCOL := cumsum(.data[[PERSONAL_HOUSE_DIFFCOL]])
    )
  
  return(pmc_df)
}


## Create housemeasure_cumulative_df
create_housemeasure_cumulative_df <- function(hm_df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable PLAYER_CODE_COL not found in R/constants.R" = exists(deparse(substitute(PLAYER_CODE_COL))),
            "Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_NUMBER_COL))),
            "Default variable COST_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(COST_HOUSE_COL))),
            "Default variable COST_ABSOLUTE_COL not found in R/constants.R" = exists(deparse(substitute(COST_ABSOLUTE_COL))),
            "Default variable IS_IHM_COL not found in R/constants.R" = exists(deparse(substitute(IS_IHM_COL))),
            "Default variable CALCULATED_COSTS_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COSTS_HOUSE_COL))),
            "Default variable TOTAL_BOUGHT_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_BOUGHT_COL))),
            "Default variable HOUSE_TOTAL_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(HOUSE_TOTAL_DIFFCOL))),
            "Default variable CUMULATIVE_COSTS_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(CUMULATIVE_COSTS_HOUSE_COL))),
            "Default variable CUMULATIVE_HOUSE_TOTAL_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(CUMULATIVE_HOUSE_TOTAL_DIFFCOL)))
  )
  
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(hm_df, c(COST_ABSOLUTE_COL, COST_HOUSE_COL))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  #check_char_cols(hm_df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  check_df_cols(hm_df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are logical
  #check_logical_cols(hm_df, IS_IHM_COL)
  check_num_cols(hm_df, IS_IHM_COL)
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
  hmc_df <- hm_df |>
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    dplyr::arrange(dplyr::across(tidyselect::all_of(c(PLAYER_CODE_COL, ROUND_NUMBER_COL)))) |>   
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(PLAYER_CODE_COL, ROUND_NUMBER_COL)))) |>
    
    #add up costs within each round for each player
    dplyr::summarise(
      
      # sum only cost_absolute where initialhousemeasure == FALSE
      !!CALCULATED_COSTS_HOUSE_COL := sum(ifelse(.data[[IS_IHM_COL]], 0, .data[[COST_ABSOLUTE_COL]])),
      
      # keep the round’s value
      !!TOTAL_BOUGHT_COL := dplyr::first(.data[[COST_HOUSE_COL]]),
      .groups = "drop"
    ) |>
    
    #ensure cumulative totals are calculated separately for each player
    dplyr::mutate(
      !!HOUSE_TOTAL_DIFFCOL := .data[[CALCULATED_COSTS_HOUSE_COL]] - .data[[TOTAL_BOUGHT_COL]]
    ) |>
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    dplyr::group_by(.data[[PLAYER_CODE_COL]]) |>
    dplyr::arrange(.data[[ROUND_NUMBER_COL]]) |>
    
    # compute the running total across rounds
    dplyr::mutate(
      !!CUMULATIVE_COSTS_HOUSE_COL     := cumsum(.data[[CALCULATED_COSTS_HOUSE_COL]]),
      !!CUMULATIVE_HOUSE_TOTAL_DIFFCOL := cumsum(.data[[HOUSE_TOTAL_DIFFCOL]])
    )
  
  return(hmc_df)
}
