# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "check-df-cols.R")))

# Functions ----




#calculate the costs of the personal measures bough
append_personalmeasure_calculated_costs <- function(pm_df, sum_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable COST_ABSOLUTE_COL not found in R/constants.R" = exists(deparse(substitute(COST_ABSOLUTE_COL))),
            "Default variable PERCENTAGE_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_INCOME_COL))),
            "Default variable PERCENTAGE_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_HOUSE_COL))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable PERCENTAGE_FACTOR not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_FACTOR))),
            "Default variable LAST_PRICE_COL not found in R/constants.R" = exists(deparse(substitute(LAST_PRICE_COL))),
  )
   
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(pm_df, c(PERCENTAGE_INCOME_COL, PERCENTAGE_HOUSE_COL, ROUND_INCOME_COL, LAST_PRICE_COL))
  
  
  ## Calculate costs by summing absolute costs, amount of income and house-related costs 
  pm_df <- pm_df %>%
    mutate(
      !!sum_col :=
        rowSums(
          cbind(
            .data[[COST_ABSOLUTE_COL]],
            (.data[[PERCENTAGE_INCOME_COL]] / PERCENTAGE_FACTOR) * .data[[ROUND_INCOME_COL]],
            (.data[[PERCENTAGE_HOUSE_COL]] / PERCENTAGE_FACTOR) * .data[[LAST_PRICE_COL]]
          ),
          na.rm = TRUE
        )
    )
  
  return(pm_df)
}


## Create personalmeasure_cumulative_df 
create_personalmeasure_cumulative_df <- function(pm_df) {

  ## Check constants used in calculation exist
  stopifnot("Default variable PLAYER_CODE_COL not found in R/constants.R" = exists(deparse(substitute(PLAYER_CODE_COL))),
            "Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_NUMBER_COL))),
            "Default variable CALCULATED_COSTS_PERSONAL_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COSTS_PERSONAL_COL))),
            "Default variable COST_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(COST_HOUSE_COL))),
            "Default variable PERSONAL_HOUSE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(PERSONAL_HOUSE_DIFFCOL))),
            "Default variable CUMULATIVE_COSTS_PERSONAL_COL not found in R/constants.R" = exists(deparse(substitute(CUMULATIVE_COSTS_PERSONAL_COL))),
            "Default variable CUMULATIVE_PERSONAL_HOUSE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(CUMULATIVE_PERSONAL_HOUSE_DIFFCOL)))
  )
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(pm_df, c(CALCULATED_COSTS_PERSONAL_COL, COST_HOUSE_COL))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  check_char_cols(pm_df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  
  
  #calculate the cumulative of the personal measures to compare it against the cost of house measures bought
  pmc_df <- pm_df %>%
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    arrange(across(all_of(PLAYER_CODE_COL, ROUND_NUMBER_COL))) %>%   
    group_by(across(all_of(PLAYER_CODE_COL, ROUND_NUMBER_COL))) %>%
    
    #add up CALCULATED_COSTS_PERSONAL_COL within each round for each player and keep COST_HOUSE_COL value
    summarise(!!CALCULATED_COSTS_PERSONAL_COL := sum(.data[[CALCULATED_COSTS_PERSONAL_COL]]),
              !!COST_HOUSE_COL := first(.data[[COST_HOUSE_COL]]),
              .groups = "drop"
    ) %>%
    
    #ensure cumulative totals are calculated separately for each player
    mutate(
      !!PERSONAL_HOUSE_DIFFCOL := .data[[CALCULATED_COSTS_PERSONAL_COL]] - .data[[COST_HOUSE_COL]]
    ) %>%
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    group_by(.data[[PLAYER_CODE_COL]]) %>%
    arrange(.data[[ROUND_NUMBER_COL]]) %>%
    
    # compute the running total across rounds
    mutate(
      !!CUMULATIVE_COSTS_PERSONAL_COL     := cumsum(CALCULATED_COSTS_PERSONAL_COL),
      !!CUMULATIVE_PERSONAL_HOUSE_DIFFCOL := cumsum(.data[[CALCULATED_COSTS_PERSONAL_COL]] - .data[[COST_HOUSE_COL]])
    )
  
  return(pmc_df)
}


## Create housemeasure_cumulative_df
retrieve_housemeasure_cumulative <- function(hm_df) {
  
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
  check_char_cols(hm_df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))

  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are logical
  check_logical_cols(hm_df, IS_IHM_COL)
  
  #calculate the cumulative of the house measures to compare it against the cost of house measures bought
  #exclude the costs of the housemeasures that came implemented in the house when bought
  hmc_df <- hm_df %>%
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    arrange(across(all_of(PLAYER_CODE_COL, ROUND_NUMBER_COL))) %>%   
    group_by(across(all_of(PLAYER_CODE_COL, ROUND_NUMBER_COL))) %>%
    
    #add up costs within each round for each player
    summarise(
      
      # sum only cost_absolute where initialhousemeasure == FALSE
      !!CALCULATED_COSTS_HOUSE_COL := sum(ifelse(.data[[IS_IHM_COL]], 0, .data[[COST_ABSOLUTE_COL]])),
      
      # keep the round’s value
      !!TOTAL_BOUGHT_COL := first(.data[[COST_HOUSE_COL]]),
      .groups = "drop"
    ) %>%
    
    #ensure cumulative totals are calculated separately for each player
    mutate(
      !!HOUSE_TOTAL_DIFFCOL := .data[[CALCULATED_COSTS_HOUSE_COL]] - .data[[TOTAL_BOUGHT_COL]]
    ) %>%
    
    # Sort and group data frame by PLAYER_CODE_COL and ROUND_NUMBER_COL
    group_by(.data[[PLAYER_CODE_COL]]) %>%
    arrange(.data[[ROUND_NUMBER_COL]]) %>%
    
    # compute the running total across rounds
    mutate(
      CUMULATIVE_COSTS_HOUSE_COL     := cumsum(.data[[CALCULATED_COSTS_HOUSE_COL]]),
      CUMULATIVE_HOUSE_TOTAL_DIFFCOL := cumsum(.data[[CALCULATED_COSTS_HOUSE_COL]] - .data[[TOTAL_BOUGHT_COL]])
    )
  
  return(hmc_df)
}

## append human‑readable ordered categories matching numeric welfare IDso 
append_welfare_labels <- function(pr_df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable WELFARE_LABELS not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABELS))),
            "Default variable WELFARE_ID_COL not found in R/constants.R" = exists(deparse(substitute(WELFARE_ID_COL))),
            "Default variable WELFARE_LABEL_COL not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABEL_COL)))
  )
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  check_char_cols(pr_df, WELFARE_ID_COL)
  
  # Save unique welfare ids. ids sorted ascendingly, as in WELFARE_LABELS match
  welfare_ids <- sort(unique(as.character(pr_df[, WELFARE_ID_COL])))
  
  
  ## Only if there are exactly six distinct IDs. Otherwise, it warns you that the mapping isn’t valid.
  if (identical(unname(WELFARE_LABELS) %in% welfare_ids)) {
    
      pr_df <- pr_df %>%
        mutate(
          !!WELFARE_LABEL_COL := factor(WELFARE_LABELS[match(.data[[WELFARE_ID_COL]], welfare_ids)],
                                        levels = WELFARE_LABELS,
                                        ordered = TRUE
          )
        )
    
  } else {
    
    warning(paste0("Expected the following welfaretype_id value: ",
                   paste(WELFARE_LABELS, collapse = ", "),
                   ". Instead the following values were found: ",
                   paste(welfare_ids, collapse = ", "),
                   "."))
  }
  
  return(pr_df)
}

## Append difference between reported and calculated measures
append_reported_calculatedcosts_difference <- function(df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable CALCULATED_COLS not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COLS))),
            "Default variable CALCULATED_COSTS_DIFF not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COSTS_DIFF))))
            
  ## Check at least one calculated cost is not missing
  missing_all <- report_missing_cols(df, CALCULATED_COLS, CALCULATED_COSTS_DIFF)
  
  ## Check the respective reported cost(s) is/are also not missing
  missing_all <- missing_all * report_missing_cols(df, names(CALCULATED_COLS), CALCULATED_COSTS_DIFF)
  
  ## Proceed in case of at least one pair of reported-calculated cost is available, 
  if(missing_all == FALSE) {
    
    ## filter complete reported-calculated cost pairs and calculate difference between those
    col_cross <- as.logical(names(CALCULATED_COLS) %in% names(df) * names(CALCULATED_COLS) %in% names(df))
    
    calc_cols <- CALCULATED_COLS[col_cross]
    repor_cols <- names(CALCULATED_COLS)[col_cross]
    
    ## Check numeric columns are defined as such
    check_num_cols(df, c(repor_cols, calc_cols))
    
    ## calculate difference between reported-calculated cost pairs
    df[, CALCULATED_COSTS_DIFF] <-
      rowSums(df[names(df) %in% repor_cols], na.rm = TRUE) - rowSums(df[names(df) %in% calc_cols], na.rm = TRUE)
  }
  
  return(df)
}
  

## Append pluvial + fluvial costs as total_damage
calculate_total_damage_costs <- function(df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable TYPE_COST_COLS not found in R/constants.R" = exists(deparse(substitute(TYPE_COST_COLS))),
            "Default variable TOTAL_DAMAGE_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_DAMAGE_COL))))
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  # Check at least one element of TYPE_COST_COLS is not missing in df
  missing_all <- report_missing_cols(df, TYPE_COST_COLS, TOTAL_DAMAGE_COL)
  
  # In case at least one element of TYPE_COST_COLS is not missing, check the columns are numeric and calculate their sum
  if(missing_all == FALSE) {
    
    ## Check numeric columns are defined as such
    check_num_cols(df, TYPE_COST_COLS[TYPE_COST_COLS %in% names(df)])
    
    df[, TOTAL_DAMAGE_COL] <- rowSums(df[names(df) %in% TYPE_COST_COLS], na.rm = TRUE)
  }
  
  return(df)
}


# Calculate the round costs to check the spendable income
# "paid_debt" not used in the calculations because is taken already when the spendable income comes as a negative value
# If either column has NA, the sum will also be NA unless the sum is done this way

append_calculate_total_costs <- function(df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable ALL_COST_COLS not found in R/constants.R" = exists(deparse(substitute(ALL_COST_COLS))),
            "Default variable TOTAL_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_COSTS_COL))))
  
  # Check at least one element of ALL_COST_COLS is not missing in df
  missing_all <- report_missing_cols(df, ALL_COST_COLS, TOTAL_COSTS_COL)
  
  # In case at least one element of ALL_COST_COLS is not missing, check columns are numeric and calculate their sum
  if(missing_all == FALSE) {
    
    check_num_cols(df, ALL_COST_COLS[ALL_COST_COLS %in% names(df)])
    
    df[, TOTAL_COSTS_COL] <- rowSums(df[names(df) %in% ALL_COST_COLS], na.rm = TRUE) 
  }
  
  return(df)
}
  
# Calculate the spendable income

calculate_spendable_income <- function(df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable PLAYER_CODE_COL not found in R/constants.R" = exists(deparse(substitute(PLAYER_CODE_COL))),
            "Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_NUMBER_COL))),
            "Default variable SPENDABLE_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(SPENDABLE_INCOME_COL))),
            "Default variable CALCULATED_SPENDABLE_COL not found in R/constants.R" = exists(deparse(substitute(CALCULATED_SPENDABLE_COL))),
            "Default variable SPENDABLE_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(SPENDABLE_DIFFCOL))))
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  check_char_cols(df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(df, SPENDABLE_INCOME_COL)
  
  # Check that players found match those expected
  expected_players <- unique(df[, PLAYER_CODE_COL])
  
  found_players <- df %>% filter(ROUND_NUMBER_COL %in% 0) %>% pull(PLAYER_CODE_COL)
  
  df <- df %>%
    arrange(across(all_of(PLAYER_CODE_COL, ROUND_NUMBER_COL))) %>%
    mutate(!!CALCULATED_SPENDABLE_COL := .data[[SPENDABLE_INCOME_COL]])
  
  # mismatch between found and expected players stops run, otherwise columns and CALCULATED_SPENDABLE_COL and SPENDABLE_DIFFCOL are calculated
  if (any(expected_players %in% found_players) == FALSE) {
    
    stop(paste("Missing Round Number 0 value detected for players", paste(expected_players[expected_players %in% found_players == FALSE], collapse = ", ")))
  
  } else {
    
    df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, CALCULATED_SPENDABLE_COL] <-
      rowSums(cbind(df[which(df[, ROUND_NUMBER_COL] %in% 0 == FALSE) - 1, CALCULATED_SPENDABLE_COL],
                    df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, ROUND_INCOME_COL],
                    df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, PROFIT_HOUSE_COL],
                    -df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, TOTAL_COSTS_COL]), na.rm = TRUE)
    
    df[, SPENDABLE_DIFFCOL] <- df[, SPENDABLE_INCOME_COL] - df[, CALCULATED_SPENDABLE_COL]
  }
  
  return(df)
}

## Append income_grp labels based on round_income to dataframe
append_income_grp <- function(df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable WELFARE_LABELS not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABELS))),
            "Default variable INCOME_GRP_COL not found in R/constants.R" = exists(deparse(substitute(INCOME_GRP_COL))),
            "Default variable K_FACTOR not found in R/constants.R" = exists(deparse(substitute(K_FACTOR))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))))
  
  ## Check data frame is in the expected format and columns to which constants refer in calculation exist
  check_df_cols(df, ROUND_INCOME_COL)
  
  # Save unique income labels. labels sorted ascendingly, as in WELFARE_LABELS match
  income_labels <- sort(unique(paste0(df[, ROUND_INCOME_COL] / K_FACTOR, names(K_FACTOR))))
  
  # append income groups based on ROUND_INCOME_COL values
  df <- df %>%
    mutate(!!INCOME_GRP_COL := factor(paste0(.data[[ROUND_INCOME_COL]] / K_FACTOR, names(K_FACTOR)),
                                      levels = income_labels,
                                      ordered = TRUE))
  
  # if income_labels does not match names(WELFARE_LABELS), issue warning
  if (identical(income_labels, names(WELFARE_LABELS)) == FALSE) {
    
    warning(paste0("Expected the following income_grp labels: ",
                   paste(names(WELFARE_LABELS), collapse = ", "),
                   ". Instead the following values were found: ",
                   paste(income_labels, collapse = ", "),
                   "."))
  }
  
  return(df)
  
}

## Calculate income - living costs
append_income_living_diff <- function(df) {

  ## Check constants used in calculation exist
  stopifnot("Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable LIVING_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(LIVING_COSTS_COL))),
            "Default variable INCOME_LIVING_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(INCOME_LIVING_DIFFCOL))))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(df, c(ROUND_INCOME_COL, LIVING_COSTS_COL))
  
  df[, INCOME_LIVING_DIFFCOL] <- rowSums(cbind(df[, ROUND_INCOME_COL],
                                               -df[ ,LIVING_COSTS_COL]), na.rm = TRUE)
  
  return(df)
}


## Calculate  "profit - spent savings house moving"
append_housemoving_diff <- function(df) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable PROFIT_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(PROFIT_HOUSE_COL))),
            "Default variable SPENT_SAVINGS_COL not found in R/constants.R" = exists(deparse(substitute(SPENT_SAVINGS_COL))),
            "Default variable HOUSEMOVING_DIFFCOL not found in R/constants.R" = exists(deparse(substitute(HOUSEMOVING_DIFFCOL))))
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(df, c(PROFIT_HOUSE_COL, SPENT_SAVINGS_COL))
  
  df[, HOUSEMOVING_DIFFCOL] <- rowSums(cbind(df[, PROFIT_HOUSE_COL],
                                             -df[, SPENT_SAVINGS_COL]), na.rm = TRUE)
  
  return(df)
}

