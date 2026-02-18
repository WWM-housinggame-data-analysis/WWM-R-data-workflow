# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here(file.path(FUNCTION_PATH, "constants.R")))
source(here(file.path(FUNCTION_PATH, "check-df-cols.R")))

# Functions ----

## calculate the costs of the personal measures bough
append_personalmeasure_calculated_costs <- function(pm_df, sum_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable COST_ABSOLUTE_COL not found in R/constants.R" = exists(deparse(substitute(COST_ABSOLUTE_COL))),
            "Default variable PERCENTAGE_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_INCOME_COL))),
            "Default variable PERCENTAGE_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_HOUSE_COL))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable LAST_PRICE_COL not found in R/constants.R" = exists(deparse(substitute(LAST_PRICE_COL))),
            "Default variable PERCENTAGE_FACTOR not found in R/constants.R" = exists(deparse(substitute(PERCENTAGE_FACTOR)))
  )
   
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(pm_df, c(COST_ABSOLUTE_COL, PERCENTAGE_INCOME_COL, PERCENTAGE_HOUSE_COL, ROUND_INCOME_COL, LAST_PRICE_COL))
  
  
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


## append human‑readable ordered categories matching numeric welfare IDso 
append_welfare_labels <- function(pr_df, label_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable WELFARE_LABELS not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABELS))),
            "Default variable WELFARE_ID_COL not found in R/constants.R" = exists(deparse(substitute(WELFARE_ID_COL))))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  check_char_cols(pr_df, WELFARE_ID_COL)
  
  ## Save unique welfare ids. ids sorted ascendingly, as in WELFARE_LABELS match
  welfare_ids <- sort(unique(as.character(pr_df[, WELFARE_ID_COL])))
  
  
  ## Only if there are exactly six distinct IDs. Otherwise, it warns you that the mapping isn’t valid.
  if (identical(unname(WELFARE_LABELS) %in% welfare_ids)) {
    
      pr_df <- pr_df %>%
        mutate(
          !!label_col := factor(WELFARE_LABELS[match(.data[[WELFARE_ID_COL]], welfare_ids)],
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
append_reported_calculated_difference <- function(df, diff_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable CALCULATED_COLS not found in R/constants.R" = exists(deparse(substitute(CALCULATED_COLS))))
            
  ## Check at least one calculated cost is not missing
  missing_all <- report_missing_cols(df, CALCULATED_COLS, diff_col)
  
  ## Check the respective reported cost(s) is/are also not missing
  missing_all <- missing_all * report_missing_cols(df, names(CALCULATED_COLS), diff_col)
  
  ## Proceed in case of at least one pair of reported-calculated cost is available, 
  if(missing_all == FALSE) {
    
    ## filter complete reported-calculated cost pairs and calculate difference between those
    col_cross <- as.logical(names(CALCULATED_COLS) %in% names(df) * names(CALCULATED_COLS) %in% names(df))
    
    calc_cols <- CALCULATED_COLS[col_cross]
    repor_cols <- names(CALCULATED_COLS)[col_cross]
    
    ## Check numeric columns are defined as such
    check_num_cols(df, c(repor_cols, calc_cols))
    
    ## calculate difference between reported-calculated cost pairs
    df[, diff_col] <- rowSums(df[names(df) %in% repor_cols], na.rm = TRUE) - rowSums(df[names(df) %in% calc_cols], na.rm = TRUE)
  }
  
  return(df)
}
  

## Append pluvial + fluvial costs as total_damage
append_total_damage_costs <- function(df, sum_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable TYPE_COST_COLS not found in R/constants.R" = exists(deparse(substitute(TYPE_COST_COLS))))
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  ## Check at least one element of TYPE_COST_COLS is not missing in df
  missing_all <- report_missing_cols(df, TYPE_COST_COLS, sum_col)
  
  ## In case at least one element of TYPE_COST_COLS is not missing, check the columns are numeric and calculate their sum
  if(missing_all == FALSE) {
    
    ## Check numeric columns are defined as such
    check_num_cols(df, TYPE_COST_COLS[TYPE_COST_COLS %in% names(df)])
    
    df[, sum_col] <- rowSums(df[names(df) %in% TYPE_COST_COLS], na.rm = TRUE)
  }
  
  return(df)
}


## Append income_grp labels based on round_income to dataframe
append_income_grp <- function(df, label_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable WELFARE_LABELS not found in R/constants.R" = exists(deparse(substitute(WELFARE_LABELS))),
            "Default variable K_FACTOR not found in R/constants.R" = exists(deparse(substitute(K_FACTOR))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))))
  
  ## Check data frame is in the expected format and columns to which constants refer in calculation exist
  check_df_cols(df, ROUND_INCOME_COL)
  
  ## Save unique income labels. labels sorted ascendingly, as in WELFARE_LABELS match
  income_labels <- sort(unique(paste0(df[, ROUND_INCOME_COL] / K_FACTOR, names(K_FACTOR))))
  
  ## append income groups based on ROUND_INCOME_COL values
  df <- df %>%
    mutate(!!label_col := factor(paste0(.data[[ROUND_INCOME_COL]] / K_FACTOR, names(K_FACTOR)),
                                      levels = income_labels,
                                      ordered = TRUE))
  
  ## if income_labels does not match names(WELFARE_LABELS), issue warning
  if (identical(income_labels, names(WELFARE_LABELS)) == FALSE) {
    
    warning(paste0("Expected the following income_grp labels: ",
                   paste(names(WELFARE_LABELS), collapse = ", "),
                   ". Instead the following values were found: ",
                   paste(income_labels, collapse = ", "),
                   "."))
  }
  
  return(df)
  
}


## Calculate the round costs to check the spendable income
append_total_costs <- function(df, sum_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable ALL_COST_COLS not found in R/constants.R" = exists(deparse(substitute(ALL_COST_COLS))))
  
  ## Check at least one element of ALL_COST_COLS is not missing in df
  missing_all <- report_missing_cols(df, ALL_COST_COLS, sum_col)
  
  ## In case at least one element of ALL_COST_COLS is not missing, check columns are numeric and calculate their sum
  if(missing_all == FALSE) {
    
    check_num_cols(df, ALL_COST_COLS[ALL_COST_COLS %in% names(df)])
    
    # If either column has NA, the sum will also be NA unless the sum is done this way
    df[, sum_col] <- rowSums(df[names(df) %in% ALL_COST_COLS], na.rm = TRUE) 
  }
  
  return(df)
}


## Calculate the spendable income
append_spendable_income_cols <- function(df, calc_col, diff_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable PLAYER_CODE_COL not found in R/constants.R" = exists(deparse(substitute(PLAYER_CODE_COL))),
            "Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_NUMBER_COL))),
            "Default variable SPENDABLE_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(SPENDABLE_INCOME_COL))),
            "Default variable ROUND_INCOME_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable PROFIT_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(PROFIT_HOUSE_COL))),
            "Default variable TOTAL_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(TOTAL_COSTS_COL))))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are character or factor
  check_char_cols(df, c(PLAYER_CODE_COL, ROUND_NUMBER_COL))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(df, c(SPENDABLE_INCOME_COL, ROUND_INCOME_COL, PROFIT_HOUSE_COL, TOTAL_COSTS_COL))
  
  ## Check that players found match those expected
  expected_players <- unique(df[, PLAYER_CODE_COL])
  
  found_players <- df %>% filter(ROUND_NUMBER_COL %in% 0) %>% pull(PLAYER_CODE_COL)
  
  ## mismatch between found and expected players stops run, otherwise columns calc_col and diff_col are calculated
  if (any(expected_players %in% found_players) == FALSE) {
    
    stop(paste("Missing Round Number 0 value detected for players", paste(expected_players[expected_players %in% found_players == FALSE], collapse = ", ")))
  
  } else {
    
    df <- df %>%
      arrange(across(all_of(c(PLAYER_CODE_COL, ROUND_NUMBER_COL))))
    
    df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, calc_col] <-
      rowSums(cbind(df[which(df[, ROUND_NUMBER_COL] %in% 0 == FALSE) - 1, SPENDABLE_INCOME_COL],
                    df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, ROUND_INCOME_COL],
                    df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, PROFIT_HOUSE_COL],
                    -df[df[, ROUND_NUMBER_COL] %in% 0 == FALSE, TOTAL_COSTS_COL]), na.rm = TRUE)
    
    df[, diff_col] <- df[, SPENDABLE_INCOME_COL] - df[, calc_col]
  }
  
  return(df)
}


## Calculate income - living costs
append_income_living_diff <- function(df, diff_col) {

  ## Check constants used in calculation exist
  stopifnot("Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))),
            "Default variable LIVING_COSTS_COL not found in R/constants.R" = exists(deparse(substitute(LIVING_COSTS_COL))))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(df, c(ROUND_INCOME_COL, LIVING_COSTS_COL))
  
  df[, diff_col] <- rowSums(cbind(df[, ROUND_INCOME_COL],
                                  -df[ ,LIVING_COSTS_COL]), na.rm = TRUE)
  
  return(df)
}


## Calculate  "profit - spent savings house moving"
append_housemoving_diff <- function(df, diff_col) {
  
  ## Check constants used in calculation exist
  stopifnot("Default variable PROFIT_HOUSE_COL not found in R/constants.R" = exists(deparse(substitute(PROFIT_HOUSE_COL))),
            "Default variable SPENT_SAVINGS_COL not found in R/constants.R" = exists(deparse(substitute(SPENT_SAVINGS_COL))))
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  ## Check data frame is in the expected format, columns to which constants refer in calculation exist, and are numeric
  check_num_cols(df, c(PROFIT_HOUSE_COL, SPENT_SAVINGS_COL))
  
  df[, diff_col] <- rowSums(cbind(df[, PROFIT_HOUSE_COL],
                                  -df[, SPENT_SAVINGS_COL]), na.rm = TRUE)
  
  return(df)
}

