# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here(file.path(FUNCTION_PATH, "constants.R")))

# Functions ----

## Report missing cols for a given dataframe
report_missing_cols <- function(df, in_cols, out_cols) {
  
  # Check DF_NAME exists
  stopifnot("Default variable DF_NAME not found in R/constants.R" = exists(deparse(substitute(DF_NAME))))
  
  ## Assumption that there are no missing columns
  missing_all <- FALSE
  
  ## Warning for if all input columns are missing
  if (any(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("(All) expected collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in ", DF_NAME, ". Column(s) ", 
                   paste(out_cols, collapse = ", "),
                   " cannot be added to this dataframe."
                   )
            )
    
    missing_all <- TRUE
            
  } else if (all(in_cols %in% names(df)) == FALSE) {
    
    warning(paste0("Expected Collumn(s) ",
                   paste(in_cols[in_cols %in% names(df) == FALSE], collapse = ", "),
                   " missing in ", DF_NAME, ". They will not be used in Calculating collumn(s) ",
                   paste(out_cols, collapse = ", "), "."
                   )
            )
  }
  
  return(missing_all)
}


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
  
  ## Check data frame is in the expected format
  stopifnot("personalmeasure_df expected to be a data frame" = is.data.frame(pm_df))
            
  ## Check columns to which constants refer in calculation exist, and are numeric
  cols <- c(PERCENTAGE_INCOME_COL, PERCENTAGE_HOUSE_COL, ROUND_INCOME_COL, LAST_PRICE_COL)
  
  if (any(cols %in% names(pm_df) == FALSE)){
    stop(paste0("These personalmeasure_df columns could not be found: ",
               paste(cols[cols %in% names(pm_df) == FALSE], collapse = ", "),
               "."))
  }
  
  if (any(unlist(lapply(pm_df[,cols], is.numeric)) == FALSE)) {
    stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                paste(names(unlist(lapply(pm_df[,cols], is.numeric)))[unlist(lapply(pm_df[,cols], is.numeric)) == FALSE], collapse = ", "),
                ".")
    )
  }
  
  
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
  
  ## Check data frame is in the expected format
  stopifnot("personalmeasure_df expected to be a data frame" = is.data.frame(pm_df))
  
  ## Check columns to which constants refer in calculation exist
  nonnum_cols <- c(PLAYER_CODE_COL, ROUND_NUMBER_COL)
  num_cols <- c(CALCULATED_COSTS_PERSONAL_COL, COST_HOUSE_COL)
  cols <- c(nonnum_cols, num_cols)
  
  if (any(cols %in% names(pm_df) == FALSE)){
    stop(paste0("These personalmeasure_df columns could not be found: ",
                paste(cols[cols %in% names(pm_df) == FALSE], collapse = ", "),
                "."))
  }
  
  ## Check factor or character (non-numeric) columns are defined as such
  detect_nonnum <- unlist(lapply(pm_df[,nonnum_cols], is.factor)) + unlist(lapply(pm_df[,nonnum_cols], is.character))
  
  if (any(detect_nonnum == 0)) {
    stop(paste0("These personalmeasure_df columns expected to be factor or character: ",
                paste(names(detect_nonnum)[detect_nonnum == 0], collapse = ", "),
                ".")
    )
  }
  
  ## Check numeric columns are defined as such
  if (any(unlist(lapply(pm_df[,num_cols], is.numeric)) == FALSE)) {
    stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                paste(names(unlist(lapply(pm_df[,num_cols], is.numeric)))[unlist(lapply(pm_df[,num_cols], is.numeric)) == FALSE], collapse = ", "),
                ".")
    )
  }

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
  
  
  ## Check data frame is in the expected format
  stopifnot("housemeasure_df expected to be a data frame" = is.data.frame(hm_df))
  
  ## Check columns to which constants refer in calculation exist
  nonnum_cols <- c(PLAYER_CODE_COL, ROUND_NUMBER_COL)
  num_cols <- c(COST_ABSOLUTE_COL, COST_HOUSE_COL)
  logic_cols <- IS_IHM_COL
  
  cols <- c(nonnum_cols, num_cols, logic_cols)
  
  if (any(cols %in% names(hm_df) == FALSE)){
    stop(paste0("These housemeasure_df columns could not be found: ",
                paste(cols[cols %in% names(hm_df) == FALSE], collapse = ", "),
                "."))
  }
  
  ## Check factor or character (non-numeric) columns are defined as such
  detect_nonnum <- unlist(lapply(hm_df[,nonnum_cols], is.factor)) + unlist(lapply(hm_df[,nonnum_cols], is.character))
  
  if (any(detect_nonnum == 0)) {
    stop(paste0("These personalmeasure_df columns expected to be factor or character: ",
                paste(names(detect_nonnum)[detect_nonnum == 0], collapse = ", "),
                ".")
    )
  }
  
  ## Check logical columns are defined as such
  if (any(unlist(lapply(hm_df[,logic_cols], is.logical)) == FALSE)) {
    stop(paste0("These personalmeasure_df columns expected to be logical: ",
                paste(names(unlist(lapply(hm_df[,logic_cols], is.logical)))[unlist(lapply(hm_df[,logic_cols], is.numeric)) == FALSE], collapse = ", "),
                ".")
    )
  }
  
  
  ## Check numeric columns are defined as such
  if (any(unlist(lapply(hm_df[,num_cols], is.numeric)) == FALSE)) {
    stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                paste(names(unlist(lapply(hm_df[,num_cols], is.numeric)))[unlist(lapply(hm_df[,num_cols], is.numeric)) == FALSE], collapse = ", "),
                ".")
    )
  }
  
  
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
  
  
  ## Check data frame is in the expected format
  stopifnot("playerround_df expected to be a data frame" = is.data.frame(pr_df))
  
  ## Check factor or character (non-numeric) columns are defined as such
  detect_nonnum <- unlist(lapply(pr_df[, WELFARE_ID_COL], is.factor)) + unlist(lapply(pr_df[, WELFARE_ID_COL], is.character))
  
  if (any(detect_nonnum == 0)) {
    stop(paste0("These personalmeasure_df columns expected to be factor or character: ",
                paste(names(detect_nonnum)[detect_nonnum == 0], collapse = ", "),
                ".")
    )
  }
  
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
            
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
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
    if (any(unlist(lapply(df[,c(calc_cols, repor_cols)], is.numeric)) == FALSE)) {
      stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                  paste(names(unlist(lapply(df[,c(calc_cols, repor_cols)], is.numeric)))[unlist(lapply(df[,c(calc_cols, repor_cols)], is.numeric)) == FALSE], collapse = ", "),
                  ".")
      )
    }
    
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
    
    num_cols <- TYPE_COST_COLS[TYPE_COST_COLS %in% names(df)]
    
    if (any(unlist(lapply(df[,num_cols], is.numeric)) == FALSE)) {
      stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                  paste(names(unlist(lapply(df[,num_cols], is.numeric)))[unlist(lapply(df[,num_cols], is.numeric)) == FALSE], collapse = ", "),
                  ".")
      )
    }
    
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
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
  # Check at least one element of ALL_COST_COLS is not missing in df
  missing_all <- report_missing_cols(df, ALL_COST_COLS, TOTAL_COSTS_COL)
  
  # In case at least one element of ALL_COST_COLS is not missing, check columns are numeric and calculate their sum
  if(missing_all == FALSE) {
    
    num_cols <- ALL_COST_COLS[ALL_COST_COLS %in% names(df)]
    
    if (any(unlist(lapply(df[,num_cols], is.numeric)) == FALSE)) {
      stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                  paste(names(unlist(lapply(df[,num_cols], is.numeric)))[unlist(lapply(df[,num_cols], is.numeric)) == FALSE], collapse = ", "),
                  ".")
      )
    }
    
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
  
  nonnum_col <- c(PLAYER_CODE_COL, ROUND_NUMBER_COL)
  num_col <- SPENDABLE_INCOME_COL
  cols <- c(nonnum_col, num_col)
  
  if (any(cols %in% names(hm_df) == FALSE)){
    stop(paste0("These personalmeasure_df columns could not be found: ",
                paste(cols[cols %in% names(hm_df) == FALSE], collapse = ", "),
                "."))
  }
  
  ## Check factor or character (non-numeric) columns are defined as such
  detect_nonnum <- unlist(lapply(df[,nonnum_cols], is.factor)) + unlist(lapply(df[,nonnum_cols], is.character))
  
  if (any(detect_nonnum == 0)) {
    stop(paste0("These personalmeasure_df columns expected to be factor or character: ",
                paste(names(detect_nonnum)[detect_nonnum == 0], collapse = ", "),
                ".")
    )
  }
  
  ## Check numeric columns are defined as such
  if (any(unlist(lapply(df[,num_cols], is.numeric)) == FALSE)) {
    stop(paste0("These personalmeasure_df columns expected to be numeric: ",
                paste(names(unlist(lapply(df[,num_cols], is.numeric)))[unlist(lapply(df[,num_cols], is.numeric)) == FALSE], collapse = ", "),
                ".")
    )
  }
  
  # Check that players found match those expected
  expected_players <- unique(df[, PLAYER_CODE_COL])
  
  found_players <- df %>% filter(ROUND_NUMBER_COL %in% 0) %>% pull(PLAYER_CODE_COL)
  
  
  df <- df %>%
    arrange(across(all_of(PLAYER_CODE_COL, ROUND_NUMBER_COL))) %>%
    mutate(!!CALCULATED_SPENDABLE_COL := .data[[SPENDABLE_INCOME_COL]])
  
  # mismatch between found and expected players stops run
  if (any(expected_players %in% found_players) == FALSE) {
    
    stop(paste("Missing Round Number 0 value detected for players", paste(expected_players[expected_players %in% found_players == FALSE], collapse = ", ")))
  
  ## Else
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
            "Default variable ROUND_NUMBER_COL not found in R/constants.R" = exists(deparse(substitute(ROUND_INCOME_COL))))
  
  ## Check data frame is in the expected format
  stopifnot("df expected to be a data frame" = is.data.frame(df))
  
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

  # Calcule the reference dataset with all players average
  ## mapply safely substracts ingnoring NAs in either column 
  ## na.rm = TRUE remove or ignore NA (missing) values when performing calculations.
  

  
  # income_dist_df$income_minus_living <- mapply(
  #   function(income, cost) sum(income, -cost, na.rm = TRUE),
  #   income_dist_df$round_income,
  #   income_dist_df$living_costs
  # )
  # 
    

  # income_dist_df$profit_minus_spent_savings_house_moving <- mapply(
  #   function(profit, spent) sum(profit, -spent, na.rm = TRUE),
  #   income_dist_df$profit_sold_house,
  #   income_dist_df$spent_savings_for_buying_house
  # )
  # 

