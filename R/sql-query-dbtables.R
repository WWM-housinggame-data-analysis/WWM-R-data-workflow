# Set defaults ----
## Set all default variables or global options and all the path variables.

## Set path to source files with functions
FUNCTION_PATH <- file.path("R")

## Load all default variables or global options. Please check this file for visual check loaded variables 
source(here::here(file.path(FUNCTION_PATH, "constants.R")))
source(here::here(file.path(FUNCTION_PATH, "check-df-cols.R")))

# Functions ---

## test whether character vector contains blank element
contains_blank_char <- function(x) {
  
    # NULL or length-0 vectors are considered blank
    if (is.null(x) || length(x) == 0L) return(TRUE)
    
    # Treat factors as their character labels
    if (is.factor(x)) x <- as.character(x)
    
    res <- is.na(x)
    
    # For character inputs, blank means "" or whitespace-only
    if (is.character(x)) {
      res <- res | trimws(x) == ""
    }
    
    # Return logical stating whether any element in character vector is blank 
    return(any(res))
  }


## Create sql query for sqldf to select and sort columns from dbtable
select_sqlquery <- function(dbtable, selected_cols) {
  
  ## write default sql query if no selected_cols are given as argument
  sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)))
  
  ## Check selected_cols is not missing. Otherwise all collums are selected with no change in order
  if (missing(selected_cols) == FALSE) {
    
    ## Check dbtable is data frame and columns exist
    check_df_cols(dbtable, selected_cols)
    
    ## Check selected_cols have blank characters and warn they will be ignored
    if (contains_blank_char(selected_cols)) {
      warning("Blank character elements detected in selected_cols. Column selection will be ignored.")
    }
    
    ## check selected_cols is not identical to the current names of dbtable. In that case columns are selected and sorted as in selected_cols, ignore non mentioned ones.
      
    if (identical(selected_cols, names(dbtable)) == FALSE) {
      sqlquery <- paste0("SELECT ", paste(selected_cols[selected_cols %in% names(dbtable)], collapse = ", "), " FROM ", deparse(substitute(dbtable)))
    }
  }
  
  return(sqlquery)
}


## Create sql query for sqldf to left join two dbtables
left_join_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, kept_dbtable1_cols, kept_dbtable2_cols) {
  
  ## Check match_dbtable1_cols and match_dbtable2_cols have no blank characters have the same exact length
  stopifnot("match_dbtable1_cols not expected to have blank character elements" = contains_blank_char(match_dbtable1_cols) == FALSE,
            "match_dbtable2_cols not expected to have blank character elements" = contains_blank_char(match_dbtable2_cols) == FALSE,
            "match_dbtable1_cols and match_dbtable2_cols expected to have same length" = length(match_dbtable1_cols) == length(match_dbtable2_cols))

  ## Check dbtable1 is data frame and match_dbtable1_cols exist
  check_df_cols(dbtable1, match_dbtable1_cols)
  
  ## Check dbtable2 is data frame and match_dbtable2_cols exist
  check_df_cols(dbtable2, match_dbtable2_cols)
    
  ## If kept_dbtable1_cols is a non-blank character_vector and differs from names(dbtable1) in size or sorting, Select statement is written considering kept_dbtable1_cols. Otherwise all the collumns are selected as they are.
  if (missing(kept_dbtable1_cols) == FALSE && contains_blank_char(kept_dbtable1_cols) == FALSE && identical(kept_dbtable1_cols, names(dbtable1)) == FALSE) {
    
    select_statement <- paste0("SELECT ", paste(paste0("dbtable1.", kept_dbtable1_cols[kept_dbtable1_cols %in% names(dbtable1)]), collapse = ", "))
    
  } else {
    
    warning("All the collumns from the first dbtable are kept.")
    
    kept_dbtable1_cols <- names(dbtable1)
    
    select_statement <- "SELECT dbtable1.*"
    
  }
  
  ## If kept_dbtable2_cols is a non-blank character_vector and differs from names(dbtable2) in size or sorting, Select statement is written considering kept_dbtable2_cols. Otherwise all the collumns are selected as they are.
  if (missing(kept_dbtable2_cols) == FALSE && contains_blank_char(kept_dbtable2_cols) == FALSE  && identical(kept_dbtable2_cols, names(dbtable2)) == FALSE) {
    
    select_statement <- paste(select_statement, paste(paste0("dbtable2.", kept_dbtable2_cols[kept_dbtable2_cols %in% names(dbtable2)]), collapse = ", "), sep = ", ")
    
  } else {
    
    warning("All the collumns from the second dbtable are kept.")
    
    kept_dbtable2_cols <- names(dbtable2)
    
    select_statement <- paste0(select_statement, ", dbtable2.*")
  }
  
  ## Check resulting table does not have repeated column names
  stopifnot("Leftjoined table cannot have repeated column names." =  length(intersect(kept_dbtable1_cols, kept_dbtable2_cols)) == 0)
  
  ## write default from statement
  from_statement <- paste0("FROM [", deparse(substitute(dbtable1)), "] AS dbtable1")
  
  ## write default left join statement
  left_join_statement <- paste0("LEFT JOIN [", deparse(substitute(dbtable2)), "] AS dbtable2")
  
  ## write default on statement
  on_statement <- paste0("ON ", paste(paste(paste0("dbtable1.", match_dbtable1_cols), paste0("dbtable2.",  match_dbtable2_cols), sep = " = "), collapse = " AND "))
  
  ## paste statements to make complete left join sql query
  sqlquery <- paste(select_statement,
                    from_statement,
                    left_join_statement,
                    on_statement
  )
  
  return(sqlquery)
}


## Create sql query to rename columns in dbtable
rename_cols_sqlquery <- function(dbtable, current_colnames, new_colnames) {
  
  ## Check dbtable is data frames
  stopifnot("dbtable expected to be a data frame by sqldf" = is.data.frame(dbtable))
  
  ## Check current_colnames and new_colnames are not blank and have the same exact length
  stopifnot("current_colnames not expected to have blank character elements" = contains_blank_char(current_colnames) == FALSE,
            "new_colnames not expected to have blank character elements" = contains_blank_char(new_colnames) == FALSE,
            "current_colnames and new_colnames expected to have same length" = length(current_colnames) == length(new_colnames))
  
  ## Check dbtable is data frame and current_colnames exist
  check_df_cols(dbtable, current_colnames)
  
  ## Check new_colnames does not repeat already existing column names in dbtable
  stopifnot("new_colnames contains already existing column names in dbtable." =  length(intersect(names(dbtable), new_colnames)) == 0)
  
  ## update new_colnames by matching its values with dbtables column names
  new_colnames <- new_colnames[match(names(dbtable), current_colnames)]
  
  ## create character vector containing rename statements to be collapsed into rename sql query
  rename_statements <- mapply(function(table_cols, new_cols) {
                           if (!is.na(table_cols) && !is.na(new_cols)) {
                             paste(table_cols, new_cols, sep = " AS ")
                           } else if (!is.na(table_cols)) {
                             table_cols
                           } else {
                             stop("Blank names detected in dbtable")
                           }
                        }, names(dbtable), new_colnames)
  
  ## return rename sql query
  sqlquery <- paste0("SELECT ", paste(rename_statements, collapse = ", "), " FROM " , deparse(substitute(dbtable)))

  return(sqlquery)
}


## Create sql query to sort dbtable by a given column. Sorting assumed to be ascending based on logical argument `asc`.
sort_dbtable_sqlquery <- function(dbtable, sort_col, asc = TRUE) {
  
  ## Check sort_col is not blank, has length one and is a table column name
  stopifnot("sort_col not expected to be blank character" = contains_blank_char(sort_col) == FALSE,
            "Only one sorting column is allowed" = length(sort_col) == 1)
  
  ## Check dbtable is data frame and sort_col exist
  check_df_cols(dbtable, sort_col)
  
  ## if asc == TRUE, sort dbtable by sort_col ascendently, other descendently
  if (asc) {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)), " ORDER BY ", sort_col, " ASC")
    
  } else {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)), " ORDER BY ", sort_col, " DESC")
  }
  return(sqlquery)
}


## Create sql query to compare matching columns between two tables. An additional column is added to dbtable1 informing whether the matching columns are the same or not
compare_dbtables_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, compare_col) {
  
  ## Check match_dbtable1_cols and match_dbtable2_cols are not blank, can be found in dbtable1 and dbtable, respectively, and have the same exact length
  stopifnot("match_dbtable1_cols not expected to have blank character elements" = contains_blank_char(match_dbtable1_cols) == FALSE,
            "match_dbtable2_cols not expected to have blank character elements" = contains_blank_char(match_dbtable2_cols) == FALSE,
            "match_dbtable1_cols and match_dbtable2_cols expected to have same length" = length(match_dbtable1_cols) == length(match_dbtable2_cols))
  
  ## Check dbtable1 is data frame and match_dbtable1_cols exist
  check_df_cols(dbtable1, match_dbtable1_cols)
  
  ## Check dbtable2 is data frame and match_dbtable2_cols exist
  check_df_cols(dbtable2, match_dbtable2_cols)
  
  ## Check compare_col does not repeat already existing column names in dbtable
  stopifnot("compare_col is already existing column name in dbtable." = compare_col %in% names(dbtable1) == FALSE)
  
  ## Write case statement where match_dbtable1_cols in dbtable1 and match_dbtable2_cols in dbtable2 are compared to check if they are the same or not.
  case_statement <- paste0("CASE WHEN EXISTS (SELECT TRUE FROM [", deparse(substitute(dbtable2)), "] AS dbtable2 ",
                           "WHERE ", paste(paste(paste0("dbtable1.", match_dbtable1_cols),
                                                 paste0("dbtable2.",  match_dbtable2_cols), sep = " = "), collapse = " AND "), ") ",
                           "THEN TRUE ELSE FALSE END AS ", compare_col)
  
  ## Prepare case sql query to be returned
  sqlquery <- paste0("SELECT dbtable1.*, ", case_statement, " FROM [", deparse(substitute(dbtable1)), "] AS dbtable1")
  
  return(sqlquery)
}


## Create cast statement to for sql query designed to combine columns
make_cast_statement <- function(cast_col, col_type) {
  
  ## Check cast_col is not blank and has length one 
  stopifnot("cast_col not expected to be blank character" = contains_blank_char(cast_col) == FALSE,
            "col_type not expected to be blank character" = contains_blank_char(col_type) == FALSE,
            "Only one sorting column is allowed" = length(cast_col) == 1,
            "Only one column type is allowed" = length(col_type) == 1)
  
  ## If col_type is "integer", then create default cast statement. Otherwise return cast_col with no casting
  if (identical(col_type, "integer")) {
    cast_statement <- paste0("CAST(", cast_col, " AS INTEGER)")
    return(cast_statement)
    
  } else {
    return(cast_col)
  }
}


## sql query designed to combine columns
combine_cols_sqlquery <- function(dbtable, cast_col1, col_type1, cast_col2, col_type2, comb_col) {

  ## Check cast_col1 and cast_col2 are not blank, has length one
  stopifnot("cast_col1 not expected to be blank character" = contains_blank_char(cast_col1) == FALSE,
            "cast_col2 not expected to be blank character" = contains_blank_char(cast_col2) == FALSE,
            "cast_col1 and cast_col2 should have length = 1 each" = length(c(cast_col1, cast_col2)) == 2)
  
  ## Check dbtable is data frame and cast_col1 and cast_col2 exist
  check_df_cols(dbtable, c(cast_col1, cast_col2))
  
  ## Check compare_col does not repeat already existing column names in dbtable
  stopifnot("comb_col is already existing column name in dbtable." = comb_col %in% names(dbtable) == FALSE)
  
  ## write sql query to combine cast_col1 and 2 as comb_col,
  sqlquery <- paste0("SELECT *, ", make_cast_statement(cast_col1, col_type1), " || ' - ' || ",
                                   make_cast_statement(cast_col2, col_type2),
                     " AS ", comb_col,
                     " FROM ", deparse(substitute(dbtable)))
  
  return(sqlquery)
}