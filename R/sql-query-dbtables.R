# Functions ---

## test for empty character vectors
is_blank <- function(x) {
  
  stopifnot("Argument expected to be a character vector" = is.character(x))
  
  # NULL or length-0 vectors count as blank
  if (is.null(x) || length(x) == 0L) return(TRUE)
  
  # Recurse over lists: all elements must be blank
  if (is.list(x)) return(all(vapply(x, is_blank, logical(1))))
  
  # Factor -> character
  if (is.factor(x)) x <- as.character(x)
  
  if (is.character(x)) {
    return(all(is.na(x) | trimws(x) == ""))
  }
  
  # For other atomic types, only NA counts
  all(is.na(x))
}



## Create sql query for sqldf to select and sort columns from dbtable
select_sqlquery <- function(dbtable, selected_cols) {
  
  ## Check dbtable is data frame
  stopifnot("dbtable expected to be a data frame by sqldf" = is.data.frame(dbtable))
  
  ## write default sql query if no selected_cols are given as argument
  sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)))
  
  ## Check selected_cols is not missing. Otherwise all collums are selected with no change in order
  if (missing(selected_cols) == FALSE) {
    
    ## Check selected_cols is blank character vector
    if (is_blank(selected_cols)) {
      warning("selected_cols detected as blank character vector. Column selection will be ignored.")
    
    ## Otherwise check selected_cols is not identical to the current names of dbtable. In that case columns are selected and sorted as in selected_cols, ignore non mentioned ones.
      
    } else if (identical(selected_cols, names(dbtable)) == FALSE) {
      sqlquery <- paste0("SELECT ", paste(selected_cols[selected_cols %in% names(dbtable)], collapse = ", "), " FROM ", deparse(substitute(dbtable)))
    }
  }
  
  return(sqlquery)
}


## Create sql query for sqldf to left join two dbtables
left_join_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, kept_dbtable1_cols, kept_dbtable2_cols) {
  
  ## Check dbtables are data frames
  stopifnot("dbtable1 expected to be a data frame by sqldf" = is.data.frame(dbtable1),
            "dbtable2 expected to be a data frame by sqldf" = is.data.frame(dbtable2))
  
  ## Check match_dbtable1_cols and match_dbtable2_cols are not blank and have the same exact length
  stopifnot("match_dbtable1_cols not expected to be blank character vector" = is_blank(match_dbtable1_cols) == FALSE,
            "match_dbtable2_cols not expected to be blank character vector" = is_blank(match_dbtable2_cols) == FALSE,
            "match_dbtable1_cols and match_dbtable2_cols expected to have same length" = length(match_dbtable1_cols) == length(match_dbtable2_cols))
  
  ## If kept_dbtable1_cols is present as a non-blank character_vector, columns from first dbtable are selected and sorted as in kept_dbtable1_cols. Otherwise all the collumns are selected as they are.
  if (missing(kept_dbtable1_cols) == FALSE && is_blank(kept_dbtable1_cols) == FALSE && identical(kept_dbtable1_cols, names(dbtable1)) == FALSE) {
    
    select_statement <- paste0("SELECT ", paste(paste0("dbtable1.", kept_dbtable1_cols[kept_dbtable1_cols %in% names(dbtable1)]), collapse = ", "))
    
  } else {
    
    warning("All the collumns from the first dbtable are kept.")
    
    select_statement <- "SELECT dbtable1.*"
    
  }
  
  ## If kept_dbtable2_cols is present as a non-blank character_vector, columns from first dbtable are selected and sorted as in kept_dbtable2_cols. Otherwise all the collumns are selected as they are.
  if (missing(kept_dbtable2_cols) == FALSE && is_blank(kept_dbtable2_cols) == FALSE  && identical(kept_dbtable2_cols, names(dbtable2)) == FALSE) {
    
    select_statement <- paste(select_statement, paste(paste0("dbtable2.", kept_dbtable2_cols[kept_dbtable2_cols %in% names(dbtable2)]), collapse = ", "), sep = ", ")
    
  } else {
    
    warning("All the collumns from the second dbtable are kept.")
    
    select_statement <- paste0(select_statement, ", dbtable2.*")
  }
  
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

rename_cols_sqlquery <- function(dbtable, current_colnames, new_colnames, renamed_cols_first = FALSE) {
  
  stopifnot(is.character(current_colnames))
  
  stopifnot(is.character(new_colnames))
  
  if (length(current_colnames) != length(new_colnames)) {
    stop("current_colnames and new_colnames need to have the same length")
  }
  
  match_colnames <- ifelse(names(dbtable) %in% current_colnames == FALSE, NA, names(dbtable))
  
  new_colnames <- new_colnames[match(match_colnames, current_colnames)[!is.na(match(match_colnames, current_colnames))]]
  
  match_colnames[is.na(match_colnames) == FALSE] <- new_colnames
  
  current_colnames <- names(dbtable)
  
  new_colnames <- match_colnames
  
  
  rename_statement <- paste(mapply(function(old_cols, new_cols) {
                                    if (!is.na(old_cols) && !is.na(new_cols)) {
                                      paste(old_cols, new_cols, sep = " AS ")
                                   } else if (!is.na(old_cols)) {
                                      old_cols
                                   } else {
                                      stop("No valid column name found")
                                   }
                                  }, current_colnames, new_colnames),
                            collapse = ", ")
  
  
  sqlquery <- paste0("SELECT ", rename_statement, " FROM " , deparse(substitute(dbtable)))

  return(sqlquery)
}

sort_dbtable_sqlquery <- function(dbtable, sorting_col, asc = TRUE) {
  if (asc) {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)), " ORDER BY ", sorting_col, " ASC")
    
  } else {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)), " ORDER BY ", sorting_col, " DESC")
  }
  return(sqlquery)
}

compare_dbtables_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, compare_col) {
  
  case_statement <- paste0("CASE WHEN EXISTS (SELECT TRUE FROM [", deparse(substitute(dbtable2)), "] AS dbtable2 ",
                           "WHERE ", paste(paste(paste0("dbtable1.", match_dbtable1_cols), paste0("dbtable2.",  match_dbtable2_cols), sep = " = "), collapse = " AND "), ") ",
                           "THEN TRUE ELSE FALSE END AS ", compare_col)
  
  sqlquery <- paste0("SELECT dbtable1.*, ", case_statement, " FROM [", deparse(substitute(dbtable1)), "] AS dbtable1")
  
  return(sqlquery)
}

make_cast_statement <- function(colname, coltype) {
  if (coltype == "integer") {
    cast_statement <- paste0("CAST(", colname, " AS INTEGER)")
  } else {
    cast_statement <- colname
  }
  
  return(cast_statement)
}

combine_cols_sqlquery <- function(dbtable, colname1, coltype1, colname2, coltype2, comb_colname) {
  
  colname1_statement <- make_cast_statement(colname1, coltype1)
  
  colname2_statement <- make_cast_statement(colname2, coltype2)
  
  sqlquery <- paste0("SELECT *, ", colname1_statement, " || ' - ' || ", colname2_statement, " AS ", comb_colname, " FROM ", deparse(substitute(dbtable)))
  
  return(sqlquery)
}