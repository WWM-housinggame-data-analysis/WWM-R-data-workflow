
select_sqlquery <- function(dbtable, selected_cols) {
  
  if (missing(selected_cols) == FALSE && identical(selected_cols, names(dbtable)) == FALSE) {
    sqlquery <- paste0("SELECT ", paste(selected_cols, collapse = ", "), " FROM ", deparse(substitute(dbtable)))
  } else {
    sqlquery <- paste0("SELECT * FROM ", deparse(substitute(dbtable)))
  }
  
  return(sqlquery)
}

left_join_sqlquery <- function(dbtable1, match_dbtable1_cols, dbtable2, match_dbtable2_cols, kept_dbtable1_vars, kept_dbtable2_vars) {
  
  if (missing(kept_dbtable1_vars) == TRUE && missing(kept_dbtable2_vars) == TRUE) {
    
    select_statement <- "SELECT dbtable1.*, dbtable2.*"
    
  } else if (missing(kept_dbtable1_vars) == TRUE && missing(kept_dbtable2_vars) == FALSE) {
    
    select_statement <- paste0("SELECT dbtable1.*, ", paste(paste0("dbtable2.", kept_dbtable2_vars), collapse = ", "))
    
  } else if (missing(kept_dbtable1_vars) == FALSE && missing(kept_dbtable2_vars) == TRUE) {
    
    select_statement <- paste0("SELECT ", paste(paste0("dbtable1.", kept_dbtable1_vars), collapse = ", "), ", dbtable2.*")
  } else {
    select_statement <- paste0("SELECT ", paste(paste0("dbtable1.", kept_dbtable1_vars), collapse = ", "), ", ",
                               paste(paste0("dbtable2.", kept_dbtable2_vars), collapse = ", "))
  }
  
  
  for_statement <- paste0("FROM [", deparse(substitute(dbtable1)), "] AS dbtable1")
  
  left_join_statement <- paste0("LEFT JOIN [", deparse(substitute(dbtable2)), "] AS dbtable2")
  
  on_statement <- paste0("ON ", paste(paste(paste0("dbtable1.", match_dbtable1_cols), paste0("dbtable2.",  match_dbtable2_cols), sep = " = "), collapse = " AND "))
  
  sqlquery <- paste(select_statement,
                    for_statement,
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
  
  colnames <- names(dbtable)
  
  colnames <- colnames[colnames %in% current_colnames == FALSE]  # exclude the column you want to rename
  
  if (renamed_cols_first) {
    sqlquery <- paste0("SELECT ", paste(paste(current_colnames, new_colnames, sep = " AS "), collapse = ", "), ", ", paste(colnames, collapse = ", "),
                       " FROM " , deparse(substitute(dbtable)))
  } else {
    sqlquery <- paste0("SELECT ", paste(colnames, collapse = ", "), ", ", paste(paste(current_colnames, new_colnames, sep = " AS "), collapse = ", "),
                       " FROM " , deparse(substitute(dbtable)))
  }
  
  
  
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