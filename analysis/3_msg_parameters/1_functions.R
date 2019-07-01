######################################
# Set up data for MESSAGE parameters #
######################################
# Set up empty parameter
empty_par <- function(parname, varlist) {
  
  print(paste0("Empty parameter [", parname, "]"))
  # Set up empty dataframe
  assign('df', matrix(ncol = length(varlist), nrow = 0, NA))
  df <- as.data.frame(df)
  names(df) <- varlist
  return(df)
}

# Fill in node and year variables
node_year_fill <- function(df, parname, varlist, technology,
                           year_act = NULL, year_vtg = NULL, year_rel = NULL,
                           node_loc = NULL, node_origin = NULL, node_dest = NULL, node_rel = NULL) {
    
      print(paste0("Filling in node + year for parameter [", parname, "]"))
      # Make sure node and year argument exists
      assign('colist', list())
      for (n in c('year_act', 'year_vtg', 'node_loc', 'node_origin', 'node_dest')) {
        if (any(grepl(n, varlist)) == T) {
          if (is.null(get(n))) {stop(paste0("Parameter [", parname, "] requires ", n, "!"))}
        assign('nlist', list(a = get(n)))
        colist <- c(colist, nlist)
        names(colist)[names(colist) == 'a'] <- n
        }
      }
      
      if (length(colist) != 0) {
        # Assign row size
        assign('dimlist', list())
        for (i in 1:length(colist)) {
          dimlist <- c(dimlist, length(colist[[i]]))
        }
        assign('basedf', matrix(ncol = (length(varlist)-length(colist)), nrow = prod(unlist(dimlist))))
        basedf <- as.data.frame(basedf)
        names(basedf) <- varlist[!(varlist %in% names(colist))]
        
        # Fill in rows
        assign('filled', expand.grid(colist))
        basedf <- cbind(basedf, filled)
        
        # Fill in technology
        basedf$technology <- technology
        
        return(basedf)
    }}
  
# Fill in value and unit variables
value_unit_fill <- function(df, parname, varlist, technology,
                            value, unit, 
                            value_constant = TRUE, value_unit_df = NULL, unique_identifiers = NULL) {
  
  print(paste0("Filling in value + unit for parameter [", parname, "]"))
  if (any(grepl('value', varlist)) == F | any(grepl('unit', varlist)) == F) {
    stop("All parameters must have value and unit!")
  }
  
  if (value_constant == T) {
    if (is.data.frame(value)) {stop("You specified that value is constant, not dataframe")}
    df$value = value
  } else {
    if (!is.data.frame(value)) {stop("You specified that value is dataframe, not constant")}
    isid('df', unique_identifiers)
    df <- left_join(df, value, by = unique_identifiers)
  }

  df$unit <- unit

  return(df)
}

# Fill auxiliary variables
aux_fill <- function(df, unique_identifiers = NULL,
                     mode = NULL, time = NULL, time_origin = NULL, time_dest = NULL,
                     commodity = NULL, level = NULL,
                     emission = NULL, relation = NULL) {
  
  for (v in c('mode', 'time', 'time_origin', 'time_dest', 'commodity', 'level', 'emission', 'relation')) {
    
    assign('vobj', get(v))
    
    if (!is.null(vobj)) {
      print(paste0("Filling in ", v, " for parameter [", parname, "]"))
      
      if (!is.data.frame(vobj)) {
        df[, v] <- vobj
      } else {
        if (is.null(unique_identifiers)) {stop("If you are merging, you must specify unique identifiers!")}
        isid('df', unique_identifiers)
        df <- left_join(df, vobj, by = unique_identifiers)
      }
    }
  }
  return(df)
}


# Run all functions to make parameter dataframe
build_parameter <- function(parname, varlist, technology, unique_identifiers = NULL,
                           year_act = NULL, year_vtg = NULL, year_rel = NULL,
                           node_loc = NULL, node_origin = NULL, node_dest = NULL, node_rel = NULL,
                           value, unit, value_constant = TRUE, value_unit_df = NULL, 
                           mode = NULL, time = NULL, time_origin = NULL, time_dest = NULL,
                           commodity = NULL, level = NULL,
                           emission = NULL, relation = NULL) {
  
  assign('parout', empty_par(parname = parname, varlist = varlist) %>%
           
                    node_year_fill(parname = parname, varlist = varlist, technology = technology,
                                   year_act = year_act, year_vtg = year_vtg, year_rel = year_rel,
                                   node_loc = node_loc, node_origin = node_origin, node_dest = node_dest, node_rel = node_rel) %>%
           
                    value_unit_fill(parname = parname, varlist = varlist, technology = technology,
                                    value = value, unit = unit,
                                    value_constant = value_constant, value_unit_df = value_unit_df, 
                                    unique_identifiers = unique_identifers) %>%
           
                    aux_fill(unique_identifiers = unique_identifiers, 
                             mode = mode, time = time, time_origin = time_origin, time_dest = time_dest,
                             commodity = commodity, level = level, emission = emission, relation = relation))
  return(parout)
}


  