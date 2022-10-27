# Ctrl + Shift + Alt + R: Parameters (do it inside the function)
# Ctrl + shift + P (on function name line) --> Prefix --> @importFrom
#' Title
#'
#' @param lab_list (The dataset that contains lists of dataframes)
#' @param lab_name (The desired labs that want to be analyzed)
#'
#' @return (A dataframe that combines all desired labs in lab_name)
#' @export
#'
#' @examples
#' combinedDF = combine_labs()
#' @importFrom dplyr filter
combine_labs = function(lab_list = cdcfluview::who_nrevss("state"), lab_name = "clinical_labs"){
  #REMOVE lab_name argument since public health labs is not needed anymore
  bigDF = data.frame()
  for(i in lab_name){
    if(i == "public_health_labs"){
      tempDF = lab_list[[i]] %>%
        rowwise() %>%
        mutate(across(total_specimens:h3n2v, ~ as.integer(.x)),
               total_a = sum(c_across(a_2009_h1n1:a_subtyping_not_performed)),
               total_b = sum(c_across(b:byam)))

      # MAKING NEW TOTAL_A AND TOTAL_B TO BIND ROW PROPERLY
      # bind_df = lab_list[[i]]
      # lab_list[[i]]$total_a = ifelse(grepl("a_", colnames(lab_list[[i]]), )
    }
    else if(i == "combined_prior_to_2015_16"){
      tempDF = lab_list[[i]] %>%
        rowwise() %>%
        mutate(across(c(total_specimens, a_2009_h1n1:h3n2v), ~ as.integer(.x)),
               total_a = sum(c_across(a_2009_h1n1:a_unable_to_subtype)),
               total_b = sum(b))
      # MAKING NEW TOTAL_A AND TOTAL_B TO BIND ROW PROPERLY
    }
    else{

      tempDF = lab_list[[i]] %>% mutate(across(total_specimens:total_b, ~ as.integer(.x)))
    }
    bigDF = dplyr::bind_rows(bigDF, tempDF)
    # BIND ROWS AND LEAVE NAS FOR DIFFERENT COLUMNS
  }

  return(bigDF)




}
