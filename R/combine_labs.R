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
#' combinedDF = rbind_labs()
#' @importFrom dplyr filter
rbind_labs = function(
    lab_list = cdcfluview::who_nrevss("state"),
    lab_name = c("clinical_labs", "combined_prior_to_2015_16"))
{
  #REMOVE lab_name argument since public health labs is not needed anymore
  bigDF = NULL
  for(i in lab_name)
  {

    if(i == "public_health_labs")
    {
      tempDF = lab_list[[i]] %>%
        mutate(
          across(total_specimens:h3n2v, ~ as.integer(.x)),
          total_a = rowSums(across(a_2009_h1n1:a_subtyping_not_performed)),
          total_b = rowSums(across(b:byam)),
          labType = i)

      # MAKING NEW TOTAL_A AND TOTAL_B TO BIND ROW PROPERLY
      # bind_df = lab_list[[i]]
      # lab_list[[i]]$total_a = ifelse(grepl("a_", colnames(lab_list[[i]]), )
    }
    else if(i == "combined_prior_to_2015_16")
    {
      tempDF = lab_list[[i]] %>%
        mutate(
          across(c(total_specimens, a_2009_h1n1:h3n2v), ~ as.integer(.x)),
          total_a = rowSums(across(a_2009_h1n1:a_unable_to_subtype)),
          total_b = (b),
          labType = i)
      # MAKING NEW TOTAL_A AND TOTAL_B TO BIND ROW PROPERLY
    }
    else
    {

      tempDF =
        lab_list[[i]] %>%
        mutate(
          across(total_specimens:total_b, ~ as.integer(.x)),
          labType = i)
    }
    bigDF = dplyr::bind_rows(bigDF, tempDF)
    # BIND ROWS AND LEAVE NAS FOR DIFFERENT COLUMNS
  }

  return(
    bigDF |>
    tibble() |>
    arrange(region, wk_date))

}
