#' Plot proportion correct (difficulty) scores of dichotomous variables
#'
#' This function loads or creates an object from psych::describe() and plots the
#' mean variable with the associated error bars. The function can easily be
#' extended with ggplot2 commands.
#'
#' @param data Either a data frame containing (only) dichotomous variables or the output of psych::describe()
#' @param guess Optional. Indicate the (theoretical) guessing probability.
#' @param sort Optional. Keep variables in the same order as in the dataset (default), order them ascending order ("asc") or or
#'             or order them in descending order ("desc").
#' @param group Optional. Character vector as long as the number of variables indicating their group. Item form different
#'              groups will have different colors in the plot.
#' @return A plot of the means with associated errors bars (2*SE)
#' @export



plot_idiff <- function(data, guess = .25, sort = "orig", group = NULL){
  
  data <- if(class(data)[2] == "describe") data else psych::describe(data)
  
    data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(., var="item") %>%
    dplyr::mutate(group = group) %>% 
    dplyr::mutate(item_n = paste0(item, "\n", "(n=", n, ")"),
                  item_n = if(sort == "asc"){fct_reorder(item_n, mean)} 
                           else if(sort == "desc") {fct_reorder(item_n, desc(mean))} 
                           else {item_n = item_n}) %>%
    ggplot2::ggplot(., aes(x=item_n, y=mean, color = group)) +
                    geom_point() +
                    coord_cartesian(ylim = c(0,1)) + 
                    scale_y_continuous(breaks=c(seq(0,1,0.1))) +
                    geom_hline(yintercept=guess, linetype = "dashed") +
                    geom_errorbar(aes(ymin=mean-1*se, ymax=mean+1*se, width = 0.3)) +
                    labs(x = "", y = "Item difficulty", caption = "Error bars = 1*SE") +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

}

