#' Plot proportion correct (difficulty) scores of dichotomous variables in two groups
#'
#'
#' Loads or creates two objects from psych::describe() and plots the
#' mean variables with the associated error bars. Can easily be
#' extended with ggplot2 commands.
#'
#' @param data1 Either a data frame containing (only) dichotomous variables or the output of psych::describe() of group (or itemset) 1.
#' @param data2 Either a data frame containing (only) dichotomous variables or the output of psych::describe() of group (or itemset) 2.
#' @param guess Optional. Indicate the (theoretical) guessing probability.
#' @param sort Optional. Keep variables in the same order as in the dataset (default), order them ascending order ("asc") or or
#'             or order them in descending order ("desc").
#' @param group Optional. Character vector as long as the number of variables indicating their group. Item form different
#'              groups will have different colors in the plot.
#' @param obs Optional. Decides whether the number of observations is printed below the item label on the x-axis (Default: FALSE).
#' @return A plot of the means with associated errors bars (1*SE)
#' @import ggplot2
#' @import forcats
#' @import dplyr
#' @export

plot_idiff_paired <- function(data1, data2, guess = .25, sort = "orig", group = NULL, obs = FALSE){

  data1 <- if(class(data1)[2] == "describe") data1 else psych::describe(data2)
  data2 <- if(class(data1)[2] == "describe") data2 else psych::describe(data2)

  data1 <- data1 %>%
           as.data.frame() %>%
           rownames_to_column(var = "item") %>%
           mutate(version = "data1")

  data2 <- data2 %>%
           as.data.frame() %>%
           rownames_to_column(var = "item") %>%
           mutate(version = "data2")

  bind_rows(data1, data2) %>%
  dplyr::mutate(group = group) %>%
  dplyr::mutate(item = if(obs){paste0(item, "\n", "(n=", n, ")")}
                       else {factor(item, levels = unique(item))},
                item = if(sort == "asc"){fct_reorder(item, mean)}
                       else if(sort == "desc") {fct_reorder(item, desc(mean))}
                       else {factor(item, levels = unique(item))}) %>%
  ggplot2::ggplot(., aes(x=item, y=mean, color = group, group = version, shape = version)) +
           geom_point(position = position_dodge(width = .5)) +
           coord_cartesian(ylim = c(0,1)) +
           scale_y_continuous(breaks=c(seq(0,1,0.1))) +
           geom_hline(yintercept=guess, linetype = "dashed") +
           geom_errorbar(aes(ymin=mean-1*se, ymax=mean+1*se, width = 0.3), position = position_dodge(width = .5)) +
           labs(x = "", y = "Item difficulty", caption = "Error bars = 1*SE") +
           theme_bw() +
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))

}
