#' Plot proportion correct (difficulty) scores of dichotomous variables in two groups
#'
#'
#' Loads or creates two objects from psych::describe() and plots the
#' mean variables with the associated error bars. Can easily be
#' extended with ggplot2 commands.
#'
#' @param data1 Either a data frame containing (only) dichotomous variables or the output of psych::describe().
#' @param data2 Either a data frame containing (only) dichotomous variables or the output of psych::describe().
#' @param guess Optional. Indicate the (theoretical) guessing probability.
#' @param sort Optional. Keep variables in the same order as in the dataset (default), order them ascending order ("asc") or or
#'             or order them in descending order ("desc").
#' @param group Optional. Character vector containing labels for the two data frames. Items form different groups will have
#'              different colors and shapes.
#' @param obs Optional. Decides whether the number of observations is printed below the item label on the x-axis (Default: FALSE).
#' @return A plot of the means with associated errors bars (1*SE)
#' @import ggplot2
#' @import forcats
#' @import dplyr
#' @export

plot_idiff_paired <- function(data1, data2, guess = .25, sort = "orig", group = c("itemset1", "itemset2"), obs = FALSE){

  data1 <- if(class(data1)[2] == "describe"){

           data1 %>%
           as.data.frame() %>%
           rownames_to_column(var = "item") %>%
           mutate(group = group[1])

           } else {

           psych::describe(data1) %>%
           as.data.frame() %>%
           rownames_to_column(var = "item") %>%
           mutate(group = group[1])

           }

  data2 <- if(class(data2)[2] == "describe"){

           data2 %>%
           as.data.frame() %>%
           rownames_to_column(var = "item") %>%
           mutate(group = group[2])

           } else {

           psych::describe(data2) %>%
           as.data.frame() %>%
           rownames_to_column(var = "item") %>%
           mutate(group = group[2])

           }

  bind_rows(data1, data2) %>%
  dplyr::mutate(item = if(obs){paste0(item, "\n", "(n=", n, ")")}
                       else {item},
                item = if(sort == "asc"){fct_reorder(item, mean)}
                       else if(sort == "desc") {fct_reorder(item, desc(mean))}
                       else {factor(item, levels = item)}) %>%
  ggplot2::ggplot(., aes(x=item, y=mean, color = group, shape = group)) +
  geom_point() +
  coord_cartesian(ylim = c(0,1)) +
  scale_y_continuous(breaks=c(seq(0,1,0.1))) +
  geom_hline(yintercept=guess, linetype = "dashed") +
  geom_errorbar(aes(ymin=mean-1*se, ymax=mean+1*se, width = 0.3)) +
  labs(x = "", y = "Item difficulty", caption = "Error bars = 1*SE") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title=element_blank())


}
