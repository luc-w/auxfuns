# v0.1
# luc.watrin@uni-ulm.de
# convenience function to plot histograms of two groups
# NAMING ETC SPECIFIC TO THE WMC KIDS STUDY DORNSTADT 10/2022
# can easily be extended with ggplot2 commands

plot_paired_hist <- function(data, var_kid, var_adult){
  
  data %>% 
    select({{var_kid}}, {{var_adult}}) %>% 
    pivot_longer(everything(), names_to = "type", values_to = "score") %>% 
    ggplot(aes(x = score, fill = type)) +
      geom_histogram(position = "dodge") +
      scale_x_continuous(limits = c(0,1), breaks=c(seq(0,1,0.1))) +
      labs(x = "Total score", y = "Count") +
      theme_bw() 
}

