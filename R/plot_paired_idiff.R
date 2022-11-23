# v0.1
# luc.watrin@uni-ulm.de
# function to plot item difficulties of (presumably) parallel items side by side
# NAMING ETC SPECIFIC TO THE WMC KIDS STUDY DORNSTADT 10/2022
# can easily be extended with ggplot2 commands

plot_paired_idiff <- function(desc_kid, desc_adult, group = NULL){
  
  desc_kid <- desc_kid %>% 
              as.data.frame() %>% 
              rownames_to_column(var = "trial") %>% 
              mutate(type = "kid",
                      trial = str_replace(trial, "kid", ""))
  
  desc_adult <- desc_adult %>% 
                as.data.frame() %>% 
                rownames_to_column(var = "trial") %>% 
                mutate(type = "adult",
                       trial = str_replace(trial, "adu", ""))
  
  group <- rep(group, 2)

  rbind(desc_kid, desc_adult) %>% 
  ggplot(aes(x=trial, y=mean, shape = type, color = group)) +
    geom_point(position = position_dodge(width = .5), size = 3) +
    coord_cartesian(ylim = c(0,1)) + 
    scale_y_continuous(breaks=c(seq(0,1,0.1))) +
    geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, width = 0.3), position = position_dodge(width = .5)) +
    labs(x = "", 
         y = "Item difficulty",
         caption = "Error bars = 2*SE") +
    theme_bw() + 
    ggeasy::easy_rotate_x_labels(angle = 45, side = "right")
  
}
