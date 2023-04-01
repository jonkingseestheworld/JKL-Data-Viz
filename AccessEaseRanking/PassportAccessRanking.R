
## Date: 2023-03-24
## Author: Johnny King L
## Source: Henley & Partners - Henley Passport Index



library(tidyverse, quietly = TRUE)
#library(stringr)

library(patchwork)
library(ggplot2)

library(cowplot)
library(ggtext)
#library(ggforce)

#library(geomtextpath)

#library(sysfonts)


#sysfonts::font_add_google(name = "Berkshire Swash", "Swash")
#sysfonts::font_add_google(name = "Dosis", "Dosis")
#sysfonts::font_add_google(name = "Roboto", "Roboto")


# showtext::showtext_auto()

knitr::opts_chunk$set(warning=F)

# set.seed(2345)

## Loading data
rank2023 <- read.csv("AccessEaseRanking/PassportRank2023.csv")

rank_df <- rank2023 %>%
  mutate( Rank = rank(desc(Access), ties.method="min")) %>%
  mutate( Ease = case_when( Rank <=20 ~ "high",
                            Rank >= nrow(.)-20 ~ "low",
                            TRUE ~ NA) )

rank_hl_df <- rank_df %>%
  filter( Ease %in% c("high", "low")) %>%
  mutate( Country = case_when( grepl("South Korea", Country) ~ "S.Korea",
                               grepl("Congo", Country) ~ "Congo", 
                               grepl("Palestinian", Country) ~ "Palestine", 
                               T~Country)) %>%
  mutate(No_access = nrow(rank2023 ) - Access) %>%
  mutate( Index = seq(nrow(.), 1) ,
          Country = fct_reorder(Country, Index)) %>%
  mutate( Index = if_else(Ease == "high", Index+2, Index) ) %>%
  mutate( angle = if_else( Ease == "high", 0, 270 ) )




col_high <- "#03436b"
col_low <- "#d14d00"
col_bg <- "#f9f6ea" 
col_noAcc <- "#EAEAEA"



# Plot
rank_polarplot <- rank_hl_df %>%
  ggplot() + 
  geom_rect(aes(xmin=Index-0.5, xmax= Index+0.5,  ymin=0, ymax= Access,  fill = Ease), color = col_bg, size=0.5) + 
  geom_rect(aes(xmin=Index-0.5, xmax= Index+0.5,  ymin= Access, ymax= nrow(rank2023)), fill = col_noAcc, color = col_bg, size=0.5) + 
  geom_text(aes(x=Index,y= 0,label=Country),size=2.5, hjust=1.1) +     #,family="open"
  geom_text(aes(x=Index, y=Access+0.5, label=Access, color=Ease, angle=, angle=angle), size=2, family="open", hjust=0) +   #, hjust=1.   y= Access+1+(1/max(No_access)/(No_access))
  
  coord_polar(theta = "y") +    #, start=pi/6
  scale_y_continuous(limits=c(0, nrow(rank2023)+3)) +
  scale_x_continuous(limits=c(-5, max(rank_hl_df$Index)+1)) +
  scale_fill_manual( values =c('high'= col_high,'low'= col_low)) +
  scale_color_manual( values =c('high'= col_high,'low'= col_low)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = col_bg, color = NA),
    plot.background  = element_rect(fill = col_bg, color = NA),
    legend.position = "none"
    )

labeltxt_df <- data.frame( label = c("Most Access", "Least Access"),
                           x = c(46, 22),
                           y = c(0, 0),
                           col = c("high", "low"))


rank_polarplot +
  geom_text(data = labeltxt_df, aes(x = x, y= y, label = label, color = col), hjust = 0.5, vjust=0.1, size =2.5 ) 
  




center_text <- ggdraw() +
  draw_text("PASSPORT\nPOWER", x = 0, y = 0.5, lineheight=1, size = 30, hjust = 0, vjust = 1)





design <- c(
  area(1, 1, 100,100),
  area(40,40,60,60)
  )

rank_polarplot + center_text + 
  plot_layout(design = design) &
  theme(plot.background = element_rect(fill=NA,color=NA))






  



# ggsave("HP_similarity_network.png", Final_graph, dpi = 300, scale = 1, width = 9, height = 10.5, units = c("in") )


              