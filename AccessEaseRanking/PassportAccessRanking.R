
## Date: 2023-03-24
## Author: Johnny King L
## Data Source & Idea from: Henley & Partners - Henley Passport Index


library(tidyverse, quietly = TRUE)

library(ggplot2)
library(cowplot)
library(patchwork)

library(ggtext)
library(ggimage)


knitr::opts_chunk$set(warning=F)
# set.seed(2345)


## Loading data
rank2023 <- read.csv("AccessEaseRanking/PassportRank2023.csv")

## Data cleaning/wrangling
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
  mutate( Index = if_else(Ease == "high", Index+3, Index) ) %>%
  mutate( angle = if_else( Ease == "high", 0, 270 ) )





## Plotting
col_high <- "#03436b"
col_low <- "#d14d00"
col_bg <- "#f9f6ea" 
col_noAcc <- "#F1F1F1"
        

rank_polarplot <- rank_hl_df %>%
  ggplot() + 
  geom_rect(aes(xmin=Index-0.5, xmax= Index+0.5,  ymin=0, ymax= Access,  fill = Ease), color = col_bg, size=0.5) + 
  geom_rect(aes(xmin=Index-0.5, xmax= Index+0.5,  ymin= Access, ymax= nrow(rank2023)+3), fill = col_noAcc, color = col_bg, size=0.5) + 
  geom_text(aes(x=Index,y= 0,label=Country),size=2, hjust=1.1) +     #,family="open"
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
    legend.position = "none",
    )


### Adding customised labels
labeltxt_df <- data.frame( label = c("Most Access", "Least Access"),
                           x = c(max(rank_hl_df$Index)+1, 22),
                           y = c(0, 0),
                           col = c("high", "low"))


rank_polarplot <- rank_polarplot +
  geom_text(data = labeltxt_df, aes(x = x, y= y, label = label, color = col), 
            hjust = 0.5, vjust=0.1, fontface="bold", size = 3 ) +
  geom_text(aes(x= max(rank_hl_df$Index)+1, y = 193, label = "N Countries Allowing \nVisa-free Access"), 
            size=3.25, lineheight=0.9, fontface = "plain", color = "black", angle = 20,
            hjust=1, vjust= -0.1)

### Layer1: Adding image background
bg_img = "img/AccessEaseRanking/stamps_bg.jpg"
polarplot_wBg <- ggbackground(rank_polarplot, bg_img, alpha=.1)



### Layer2: Adding (overlaying) central text
cntr_text <- ggdraw() +
  geom_richtext(aes(label="<span style='color:blue'>O</span>", x = 0.54, y = 0.54), alpha=0.1,
                fill=NA, label.size=NA, lineheight=0.8, size = 10, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="PASSP<span style='color:#d14d00'>O</span>RT<br>POWER", x = 0, y = 0.5), 
                fill=NA, label.size=NA, lineheight=0.8, size = 10, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="<b><span style='color:#03436b'>The World's Most</span><span style='color:#d14d00'> + Least Powerful Passports</span></b>", 
                    x = 0, y = 0.125), 
                fill=NA, label.size=NA, lineheight=0.8, fontface="italic", size = 2.5, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="Post-pandemic travel, revenge travel, regular<br>
                           travel - however you put it, travel is back. But<br>
                           for some the idea of hopping on a plane to visit<br>
                           a new destination has never been that simple", x = 0.01, y = 0.07), 
                fill=NA, label.size=NA,  lineheight=1, fontface="plain", size = 1.8, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="This list shows which passport holders have<br>
                           the highest and lowest visa-free travel scores,<br>
                           meaning the number of countries that citizens<br>
                           can travel to without having to obtain a visa.", x = 0.67, y = 0.07), 
                fill=NA, label.size=NA,  lineheight=1, fontface="plain", size = 1.8, hjust = 0, vjust = 1) 


### Layer3: Adding customised captions
caption_txt = ggdraw() + 
  geom_richtext(aes(label="<b>Source: Henley & Partners - Global Passport Ranking 2023</b>", 
                    x = 0.5, y = -0.8), 
                fill=NA, label.size=NA, lineheight=0.8, fontface="italic", size = 3.5, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="<b>Graphic: Johnny K Lau | @jonkingseestheworld</b>", 
                    x = 10.25, y = -0.8), 
                fill=NA, label.size=NA, lineheight=0.8, fontface="italic", size = 3.5, hjust = 1, vjust = 1)



## Final plot putting multiple layers together (layers1+2+3)
design <- c(
  area(1, 1, 100,100),   #t,l,b,r
  area(37,32,60,60),
  area(95,2,95,10)
  )

fin_plot <- polarplot_wBg + cntr_text + caption_txt +
  plot_layout(design = design) &
  theme(plot.background = element_rect(fill=NA,color=NA))




# Saving
ggsave("AccessEaseRanking/test3.png", fin_plot, dpi = 300, scale = 1, width = 8.5, height = 8.5, units = c("in") )


              