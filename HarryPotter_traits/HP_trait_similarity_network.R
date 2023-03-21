
## Date: 2023-03-11
## Author: Johnny King L
## Source: The Open-Source Psychometrics Project

## Acknowledgement - The illustration output here is adopted and modified from TamayoLeivaJ's example


library(tidyverse, quietly = TRUE)
library(stringr)

library(igraph)
library(tidygraph)
library(ggraph)

library(ggtext)
library(ggforce)

library(sysfonts)


sysfonts::font_add_google(name = "Berkshire Swash", "Swash")
sysfonts::font_add_google(name = "Dosis", "Dosis")
sysfonts::font_add_google(name = "Roboto", "Roboto")


showtext::showtext_auto()

knitr::opts_chunk$set(warning=F)

set.seed(2345)

## Loading data
HP_psydf <- read.csv("Harrypotter_traits/HarryPotter_traits_rating.csv")


## Data wrangling
### Set criteria to decide which (personality) attributes to keep
HP_psydf <- HP_psydf %>%
  ## keep only attributes that have a 'consistent' avg rating of >= 75 (out of 100)
  filter(avg_rating >= 75) %>% 
  ## remove items that are emoticon-based
  filter( !grepl("[^\x01-\x7F]", question)) %>%
  ## if duplicated, select only the first row of the group-by data ordered by 'avg_rating' in desc order (using slice_max)
  group_by(char_name, question) %>%
  slice_max(order_by = avg_rating, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  ## for each (group-by) question, give it a unique numeric identifier
  group_by(question) %>%
  mutate(
    question_id = dplyr::cur_group_id(),
    personality = stringr::str_c(question_id, personality, sep = "_")
    ) %>% 
  ungroup() %>%
  # keep only a few selected variables/columns
  select(char_name, question, personality, avg_rating) 



## Creating network elements (network to visualise similarity in personality attributes among Harry Potter characters)
### Edges: to prepare a dataframe to be used by 'graph_from_data_frame()' later ###
psy_edges  <- HP_psydf %>% 
  ## for each character (use 'group_by(char_name)'), concatenate all associated personality inside a list 
  group_by(char_name) %>%
  summarise(personality = list(as.character(personality))) %>%
  rename("from" = "char_name") %>% 
  ## crossing the 2-column dataframe itself to get all combinations (vars have also been renamed in the 2nd list)
  tidyr::crossing(., rename(., "to" = "from", "personality_to" = "personality")) %>%
  ## 'map2_' functions iterate two args at the same time
  ## map2_dbl takes 'list' inputs to generate a 'vector' output and '~length(intersect())' functions here count the number of elements that are shared having intersected the two lists
  mutate(frequency = purrr::map2_dbl(personality, personality_to, ~length(base::intersect(.x, .y)))) %>% 
  select(from, to, frequency) %>% 
  filter(from != to) %>%  ## remove rows having the same character in the pair
  ## categorise the level of similarity (frequency of the same attributes recorded) in each pair of characters
  mutate(similarity = case_when(frequency <= quantile(frequency, probs = 0.75) ~ "q75",
                                frequency <= quantile(frequency, probs = 0.85) ~ "q85",
                                frequency <= quantile(frequency, probs = 0.95) ~ "q95",
                                frequency <= quantile(frequency, probs = 0.99) ~ "q99",
                                TRUE ~ "Top1"
                                ) ) %>%
  arrange(-frequency)


### Vertices ###
psy_vert <- psy_edges %>%
  group_by(to) %>%
  ## the influence of a character here as determined by how much he/she share similar personality traits with the others
  summarise( nSimilar = sum(frequency)) %>%
  select(to, nSimilar)


### Creating a network object - taking the vert & edges lists
psy_ig <- igraph::graph_from_data_frame(d = psy_edges, 
                                           vertices = psy_vert, 
                                           directed = FALSE)

### tidygraph
psy_tg <- tidygraph::as_tbl_graph(psy_ig) %>% 
  tidygraph::activate(nodes) %>% 
  mutate(label = name)




## Prepping for creation of network graph
### Setting Plot color scheme ###
bg_color    <-  "#000035"  
lines_color    <- "#A6444C"
title_color    <- "#fee100" 
subtitle_color <- "#ffef74"
annot_color <- "#ffbe47"
text_color     <- "#F2F2F2"
caption_color  <- "#D6cfc7"
            



### Graph ###
HP_graph <- psy_tg %>%
  ggraph(layout = "igraph", algorithm = 'kk') +
  geom_edge_arc(aes(edge_width = similarity, alpha = similarity, colour = similarity), lineend = "round", strength = .08) +
  ## The next two lines create dots with shadow
  geom_node_point(aes(colour = nSimilar), fill = bg_color, size = log(psy_vert$nSimilar*1.5) * 1.5,  alpha = 1.0, shape = 21) +  
  geom_node_point(aes(colour = nSimilar), size = log(psy_vert$nSimilar*1.0) * 1.0, alpha = 1.0) +
  ## adding node label (name of each character)
  geom_node_label(aes(label = label), colour = text_color, size = log(psy_vert$nSimilar)*1.4, family = "Dosis", 
                  fontface = "bold", repel = FALSE, nudge_y = -0.35, nudge_x = 0.00, alpha = 0.6, 
                  fill = bg_color, label.size = NA) +
  ### Customisation of scales ###
  ### split by the 5 differemt similarity categories
  scale_edge_width_manual(values = c(seq(0.4, 1.0, length.out = 4), 1.5), breaks = c("q75","q90","q95","q99","Top1")) +  
  scale_edge_alpha_manual(values = c(seq(0.2, 0.6, length.out = 4), 0.9), breaks = c("q75","q90","q95","q99","Top1")) +
  scale_edge_colour_manual(values = viridis::inferno(n = length(unique(psy_edges$similarity)), begin = 0.5, end = 0.9, direction = 1)) +
  scale_colour_gradientn(colors = viridis::plasma(n = length(unique(psy_vert$nSimilar)), begin = 0.5, end = 0.9, direction = 1)) +
  guides(edge_width = "none", edge_alpha = "none", colour = "none", edge_colour = "none") +
  #coord_cartesian(ylim = c(0.1, 1.5)) +
  ### Theme: Plot Aesthetic ###
  theme_void()  +
  theme(
    panel.background = element_rect(fill = bg_color, color = NA),
    plot.background  = element_rect(fill = bg_color, color = NA),
    ## Title & Caption design
    plot.title.position = "panel",
    plot.title = ggtext::element_textbox(color = title_color, family = "Swash", face = "bold", 
                                          size = 145, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, 
                                          margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")),   ## trbl
    plot.subtitle = ggtext::element_markdown(color = subtitle_color, family = "Dosis", face = "plain", 
                                          size = 38, hjust = 0.5, halign = 0.5, vjust = 0.5, valign = 0.5, 
                                          margin = unit(c(0.2, 0.1, 1, 0.1), "cm")),
    plot.caption.position = "panel",
    plot.caption = ggtext::element_markdown(color = caption_color, family = "Roboto", size = 30, hjust = 1, vjust = 0, lineheight = 0.5,
                                            margin = margin(t = 1.3, r = 0.1, b = 0.5, l = 0.0, unit = "cm")),
    ### margin ###
    plot.margin = margin(t = 0.5, r = 1.8, b = 0.1, l = 1.2, unit = "cm")
    ) +
  ### Title & caption content ###
  labs(x = "", y = "",
       title = "Harry Potter",
       subtitle = "Similarity of the profiles of personality attributes of the characters",
       caption = "Graphic by Johnny K Lau <br>Github: jonkingseestheworld" 
  )   


### Adding annotation & customised legend ###
#### Annotations
annot_title <- "Data from 'The Open-Source Psychometrics Project' -- <br>"
annot_content <- c("Personality attributes (descriptive adjectives) related to the characters from Harry Potter and other TV series 
                     were rated by 3 million survey respondents. The network here shows the level of similarity of the attribute profiles of the main HP characters. 
                     Only the strongest attributes (with an average rating > 80%) are selected for each character. 
                     Among all, Hermoine & minerva are considered having the most similar characteristics. For villains, Voldemort, Bellatrix and Dolores share a lot of common attributes.")

annotation_text <- annot_title %>% 
  paste(stringr::str_wrap(annot_content, 76)) %>% 
  stringr::str_replace_all("\n","<br>")
                           
              
#### Customised legend
label_data <- tibble(x = seq(-1.5, 1.4, length.out = 30),
                     y = seq(-7.6, -7.6, length.out = 30),
                     color  = psy_vert %>% pull(nSimilar) %>% sort(),
                     alpha  = psy_vert %>% pull(nSimilar) %>% sort(),    
                     size   = (psy_vert %>% pull(nSimilar) %>% sort())*0.85 )

                           
Final_graph <- HP_graph +
  ### using ggtext to add annotation text
  ggtext::geom_richtext(aes(x = 2.4, y = -7), label = annotation_text, 
                        color = annot_color, size = 9, family = "Roboto", face = "plain", fill = "transparent", 
                        label.size = NA, hjust = 0,  vjust = 0.5,  lineheight=0.5, halign = 0, valign = 0.5,
                        margin = margin(t = 0.1, r = 0.1, b = 0.5, l = 0.1, unit = "cm")) +    
  ### Customised legend
  ggforce::geom_link2(data = label_data, aes(x = x, y = y, colour = color, alpha = alpha, size = log(size)*1.1, group = 1 ), lineend = 'round') + 
  ### taking only the first and last points for the 'dot' indicators
  geom_point(data = label_data[c(1, 30),], aes(x = x, y = y, colour = color), 
             fill = bg_color, alpha = 1.0, size = range(log(label_data$size) * 1.5), shape = 21) +
  geom_point(data = label_data[c(1, 30),], aes(x = x, y = y, colour = color), alpha = 1.0, size = range(log(label_data$size))) +
  geom_text(data = label_data[c(1, 30),], aes(x = x, y = y + 0.002), label = c("-","+"), colour = bg_color, 
            alpha = 1.0, size = sort(log(label_data$size))[c(1,30)]*1.5, family = "Dosis", fontface = "bold", 
            hjust = 0.5, vjust = 0.5) +
  geom_text(data = label_data[15,], aes(x = x, y = y + 0.65), label = "How to interpret the network?", 
            colour = annot_color, alpha = 1.0, size = sort(log(label_data$size))[30]*2, family = "Dosis", fontface = "plain") +
  geom_text(data = label_data[15,], aes(x = x, y = y + 0.25), label = "Similarity Scale", 
            colour = text_color, alpha = 1.0, size = sort(log(label_data$size))[30]*1.5, family = "Dosis", fontface = "bold") +
  scale_alpha_continuous(range = c(0.0, 1.0)) +
  scale_size_continuous(range = c(0.2, 2.0)) +
  guides(colour = "none", alpha = "none", size = "none")


# ggsave("HP_similarity_network.png", Final_graph, dpi = 300, scale = 1, width = 9, height = 10.5, units = c("in") )


              