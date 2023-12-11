
## Date: 2023-12-10
## Author: Johnny King L
## Data Source: Kaggle > Jon Bown's imdb-christmas-movie-generator
## Reference: Graphic idea came from Dan Oehm - https://gradientdescending.com/tidy-tuesday-week-44-horror-articles/


# Load required packages -------------------------------------------------------
library(tidyverse)

library(packcircles)
library(ggimage)

library(showtext)
library(openai)

#library(config)


# Load data file & Derive new data ---------------------------------------------

#uncomment to retrieve the api_key
#config <- config::get()

Sys.setenv(
  OPENAI_API_KEY = config$api_key
)


xmas_films <- readr::read_csv("ChristmasMovies/Data/christmas_movies.csv")
# remove films that don't have a plot
xmas_films <- xmas_films %>%
  filter(!description %in% c("Add a Plot", "Plot unknown.") )

### Check for any duplicated movies/plots
xmas_films[duplicated(xmas_films[c("title", "release_year", "description")]),  ]



### Data derivation: getting descriptions for each movie plot using ChatGPT API
five_adj <- rep(NA, nrow(xmas_films))

for(k in 1:nrow(xmas_films)) {
  five_adj[k] <- create_chat_completion(
    model = "gpt-4",
    messages = list(
      list(
        "role" = "user",
        "content" = glue("Summarise the following movie plots in 5 adjectives:
        {xmas_films$description[k]}")
      )
    )
  )$choices$message.content
  
  cat(k, "\n")
  }

#write_rds(five_adj, "ChristmasMovies/Data/five_adj.rds")

#five_adj <- readRDS("ChristmasMovies/Data/five_adj.rds")


# 🤼 wrangling -----------------------------------------------------------------
df_adj_0 <- data.frame( adj = five_adj) |>
  rownames_to_column(var = "id") |>
  separate_longer_delim( adj, delim = ",") |>
  mutate( adj = str_remove_all( adj, "\\.") |> str_trim() |>
            str_to_title() )

## tallying the frequency of each word
df_adj <- df_adj_0 |>
  count(adj) |>
  arrange( desc( n ) ) |>
  filter( n > 5) |>
  mutate(
    adj = fct_reorder(adj, n, max),
    alpha = scales::rescale( n, to=c(0.5, 1))
  ) |>
  mutate(id = 1:n())
  

## creating circle properties
packing <- circleProgressiveLayout(df_adj$n, sizetype='area') %>%
  as_tibble()

df_centers <- df_adj %>%
  bind_cols(packing) %>%
  mutate(
    group = as.character(id), 
    alpha = scales::rescale(n, to=c(0.5, 1))
  )

df_poly <- circleLayoutVertices(packing, npoints = 50) %>%
  left_join(
    df_centers %>% select(id, n, adj),   
    by = "id"
    ) %>%
  as_tibble() %>%
  mutate(
    group = as.character(id), 
    alpha = scales::rescale(n, to=c(0.5, 1))
    )


# 📊 plot ----------------------------------------------------------------------

## Asthetics: fonts and palettes
bg <- "#142715"
txt <- "white"
title_col <- "#FFFF00"
sub_col <- "#F2F0D8"
fill_col =  "#E02526" 
  
font_add_google("Roboto Condensed", "roboto")
font_add_google("Festive", "festive")

ft <- "roboto"
ft1 <- "festive"

showtext_auto()

## Background image dir
bg_img = "ChristmasMovies/Data/holly_leaf.jpg"


## Texts 
title <- "Christmas Movies"

subtitle <- "What's common among them? 

Each plot of 765 films tagged as 'Christmas' on IMDB 
was summarised by GPT-4 in five adjectives. 
Here are the most common ones"

caption <- "Graphic by JOHNNY K LAU  |  Github: jonkingseestheworld  |  Data from Kaggle/jonbown"


## Plot
g_base <- ggplot() +
  geom_polygon(aes(x, y, group = group,  alpha = alpha), df_poly, colour = bg, fill=fill_col) +   
  geom_text(aes(x, y, size = n/100, group = group, label = ifelse(n > 25, paste0(adj, "\n", n), ""), alpha = alpha), 
            df_centers, family = ft, colour = txt, fontface = "italic", lineheight = 0.3) +
  scale_fill_manual(values = fill_col) +
  scale_colour_manual(values = fill_col) +
  scale_size_continuous(range = c(4,30)) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position="none") +
  coord_equal() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
    ) +
  theme(
    text = element_text(family = ft, size = 15, lineheight = 0.3, colour = "white"),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 400, hjust = 0.5, family = ft1, color = title_col, margin = margin(b=15)),
    plot.subtitle = element_text(hjust = 0.5, size = 75, color = sub_col ),
    plot.caption = element_text(hjust = 0.5, size = 50, color = sub_col, margin = margin(t = 20, b=25)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    panel.spacing = unit(4, "cm"),
    strip.text = element_blank()
    )


## Adding background image
ggbackground(g_base, bg_img, alpha=.1)


ggsave("ChristmasMovies/XmasFilms_gpt4_5adjectives.png", height = 18, width = 14)

