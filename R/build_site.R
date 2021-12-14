# build_site.R
# create the site

library(magrittr)
library(readr)
library(glue)
library(forcats)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(htmltools)
library(ggplot2)
library(cowplot)
library(Manu)
library(showtext)
showtext_auto()

font_add_google("Nunito Sans", "nunitosans")
CHART_FONT <- "nunitosans"

BS_THEME <- "lux"
source("R/_template.R", encoding = "UTF-8")



# Load Metadata --------------------------------------------------------

themes <- 
  read_tsv("data/themes.tsv", col_types = "icccc")

# shuffle to give an interesting start
maps <- read_tsv("data/maps.tsv", col_types = "ciccc") %>% sample_frac(1)
packages <- read_tsv("data/packages.tsv", col_types = "cc")
descriptions <- read_tsv("data/descriptions.tsv", col_types = "ccc")
links <- read_tsv("data/links.tsv", col_types = "cccc")
stopifnot(links %>% map(~ !is.na(.x)) %>% reduce(and))
images <- read_tsv("data/images.tsv", col_types = "ccc")
stopifnot(images %>% map(~ !is.na(.x)) %>% reduce(and))
aspect_cols <-
  read_tsv("data/aspects.tsv", col_types = "ciiiii") %>% 
  mutate(aspect_class = glue("col-xs-{xs} col-sm-{sm} col-md-{md} col-lg-{lg} col-xl-{xl}"))

map_years <- 
  maps %>% 
  group_by(Year) %>% 
  summarise(num_maps = n(),
            num_cartographers = n_distinct(handle)) %>% 
  ungroup()

total_num <- 
  maps %>% 
  summarise(num_maps = n(),
            num_cartographers = n_distinct(handle))

map_days <- 
  maps %>% 
  group_by(Year, Day) %>% 
  count() %>% 
  ungroup()

map_themes <- 
  themes %>% 
  left_join(maps %>% mutate(n = 1),
             by = c("Year", "Day")) %>% 
  group_by(Grouping, Theme) %>% 
  summarise(num_maps = sum(n, na.rm = T)) %>% 
  ungroup()

map_packages <- 
  packages %>% 
  separate_rows(packages,
                sep = ",") %>% 
  mutate(packages = packages %>% str_trim()) %>% 
  count(packages) %>% 
  rename(package = packages,
         num_maps = n)
  

# Make Index --------------------------------------------------------------

index_page <- 
  paste(
    index_header,
    div(class = "row",
        div(class = "col-12",
            p("In 2019 Topi Tjukanov", a(href="https://twitter.com/tjukanov/status/1187713840550744066", "announced"),
              "the #30DayMapChallenge on Twitter out of the blue, and there was so much enthusiasm that it has",
              "become an annual event - see",
              a(href="https://github.com/tjukanovt/30DayMapChallenge", "https://github.com/tjukanovt/30DayMapChallenge")),
            p("The event is a fun and friendly opportunity for cartographical practice, to try new things without the",
              "pressure of perfection, and to be inspired by what other people have created.",
              "And what's better than a cool map to inspire you to try something similar?",
              "A cool map with code so you can immediately reproduce and adapt it!",
              "(At least if you know, or want to learn,", a(href="https://www.r-project.org/", "R"), ")."),
            h3("Where are the maps?"),
            p(span(class = "text-info",
                  "Click through to the", a(href = "maps.html", "map gallery"),
                  "where you can explore some maps."),
              "Filter by year, challenge theme, or R packages used, and search by",
              "manually added tags (eg 'interactive', 'africa', 'sport') or user handle."
              ),
            p("Then click on a map to see a larger version, and links to the code,",
              "tweet and web page (if one exists)."),
            h3("Curatorial Notes"),
            h4("Selection"),
            p("The aim is to include all submissions from the #30DayMapChallenge from 2019 onwards that have full code",
              "available (not just a link to a related tutorial). What counts as both a 'map' and a 'submission' is usually",
              "straightforward, but sometimes I have to make a judgement call."),
            h4("Themes"),
            p("The 30 challenge themes from each year have been classified as a 'type', a 'topic' or a 'constraint', and then",
              "some have been combined to try and make them as consistent as possible across the years.",
              "For example 'hydrology' and 'water' are combined, as are 'urban', 'rural' and 'urban/rural'."),
            h4("Packages"),
            p("Packages are taken from the code, generally from library calls at the start of the script.",
              "But sometimes a package is loaded and never used, or called directly later on, so these are",
              "not necessarily 100% accurate."),
            p("Trying to keep the focus on map-related packages, I've ignored some of the minor unrelated ones",
              "(eg {curl}, {janitor}). I've listed {dplyr} and friends as {tidyverse}, whether the meta-package",
              "itself is loaded directly or not."),
            h4("Tags"),
            p("I've tried to use tags as a simple place to record all other classification information - countries,",
              "cities, map styles, general topics, etc. And whilst I've tried to be as complete as possible, this is",
              "deliberately not a formally structured and complete classification system so there will likely be",
              "missing and inconsistent tagging."),
            h3("Updates"),
            p("Now that the 2021 challenge has finished I will find some time to catch up on",
              "all the previous maps. If I have some 2021 maps from you, then I will get to the others",
              "eventually. If I don't have any of your maps and you're not on my",
              a(href="todo.txt", "todo list"),
              "then drop me a line on",
              a(href="https://twitter.com/dakvid", "Twitter"),
              "or email (change a dot to an @ in the domain).",
              "I've only got a few people listed from 2019 and 2020 so far, so please let me know",
              "of others."),
            p("Note that I have only been following the hashtag on Twitter (and my",
              "previous galleries have been Twitter-only) but I'm happy to include",
              "maps posted anywhere - Instragram, LinkedIn, Github, ..."),
            h3("Colophon"),
            p("This site is made with", a(href = "https://getbootstrap.com/", "Bootstrap 4"),
              "and the", a(href = "https://bootswatch.com/lux/", "Lux"), "theme from Bootswatch.",
              "The gallery was made with", a(href="https://vestride.github.io/Shuffle/", "shuffle.js"),
              "and", a(href="https://github.com/aFarkas/lazysizes", "lazysizes."),
              "The data munging and HTML construction is performed by some rough",
              a(href = "https://github.com/dakvid/3RDayMapChallenge", "R code."),
              "It needs a bit more polishing, especially on mobile, but I've",
              "preferred to spend my time loading the entries and making my",
              "own maps."),
            p("I quickly cobbled together a gallery of all the maps in 2019 just after",
              "the challenge ended - it wasn't sophisticated but it worked.",
              "In 2020 I was able to reuse the gallery code, but it was even more",
              "time-consuming to process the tweets, and the page was only just",
              "working with the amount of maps people produced (a sign of success!).",
              "I decided early on that I wouldn't redo this in 2021 as I wouldn't",
              "have time either to process the tweets or write a new more",
              "technically viable gallery. But the thought occurred to try a",
              "gallery of just R maps with code - I could reuse the gallery code",
              "and create a useful resource."),
            p(tags$em("David Friggens, November 2021.")),
        )
    ),
    index_footer,
    collapse = "\n"
  )

write_file(index_page, "index.html")





# # Make Statistics ---------------------------------------------------------

total_num_themes <- 
  themes %>% 
  distinct(Theme) %>% 
  nrow()

# # > Graphs ----------------------------------------------------------------

PALETTE <- get_pal("Hihi")
COLOUR_MAIN <- PALETTE[1]
COLOUR_SUB <- PALETTE[2]
COLOURS_3 <- PALETTE[3:1]

# top_cartographers
num_limit_cartographers <- 30L
g_cartographers_data <- 
  maps %>% 
  group_by(handle) %>% 
  summarise(n_total = n()) %>% 
  ungroup() %>% 
  arrange(-n_total) %>% 
  head(num_limit_cartographers) %>% 
  inner_join(
    maps %>% 
      group_by(handle, Year) %>% 
      summarise(n = n()) %>% 
      ungroup(),
    by = "handle"
  ) %>% 
  mutate(handle = handle %>% fct_inorder() %>% fct_rev(),
         Year = factor(Year, levels = 2019:2021) %>% fct_rev())
g_cartographers <- 
  ggplot(g_cartographers_data,
         aes(x = handle, y = n)) +
  geom_col(aes(fill = Year)) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  scale_fill_manual(values = COLOURS_3) +
  guides(fill = guide_legend(reverse = TRUE,
                             keyheight = 1)) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT,
                      font_size = 38) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("Top {num_limit_cartographers} Cartographers"),
       caption = "https://david.frigge.nz/3RDayMapChallenge") +
  theme(legend.position = "bottom")
ggsave(plot = g_cartographers,
       filename = "top_cartographers.png",
       path = "images/",
       width = 7, height = 5.8, units = "cm", scale = 3)

# top_themes
num_limit_themes <- 20L
g_themes_data <- 
  maps %>% 
  inner_join(themes,
             by = c("Year", "Day")) %>% 
  group_by(Theme) %>% 
  summarise(n = n(),
            cartographers = n_distinct(handle)) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  head(num_limit_themes) %>% 
  mutate(Theme = Theme %>% fct_inorder() %>% fct_rev())
g_themes <- 
  ggplot(g_themes_data,
         aes(x = Theme, y = n)) +
  geom_col(fill = COLOUR_MAIN) +
  geom_col(aes(y = cartographers),
           fill = COLOUR_SUB, width = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT,
                      font_size = 36) +
  labs(x = NULL, y = NULL,
       title = glue("Top {num_limit_themes} Challenge Themes"),
       caption = "https://david.frigge.nz/3RDayMapChallenge")
ggsave(plot = g_themes,
       filename = "top_themes.png",
       path = "images/",
       width = 7, height = 4.2, units = "cm", scale = 3)

# maps per day
g_days_data <- 
  expand_grid(
    Year = themes %>% distinct(Year) %>% pull(),
    Day = themes %>% distinct(Day) %>% pull()
  ) %>% 
  left_join(
    maps %>% 
      group_by(Year, Day) %>% 
      count() %>% 
      ungroup(),
    by = c("Year", "Day")
  ) %>% 
  arrange(Year, Day) %>% 
  replace_na(list(n = 0L)) %>% 
  mutate(Day = Day %>% fct_inseq() %>% fct_rev(),
         Year = Year %>% as.character() %>% fct_inseq() %>% fct_rev())
g_days <- 
  ggplot(g_days_data,
         aes(x = Day, y = n)) +
  geom_col(aes(fill = Year),
           position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  scale_fill_manual(values = COLOURS_3) +
  guides(fill = guide_legend(reverse = TRUE,
                             keyheight = 1)) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT,
                      font_size = 36) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Maps by Challenge Day",
       subtitle = "From the theme day, not necessarily the calendar day they were published.",
       caption = "https://david.frigge.nz/3RDayMapChallenge") +
  theme(legend.position = "bottom")
ggsave(plot = g_days,
       filename = "day_by_day.png",
       path = "images/",
       width = 7, height = 7, units = "cm", scale = 3)

# top areas
num_limit_areas <- 20L
g_areas_data <- 
  descriptions %>% 
  inner_join(maps, by = "mapid") %>% 
  mutate(area = tags %>% str_extract("^[^,]+")) %>% 
  group_by(area) %>% 
  summarise(n = n(),
            cartographers = n_distinct(handle)) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  head(num_limit_areas) %>% 
  mutate(area = if_else(nchar(area) <= 3,
                        area %>% str_to_upper(),
                        area %>% str_to_title()),
         area = area %>% fct_inorder() %>% fct_rev())
g_areas <- 
  ggplot(g_areas_data,
         aes(x = area, y = n)) +
  geom_col(fill = COLOUR_MAIN) +
  geom_col(aes(y = cartographers),
           fill = COLOUR_SUB, width = 0.3) +
  geom_text(data = g_areas_data,
            aes(x = area, y = n, label = n),
            hjust = 1, nudge_y = -1,
            colour = "white",
            size = 10,
            family = CHART_FONT) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT,
                      font_size = 36) +
  labs(x = NULL, y = NULL,
       title = glue("Top {num_limit_areas} Areas Mapped"),
       caption = "https://david.frigge.nz/3RDayMapChallenge")
ggsave(plot = g_areas,
       filename = "top_areas.png",
       path = "images/",
       width = 7, height = 5.5, units = "cm", scale = 3)

# top packages
num_limit_packages <- 30L
g_packages_data <- 
  packages %>% 
  separate_rows(packages, sep = ",") %>% 
  inner_join(maps %>% select(mapid, handle),
             by = "mapid") %>% 
  group_by(packages) %>% 
  summarise(n = n(),
            cartographers = n_distinct(handle)) %>% 
  ungroup() %>% 
  filter(packages != "tidyverse") %>% 
  arrange(-n) %>% 
  head(num_limit_packages) %>% 
  mutate(package = packages %>% fct_inorder() %>% fct_rev())
g_packages <- 
  ggplot(g_packages_data,
         aes(x = package, y = n)) +
  geom_col(fill = COLOUR_MAIN) +
  geom_col(aes(y = cartographers),
           fill = COLOUR_SUB, width = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, .02))) +
  coord_flip() +
  theme_minimal_vgrid(font_family = CHART_FONT,
                      font_size = 36) +
  labs(x = NULL, y = NULL,
       title = "Top 30 Packages",
       caption = "https://david.frigge.nz/3RDayMapChallenge")
ggsave(plot = g_packages,
       filename = "top_packages.png",
       path = "images/",
       width = 7, height = 6, units = "cm", scale = 3)


# # > Page & Save -----------------------------------------------------------

stats_page <-
  paste(
    stats_header,
    stats_header_nav,
    div(class = 'container',
        h1("Statistics"),
        div(class = "row",
            div(class = "col-12",
                p(glue("This gallery has {total_num$num_maps} maps by {total_num$num_cartographers} cartographers in total. ",
                       "From 2019, there are {map_years %>% filter(Year == 2019) %>% pull(num_maps)} maps",
                       " by {map_years %>% filter(Year == 2019) %>% pull(num_cartographers)} people; ",
                       "from 2020, there are {map_years %>% filter(Year == 2020) %>% pull(num_maps)} maps",
                       " by {map_years %>% filter(Year == 2020) %>% pull(num_cartographers)} people; ",
                       "from 2021, there are {map_years %>% filter(Year == 2021) %>% pull(num_maps)} maps",
                        "by {map_years %>% filter(Year == 2021) %>% pull(num_cartographers)} people.")),
                
                h3("People"),
                p("This gallery exists because of the generosity of people sharing their code. Here are the",
                  glue("top {num_limit_cartographers} cartographers in the gallery."),
                  "Of course, this gives no indication of how many maps they made without R!"),
                img(src = "images/top_cartographers.png", style="width: 800px;"),
                
                h3("Themes"),
                p(glue("These are the top {num_limit_themes} of the {total_num_themes} meta-themes."),
                  "Monochrome's supremacy is unsurprising, as it includes 4-5 days each year.",
                  "The main bar is the total number of maps and the smaller bar is the",
                  "number of individual cartograhers."),
                img(src = "images/top_themes.png", style="width: 880px;"),
                
                p("How many maps do we have for each day?",
                  "(Remember that some days are suggested not to use R!)"),
                img(src = "images/day_by_day.png", style="width: 880px;"),
                
                h3("Geographic Areas"),
                p("This gallery is mostly about technique over content, but it's interesting to see the",
                  "most popular areas mapped. These are taken from the first tag assigned, which is",
                  "generally the main country/continent.",
                  "The main bar is the total number of maps and the smaller bar is the",
                  "number of individual cartograhers."),
                img(src = "images/top_areas.png", style="width: 800px;"),
                
                h3("Packages"),
                p("The most popular packages recorded, excluding the meta tidyverse.",
                  "The main bar is the total number of maps and the smaller bar is the",
                  "number of individual cartograhers."),
                img(src = "images/top_packages.png", style="width: 800px;"),

            )
        ),
    ),
    stats_footer,
    collapse = "\n"
)

write_file(stats_page, "stats.html")
        




# Make Maps ---------------------------------------------------------------


# > Cards -----------------------------------------------------------------

make_a_card <- 
  function(mapid, extension, Year, Theme, handle, date_posted, packages, tags, aspect, aspect_class, ...) {
    div(class = glue("map-card {aspect_class}"),
        `data-theme` = Theme,
        `data-year` = Year,
        `data-packages` = packages,
        `data-date-posted` = date_posted,
        `data-handle` = handle %>% str_to_lower(),
        `data-tags` = tags,
        a(`data-toggle` = "modal",
          `data-target` = glue("#{if_else(str_detect(mapid,'^[0-9]'),'_','')}{mapid}_details"),
          div(class = "card",
              div(class = glue("aspect aspect--{aspect}"),
                  div(class = "aspect__inner",
                      img(class = "card-img lazyload",
                          `data-src` = glue("thumbnails/{mapid}.{extension}"))
                  )
              ),
              div(class = "card-img-overlay",
                  span(class = "badge badge-pill badge-primary",
                       Theme),
                  span(class = "badge badge-pill badge-secondary",
                       handle)
              )
          )
        )
    )
  }

map_cards <- 
  maps %>% 
  inner_join(packages, by = "mapid") %>% 
  inner_join(images, by = "mapid") %>% 
  inner_join(descriptions, by = "mapid") %>% 
  inner_join(themes, by = c("Year", "Day")) %>% 
  inner_join(aspect_cols, by = "aspect") %>% 
  pmap(make_a_card)




# > Modals ----------------------------------------------------------------

make_a_modal <- 
  function(mapid, extension, Year, Day, Theme, Challenge, handle, date_posted, packages, map_tags, description, code_link, sm_link, web_link, ...) {
    div(id = glue("{if_else(str_detect(mapid,'^[0-9]'),'_','')}{mapid}_details"),
        class = "modal fade",
        tabindex = "-1",
        role = "dialog",
        div(class = "modal-dialog modal-xl",
            role = "document",
            div(class = "modal-content",
                div(class = "modal-header",
                    h5(class = "modal-title",
                       glue("{Year} - Day {Day} - {Challenge} {if_else(Theme == Challenge, '', paste0('- (',Theme,')'))} by {handle}")),
                    tags$button(type = "button", class = "close",
                                `data-dismiss` = "modal",
                                `aria-label` = "Close",
                                span(`aria-hidden` = "true",
                                     "×"))
                    ),
                div(class = "modal-body",
                    img(style = "max-width: 100%;",
                        class = "lazyload",
                        `data-src` = glue("maps/{mapid}.{extension}")),
                    p(description),
                    p(glue("By {handle}")),
                    tags$ul(class = "list-group",
                            tags$li(class = "list-group-item",
                                    strong("Packages:"),
                                    if_else(packages == "_",
                                            "unclassified",
                                            packages %>% str_replace_all(",", "; "))),
                            tags$li(class = "list-group-item",
                                    strong("Tags:"),
                                    if_else(map_tags == "_",
                                            "unclassified",
                                            map_tags %>% str_replace_all(",", "; "))),
                           )
                    ),
                div(class = "modal-footer",
                    tags$button(type = "button", class = "btn btn-secondary",
                                `data-dismiss` = "modal",
                                "Close"),
                    tags$button(type = "button", class = "btn btn-info",
                                a(target = "_blank",
                                  href = code_link,
                                  "See Code")),
                    if (sm_link != "_") {
                      tags$button(type = "button", class = "btn btn-info",
                                  a(target = "_blank",
                                    href = sm_link,
                                    "See tweet"))
                    },
                    if (web_link != "_") {
                      tags$button(type = "button", class = "btn btn-info",
                                  a(target = "_blank",
                                    href = web_link,
                                    "See webpage"))
                    }
                )
                )
            )
        )
  }

maps_modals <- 
  maps %>% 
  inner_join(packages, by = "mapid") %>% 
  inner_join(images, by = "mapid") %>% 
  inner_join(descriptions, by = "mapid") %>% 
  inner_join(links, by = "mapid") %>% 
  inner_join(themes, by = c("Year", "Day")) %>% 
  rename(map_tags = tags) %>% 
  pmap(make_a_modal) %>% 
  map(as.character) %>% 
  paste(collapse = "\n")






# > Filter Theme ------------------------------------------------------

maps_filter_theme_type <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterThemeType",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      `data-toggle-tt` = "tooltip",
                      `data-placement` = "top",
                      title = "Some similar challenges are grouped as one 'theme'",
                      "Theme: Type"),
          div(class = "dropdown-menu mapfilter-theme",
              `aria-labelledby` = "FilterThemeType",
              map_themes %>% 
                filter(Grouping == "type") %>% 
                pmap(function (Theme, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Theme,
                              Theme,
                              span(class = "badge badge-primary badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_theme_constraint <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterThemeConstraint",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      `data-toggle-tt` = "tooltip",
                      `data-placement` = "top",
                      title = "Some similar challenges are grouped as one 'theme'",
                      "Constraint"),
          div(class = "dropdown-menu mapfilter-theme",
              `aria-labelledby` = "FilterThemeConstraint",
              map_themes %>% 
                filter(Grouping == "constraint") %>% 
                pmap(function (Theme, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Theme,
                              Theme,
                              span(class = "badge badge-primary badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_theme_topic_AL <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterThemeTopicAL",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      `data-toggle-tt` = "tooltip",
                      `data-placement` = "top",
                      title = "Some similar challenges are grouped as one 'theme'",
                      "Topic A-L"),
          div(class = "dropdown-menu mapfilter-theme",
              `aria-labelledby` = "FilterThemeTopicAL",
              map_themes %>% 
                filter(Grouping == "topic",
                       Theme < "M") %>% 
                pmap(function (Theme, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Theme,
                              Theme,
                              span(class = "badge badge-primary badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_theme_topic_MZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-primary dropdown-toggle",
                      type = "button",
                      id = "FilterThemeTopicMZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      `data-toggle-tt` = "tooltip",
                      `data-placement` = "top",
                      title = "Some similar challenges are grouped as one 'theme'",
                      "Topic M-Z"),
          div(class = "dropdown-menu mapfilter-theme",
              `aria-labelledby` = "FilterThemeTopicMZ",
              map_themes %>% 
                filter(Grouping == "topic",
                       Theme > "M") %>% 
                pmap(function (Theme, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Theme,
                              Theme,
                              span(class = "badge badge-primary badge-pill",
                                   num_maps))
                })
          )
      )
  )



# > Filter Packages -----------------------------------------------------------

maps_filter_package_AB <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageAB",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "R package: A-B"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageAB",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %in% c("a","b")) %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_CD <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageCD",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "C-D"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageCD",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %in% c("c","d")) %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_EG <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageEG",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "E-Gf"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageEG",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() %>% is_greater_than("d"),
                       package %>% str_sub(1, 2) %>% str_to_lower() %>% is_less_than("gg")) %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_GG <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageGG",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Gg"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageGG",
              map_packages %>% 
                filter(package %>% str_sub(1, 2) %>% str_to_lower() == "gg") %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_GK <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageGK",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Gh-K"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageGK",
              map_packages %>% 
                filter(package %>% str_sub(1, 2) %>% str_to_lower() %>% is_greater_than("gg"),
                       package %>% str_sub(1, 1) %>% str_to_lower() %>% is_less_than("l")) %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_LM <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageLM",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "L-M"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageLM",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() %>% is_greater_than("k"),
                       package %>% str_sub(1, 1) %>% str_to_lower() %>% is_less_than("n")) %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_NQ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageNQ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "N-Q"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageNQ",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() %>% is_greater_than("m"),
                       package %>% str_sub(1, 1) %>% str_to_lower() %>% is_less_than("r")) %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_R <- 
    div(class = "col-12@sm",
        div(class = "dropdown",
            tags$button(class = "btn btn-info dropdown-toggle",
                        type = "button",
                        id = "FilterPackageR",
                        `data-toggle` = "dropdown",
                        `aria-haspopup` = "true",
                        `aria-expanded` = "false",
                        "R"),
            div(class = "dropdown-menu mapfilter-package",
                `aria-labelledby` = "FilterPackageR",
                map_packages %>% 
                  filter(package %>% str_sub(1, 1) %>% str_to_lower() == "r") %>% 
                  pmap(function (package, num_maps, ...) {
                    tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                                type = "button",
                                `data-value` = package,
                                package,
                                span(class = "badge badge-info badge-pill",
                                     num_maps))
                  })
            )
        )
    )
maps_filter_package_S <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageS",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "S"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageS",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() == "s") %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_T <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageT",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "T"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageT",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() == "t") %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )
maps_filter_package_UZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageUZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "U-Z"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageUZ",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() > "t") %>% 
                pmap(function (package, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = package,
                              package,
                              span(class = "badge badge-info badge-pill",
                                   num_maps))
                })
          )
      )
  )




# > Filter Year -----------------------------------------------------------

maps_filter_year <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-secondary dropdown-toggle",
                      type = "button",
                      id = "FilterYear",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Year"),
          div(class = "dropdown-menu mapfilter-year",
              `aria-labelledby` = "FilterYear",
              map_years %>% 
                pmap(function(Year, num_maps, ...) {
                  tags$button(class = "dropdown-item d-flex justify-content-between align-items-center",
                              type = "button",
                              `data-value` = Year,
                              Year,
                              span(class = "badge badge-dark badge-pill",
                                   num_maps))
                })
          )
      )
  )




# > Sort ------------------------------------------------------------------

maps_sorting <-
  div(class = "col-12@sm",
      span(class = "button_legend",
           "Sort: "),
      div(class = "btn-group btn-group-toggle sort-options",
          `data-toggle` = "buttons",
          tags$label(class = "btn btn-success active",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "dom",
                                checked = NA),
                     "Default"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "theme"),
                     "Theme"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "posted-old-new"),
                     "Date (old-new)"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "posted-new-old"),
                     "Date (new-old)"),
          tags$label(class = "btn btn-success",
                     tags$input(type = "radio",
                                name = "sort-value",
                                value = "handle"),
                     "Handle")
          )
     )



# > Search ---------------------------------------------------------

maps_search_handle <- 
  div(class = "col-12@sm",
      tags$input(type = "text",
                 class = "form-control mapfilter-handle-search",
                 placeholder = "Search User Handle",
                 id = "searchHandle")
  )

maps_search_tags <- 
  div(class = "col-12@sm",
      tags$input(type = "text",
                 class = "form-control mapfilter-tags-search",
                 placeholder = "Search Tags",
                 id = "searchTags")
  )


# > Reset Filters ---------------------------------------------------------

maps_reset_filters <- 
  div(class = "col-12@sm",
      tags$button(type = "button",
                  class = "btn btn-danger mapfilter-reset",
                  id = "mapfilter-reset-button",
                  "Reset All Filters"
                  )
      )



# > Page & Save -----------------------------------------------------------
  
maps_page <- 
  paste(
    maps_header,
    maps_header_nav,
    div(class = 'container',
        h1("Map Gallery"),
        # filter/sort
        div(class = "row",
            maps_filter_year,
            maps_filter_theme_type, maps_filter_theme_topic_AL, maps_filter_theme_topic_MZ, maps_filter_theme_constraint
        ),
        div(class = "row",
            maps_filter_package_AB, maps_filter_package_CD, maps_filter_package_EG, maps_filter_package_GG,
            maps_filter_package_GK, maps_filter_package_LM, maps_filter_package_NQ, maps_filter_package_R,
            maps_filter_package_S, maps_filter_package_T, maps_filter_package_UZ
        ),
        div(class = "row",
            maps_search_tags,
            maps_search_handle,
            maps_reset_filters),
        div(class = "row",
            maps_sorting
            ),
        ),
    # cards
    div(class = "container-fluid",
        div(id = "grid", class = "row my-shuffle-container",
            map_cards,
            div(class = "col-1 my-sizer-element"))),
    # modals
    maps_modals,
    # JS libraries & code
    maps_scripts,
    maps_footer,
    collapse = "\n"
  )


write_file(maps_page, "maps.html")

