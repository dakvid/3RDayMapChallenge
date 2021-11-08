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
  themes %>% 
  left_join(maps %>% mutate(n = 1),
            by = c("Year", "Day")) %>% 
  group_by(Year) %>% 
  summarise(num_maps = sum(n, na.rm = T)) %>% 
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
            p("A gallery of maps made with", a(href="https://www.r-project.org/", "R"),
              "with reproducible (hopefully!) code."),
            p("In 2019 Topi Tjukanov", a(href="https://twitter.com/tjukanov/status/1187713840550744066", "announced"),
              "the #30DayMapChallenge on Twitter out of the blue, and there was so much enthusiasm that it has",
              "become an annual event - see",
              a(href="https://github.com/tjukanovt/30DayMapChallenge", "https://github.com/tjukanovt/30DayMapChallenge")),
            p("The event is a fun and friendly opportunity for cartographical practice, to try new things without the",
              "pressure of perfection, and to be inspired by what other people have created.",
              "And what's better than a cool map to inspire you to try something similar?",
              "A cool map with code so you can immediately reproduce and adapt it!"),
            h3("Where are the maps?"),
            p(span(class = "text-info",
                  "Click through to the", a(href = "maps.html", "map gallery"),
                  "where you can explore some maps."),
              "Filter by year, challenge theme, or R packages used, and search by",
              "manually added tags (eg 'interactive', 'africa', 'sport') or user handle."
              ),
            p("Then click on a map to see a larger version, and links to the code,",
              "tweet and web page (if one exists)."),
            h3("Updates"),
            p("I aim to include all relevant maps from 2019 onwards, but it's time consuming so I",
              "make no guarantees of speed. If I've got some maps from a person then you can",
              "assume I'll pick up later maps they release. If you think I've missed some maps
              and they're not on my",
              a(href="todo.txt", "todo list"),
              "then drop me a line on",
              a(href="https://twitter.com/dakvid", "Twitter"),
              "or email (change a dot to an @ in the domain)."),
            p("Note that I have only been following the hashtag on Twitter (and my",
              "previous galleries have been Twitter-only) but I'm happy to include",
              "maps posted anywhere - Instragram, LinkedIn, Github, ..."),
            h3("Colophon"),
            p("This site is made with", a(href = "https://getbootstrap.com/", "Bootstrap 4"),
              "and the", a(href = "https://bootswatch.com/lux/", "Lux"), "theme from Bootswatch.",
              "The gallery was made with", a(href="https://vestride.github.io/Shuffle/", "shuffle.js"),
              "and", a(href="https://github.com/aFarkas/lazysizes", "lazysizes."),
              "The data munging and HTML construction is performed by some rough",
              a(href = "https://github.com/dakvid/3RDayMapChallenge", "R code.")),
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
# 
# num_indexed_cartographers <- classifications %>% distinct(handle) %>% nrow()
# num_countries <- nrow(countries)
# num_cities <- nrow(cities)-1
# num_maps <- nrow(classifications)
# 
# num_unc_cartloc <- cartographers %>% filter(is.na(location)) %>% nrow()
# pc_unc_cartloc <- round(num_unc_cartloc / nrow(cartographers) * 100, 1)
# num_unc_area <- classifications %>% filter(area == "_") %>% nrow()
# pc_unc_area <- round(num_unc_area / num_maps * 100, 1)
# num_unc_city <- classifications %>% filter(city == "_") %>% nrow()
# pc_unc_city <- round(num_unc_city / num_maps * 100, 1)
# num_unc_topic <- classifications %>% filter(topics == "_") %>% nrow()
# pc_unc_topic <- round(num_unc_topic / num_maps * 100, 1)
# num_unc_type <- classifications %>% filter(types == "_") %>% nrow()
# pc_unc_type <- round(num_unc_type / num_maps * 100, 1)
# num_unc_tool <- classifications %>% filter(tools == "_") %>% nrow()
# pc_unc_tool <- round(num_unc_tool / num_maps * 100, 1)
# 
# num_per_person <- 
#   classifications %>% 
#   count(handle) %>% 
#   rename(num_maps = n) %>% 
#   count(num_maps) %>% 
#   rename(num_people = n)
# 
# full30 <- 
#   classifications %>% 
#   count(handle) %>% 
#   filter(n == 30) %>% 
#   inner_join(cartographers, by = "handle")
# 
# 
# # > Graphs ----------------------------------------------------------------
# 
# g_cartographers_data <- 
#   cartographers %>% 
#   count(location) %>% 
#   drop_na() %>% 
#   arrange(-n) %>% 
#   head(30) %>% 
#   mutate(location = location %>% fct_inorder() %>% fct_rev())
# g_cartographers <- 
#   ggplot(g_cartographers_data,
#          aes(x = location, y = n)) +
#   geom_col() +
#   geom_text(data = g_cartographers_data %>% 
#               filter(n > 9),
#             aes(x = location, y = n, label = n),
#             hjust = 1, nudge_y = -1,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_cartographers_data %>% 
#               filter(n <= 9),
#             aes(x = location, y = n, label = n),
#             hjust = 1, nudge_y = 2,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "Top 30 Cartographer Locations")
# ggsave(filename = "cartographer_location_count.png",
#        path = "images/",
#        plot = g_cartographers,
#        width = 7, height = 5.5, units = "cm", scale = 3)
# 
# g_challenges_data <- 
#   classifications %>% 
#   inner_join(challenges, by = "Day") %>% 
#   mutate(challenge = paste(Day, Challenge)) %>% 
#   count(Day, challenge) %>% 
#   mutate(challenge = challenge %>% fct_inorder() %>% fct_rev())
# g_challenges <- 
#   ggplot(g_challenges_data,
#          aes(x = challenge, y = n)) +
#   geom_col() + 
#   geom_text(data = g_challenges_data,
#             aes(x = challenge, y = n, label = n),
#             hjust = 1, nudge_y = -3,
#             color = "white",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "People who completed each daily map")
# ggsave(filename = "challenge_count.png",
#        path = "images/",
#        plot = g_challenges,
#        width = 7, height = 5.5, units = "cm", scale = 3)
# 
# g_num_per_person_data <- 
#   num_per_person %>% 
#   mutate(num_maps = num_maps %>% as.character() %>% fct_inorder())
# g_num_per_person <- 
#   ggplot(g_num_per_person_data,
#          aes(x = num_maps, y = num_people)) +
#   geom_col() +
#   geom_text(data = g_num_per_person_data %>% filter(num_people >= 5),
#             aes(x = num_maps, y = num_people, label = num_people),
#             hjust = 1, nudge_y = -1,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_num_per_person_data %>% filter(num_people < 5),
#             aes(x = num_maps, y = num_people, label = num_people),
#             hjust = 1, nudge_y = 3,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = "Number of Maps Completed", y = "Number of People",
#        title = "How many maps did people complete?")
# ggsave(filename = "submission_count.png",
#        path = "images/",
#        plot = g_num_per_person,
#        width = 7, height = 5.5, units = "cm", scale = 3)
# 
# g_countries_data <- 
#   bind_rows(
#     continents,
#     countries
#   ) %>% 
#   filter(area != "_") %>% 
#   arrange(desc(num_maps)) %>% 
#   head(30) %>% 
#   mutate(area = area %>% fct_inorder() %>% fct_rev())
# g_countries <- 
#   ggplot(g_countries_data,
#          aes(x = area, y = num_maps)) +
#   geom_col() +
#   geom_col(data = g_countries_data,
#            aes(x = area, y = num_people),
#            fill = "orange", width = 0.3) +
#   geom_text(data = g_countries_data %>% 
#               filter(num_maps > 15),
#             aes(x = area, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = -1,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_countries_data %>% 
#               filter(num_maps <= 15),
#             aes(x = area, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = 6,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "Top 30 Map Areas")
# ggsave(filename = "area_count.png",
#        path = "images/",
#        plot = g_countries,
#        width = 7, height = 5.5, units = "cm", scale = 3)
# 
# g_cities_data <- 
#   cities %>% 
#   filter(city != "_") %>% 
#   arrange(desc(num_maps)) %>% 
#   head(20) %>% 
#   mutate(city = city %>% fct_inorder() %>% fct_rev())
# g_cities <- 
#   ggplot(g_cities_data,
#          aes(x = city, y = num_maps)) +
#   geom_col() +
#   geom_col(data = g_cities_data,
#            aes(x = city, y = num_people),
#            fill = "orange", width = 0.3) +
#   geom_text(data = g_cities_data %>% 
#               filter(num_maps > 7),
#             aes(x = city, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = -0.5,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_cities_data %>% 
#               filter(num_maps <= 7),
#             aes(x = city, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = 0.5,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "Top 20 Cities Mapped")
# ggsave(filename = "city_count.png",
#        path = "images/",
#        plot = g_cities,
#        width = 7, height = 4, units = "cm", scale = 3)
# 
# g_tools_data <- 
#   tools %>% 
#   filter(tools != "_") %>% 
#   arrange(desc(num_maps)) %>% 
#   head(20) %>% 
#   mutate(tools = tools %>% fct_inorder() %>% fct_rev())
# g_tools <- 
#   ggplot(g_tools_data,
#          aes(x = tools, y = num_maps)) +
#   geom_col() +
#   geom_col(data = g_tools_data,
#            aes(x = tools, y = num_people),
#            fill = "orange", width = 0.3) +
#   geom_text(data = g_tools_data %>% 
#               filter(num_maps > 50),
#             aes(x = tools, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = -2,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_tools_data %>% 
#               filter(num_maps <= 50),
#             aes(x = tools, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = 18,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "Top 20 Tools Used to Make Maps")
# ggsave(filename = "tool_count.png",
#        path = "images/",
#        plot = g_tools,
#        width = 7, height = 4, units = "cm", scale = 3)
# 
# g_topics_data <- 
#   topics %>% 
#   filter(topics != "_") %>% 
#   arrange(desc(num_maps)) %>%
#   head(20) %>% 
#   mutate(topics = topics %>% fct_inorder() %>% fct_rev())
# g_topics <- 
#   ggplot(g_topics_data,
#          aes(x = topics, y = num_maps)) +
#   geom_col() +
#   geom_col(data = g_topics_data,
#            aes(x = topics, y = num_people),
#            fill = "orange", width = 0.3) +
#   geom_text(data = g_topics_data %>% 
#               filter(num_maps > 15),
#             aes(x = topics, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = -1,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_topics_data %>% 
#               filter(num_maps <= 15),
#             aes(x = topics, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = 2,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "Top 20 Topics of Maps Recorded")
# ggsave(filename = "topic_count.png",
#        path = "images/",
#        plot = g_topics,
#        width = 7, height = 4, units = "cm", scale = 3)
# 
# g_types_data <- 
#   types_of_maps %>% 
#   filter(types != "_") %>% 
#   arrange(desc(num_maps)) %>% 
#   head(20) %>% 
#   mutate(types = types %>% fct_inorder() %>% fct_rev())
# g_types <- 
#   ggplot(g_types_data,
#          aes(x = types, y = num_maps)) +
#   geom_col() +
#   geom_col(data = g_types_data,
#            aes(x = types, y = num_people),
#            fill = "orange", width = 0.3) +
#   geom_text(data = g_types_data %>% 
#               filter(num_maps > 10),
#             aes(x = types, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = -0.5,
#             color = "white",
#             family = CHART_FONT) +
#   geom_text(data = g_types_data %>% 
#               filter(num_maps <= 10),
#             aes(x = types, y = num_maps, label = num_maps),
#             hjust = 1, nudge_y = 0.5,
#             color = "black",
#             family = CHART_FONT) +
#   coord_flip() +
#   theme_minimal_vgrid(font_family = CHART_FONT) +
#   labs(x = NULL, y = NULL,
#        title = "Top 20 Types of Maps Recorded")
# ggsave(filename = "type_count.png",
#        path = "images/",
#        plot = g_types,
#        width = 7, height = 4, units = "cm", scale = 3)
# 
# 
# # > Page & Save -----------------------------------------------------------
# 
# stats_page <- 
#   paste(
#     stats_header,
#     stats_header_nav,
#     div(class = 'container',
#         h1("Statistics"),
#         div(class = "row",
#             div(class = "col-12",
#                 p(glue("There have been at least {num_tweeters} ",
#                        "people tweeting on the hashtag. ",
#                        "Currently I've indexed {num_maps} maps ",
#                        "by {num_indexed_cartographers} people.")),
#                 h3("Progress"),
#                 p("Every map that appears here has been assigned a day/challenge",
#                   "by me but the majority of the other classifications will take",
#                   "months without the wonders of",
#                   a(href="https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "crowdsourcing!")),
#                 p("The graphs below should give you an idea of progress..."),
# 
#                 h3("Daily Challenges"),
#                 img(src = "images/challenge_count.png", style="width: 800px;"),
#                 img(src = "images/submission_count.png", style="width: 800px;"),
# 
#                 h3("People"),
#                 p(glue("There were {nrow(full30)} people who managed the massive task of creating all 30 maps!"),
#                   "(If you're not on this list and should be then let me know.)"),
#                 tags$ul(
#                   full30 %>%
#                     pmap(function(handle, username, realname, location, ...) {
#                       tags$li(
#                         if (is.na(realname)) { username } else { realname },
#                         "-",
#                         a(href = glue("https://twitter.com/{handle}/"), glue("@{handle}")),
#                         if (!is.na(location)) { glue("- in {location}") }
#                       )
#                     })
#                 ),
#                 # p("So far I have recorded",
#                 #   num_per_person %>% tail(1) %>% pull(num_people),
#                 #   "people submitting",
#                 #   num_per_person %>% tail(1) %>% pull(num_maps),
#                 #   "maps, and",
#                 #   num_per_person %>% tail(2) %>% head(1) %>% pull(num_people),
#                 #   "people submitting",
#                 #   num_per_person %>% tail(2) %>% head(1) %>% pull(num_maps),
#                 #   "maps, leaving them on track for the magic 30 at the end of the month.",
#                 #   "(Something I can't comprehend myself! 😀)"),
#                 
#                 p("Currently",
#                   span(class = "text-danger", glue("only {round(100 - pc_unc_cartloc, 1)}%")),
#                   "of cartographers have a country assigned to them."),
#                 img(src = "images/cartographer_location_count.png", style = "width: 800px;"),
#                 tags$iframe(
#                   title="Participants in #30DayMapChallenge 2020 by country",
#                   `aria-label`="Map",
#                   id="datawrapper-chart-Ikcdr",
#                   src="https://datawrapper.dwcdn.net/Ikcdr/1/",
#                   scrolling="no",
#                   frameborder="0",
#                   style="width: 0; min-width: 100% !important; border: none;",
#                   height="602"),
#                 
#                 h3("Places"),
#                 p("Currently",
#                   span(class = "text-danger", glue("only {round(100 - pc_unc_area, 1)}%")),
#                   "have an area assigned (ie continent or country) and",
#                   span(class = "text-danger", glue("only {round(100 - pc_unc_city, 1)}%")),
#                   "have a city assigned (though many don't need one)."),
#                 p("The main bar is the number of maps with that label.",
#                   "The small orange bar is the number of cartographers who have produced the maps in that area."),
#                 img(src = "images/area_count.png", style="width: 800px;"),
#                 img(src = "images/city_count.png", style="width: 800px;"),
#                 tags$iframe(
#                     title="Maps from #30DayMapChallenge 2020 by country",
#                     `aria-label`="Map",
#                     id="datawrapper-chart-D1Xt6",
#                     src="https://datawrapper.dwcdn.net/D1Xt6/1/",
#                     scrolling="no",
#                     frameborder="0",
#                     style="width: 0; min-width: 100% !important; border: none;",
#                     height="566"),
#                 tags$script(
#                     type="text/javascript",
#                     '!function(){"use strict";window.addEventListener("message",(function(a){if(void 0!==a.data["datawrapper-height"])for(var e in a.data["datawrapper-height"]){var t=document.getElementById("datawrapper-chart-"+e)||document.querySelector("iframe[src*=\'"+e+"\']");t&&(t.style.height=a.data["datawrapper-height"][e]+"px")}}))}();'),
#                 h3("Tools"),
#                 p("Currently",
#                   span(class = "text-danger", glue("only {round(100 - pc_unc_tool, 1)}%")),
#                   "have any tools assigned. I have/will automate harvesting as much information",
#                   "included in tweets as I can, but many tweets don't mention tools.",
#                   a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing",
#                     "Please add your missing tool info!"), "🙏"),
#                 img(src = "images/tool_count.png", style="width: 800px;"),
#                 
#                 h3("Map Types"),
#                 p("Currently",
#                   span(class = "text-danger", glue("only {round(100 - pc_unc_type, 1)}%")),
#                   "have the type of map assigned.",
#                   "This is a manual and semi-subjective classification, so please feel free",
#                   a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "to contribute.")),
#                 img(src = "images/type_count.png", style="width: 800px;"),
#                 
#                 h3("Topics"),
#                 p("Currently",
#                   span(class = "text-danger", glue("only {round(100 - pc_unc_topic, 1)}%")),
#                   "have topics assigned.",
#                   "This is a manual and semi-subjective classification, so please feel free",
#                   a(href = "https://docs.google.com/spreadsheets/d/1j2iLnWtBATMxpvDZLXlqaOd0zmcclyg8VIgkPgVMklQ/edit?usp=sharing", "to contribute.")),
#                 img(src = "images/topic_count.png", style="width: 800px;"),
#             )
#         ),
#     ),
#     stats_footer,
#     collapse = "\n"
# )
# 
# write_file(stats_page, "stats.html")
        




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

maps_filter_package_AG <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageAG",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "R package: A-Gf"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageAG",
              map_packages %>% 
                filter(package %>% str_sub(1, 2) %>% str_to_lower() < "gg") %>% 
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
maps_filter_package_GL <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageGL",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Gh-L"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageGL",
              map_packages %>% 
                filter(package %>% str_sub(1, 2) %>% str_to_lower() %>% is_greater_than("gg"),
                       package %>% str_sub(1, 1) %>% str_to_lower() %>% is_less_than("m")) %>% 
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
maps_filter_package_MQ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageMQ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "M-Q"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageMQ",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() %>% is_greater_than("l"),
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
  maps_filter_package_TZ <- 
  div(class = "col-12@sm",
      div(class = "dropdown",
          tags$button(class = "btn btn-info dropdown-toggle",
                      type = "button",
                      id = "FilterPackageTZ",
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "T-Z"),
          div(class = "dropdown-menu mapfilter-package",
              `aria-labelledby` = "FilterPackageTZ",
              map_packages %>% 
                filter(package %>% str_sub(1, 1) %>% str_to_lower() > "s") %>% 
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
            maps_filter_package_AG, maps_filter_package_GG, maps_filter_package_GL, maps_filter_package_MQ, maps_filter_package_R, maps_filter_package_S, maps_filter_package_TZ
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

