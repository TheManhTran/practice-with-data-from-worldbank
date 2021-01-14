# Load needed package
library(tidyverse)  # For ggplot, dplyr, and more
library(WDI)        # Where getting data from the World Bank
library(scales)     # For helpful scale functions 
library(ggrepel)    # For non-overlapping labels
library(ggtext)     # For good looking text handling

# Create indicators want to get from world bank
indicators <- c("SP.DYN.LE00.IN",  # Life expectancy
                "EN.ATM.CO2E.PC",  # CO2 emissions
                "NY.GDP.PCAP.KD")  # GDP per capital

# Download data,
wdi_original <- WDI(country = "all", 
                    indicators, 
                    # extra = true to get some information like region, long,lat
                    extra = TRUE, 
                    # At this time 2018 is up to date
                    start = 1995, end = 2018)

# Check data
head(wdi_original)
tail(wdi_original)

# Save it 
write_csv(wdi_original, "data/wdi_original.csv")

# Load data from data folder
wdi_original <- read_csv("data/wdi_original.csv")
wdi_adj <- wdi_original %>% 
  filter(region != "NA") %>% 
  select(iso3c, country, year, 
         life_expectancy = SP.DYN.LE00.IN, 
         co2_emissions = EN.ATM.CO2E.PC, 
         gdp_per_cap = NY.GDP.PCAP.KD, 
         region, income, longitude, latitude)

# Plot life expectancy for asian zone
life_expectancy_asizone <- wdi_adj %>% 
  filter(country %in% c("Myanmar", "Lao PDR","Vietnam", "Thailand",
                        "Cambodia", "Brunei Darussalam","Malaysia", "Singapore",
                        "Indonesia", "Timor-Leste", "Philippines"))

ggplot(data = life_expectancy_asizone, 
       mapping = aes(x = year, y = life_expectancy)) +
  geom_line(size = 1) +
  facet_wrap(vars(country), scales = "free_y", nrow = 3) +
  labs(x = "year", y = "life_expectancy", title = "Life expectancy from 1995-2018",
       caption = "Source: The World Bank (SP.DYN.LE00.IN)") +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# Slope graphs
gdp_asizone_sgraph <- life_expectancy_asizone %>% 
  filter(year %in% c(1995, 2000, 2010, 2015, 2018)) %>% 
  # Looking for each country individually
  group_by(country) %>%
  # Remove the country if its gdp_per_cap values are missing
  filter(!any(is.na(gdp_per_cap))) %>%
  ungroup() %>%
  # Make year a factor
  mutate(year = factor(year)) %>% 
  # Make some nice label columns
  # If the year is 1995, format it like "Country name: $GDP". If the year is
  # 2018, format it like "$GDP"
  mutate(label_first = ifelse(year == 1995, paste0(country, ": ", dollar(round(gdp_per_cap))), NA),
         label_second = ifelse(year == 2000, dollar(round(gdp_per_cap, 0)), NA),
         label_third = ifelse(year == 2005, dollar(round(gdp_per_cap, 0)), NA),
         label_fourth = ifelse(year == 2010, dollar(round(gdp_per_cap, 0)), NA),
         label_fourth = ifelse(year == 2015, dollar(round(gdp_per_cap, 0)), NA),
         label_last = ifelse(year == 2018, dollar(round(gdp_per_cap, 0)), NA))

ggplot(gdp_asizone_sgraph, aes(x = year, y = gdp_per_cap, group = country, color = country)) +
  geom_line(size = 1) +
  geom_text_repel(aes(label = label_first), direction = "y", nudge_x = -1, seed = 15) +
  geom_text_repel(aes(label = label_last), direction = "y", nudge_x = 1, seed = 15) +
  guides(color = FALSE) +
  labs(title = "GDP per capital",
       caption = "Source: The World Bank") +
  scale_color_viridis_d(option = "plasma", end = 0.9)

# Bump charts for Co2 emissions
asizone_co2 <- life_expectancy_asizone %>% 
  # From year 2015 to 2018 don't have record for Co2 emissions
  filter(year >= 2000, year < 2015) %>% 
  group_by(year) %>% 
  mutate(rank = rank(co2_emissions))

ggplot(asizone_co2, aes(x = year, y = rank, color = country)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  geom_text(data = filter(asizone_co2, year == 2000),
            aes(label = iso3c, x = 1999.5),
            fontface = "bold") +
  geom_text(data = filter(asizone_co2, year == 2014),
            aes(label = iso3c, x = 2014.5),
            fontface = "bold") +
  guides(color = FALSE) +
  scale_y_reverse(breaks = 1:11) +
  scale_x_continuous(breaks = 2000:2014) +
  scale_color_viridis_d(option = "inferno", begin = 0.2, end = 0.9) +
  labs(x = NULL, 
       y = "Rank",
       title = "CO2 emissions ranking") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())


# Make a little bit of joying following Andrew Heiss
wdi_co2_raw <- read_csv("data/wdi_co2.csv")
wdi_clean <- wdi_co2_raw %>% 
  filter(region != "Aggregates") %>% 
  select(iso2c, iso3c, country, year, 
         population = SP.POP.TOTL,
         co2_emissions = EN.ATM.CO2E.PC, 
         gdp_per_cap = NY.GDP.PCAP.KD, 
         region, income)
co2_rankings <- wdi_clean %>% 
  # Get rid of smaller countries
  filter(population > 200000) %>% 
  # Only look at two years
  filter(year %in% c(1995, 2014)) %>% 
  # Get rid of all the rows that have missing values in co2_emissions
  drop_na(co2_emissions) %>% 
  # Look at each year individually and rank countries based on their emissions that year
  group_by(year) %>% 
  mutate(ranking = rank(co2_emissions)) %>% 
  ungroup() %>% 
  # Only select a handful of columns, mostly just the newly created "ranking"
  # column and some country identifiers
  select(iso3c, country, year, region, income, ranking) %>% 
  # Right now the data is tidy and long, but we want to widen it and create
  # separate columns for emissions in 1995 and in 2014. pivot_wider() will make
  # new columns based on the existing "year" column (that's what `names_from`
  # does), and it will add "rank_" as the prefix, so that the new columns will
  # be "rank_1995" and "rank_2014". The values that go in those new columns will
  # come from the existing "ranking" column
  pivot_wider(names_from = year, names_prefix = "rank_", values_from = ranking) %>% 
  # Find the difference in ranking between 2014 and 1995
  mutate(rank_diff = rank_2014 - rank_1995) %>% 
  # Remove all rows where there's a missing value in the rank_diff column
  drop_na(rank_diff) %>% 
  # Make an indicator variable that is true of the absolute value of the
  # difference in rankings is greater than 25. 25 is arbitrary here-that just
  # felt like a big change in rankings
  mutate(big_change = ifelse(abs(rank_diff) >= 25, TRUE, FALSE)) %>% 
  # Make another indicator variable that indicates if the rank improved by a
  # lot, worsened by a lot, or didn't change much. We use the case_when()
  # function, which is like a fancy version of ifelse() that takes multiple
  # conditions. This is how it generally works:
  #
  # case_when(
  #  some_test ~ value_if_true,
  #  some_other_test ~ value_if_true,
  #  TRUE ~ value_otherwise
  #)
  mutate(better_big_change = case_when(
    rank_diff <= -25 ~ "Rank improved",
    rank_diff >= 25 ~ "Rank worsened",
    TRUE ~ "Rank changed a little"
  ))

ggplot(co2_rankings,
       aes(x = rank_1995, y = rank_2014)) +
  # Add a reference line that goes from the bottom corner to the top corner
  annotate(geom = "segment", x = 0, xend = 175, y = 0, yend = 175) +
  # Add points and color them by the type of change in rankings
  geom_point(aes(color = better_big_change)) +
  # Add repelled labels. Only use data where big_change is TRUE. Fill them by
  # the type of change (so they match the color in geom_point() above) and use
  # white text
  geom_label_repel(data = filter(co2_rankings, big_change == TRUE),
                   aes(label = country, fill = better_big_change),
                   color = "white") +
  # Add notes about what the outliers mean in the bottom left and top right
  # corners. These are italicized and light grey. The text in the bottom corner
  # is justified to the right with hjust = 1, and the text in the top corner is
  # justified to the left with hjust = 0
  annotate(geom = "text", x = 170, y = 6, label = "Outliers improving", 
           fontface = "italic", hjust = 1, color = "grey50") +
  annotate(geom = "text", x = 2, y = 170, label = "Outliers worsening", 
           fontface = "italic", hjust = 0, color = "grey50") +
  # Add mostly transparent rectangles in the bottom right and top left corners
  annotate(geom = "rect", xmin = 0, xmax = 25, ymin = 0, ymax = 25, 
           fill = "#2ECC40", alpha = 0.25) +
  annotate(geom = "rect", xmin = 150, xmax = 175, ymin = 150, ymax = 175, 
           fill = "#FF851B", alpha = 0.25) +
  # Add text to define what the rectangles abovee actually mean. The \n in
  # "highest\nemitters" will put a line break in the label
  annotate(geom = "text", x = 40, y = 6, label = "Lowest emitters", 
           hjust = 0, color = "#2ECC40") +
  annotate(geom = "text", x = 162.5, y = 135, label = "Highest\nemitters", 
           hjust = 0.5, vjust = 1, lineheight = 1, color = "#FF851B") +
  # Add arrows between the text and the rectangles. These use the segment geom,
  # and the arrows are added with the arrow() function, which lets us define the
  # angle of the arrowhead and the length of the arrowhead pieces. Here we use
  # 0.5 lines, which is a unit of measurement that ggplot uses internally (think
  # of how many lines of text fit in the plot). We could also use unit(1, "cm")
  # or unit(0.25, "in") or anything else
  annotate(geom = "segment", x = 38, xend = 20, y = 6, yend = 6, color = "#2ECC40", 
           arrow = arrow(angle = 15, length = unit(0.5, "lines"))) +
  annotate(geom = "segment", x = 162.5, xend = 162.5, y = 140, yend = 155, color = "#FF851B", 
           arrow = arrow(angle = 15, length = unit(0.5, "lines"))) +
  # Use three different colors for the points
  scale_color_manual(values = c("grey50", "#0074D9", "#FF4136")) +
  # Use two different colors for the filled labels. There are no grey labels, so
  # we don't have to specify that color
  scale_fill_manual(values = c("#0074D9", "#FF4136")) +
  # Make the x and y axes expand all the way to the edges of the plot area and
  # add breaks every 25 units from 0 to 175
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 175, 25)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 175, 25)) +
  # Add labels! There are a couple fancy things here.
  # 1. In the title we wrap the 2 of CO2 in the HTML <sub></sub> tag so that the
  #    number gets subscripted. The only way this will actually get parsed as 
  #    HTML is if we tell the plot.title to use element_markdown() in the 
  #    theme() function, and element_markdown() comes from the ggtext package.
  # 2. In the subtitle we bold the two words **improved** and **worsened** using
  #    Markdown asterisks. We also wrap these words with HTML span tags with 
  #    inline CSS to specify the color of the text. Like the title, this will 
  #    only be processed and parsed as HTML and Markdown if we tell the p
  #    lot.subtitle to use element_markdown() in the theme() function.
  labs(x = "Rank in 1995", y = "Rank in 2014",
       title = "Changes in CO<sub>2</sub> emission rankings between 1995 and 2014",
       subtitle = "Countries that <span style='color: #0074D9'>**improved**</span> or <span style='color: #FF4136'>**worsened**</span> more than 25 positions in the rankings highlighted",
       caption = "Source: The World Bank.\nCountries with populations lesser than 200,000 excluded.") +
  # Turn off the legends for color and fill, since the subtitle includes that
  guides(color = FALSE, fill = FALSE) +
  # Use theme_bw() with Garamond
  theme_bw() +
  # Tell the title and subtitle to be treated as Markdown/HTML, make the title
  # 1.6x the size of the base font, and make the subtitle 1.3x the size of the
  # base font. Also add a little larger margin on the right of the plot so that
  # the 175 doesn't get cut off.
  theme(plot.title = element_markdown(face = "bold", size = rel(1.6)),
        plot.subtitle = element_markdown(size = rel(1.3)),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), units = "lines"))







































































