# TidyTuesday vol 1 
# Ben Gramza
# 10/18/2023
# 12:14 Start time

# Require packages
require(tidytuesdayR)
require(dplyr)
require(ggplot2)
require(data.table)
remotes::install_github("asteves/tayloRswift")
library(tayloRswift)

# Loading System Fonts
library(systemfonts)
library(stringr)
fonts <- system_fonts() %>% 
  filter(family == "Open Sans") %>% 
  transmute(
    family, style,
    file = str_extract(path, "[\\w-]+\\.ttf$")
  )

# Register ExtraBold Font
fa_solid_path <- system_fonts() |> 
  filter(family == "Open Sans", style == "ExtraBold") |>
  pull(path)

systemfonts::register_font(
  name = "Open Sans ExtraBold",
  plain = fa_solid_path
)
# Contrast Text Color Function
# https://stackoverflow.com/questions/49437263/contrast-between-label-and-background-determine-if-color-is-light-or-dark
isDark <- function(colr) {
  ifelse(
    sum(col2rgb(colr) * c(299, 587,114))/1000 < 123,
  "white",
  "black"
  ) 
}

# 
pal <- swift_pal("midnights")(13) 
text_col <- lapply(pal, isDark) |>
  unlist()

# Read songs
TT <- tidytuesdayR::tt_load('2023-10-17')

# Using album songs dataset
taylor_album_songs <- TT$taylor_album_songs |>
  # Remove bonus tracks
  filter(
   !bonus_track
  ) |>
  # Select relevant columns
  select(
    album_name, album_release, track_number, track_name, duration_ms
  ) |>
  # Rank Albums in order of release date
  # Add label in MM:SS format
  mutate(
    album_number = data.table::frank(x=album_release,ties.method = "dense"),
  ) 


# Plot Album (just one album)
midnights <- taylor_album_songs |>
  filter(album_name == "Midnights")

# Stacked Bar Chart showing runtime
# Used the following as a reference
# https://stackoverflow.com/questions/50172591/use-scale-y-time-to-convert-ms-to-minutes-and-seconds-in-boxplot-ggplot
ggplot(midnights, aes(x=0, y=duration_ms/1e3)) +
  geom_col(aes(group = album_name, fill=reorder(track_name, track_number)), color="black", width=.5) + 
  # X-axis modifications
  xlab("") + 
  scale_x_continuous(expand=c(0,0)) + 
  # Y-axis modifications
  scale_y_time(breaks = c(seq(0,20)*300), labels = function(l) strftime(l, '%M'), expand = c(0, 100)) + 
  ylab("Album Runtime") + 
  # 
  ggtitle(
    label="Taylor Swift - Midnights", 
    subtitle = "Album Runtime by Track (minutes)"
    ) + 
  geom_text(
    aes(label=track_name), 
    color = text_col,
    size = 5, 
    position = position_stack(vjust = 0.5),
    family = "Open Sans", fontface="bold") +
  scale_color_manual(guide = FALSE, values = c("black", "white")) + 
  theme_minimal() + 
  scale_fill_taylor(palette = "midnights") +
  theme(
    # Remove legend, x-axis labels, and gridlines 
    legend.position="none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=10, color = swift_palettes$midnights[4]),
    axis.title.x = element_text(size=10, color = swift_palettes$midnights[4]),
    # Add border and background color
    panel.grid = element_blank(),
    plot.background = element_rect(swift_palettes$midnights[3]),
    # Center title
    plot.title = element_text(hjust=0.5, vjust=0, size=18),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=15),
    text = element_text(family = "Open Sans ExtraBold", color = swift_palettes$midnights[4])
  ) + 
  # Add appendix
  labs(caption="Plot by Ben Gramza")