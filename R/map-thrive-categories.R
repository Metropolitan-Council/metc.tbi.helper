# Explore the TBI
suppressMessages(library(dplyr, quietly = T))
suppressMessages(library(sf, quietly = T))
library(ggmap)


# load fonts -------------------------------------------
library(sysfonts); library(showtext)
font_add("HelveticaNeueLTStd", "HelveticaNeueLTStd-Lt.otf")
font_add(
  "HELVETICANEUELTSTD-MDCN_1",
  "HELVETICANEUELTSTD-MDCN_1.otf"
)
font_add("Arial Narrow", "ARIALN.ttf")
font_add("Arial Narrow Italic",
         regular = "ARIALN.ttf",
         italic = "ARIALNI.ttf"
)

showtext_auto()

db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")
metro_area_shp <- DBI::dbGetQuery(
  db,
  "SELECT
                        *,
                        SHAPE.STAsText() as geometry
                        FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = 4326)


thrive2040 <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.THRIVEMSP2040COMMUNITYDESIGNATION;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  select(COMDESNAME) %>%
  rename(thriveCategory = COMDESNAME) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

counties <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.COUNTIES;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  select(CO_NAME_FU ) %>%
  rename(county = CO_NAME_FU ) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()


thrive2040 <-
  thrive2040 %>%
  mutate(
    thriveCatBroad = recode_factor(
      thriveCategory,
      "Agricultural" = "Rural",
      "Diversified Rural" = "Rural",
      "Rural Center" = "Rural",
      "Rural Residential" = "Rural"
    )
  ) %>%
  mutate(thriveCatBroad = factor(
    thriveCatBroad,
    levels = c(
      "Urban Center",
      "Urban",
      "Suburban",
      "Suburban Edge",
      "Emerging Suburban Edge",
      "Rural"
    )
  )) %>%
  mutate(
    thriveCatBroader = recode_factor(
      thriveCatBroad,
      "Urban Center" = "Urban",
      "Suburban Edge" = "Suburban",
      "Emerging Suburban Edge" = "Suburban"
    )
  )


# ggmap(get_stamenmap(st_bbox(thrive2040)))
bbox <- st_bbox(thrive2040)
names(bbox) <- c("left", "bottom", "right", "top")
st_centroid(thrive2040)

map.background <- 
  get_stamenmap(
  bbox,
  map = "terrain-background",
  color = "bw",
  souce = "stamen",
  force = T
)

map.lines <-
  get_stamenmap(
    bbox,
    map = "terrain-lines",
    color = "bw",
    souce = "stamen",
    force = T
  )

map.labels <-
  get_stamenmap(
    bbox,
    map = "terrain-labels",
    color = "bw",
    souce = "stamen",
    force = T,
    zoom = 9
  )


cty_labels <- st_centroid(counties)
cty_labels[cty_labels$county == "Washington County", ] <-
  st_as_sf(
    x =  data.frame(
      county = "Washington County",
      lat = -92.9,
      lon = 45.15
    ),
    coords = c("lat", "lon")
  )

library(councilR)
fac <- 1.5

map <- 
ggmap(map.lines) +
  geom_sf(
    data = thrive2040,
    inherit.aes = FALSE,
    col = NA,
    aes(fill = thriveCatBroader),
    alpha = 0.9
  ) +
  geom_sf(data = counties, inherit.aes = F, fill = NA, lwd = 0.5, color = "gray96") + 
  geom_sf_text(data = cty_labels, inherit.aes = F, color = "gray96", 
               aes(label = gsub("County", "", county)), size = 8, family = "Arial Narrow") + 
  scale_fill_manual(values = c("#009AC7", "#B96EC2", "#003E78"),
                    guide = "none") +
  # inset_ggmap(map.lines) +
  # inset_ggmap(map.labels) + 
  coord_sf(datum = NA) +
  labs(
    x = NULL,
    y = NULL,
    title =
    "<span style='color:#009AC7;'>**Urban**</span>,
    <span style='color:#B96EC2;'>**Suburban**</span> and
    <span style='color:#003E78;'>**Rural**</span>
    </span>",
    subtitle = stringr::str_wrap(width = 60,
                        "communities in the Twin Cities metro region. Based on Metropolitan Council Thrive 2040 Community Designations")
  ) + 
  councilR::theme_council(
    use_showtext = T,
    use_manual_font_sizes = T,
    font_sizes = list(
      title = 22 * fac * 1.1,
      subtitle = 16 * fac,
      axis_title = 14 * fac,
      axis_text = 11 * fac,
      legend_title = 14 * fac,
      legend_text = 10 * fac,
      caption = 8 * fac,
      strip = 14 * fac
    )
  ) +
  theme(
    legend.position = "none",
    panel.spacing = unit(2, "lines")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(
    axis.text = element_text(color = "black"),
    strip.text.x = element_text(color = "black", hjust = 0)
  ) +
  theme(
    axis.text = element_text(lineheight = 1.1),
    axis.title = element_text(lineheight = 1.1)
  ) +
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 15, l = 0)),
    plot.title = ggtext::element_markdown(margin = margin(t = 0, r = 0, b = 15, l = 0))
  ) +
  theme(axis.title.y = element_text(vjust = 0.6)) +
  theme(axis.text.x = element_text(vjust = 0.95)) + 
  theme(plot.title.position = "plot")

# save map ----------------------
jpeg("fig/map-thrive-categories.jpeg", width = 800, height = 800, units = "px")
map
dev.off()

png("fig/map-thrive-categories.png", width = 800, height = 800, units = "px")
map
dev.off()
