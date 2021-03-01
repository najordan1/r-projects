library(tidyverse)
library(readr)
library(tidycensus)
library(tmap)
library(sf)

df <- read_csv("Vaccination_Race.csv", 
               col_types = cols(`American Indian or Alaskan Native` = col_number(), 
                                `Asian or Pacific Islander` = col_number(), 
                                `Black or African-American` = col_number(), 
                                White = col_number(), Other = col_number(), 
                                Suppressed = col_number(),
                                `Missing or Undisclosed` = col_number())) %>%
  mutate(nonWhite = 100 - White) %>%
  select(County, White, nonWhite)

#thanks Sam!!:
racevars <- c(White = "P005003", 
              Black = "P005004", 
              Hispanic = "P004003",
              `Native American` = "P005005",
              Asian = "P005006",
              PacIsl = "P005007")

nc_geo <- get_decennial(geography = "county", state = "NC", variables = racevars, 
                        summary_var = "P001001",
                        year = 2010, geometry = T) %>%
          select(GEOID) %>%
          distinct(.keep_all=TRUE)

nc <- get_decennial(geography = "county", state = "NC", variables = racevars, 
                    summary_var = "P001001",
                    year = 2010, geometry = F) %>%
  rename(Race = variable, 
         race_pop = value,
         total_pop = summary_value,
         County = NAME) %>%
  mutate(pct_race = race_pop / total_pop) %>%
  separate(County, into = c("County", "del"), sep = " County") %>%
  select(-del)

nc2 <- nc %>% 
  pivot_wider(id_cols = c(GEOID, County), 
              names_from = Race, values_from = race_pop) %>%
  mutate(`Asian or Pacific Islander` = Asian + PacIsl) %>%
  pivot_longer(cols = 3:9, names_to = "Race", values_to = "race_pop") %>%
  filter(Race != "Asian", Race != "PacIsl", Race != "Hispanic")

total_pop_df <- nc %>%
  group_by(County) %>%
  summarize(total_pop = unique(total_pop))

nc_final <- left_join(nc2, total_pop_df, by = "County") %>%
  filter(Race == "White") %>%
  mutate(pctWhite = 100*race_pop/total_pop) %>%
  select(GEOID, County, pctWhite, total_pop)

df <- left_join(nc_geo, nc_final, by = "GEOID")%>%
  left_join(df, by="County") %>%
  
  mutate(diff = White-pctWhite)

#Live Love Stack Overflow:
bbox_new <- st_bbox(df) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
#bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
#bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

tm_shape(df, bbox = bbox_new) +
  tm_polygons("diff",
              midpoint=0,
              breaks = c(-10, 0, 10, 20, 32),
              palette="BuPu",
              border.col = "white", 
              border.alpha = 0.5,
              textNA = "Ommitted",
              title="",
              legend.format=list(fun=function(x) paste0(formatC(x,digits=0,format="f"), "%"))) +
  tm_layout(legend.position = c("left", "center"),
            frame = F) 
