library(tidytuesdayR)
library(tidyverse)
library(png)
library(grid)
library(ggpubr)

#Import Data
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts

#import background image
img<- readPNG('/Users/baseballrick/Desktop/space.png')

# Create customized color palette
newcolors <- c("dodgerblue3", "red", "green")

# Group all non-US and non-USSR nationalities into 'Other' category and plot
astronauts %>% 
  mutate(nationalitygroup = fct_lump(f=nationality, n=2, other_level = "Other")) %>% 
  ggplot(aes(year_of_mission, nationalitygroup, group=nationalitygroup, fill=nationalitygroup))+
  background_image(img)+ #insert background image
  geom_dotplot(aes(y=..count..),
               stackdir = "up",
               stackratio = 1,
               binwidth = 0.5,
               binpositions = "all",
               stackgroups = TRUE,
               dotsize = 1.2)+ #customize the dots in dotplot
  labs(y="", x="Year", caption = "Author:  @baseballrick for #tidytuesday")+ #labels
  scale_fill_manual(values = newcolors)+ #use the custom colors for fill
  scale_x_continuous(breaks = seq(1960,2020,10))+ #add breaks to x-axis each decade
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), #remove text/ticks of y axis
        legend.position = c(0.11, 0.9), #reposition legend
        legend.background = element_blank(), #make legend transparent
        title = element_text(face = "bold", size = 18), #customize title font
        legend.title = element_blank(), #remove legend title
        legend.text = element_text(face = "bold", size = 16))+ #bold the legend title
  ggtitle("Nationality of Astronauts Over Time") #title plot

ggsave('astronauts.png', width = 12, height = 9)


