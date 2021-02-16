# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(showtext)
font_add(family = "Charter", regular = "Charter.ttc")

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-02-16')
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

#Isolate  freed slaves data into its own data frames
freed.slaves <- tuesdata$freed_slaves

# Pivot df into longer df
freed.slaves<- freed.slaves %>% 
  pivot_longer(c(Slave, Free), names_to = "Class", values_to = "Pct") 

# Correct error in df
freed.slaves[3,3] = 89.0

# Re-create Area Chart of Freed Slaves
p<-freed.slaves %>% 
  ggplot(aes(x=Year, y=Pct, fill=Class))+
  geom_area()+
  geom_text(data=freed.slaves %>% filter(freed.slaves$Pct!=0&freed.slaves$Pct>20&
                                           freed.slaves$Pct!=100), 
            aes(label=paste0(100-Pct,"%")), size = 16, vjust=-0.6, 
            fontface = "bold", family = "Charter")+
  labs(title = "\nPROPORTION OF FREEMEN AND SLAVE AMONG AMERICAN NEGROES .
           \n PROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .",
       subtitle = "\n  \n DONE BY ATLANTA UNIVERSITY .\n\n\n\n",
       caption = "@baseballrick for #TidyTuesday, Recreation of W.E.B. DuBois data viz #DuBoisChallenge")+
  scale_fill_manual(values = c("darkgreen", "black"))+
  scale_x_continuous(breaks = freed.slaves$Year, position = "top")+
  annotate("text", x=1830, y=60, label="SLAVES", color="white",size=28, family = "Charter")+
  annotate("text", x=1830, y=54, label="ESCLAVES", color="white", size=28, family="Charter")+
  annotate("text", x=1830, y=95, label="FREE - LIBRE", color="black", size=18,fontface= "bold",family="Charter")+
  annotate("text", x=1870, y=89, label= "100%", color="black", size = 16, fontface="bold", family = "Charter")+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "bisque"),
        panel.background = element_rect(fill = "bisque"),
        text = element_text(size = 52, face = "bold", family = "Charter"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 44),
        plot.subtitle = element_text(hjust = 0.5, size = 36),
        plot.caption = element_text(size = 36))

# Save ggplot to png
ggsave("dubois.png", width = 22, height = 28, units = "in")
