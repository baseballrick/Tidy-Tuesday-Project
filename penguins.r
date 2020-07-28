#Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(png)

#Import Data
penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')
penguins_raw.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')

#Remove NA's
penguins<- penguins.csv[!is.na(penguins.csv$bill_depth_mm),]

#Custom Colors to match Alison Horst's artwork
pengcolors<- c("darkorange1", "magenta3", "aquamarine4")

#Import Alison Horst's penguin image
img<- readPNG('/Users/baseballrick/Desktop/penguin artwork.png')

#Plot bill length vs bill depth
bill <- penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, group=species, color = species))+
  geom_point()+
  scale_color_manual(values = pengcolors)+ #Use the custom colors created earlier
  labs(x="Bill Length (mm)",
       y="Bill Depth (mm)",
       title = "Bill Length vs Bill Depth")+ #Add labels to plot
  theme(legend.position = "none") #Remove legend
bill_lm<- bill+geom_smooth(method = lm) #Add linear model

#Plot body mass vs flipper length
body_mass <- penguins %>% 
  ggplot(aes(body_mass_g, flipper_length_mm, group = species, color=species))+
  geom_point()+
  scale_color_manual(values = pengcolors)+ #Use the custom colors
  labs(x="Body Mass (g)",
       y="Flipper Length (mm)",
       title = "Body Mass vs Flipper Length",
       color = "Penguin Species")+ #Add labels to plot
  theme(legend.title = element_text(face = "bold",size = 9),
        legend.text = element_text(face = "bold", size = 9),
        legend.position = c(0.85, 0.25),
        legend.background = element_rect(colour = NA,
                                   fill = NA)) # Format legend and place it on the plot, with no background
bodymass_lm <-body_mass+geom_smooth(method = lm) #Add linear model

#Plot flipper length vs bill length
flipbill <- penguins %>% 
  ggplot(aes(flipper_length_mm, bill_length_mm, group=species, color=species))+
  geom_point()+
  scale_color_manual(values = pengcolors)+ #Use the custom colors
  labs(x="Flipper Length (mm)",
       y="Bill Length (mm)",
       title = "Flipper Length vs Bill Length")+ #Add labels to plot
  theme(legend.position = "none") #Remove legend
flipbill_lm <- flipbill+geom_smooth(method=lm) #Add linear model

#Plot the artwork image
imgplot <- ggplot()+background_image(img)

#Create summary text
text <- paste("Palmer penguin data shows that Gentoo penguins have longer flippers,",
              "higher body mass, and thinner bills than Adelie and Chinstrap penguins.",
              "Adelie penguins have the shortest beaks of the three species.",
              "Artwork by Allison Horst", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color="black") #Plot and format the text
#Combine multiple plots into one plot
side1 <- ggarrange(bill_lm, flipbill_lm,
          ncol = 1, nrow = 2) #Create left side w/two plots
side2 <- ggarrange(bodymass_lm, imgplot, text.p,
                   ncol = 1, nrow = 3,
                   heights = c(1,0.7,0.3)) #Create right side w/third plot, image, and summary text

#Combine two sides into one page
final <- ggarrange(side1, side2, ncol = 2, nrow = 1)

#Source text
text2<-paste("Data source: HorstAM, Hill AP, Gorman KB (2020),",
              "palmerpenguins: Palmer Archipelago (Antarctica)",
              "penguin data https://allisonhorst.github.io/palmerpenguins\n",
             "Submitted for #tidytuesday by @baseballrick", sep = " ")

#Annotate the page
annotate_figure(final,
                top = text_grob("Comparing Three Species of Penguins", color = "black",
                                face = "bold", size = "16"),
                bottom = text_grob(text2,color = "black", size = 9, face = "bold"))
ggsave('penguins.png', width = 12, height = 9)
