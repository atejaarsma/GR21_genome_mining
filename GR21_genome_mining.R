setwd("~/R/230413_P2")

# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(forcats)
library(ggsci)
# load datasets

phyla <- read.csv("phyla.csv")

METorigin <- read.csv("METorigin.csv")

# BoxPlot of BGC per genome for each phylum
ggsave("phyla.png", bg = 'white', width = 10, height = 7, print(
phyla %>%
  mutate(name = fct_reorder(name, value, .fun='median')) %>%
    ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot(outlier.shape = NA, notch= T) +
  scale_fill_frontiers(alpha=0.6) + 
  geom_jitter(color="black", size=1, alpha=0.2, height = 0.1) +
  theme_ipsum() + coord_flip() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20),
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(size=15)) +
  ggtitle("BGC per genome") +
  xlab("") + ylab("") + scale_y_continuous(breaks = seq(0, 22, by = 2)))) 




### Boxplot of BGC per genome for each method

ggsave("METorigin.png", bg = 'white', width = 10, height = 7, print(
  METorigin %>%
    mutate(name = fct_reorder(name, value, .fun='median')) %>%
    ggplot( aes(x=name, y=value, fill=name), alpa=0.2) +
    geom_boxplot(outlier.shape = NA, notch = T) +
    scale_fill_frontiers(alpha=0.2) +
    geom_jitter(aes(color= Environment), size=1.5, alpha=1, height = 0.1, width = 0.3, show.legend = T) +
    theme_ipsum() + coord_flip() +
    theme(
      legend.position = "right",
      plot.title = element_text(size=20), 
      axis.text.y = element_text(size=15),
      axis.text.x = element_text(size=15)) +
    ggtitle("BGC per genome") +
    xlab("") + ylab("") + scale_y_continuous(breaks = seq(0, 22, by = 2)))) 

####################

# bar plots
## load data

ISOclass <- read.csv("ISOclass.csv")
MAGclass <- read.csv("MAGclass.csv")
METclass <- read.csv("METclass.csv")

## Bar plot of classes ISO only
ggsave("ISOclass.png", bg = 'white', width = 10, height = 7, print(
ISOclass %>%
mutate(Origin = fct_reorder(Origin, Order)) %>%
  ggplot(aes(fill=Class, y=Value, x=Origin)) + 
  geom_bar(position="fill", stat="identity", lwd = 0, color = "white") + scale_fill_frontiers(alpha = 0.6) + 
  ggtitle("Distribution of BGC classes (isolates)") +   theme_ipsum() +   xlab("") + ylab("") + coord_flip() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size=20), 
        axis.text.y = element_text(size=15),
        legend.text = element_text(size = 15),
        legend.title = element_blank())))

## Bar plot of classes MAG only
ggsave("MAGclass.png", bg = 'white', width = 10, height = 7, print(
  MAGclass %>%
    mutate(Origin = fct_reorder(Origin, Order)) %>%
    ggplot(aes(fill=Class, y=Value, x=Origin)) + 
    geom_bar(position="fill", stat="identity", lwd = 0, color = "white") + scale_fill_frontiers(alpha = 0.6) + 
    ggtitle("Distribution of BGC classes (MAGs)") +   theme_ipsum() +   xlab("") + ylab("") + coord_flip() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(size=20), 
          axis.text.y = element_text(size=15),
          legend.text = element_text(size = 15),
          legend.title = element_blank())))

## Bar plot of classes by method
ggsave("METclass.png", bg = 'white', width = 10, height = 7, print(
  METclass %>%
    mutate(Origin = fct_reorder(Origin, Order)) %>%
    ggplot(aes(fill=Class, y=Value, x=Origin)) + 
    geom_bar(position="fill", stat="identity", lwd = 0, color = "white") + scale_fill_frontiers(alpha = 0.6) + 
    ggtitle("Distribution of BGC classes") +   theme_ipsum() +   xlab("") + ylab("") + coord_flip() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.title = element_text(size=20), 
          axis.text.y = element_text(size=15),
          legend.text = element_text(size = 15),
          legend.title = element_blank())))





############# VENN


library(VennDiagram)


x <- read.csv("Vennorigin.csv")

lst1=list()

for(i in 1:ncol(x)) {	
  lst1[[i]] <- x[ , i]	
}

names(lst1)=colnames(x)
print(lst1)

venn.diagram(lst1, na = "remove", "vennorigin.png")


y <- read.csv("Vennenv.csv")

lst2=list()

for(i in 1:ncol(y)) {	
  lst2[[i]] <- y[ , i]	
}

names(lst2)=colnames(y)
print(lst2)

venn.diagram(lst2, na = "remove", "vennenv.png")




######### stats
library(tidyverse)
library(rstatix)
library(ggpubr)


METorigin %>% group_by(name) %>%
  get_summary_stats(value, type = "median_iqr")

# wilcoxon rank sum test

stat.test <- METorigin %>% 
  wilcox_test(value ~ name) %>%
  add_significance()
stat.test


#cite used packages

library(grateful)


cite_packages(out.dir = ".")  
































