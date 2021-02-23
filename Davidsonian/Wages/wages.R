
library(tidyverse)
library(readr)
library(ggrepel)
wages <- read_csv("wages.csv")



one <- ggplot(wages,aes(x=reorder(Institution,-Rate),y=Rate))+
  geom_bar(aes(fill=ifelse(Institution=="Davidson", "Red", "Black")),
           stat="identity",show.legend = F) + 
  scale_fill_manual(values=c("#377EB8","#E41A1C")) +
  labs(x="", y="Student Minimum Wage", 
       title="Davidson's Student Minimum Wage Compared With Peers",
       caption="Visualization and data collection by Nathan Jordan '22. Data are current as of February 22, 2021.") +
  geom_text(aes(label=Label), hjust=1.1, color="white") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

two <- ggplot(wages,aes(x=reorder(Institution,-Endowment),y=Endowment))+
  geom_bar(aes(fill=ifelse(Institution=="Davidson", "Red", "Black")),
           stat="identity",show.legend = F) + 
  scale_fill_manual(values=c("#377EB8","#E41A1C")) +
  labs(x="", y="Endowment Value (2018)", 
       title="Davidson's 2018 Endowment Value Compared With Peers",
       caption="Visualization by Nathan Jordan '22. Data from The Chronicle of Higher Education.") +
  geom_text(aes(label=Endowmentlabel), hjust=1.1, color="white") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank())

three <- ggplot(wages, aes(x=Endowment,y=Rate, label=Institution))+
  geom_point(color="#377EB8", size=3) +
  geom_text_repel() +
  labs(title="Exploring the Relationship Between Endowment Value and Minimum Wage",
       x="Endowment Value (2018)", y="Student Minimum Wage") +
  theme_minimal()
three
