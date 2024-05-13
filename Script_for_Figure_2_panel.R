# Script for Figure 2 in Novaglio, C., Bryndum-Buchholz A., Tittensor, D.P., Eddy, T.D., Lotze, H.K., Harrison, C.S., ... & Blanchard, J.L. (2024). The Past and Future of the Fisheries and Marine Ecosystem Model Intercomparison Project. AGU Preprint. doi.org/10.22541/essoar.170542252.20348236/v1 
library(tidyverse)
library(ggplot2)
library(devtools)
library(gridExtra)
df1 <- read.csv("data/top_fishmip_citations_from_GoogleScholar.csv")
# exclude year 2024
df2<- df1 %>% filter(year < "2024")
#plot
p1 <- ggplot(data = df2, aes(x = year, y = num_citations, fill = paper)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size = 22, face="bold"),
        axis.title.y = element_text(size = 22, face="bold"),
        axis.text.x = element_text(hjust = 1, vjust= 1, size = 20, colour="black", angle = 45),
        axis.text.y = element_text(size = 20, colour = "black")) +
  theme(plot.title = element_text(size = 22, face="bold")) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key = element_rect(colour = NA), legend.text=element_text(size = 20)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(x = "Year", y = "Number of citations", title = "A") +
  scale_fill_manual(values = c("#736F6E", "#C0C0C0","steelblue3", "blue","#6698FF","navy"))+
  scale_x_continuous(breaks = c(2016,2017, 2018, 2019, 2020, 2021, 2022, 2023))

df3 <- read.csv("data/policy_impact_from_Sage.csv")
# Summarise by year
df4 <-df3 %>% mutate(year = format(as.Date(Published.on, format="%d/%m/%Y"),"%Y")) %>% 
  filter(year < "2024") %>% group_by(year, Main_topic) %>% summarise(count=sum(citation))
#plot
p2 <- ggplot(data= df4, aes(x = year, y = count, fill = Main_topic)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#736F6E", "#C0C0C0","steelblue3", "blue","navy"), labels = c("Biodiversity", "Climate Change", "Fishing", "Food security", "Other"))+
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        panel.background = element_blank()) +
  theme(axis.title.x = element_text(size = 22, face="bold"),
        axis.title.y = element_text(size = 22, face="bold"),
        axis.text.x = element_text(hjust = 1, vjust= 1, size = 20, colour="black", angle = 45),
        axis.text.y = element_text(size = 20, colour = "black")) +
  theme(plot.title = element_text(size = 22, face="bold")) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key = element_rect(colour = NA), legend.text=element_text(size = 20)) +
  guides(fill=guide_legend(ncol=2)) +
  labs(title = "B", x = "Year", y = "Number of citations") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20))

## arrange plots into panel
p3 <- grid.arrange(p1, p2, nrow = 1)
#filename<- "Figures_Tables/Hist_Future_FishMIP_Figure_2.png"
#save plot
ggsave(plot = p3, filename = filename, width = 17, height = 10) 

