library(ggplot2)
library(ggalluvial)
library(readxl)
library(dplyr)
df <- read_excel("prognostic_sankey.xlsx")
LIHCData <- group_by(df,Cluster,PINK.score,IPI.score) %>% summarise(., count = n())


ggplot(as.data.frame(LIHCData),
       aes(axis1 = Cluster, axis2 = PINK.score, axis3 = IPI.score,
           y= count)) +
  geom_flow(width = 0.2,
            curve_type = "sine",
            alpha = 0.5,
            color = 'white',
            size = 0.1)+
  scale_x_discrete(limits = c("Cluster", "PINK.score", "IPI.score"), expand = c(.1, .05)) +
  geom_alluvium(aes(fill = Cluster)) + 
  geom_stratum(width = 0.20)+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+ 
  #theme_minimal() 
  scale_fill_manual(values = c("#bcbcbc","#803c96","#00a1d1","#f09121","#dd292a"))+ 
  theme_void() 
  #ggtitle("Sankey-diagram of the clinical characteristics and molecular subtypes")


