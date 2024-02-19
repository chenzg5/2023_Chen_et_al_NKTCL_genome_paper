library(ggplot2)
library(reshape2)
library(readxl)

#compare CNV between primary and R/R cohort
library(tidyverse)
library(reshape2)
library(ggpubr) 
library(brunnermunzel)
CNV <- read_excel("CNV_boxplot.xlsx")
p=ggboxplot(CNV, x = "group", y = "CN",
            xlab = "Sampling status",ylab = "The number of CNVs",
            bxp.errorbar=T,color = "group", palette = "jco",
            #label.select=list(criteria = "y < 10"),
            add = "jitter")
#  Add p-value
a=brunnermunzel.test(CN ~ group, data = CNV)
p + ggplot2::annotate("text", x=1.5,y = 2500,
                      label = paste("Brunner–Munzel, P =",a[["p.value"]]),size=5)


##plot the distribution of CNVs
df <- read_excel("CNV_count.xlsx")
order <- df$Tumor_Sample_Barcode
data_plot = melt(df,id = "Tumor_Sample_Barcode")
cols = c("CN_3" = "#f7beca", "CN>3"="#df302e", "CN_1" = "#a4d3e0","CN_0" = "#4156a8")
ggplot( data_plot, aes( x = factor(Tumor_Sample_Barcode,levels=order), weight = value, fill = variable))+
geom_bar( position = "stack")+
  scale_fill_manual( values = cols)+
  theme_classic()+
  ylab("The number of CNVs")+
  theme(text = element_text(size = 15))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))



###plot the distribution of SVs
SV <- read_excel("SV_count.xlsx")
order <- SV$Tumor_Sample_Barcode
data_plot = melt(SV,id = "Tumor_Sample_Barcode")

cols = c("Deletion" = "#2637a0", "Translocation"="#2ca53b", "Duplication" = "#f90026","Insertion" = "#fb8527","Inversion"="#6b1f6c")
ggplot( data_plot, aes( x = factor(Tumor_Sample_Barcode,levels=order), weight = value, fill = variable))+
  geom_bar( position = "stack")+
  scale_fill_manual( values = cols)+
  theme_classic()+
  ylab("The number of SVs")+
  theme(text = element_text(size = 15))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))


#compare SVs between primary and R/R cohort
library(tidyverse)
library(reshape2)
library(ggpubr) 
library(brunnermunzel)
SV_num <- read_excel("SV_boxplot.xlsx")
p=ggboxplot(SV_num, x = "group", y = "SV",
            xlab = "Sampling status",ylab = "The number of SVs",
            bxp.errorbar=T,color = "group", palette = "jco",
            add = "jitter")
#  Add p-value
a=brunnermunzel.test(SV ~ group, data = SV_num)
p + ggplot2::annotate("text", x=1.5,y = 600,
                      label = paste("Brunner–Munzel, P=",a[["p.value"]]),size=5)


