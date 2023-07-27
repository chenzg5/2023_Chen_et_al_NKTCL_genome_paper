library(ComplexHeatmap)
library(stringr)
library(tidyr)
library(circlize)

subclass_genes <- read.table("subclass_genes.txt",header = T,sep = "\t")
genes <- subclass_genes$gene
subclass <- subclass_genes$subclass
mat <- read.table('NKT_allgene_matrix.txt',sep = '\t',header = T,row.names = 1)


# set the colar of mutations
col = c("Non_synonymous_mutations" = "#101820", "low_level_CN_gain" = "#f7beca", "CN_gain"="#df302e",
        "single_CN_loss" = "#a4d3e0","CN_loss" = "#4156a8",
        "SV" = "#50b747")

#Specify the shape, width, height, and color of the grid
alter_fun = list(
  background = alter_graphic("rect", fill = "#ffffff",horiz_margin = unit(0, "pt"), vertical_margin = unit(0, "pt")),   
  Non_synonymous_mutations = alter_graphic("rect", fill = col["Non_synonymous_mutations"],horiz_margin = unit(0, "pt"), vertical_margin = unit(0, "pt")),
  CN_gain = alter_graphic("rect", fill = col["CN_gain"],horiz_margin = unit(0, "pt"), vertical_margin = unit(0, "pt")),
  CN_loss = alter_graphic("rect", fill = col["CN_loss"],horiz_margin = unit(0, "pt"), vertical_margin = unit(0, "pt")),
  SV = alter_graphic("rect", fill = col["SV"],horiz_margin = unit(0, "pt"), vertical_margin = unit(0, "pt")))


# title
column_title = "Molecular subtypes of NK/T cell lymphoma"

# Change the name of tag
heatmap_legend_param = list(title = "Alternations", 
                            at = c('Non_synonymous_mutations', 
                                   'CN_gain',
                                   'CN_loss', 'SV'), 
                            labels = c('Non-synonymous mutations', 
                                       'CN gain',
                                       'CN loss', 
                                       'Structural variation'))

###Preliminarily drawn the mutation profile
test <- oncoPrint(mat,
          alter_fun = alter_fun, 
          alter_fun_is_vectorized = FALSE,
          pct_gp = gpar(fontsize = 6), 
          col = col,
          column_title = column_title)
test
#Output the order of samples and match the corresponding clinical data
col_name <- as.data.frame(colnames(test@matrix))
sample_cluster <- read.table("sample_cluster.txt",sep = '\t',header = T)
clinic_data <- read.table("clinicaldata.txt",sep = '\t',header = T)
library(dplyr)
cdata <- left_join(col_name,sample_cluster,by=c("colnames(test@matrix)"="sample"))
cdata <- left_join(cdata,clinic_data,by=c("colnames(test@matrix)"="Tumor_Sample_Barcode"))
#write.table(cdata,file='cluster_survival.txt',quote = F,sep = '\t',row.names = F)


##define the order
order_df <- read.table("sample_cluster.txt",sep = '\t',header = T)
order <- order_df$sample

##Determine the comment information
ha <- HeatmapAnnotation(sampling_status=cdata$sampling_status,
                        sequencing_method=cdata$sequencing_method,
                        stage=cdata$Ann_Arbor_stage,
                        show_annotation_name = TRUE, 
                        col = list(sampling_status = c("0" =  "#cab2d6", "1" = "#6a3d9a"),
                                   sequencing_method = c("WGS" =  "#FF9200FF", "WES" = "#FFDB00FF"),
                                   stage=c("I_II" = "#66BCD9", "III_IV" = "#CA8C8B","unknow"="#859297")),
                        annotation_name_gp = gpar(fontsize = 10)) 

top <- HeatmapAnnotation(cluster=cdata$cluster,
                        show_annotation_name = TRUE, 
                        col = list(cluster = c("C0" =  "#bdbdbd", "C1" = "#803d97","C2" = "#00a2d1","C3" = "#f19122","C4" = "#dd292a")),
                        annotation_name_gp = gpar(fontsize = 10)) 


oncoplot_anno <- oncoPrint(mat,
                           alter_fun = alter_fun, 
                           alter_fun_is_vectorized = FALSE,
                           col = col,
                           row_names_side = "left",
                           pct_side = "right",
                           bottom_annotation = ha, 
                           top_annotation =top,
                           left_annotation = rowAnnotation(bar=subclass), 
                           remove_empty_columns = F, #remove_empty_rows = TRUE, 
                           column_title = column_title,
                           column_order = order,
                           #row_order = genes,
                           #column_split=cluster,
                           row_split = subclass,
                           column_split = cdata$cluster,
                           column_names_gp = gpar(fontsize = 6),
                           show_column_names=F,right_annotation =NULL,show_pct = F)

oncoplot_anno




