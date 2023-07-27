##survival analysis
library("survival")
library("survminer")
library(readxl)

cdata <- read_xlsx("cluster_survival.xlsx")
#OS of all NKTCL patients from different molecular subtyping clusters
fit <- survfit(Surv(OS,dead) ~cluster,data=cdata)
plot(fit)
p2 <- ggsurvplot(fit, data = cdata,
                 surv.median.line = "hv",lwd=1.2, 
                 palette=c("#bcbcbc","#803c96","#00a1d1","#f09121","#dd292a"),#"#bcbcbc","#803c96","#00a1d1","#f09121","#dd292a"
                 legend.labs=c( "C0","C1","C2","C3","C4"), 
                 legend.title="Cluster",
                 ylab="Overall survival ",xlab = " Time (months)", 
                 censor.shape = 124,censor.size = 2,
                 break.x.by = 12,
                 font.legend=13,
                 xlim = c(0,48),
                 font.x=14,
                 font.y=14,
                 risk.table = F,tables.height = 0.23,
                 tables.theme = theme_cleantable(),
                 #ggtheme = theme_bw(),
                 mtext("My Y Label", side = 2, line = -0.1))

survdiff(Surv(OS,dead) ~ cluster, data = cdata)
p2$plot = p2$plot +  ggplot2::annotate("text",x = 6, y = 0.30,
                                       label = paste("P = 0.005"),size=5)  
p2

#PFS of all NKTCL patients from different molecular subtyping clusters
fit <- survfit(Surv(PFS,relapsed) ~cluster,data=cdata)
plot(fit)
p2 <- ggsurvplot(fit, data = cdata,
                 surv.median.line = "hv",lwd=1.2, 
                 palette=c("#bcbcbc","#803c96","#00a1d1","#f09121","#dd292a"),#"#bcbcbc","#803c96","#00a1d1","#f09121","#dd292a"
                 legend.labs=c( "C0","C1","C2","C3","C4"), 
                 legend.title="Cluster",
                 ylab="Overall survival ",xlab = " Time (months)", 
                 censor.shape = 124,censor.size = 2,
                 break.x.by = 12,
                 font.legend=13,
                 xlim = c(0,48),
                 font.x=14,
                 font.y=14,
                 risk.table = F,tables.height = 0.23,
                 tables.theme = theme_cleantable(),
                 #ggtheme = theme_bw(),
                 mtext("My Y Label", side = 2, line = -0.1))

survdiff(Surv(PFS,relapsed) ~ cluster, data = cdata)
p2$plot = p2$plot +  ggplot2::annotate("text",x = 6, y = 0.30,
                                       label = paste("P < 0.001"),size=5)  
p2
