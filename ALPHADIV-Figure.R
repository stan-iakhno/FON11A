# plots the boxplots of alpha diversity measures


shannonOBS$groups <- factor(shannonOBS$new2, levels = c("14.COL.Y",
                                                        "14.COL.C",
                                                        "7.COL.Y",
                                                        "7.COL.C",
                                                        "0.COL.B",
                                                        "14.CAE.Y",
                                                        "14.CAE.C",
                                                        "7.CAE.Y",
                                                        "7.CAE.C",
                                                        "0.CAE.B",
                                                        "14.IL.Y",
                                                        "14.IL.C",
                                                        "7.IL.Y",
                                                        "7.IL.C",
                                                        "0.IL.B"))
shannonOBS$gut_site


library(ggpval)
plt <- ggplot(shannonOBS, aes(x=groups, y=value, color=feed)) +
  geom_boxplot(fill=c("#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                      "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                      "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00"),
                      color=c("#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                              "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                              "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00"),
               alpha=0.25, 
               lwd=0.4,
               outlier.shape = NA,
               fatten = 1.5)  + 
  ylim(1.6,8) + 
  coord_flip()
m<-add_pval(plt, pairs = list(c(1,2), 
                           c(3,4),
                           c(6,7),
                           c(8,9),
                           c(11,12),
                           c(13,14)), 
            test='wilcox.test', 
            barheight = 0.1,
            pval_star = F,
            pval_text_adj = 1.5, textsize = 9) +
  theme_bw(base_size = 12, base_family = 'serif')+
  labs(y='Shannon index')+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.border = element_blank()) +
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major.y =element_line(linetype = 2))+
  theme(axis.line = element_line(colour = "grey"))+
  theme(axis.line.x.bottom = element_line(colour = "grey"))
m
ggsave("diversity.png",device = "png", 
       plot = m, 
       dpi = 300, 
       width = 9, height = 9, 
       units = "cm")




