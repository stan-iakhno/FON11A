#fecth the file
ft <- read.csv("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Caroline_fon11a/FT2.csv")

#remove undesirable variables
ft <- ft[ft$variable != "mucus_fibrosis_atrophy",]
ft <- ft[ft$variable !="LP_vacuolisation_oedema",]

ft$variable
#manually reorder factor
ft$variable <- as.character(ft$variable)
ft$variable<- factor(ft$variable, levels=unique(ft$variable))
ft$variable <- factor(ft$variable, levels=c("diagnose",
                                            "LP_eosinophils",
                                            "LP_lymphocytes_&_plasma cells",
                                            "LP_macrophages", 
                                            "LP_neutrophils",
                                            "crypt_abscess",
                                            "intra-epithelial_lymphocytes",
                                            "epithelial_damage"))
# plot and flip the axises
library(ggplot2)
pl01<-ggplot(ft, aes(x=variable, y=Freq, fill=value))+
  geom_bar(stat="identity")+
  facet_grid(.~diet)+
  coord_flip()+
  labs(y = "Frequency", x="Histology analysis")+
  theme_minimal(base_size = 14,base_family = 'serif')+
 # theme(legend.title = element_text(hjust = 0.5, vjust = 0.5), legend.title.align = 0.)+
  labs(fill="Histology score")+
  theme(axis.title.y = element_blank())+
  theme(legend.direction = "horizontal",legend.position = c(0.25, 1.15))+
  theme(legend.text = element_text(colour="black", size=10,vjust = 0.5),
        legend.key.size = unit(2, 'mm'))


pl01 
#save the plot on the HDD
ggsave("hist001.png",device = "png", 
       plot = pl01, 
       dpi = 300, 
       width = 14, 
       height = 7, units = 'cm')
