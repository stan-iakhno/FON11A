# plots correaltion scatter-plot with the abline: colonic butyrate ~ faecalibacterium
#                                Estimate   SE         t value  Pr(>|t|)    
#(Intercept)                     7.8238     0.9567     8.178    3.96e-09 ***
# Faecalibacterium              1116.8538   188.8648   5.914    1.78e-06 ***
#Residual standard error: 3.343 on 30 degrees of freedom
#Adjusted R-squared:  0.5229 
#F-statistic: 34.97 on 1 and 30 DF,  p-value: 1.776e-06


library(readxl)
library('scales')
#fetches the data from an excel file
butyrate_CD<-
  read_xlsx("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Caroline_fon11a/butyrate-CD.xlsx")

butyrate_CD$diet[butyrate_CD$diet=='y'] <- "yeast"
butyrate_CD$diet[butyrate_CD$diet=='c'] <- "control"

corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}
labels = data.frame(x = 15, y = 0.01, label = corr_eqn(butyrate_CD$butyrate,
                                                      butyrate_CD$Faecalibacterium))

pl5<-ggplot(butyrate_CD, aes(x = butyrate, y =(Faecalibacterium))) +
  geom_point(shape = 19,alpha=.7, size = 4, aes(colour = diet)) +
  scale_color_manual(values=c("#CC79A7",  "#009E73"))+
  # scale_fill_manual(values=c("#CC79A7","#009E73"))
  geom_smooth(colour = "darkgrey", fill = "grey", method = 'lm') +
  #  ggtitle("Example") +
  ylab("F. prausnitzii") +
  xlab("colonic butyrate") +
  theme(legend.key = element_blank(),
        legend.background = element_rect(colour = 'black'),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 12, vjust = 0.5,
                                   hjust = 1, colour = 'black'),
        
        axis.title.y = element_text(face = "italic"),
        axis.text.y = element_text(size = 12, colour = 'black', vjust = 0.01),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black'),
        panel.background = element_blank()) +
  theme_minimal(base_size = 14)+
  geom_text(data = labels, aes(x = x, y = y,
                               label = label), parse = TRUE)+
  
  theme(panel.grid = element_blank())+
  theme(legend.position = c(0.15,0.95))+
  theme(legend.title = element_blank())
pl5

cor.test(butyrate_CD$Faecalibacterium,butyrate_CD$butyrate)
linmod<-lm(butyrate_CD$butyrate~butyrate_CD$Faecalibacterium)
summary(linmod)
#save the plot on the HDD
ggsave("cor003-but-faec.png",device = "png", 
       plot = pl5, 
       dpi = 300, 
       width = 8, 
       height = 8, 
       units = "cm")

