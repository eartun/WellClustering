library(tidyverse)
library(readxl)
library(caret)
library(pastecs)
library(ggpubr)
library(cluster)  
library(factoextra)
library(gridExtra)
library(pheatmap)
library(ggplotify)
library(knitr)

set.seed(1)
#Descriptive Statistics of Data
wellData <- read.csv("data.csv")

#Correlogram
cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  # turn all three matrices (r (pearson correlation values), n (sample size = 179), and P (p-values)) into a data frame
  Mdf <- map(M, ~data.frame(.x))
  # return the three data frames in a list
  return(Mdf)
}

formatted_cors <- function(df) {
  cors(df) %>%
    map(~rownames_to_column(.x, var="Parameters")) %>%
    map(~pivot_longer(.x, -Parameters, values_to = "Values")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = Values) %>%  
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F), p_if_sig = ifelse(sig_p, p, NA), r_if_sig = ifelse(sig_p, r, NA))
}

#Create the Correlation Matrix with the necessary parameters (Average GOR, WC, Oil Rate and Total Oil & Water Production) by removing the unnecessary rows

corrMat <- formatted_cors(wellData)
corrMatALL <- corrMat[-c(1:130, 139:199, 208:268, 277:291, 300:337, 346:529), ]

#Visualizing the Correlation Matrix

corrMatALL %>%
  ggplot(aes(x = Parameters, y = name, fill = r, label=round(r,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation\nCoefficient", title="") +
  # map a red, white and blue color scale to correspond to -1:1 sequential gradient
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic()
ggsave("cor.pdf",width = 8.5, height = 2.6,dpi=300)


#Descriptive Statistics
descstatnormData <- stat.desc(wellData)

#Get Distance of the Normalized Data
distance <- get_dist(wellData, method = "pearson")

## ## ## ## ## ## ## ## ## ## ##  
#Part 1. Clustering Comparison
## ## ## ##  ## ##  ## ## ## ## 

#Optimal number of clusters plot
#by silhoutte coefficient
p1 <-fviz_nbclust(wellData, kmeans, method = "silhouette")+ ggtitle("")+theme_classic()
p2 <-fviz_nbclust(wellData, hcut, method = "silhouette")+ ggtitle("")+theme_classic()
#by within sum of squares
p3 <-fviz_nbclust(wellData, kmeans, method = "wss")+ ggtitle("")+theme_classic()
p4 <-fviz_nbclust(wellData, hcut, method = "wss")+ ggtitle("")+ theme_classic() 

Sil_kmeans <- data.frame(p1$data,rep("k-Means", 10))
names(Sil_kmeans) <- c("Number of Clusters","Average Silhoutte Width","Method")
Sil_hierc <-  data.frame(p2$data,rep("Hierarchical", 10))
names(Sil_hierc) <- c("Number of Clusters","Average Silhoutte Width","Method")
plotdata <- rbind(Sil_kmeans,Sil_hierc)
plotdata$`Number of Clusters` <-as.numeric(plotdata$`Number of Clusters`)
Sil<-ggplot(plotdata, aes(x=`Number of Clusters`, y=`Average Silhoutte Width`, color=Method)) +
  geom_line(aes(linetype=Method), linewidth=0.9)+geom_point(aes(shape=Method),size=1.7)+
  scale_x_continuous(breaks=seq(1,10,1))+
  scale_color_brewer(palette="Paired")+
  xlab("Number of Clusters")+ylab("Average Silhoutte Width")+
  theme_minimal()+theme(legend.position="top",legend.title=element_blank())



WSS_kmeans <- data.frame(p3$data,rep("k-Means", 10))
names(WSS_kmeans) <- c("Number of Clusters","Within Sum of Squares","Method")
WSS_hierc <-  data.frame(p4$data,rep("Hierarchical", 10))
names(WSS_hierc) <- c("Number of Clusters","Within Sum of Squares","Method")
plotdata <- rbind(WSS_kmeans,WSS_hierc)
plotdata$`Number of Clusters` <-as.numeric(plotdata$`Number of Clusters`)
WSS<-ggplot(plotdata, aes(x=`Number of Clusters`, y=`Within Sum of Squares`, color=Method)) +
  geom_line(aes(linetype=Method), linewidth=0.9)+geom_point(aes(shape=Method),size=1.7)+
  scale_x_continuous(breaks=seq(1,10,1))+
  scale_color_brewer(palette="Paired")+
  xlab("Number of Clusters")+ylab("Within Sum of Squares")+
  theme_minimal()+theme(legend.position="top",legend.title=element_blank())
ggarrange(Sil,WSS, ncol = 2, nrow = 1,labels=c("a)","b)"))
ggsave("comparison1.png",width = 8, height = 4,dpi=300)
ggsave("comparison1.pdf",width = 8, height = 4,dpi=300) 

#PCA based clusters plot
C2 <- kmeans(wellData, centers = 2)
C3 <- kmeans(wellData, centers = 3)
C4 <- kmeans(wellData, centers = 4)
C5 <- kmeans(wellData, centers = 5)
p1 <- fviz_cluster(C2, geom = "point", data = wellData) + ggtitle("k = 2")
p2 <- fviz_cluster(C3, geom = "point",  data = wellData) + ggtitle("k = 3")
p3 <- fviz_cluster(C4, geom = "point",  data = wellData) + ggtitle("k = 4")
p4 <- fviz_cluster(C5, geom = "point",  data = wellData) + ggtitle("k = 5")
ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2,labels=c("a)","b)","c)","d)"))
ggsave("comparison2.png",width = 7, height = 5,dpi=300)
ggsave("comparison2.pdf",width = 7, height = 5,dpi=300) 

#Heatmap of clusters using kmeans
set.seed(600)
#p1 <-as.ggplot(pheatmap(wellData, kmeans_k = 2, cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE, number_color = "black", fontsize_number = 12))
p2 <-as.ggplot(pheatmap(wellData, kmeans_k = 3, cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE, number_color = "black", fontsize_number = 10))
#p3 <-as.ggplot(pheatmap(wellData, kmeans_k = 4, cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE, number_color = "black", fontsize_number = 12))
p4 <-as.ggplot(pheatmap(wellData, kmeans_k = 5, cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE, number_color = "black", fontsize_number = 10))
ggarrange(p2,p4, ncol = 1, nrow = 2,labels=c("a)","b)"),vjust=21)
ggsave("heatmap.png",width = 9, height = 7,dpi=300)
ggsave("heatmap.pdf",width = 9, height = 7,dpi=300) 

## ## ## ## ## ## ## ## ## 
## Part 2. Clustering ##
## ## ## ## ## ## ## ## ## 
#Clustering (K-Means, k=3)

cluster_kmeans <- kmeans(wellData, centers = 3)

cluster_hier <- hclust(distance, method = "complete")
#Hierarchical Clustering (Complete Linkage, k=3)
hier_clusters <- as.data.frame(cutree(cluster_hier, 3))
#Silhouette Chart and Coefficient (Hierarchical, k=3)

#Plot silhoutte coefficients for both methods
pdf("sil.pdf", width = 12, height = 6)
par(mfrow = c(1, 2))
plot(silhouette(cluster_kmeans$cluster, distance),main="a)")
plot(silhouette(cutree(cluster_hier, 3), distance),main="b)")
dev.off()


## ## ## ## ## ## ## ## ## 
#Part 3. Well Map comparison
## ## ## ## ## ## ## ## ## 

set.seed(600)
#Hierarchical Clustering (Complete Linkage, k=3)
hier_clusters <- as.data.frame(cutree(cluster_hier, 3))
kmeans_map <- data.frame(wellData$X, wellData$Y, cluster_kmeans$cluster)
names(kmeans_map) <- c("X","Y","Cluster")
hierc_map <- data.frame(wellData$X, wellData$Y, hier_clusters)
names(hierc_map) <- c("X","Y","Cluster")

p1<-ggplot(kmeans_map,aes(x=X,y=Y,color=as.character(Cluster),shape=as.character(Cluster)))+
  geom_point(size=2)+xlab("X")+ylab("Y")+
  scale_color_discrete(name = "Clusters")+
  scale_shape_discrete(name = "Clusters")+
  theme_bw()
p2<-ggplot(hierc_map,aes(x=X,y=Y,color=as.character(Cluster),shape=as.character(Cluster)))+
  geom_point(size=2)+xlab("X")+ylab("Y")+
  scale_color_discrete(name = "Clusters")+
  scale_shape_discrete(name = "Clusters")+
  theme_bw()
ggarrange(p1,p2, ncol = 2, nrow = 1,labels=c("a)","b)"))
ggsave("map.png",width = 9, height = 5,dpi=300)
ggsave("map.pdf",width = 9, height = 5,dpi=300) 
table(kmeans_map$Cluster,hierc_map$Cluster)

save.image("Clustering.RData")

#Production Scenarios
prod <- read_excel("Scenarios.xlsx", sheet="Plot")
prod$`Downturn (Shut-In) Length` <- factor(prod$`Downturn (Shut-In) Length`, levels=c("6 months", "12 months", "18 months"))

ggplot(prod,aes(`Time, months`,`Cumulative Oil Production, MM bbls`, color=Scenario))+
  geom_line(aes(linetype = Scenario),linewidth=0.8)+
  facet_grid(`Clustering Algorithm`~`Downturn (Shut-In) Length`)+
  scale_colour_brewer(palette="Paired")+
  theme_minimal()
ggsave("prod.pdf",width = 5, height = 2.6,dpi=300)

#Tornado Diagrams
all2 <- read_excel("Scenarios.xlsx", sheet="ALL2")
lm1 <- lm(NPVperWell ~ i+LOWt+LOWp+HIGHp+NoWells, data = all2)
torn1 <- tornado::tornado(lm1, type = "PercentChange", alpha = 0.50)
npv_torn<-plot(torn1, xlabel = "NPV of Income per Active Well (MM USD)", geom_bar_control = list(width = 0.4))
lm2 <- lm(NPVCF ~ i+LOWt+LOWp+HIGHp+NoWells, data = all2)
torn2 <- tornado::tornado(lm2, type = "PercentChange", alpha = 0.50)
npvw_torn<-plot(torn2, xlabel = "NPV from Cash Flow Analysis (MM USD) ", geom_bar_control = list(width = 0.4))


all3 <- read_excel("Scenarios.xlsx", sheet="ALL3")
lm3 <- lm(NPVperWell ~ i+LOWt+LOWp+HIGHp+NoWells, data = all3)
torn3 <- tornado::tornado(lm3, type = "PercentChange", alpha = 0.50)
npv_torn2<-plot(torn3, xlabel = "NPV of Income per Active Well (MM USD)", geom_bar_control = list(width = 0.4))
lm4 <- lm(NPVCF ~ i+LOWt+LOWp+HIGHp+NoWells, data = all3)
torn4 <- tornado::tornado(lm4, type = "PercentChange", alpha = 0.50)
npvw_torn2<-plot(torn4, xlabel = "NPV from Cash Flow Analysis (MM USD) ", geom_bar_control = list(width = 0.4))



ggarrange(npv_torn,npvw_torn,npv_torn2,npvw_torn2, ncol = 1, nrow = 4,labels=c("a)","b)","c)","d)"))
ggsave("torn.png",width = 6, height = 8,dpi=300)
ggsave("torn.pdf",width = 6, height = 8,dpi=300) 
