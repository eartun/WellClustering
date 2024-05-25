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

set.seed(5)
wellData <- read.csv("data.csv") #import normalized data set

## ## ## ## ## ## ## ## ## ## ## ## ##   
## Part 1. Exploratory Data Analysis ## 
## ## ## ##  ## ##  ## ## ## ## ## ## 

#Sources of Figs.5 and Fig.6 are excluded due to confidentiality of the original data set

#Correlogram (Fig. 7)
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
ggsave("Fig7.pdf",width = 8.5, height = 2.6,dpi=300)

#Descriptive Statistics
descstatnormData <- stat.desc(wellData)

#Get Distance of the Normalized Data
distance <- get_dist(wellData, method = "pearson")

## ## ## ## ## ## ## ## ## ## ## ## ##   
## Part 2. Clustering ## ## ## ## ## 
## ## ## ##  ## ##  ## ## ## ## ## ## 
#PRELIMINARY ANALYSIS OF CLUSTERS (Comparison and evaluation)
#Optimal number of clusters plot
#by silhoutte coefficient
set.seed(11)
wellData <- read.csv("data.csv")
set.seed(11)
p1 <-fviz_nbclust(wellData, kmeans, method = "silhouette")+ ggtitle("")+theme_classic()
set.seed(11)
p2 <-fviz_nbclust(wellData, hcut, method = "silhouette")+ ggtitle("")+theme_classic()
#by within sum of squares
set.seed(11)
p3 <-fviz_nbclust(wellData, kmeans, method = "wss")+ ggtitle("")+theme_classic()
set.seed(11)
p4 <-fviz_nbclust(wellData, hcut, method = "wss")+ ggtitle("")+ theme_classic() 

Sil_kmeans <- data.frame(p1$data,rep("K-means", 10))
names(Sil_kmeans) <- c("Number of Clusters","Average Silhouette Width","Method")
Sil_hierc <-  data.frame(p2$data,rep("Hierarchical", 10))
names(Sil_hierc) <- c("Number of Clusters","Average Silhouette Width","Method")
plotdata <- rbind(Sil_kmeans,Sil_hierc)
plotdata$`Number of Clusters` <-as.numeric(plotdata$`Number of Clusters`)
Sil<-ggplot(plotdata, aes(x=`Number of Clusters`, y=`Average Silhouette Width`, color=Method)) +
  geom_line(aes(linetype=Method), linewidth=0.9)+geom_point(aes(shape=Method),size=1.7)+
  scale_x_continuous(breaks=seq(1,10,1))+
  scale_color_brewer(palette="Paired")+
  xlab("Number of Clusters")+ylab("Average Silhouette Width")+
  theme_minimal()+theme(legend.position="top",legend.title=element_blank())



WSS_kmeans <- data.frame(p3$data,rep("K-means", 10))
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
ggsave("Fig8.png",width = 8, height = 3,dpi=300)
ggsave("Fig8.png",width = 8, height = 3,dpi=300) 

#PCA based cluster plot
C2 <- kmeans(wellData, centers = 2)
C3 <- kmeans(wellData, centers = 3)
C4 <- kmeans(wellData, centers = 4)
C5 <- kmeans(wellData, centers = 5)
p1 <- fviz_cluster(C2, geom = "point", data = wellData) + ggtitle("k = 2")
p2 <- fviz_cluster(C3, geom = "point",  data = wellData) + ggtitle("k = 3")
p3 <- fviz_cluster(C4, geom = "point",  data = wellData) + ggtitle("k = 4")
p4 <- fviz_cluster(C5, geom = "point",  data = wellData) + ggtitle("k = 5")
ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2,labels=c("a)","b)","c)","d)"))
ggsave("Fig9.png",width = 7, height = 5,dpi=300)
ggsave("Fig9.pdf",width = 7, height = 5,dpi=300) 

#Heatmap of clusters using kmeans
set.seed(5)
wellData <- read.csv("data.csv")
p1 <-as.ggplot(pheatmap(wellData, kmeans_k = 3, cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE, number_color = "black", fontsize_number = 10))
p2 <-as.ggplot(pheatmap(wellData, kmeans_k = 5, cluster_cols = FALSE, cluster_rows = FALSE, display_numbers = TRUE, number_color = "black", fontsize_number = 10))
ggarrange(p1,p2, ncol = 1, nrow = 2,labels=c("a)","b)"),vjust=21)
ggsave("Fig10.png",width = 9, height = 7,dpi=300)
ggsave("Fig10.pdf",width = 9, height = 7,dpi=300) 


#CLUSTERING
set.seed(5)
wellData <- read.csv("data.csv")
#Clustering (K-Means, k=3)
cluster_kmeans <- kmeans(wellData, centers = 3)
#Hierarchical Clustering (Complete Linkage, k=3)
cluster_hier <- hclust(distance, method = "complete")
hier_clusters <- as.data.frame(cutree(cluster_hier, 3))
kmeans_result <- kmeans(wellData, centers = 3)
hierarchical_result <- hclust(dist(wellData))
#Adding Cluster assignments to the original data set
wellData$KMeans_Cluster <- as.factor(kmeans_result$cluster)
wellData$Hierarchical_Cluster <- as.factor(cutree(hierarchical_result, k = 3))
# Define custom labels for K-means and Hierarchical clustering
kmeans_labels <- c("Cluster-1 (HIGH)", "Cluster-2 (MID)", "Cluster-3 (LOW)")
hierarchical_labels <- c("Cluster-1 (HIGH)", "Cluster-2 (MID)", "Cluster-3 (LOW)")

# Function to plot clusters with shapes and colors for K-means
plot_clusters_with_shapes_kmeans <- function(wellData, cluster_column, title) {
  ggplot(wellData, aes_string(x = "X", y = "Y", shape = cluster_column, color = cluster_column)) +
    geom_point(size = 2) +
    scale_shape_manual(values = c(16, 17, 15), labels = kmeans_labels) +
    scale_colour_brewer(palette="Set1", labels = kmeans_labels) +
    labs(title = title, x = "X", y = "Y") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

# Function to plot clusters with shapes and colors for Hierarchical clustering
plot_clusters_with_shapes_hierarchical <- function(wellData, cluster_column, title) {
  ggplot(wellData, aes_string(x = "X", y = "Y", shape = cluster_column, color = cluster_column)) +
    geom_point(size = 2) +
    scale_shape_manual(values = c(16, 17, 15), labels = hierarchical_labels) +
    scale_colour_brewer(palette="Set1", labels = hierarchical_labels) +
    labs(title = title, x = "X", y = "Y") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

# Map of wells based on K-means and Hierarchical Clustering in two separate plots
plot1 <- plot_clusters_with_shapes_kmeans(wellData, "KMeans_Cluster", "K-means Clustering")
plot2 <- plot_clusters_with_shapes_hierarchical(wellData, "Hierarchical_Cluster", "Hierarchical Clustering")
# Display and save the plots
ggarrange(plot1,plot2, ncol = 2, nrow = 1,labels=c("a)","b)"))
ggsave("Fig12.png",width = 10, height = 5,dpi=300)
ggsave("Fig12.pdf",width = 10, height = 5,dpi=300) 

#Plot silhoutte coefficients for both methods
pdf("Fig13.pdf", width = 12, height = 6)
par(mfrow = c(1, 2))
plot(silhouette(kmeans_result$cluster, distance),main="a)")
plot(silhouette(cutree(hierarchical_result, 3), distance),main="b)")
dev.off()


save.image("Clustering.RData")

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##   
## Part 3. Analysis of Shut-in Scenarios ## ## ## ## ## 
## ## ## ##  ## ##  ## ## ## ## ## ## ## ## ## ## ## 
prod <- read_excel("Scenarios.xlsx", sheet="Plot") #import scenario results
prod$`Downturn (Shut-In) Length` <- factor(prod$`Downturn (Shut-In) Length`, levels=c("6 months", "12 months", "18 months"))

ggplot(prod,aes(`Time, months`,`Cumulative Oil Production, MM bbls`, color=Scenario))+
  geom_line(aes(linetype = Scenario),linewidth=0.8)+
  facet_grid(`Downturn (Shut-In) Length`~`Clustering Algorithm`)+
  scale_colour_brewer(palette="Paired")+
  theme_minimal()
ggsave("Fig14.pdf",width = 7.5, height = 3.5,dpi=300)

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

#Display and save Tornado Diagrams for all cases
ggarrange(npv_torn,npvw_torn,npv_torn2,npvw_torn2, ncol = 1, nrow = 4,labels=c("a)","b)","c)","d)"))
ggsave("Fig15.png",width = 6, height = 8,dpi=300)
ggsave("Fig15.pdf",width = 6, height = 8,dpi=300) 
save.image("Clustering.RData")
