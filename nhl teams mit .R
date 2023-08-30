###############################NHL Teams#####################################

###Install and load required packages
install.packages("d3heatmap")
library(d3heatmap)

###Import data
teams <- read.csv(file.choose(), header = T)

###Preview teams
summary(teams)

###Prepare dataset
row.names(teams) <- teams$Team #Set row names to be teams
teams$Season <- NULL #Remove seasons column
teams$Team <- NULL #Remove teams column
teams$GP <- NULL #Remove GP
teams$TOI <- NULL #Remove TOI


###Rename column names
names(teams) <- c("CF%", "CF/60", "CA/60","GF%", "GF/60", 
                  "GA/60", "xGF%", "xGF/60", "xGA/60", 
                  "Sh%", "Sv%", "PDO")

###View objects stored in teams dataset
names(teams) 

###Set dataframe to a matrix
team_matrix <- data.matrix(teams)

###Make interactive heatmap
heatmap <- d3heatmap(team_matrix, colors = "Blues", scale = "col",
                     dendrogram = "row", k_row = 4)

###View heatmap 
heatmap

#########################PCA CLUSTERING#######################################

###Install and load required packages
install.packages("ggrepel")
library(ggrepel)

###Perform PCA on teams data
pca_teams <- princomp(teams, cor = TRUE) 

###View objects stored in pcaCars
names(pca_teams)

###Proportion of variance explained
summary(pca_teams)

###Plot scree plot
plot(pca_teams, type = "l")

###Cluster teams
teamsHC <- hclust(dist(pca_teams$scores), method = "ward.D2")

###Plot dendrogram
plot(teamsHC)

###Cut dendogram into 5 clusters
teams_clusters <- cutree(teamsHC, k = 5)

###Add clusters to dataframe of scores
teams_df <- data.frame(pca_teams$scores, "cluster" = factor(teams_clusters))
teams_df <- transform(teams_df, cluster_name = paste("Cluster",teams_clusters))

###Plot
pcaplot <- ggplot(teams_df,aes(x=Comp.1, y=Comp.2)) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(aes(color = cluster), alpha = 0.55, size = 3) +
  xlim(-5, 6) + 
  labs(x = "PC1", y ="PC2", 
       title="PCA Clusters from Hierarchichal Clustering of NHL Teams", 
       subtitle="Data: corsica.hockey")

###View plot
pcaplot

###Add labels
pcaplot + geom_text(aes(y = Comp.2 + 0.25, label = rownames(teams_df)))

###Use ggrepel to repel overlapping text labels 
pcaplot + geom_text_repel(aes(y = Comp.2 + 0.009, label = rownames(teams_df))) 

##################################################################################