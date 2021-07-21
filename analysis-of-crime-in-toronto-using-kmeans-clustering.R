

# Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(viridis)
library(tidyr)
library(cluster)
library(ggmap)
library(maps)
library(factoextra)
library(GGally)

# setwd("C:/USER/Desktop/ASDM/crime_analysis_using_kmeans")

# load data------------
crimesDF <- read.csv('MCI_2014_to_2019.csv')
head(crimesDF)
str(crimesDF)
sapply(crimesDF, class)
colnames(crimesDF)
dim(crimesDF)
summary(crimesDF)

# Data cleaning---------------------------
# Check for duplicated event_unique_ID
sum(duplicated(crimesDF$event_unique_id))

# Drop duplicated eventID
crimesDF <- subset(crimesDF, !duplicated
                        (crimesDF$event_unique_id))
dim(crimesDF)

# The dataset was supposed to be from 2014-2019 but i could see earlier years in some records
# Check these years
unique(crimesDF$occurrenceyear)
unique(crimesDF$reportedyear)

# Some events which occurred between 2013 to 2003 were reported between 2014 to 2019
# See these records
years <- group_by(crimesDF, occurrenceyear)
yearly_crime_rec <- dplyr::summarise(years,
                           n = n())
head(yearly_crime_rec, 20)
# Drop all events which occurred between 2003 to 2013
crimesDF <- crimesDF %>% filter(occurrenceyear %in% c("2014", "2015", "2016", "2017", "2018", "2019"))
unique(crimesDF$occurrenceyear)


colnames(crimesDF)
# Remove columns that are not needed for this analysis:
crimesDF <- select(crimesDF, -c("�..X", "Y", "Index_", "ucr_code", 
                                           "ucr_ext", "reporteddate", "reportedmonth", "reportedday", 
                                           "reporteddayofyear", "reporteddayofweek", "reportedhour", 
                                           "occurrencedayofyear", "reportedyear", "Division", "Hood_ID",
                                           "ObjectId"))
head(crimesDF)
colnames(crimesDF)

# check for missing values in every column
colSums(is.na(crimesDF))

# Drop records with NA values
crimesDF <- crimesDF[complete.cases(crimesDF), ]
head(crimesDF)
dim(crimesDF)

# DATA TRANSFORMATION------------------------------------------------------------------

# Rearrange the dataset so it can be used for clustering
crime_groups <- group_by(crimesDF, MCI, Neighbourhood)
groups <- dplyr::summarise(crime_groups, Total=n())
groups <- groups[c("Neighbourhood", "MCI", "Total")]

# Transform the data layout to a form that it can be used for clustering analysis
clusteringDF <- spread(groups, key = MCI, value = Total)
head(clusteringDF)

# Save transformed data for later use
write.csv(clusteringDF,"C:/Users/USER/Desktop/ASDM/crime_analysis_using_kmeans/sas_clusteringDF.csv", 
          row.names = FALSE)


# Data Exploration-------------------------------------------------------------------
# Descriptive Statistics of the clustering dataset
head(clusteringDF)
names(clusteringDF) 
summary(clusteringDF)
str(clusteringDF)
dim(clusteringDF)

# Relationship between the different crime types
ggpairs(clusteringDF[, 2:6])

# Check columns with missing data
colSums(is.na(clusteringDF))

# Check for correlation between variables
correlation <- cor(clusteringDF[2:6])
print(round(correlation, 2))
corrplot::corrplot(correlation)


# KMEANS CLUSTERING------------------------------------------------------------------
# Scale clusteringDF without adding the rownames
scaleDF <- scale(clusteringDF[,2:6])
#Set company names as row names 
rownames(scaleDF)<-clusteringDF$Neighbourhood

head(scaleDF)

# Calculated and visualize distance matrix
distance <- dist(scaleDF,method = "euclidean") 
fviz_dist(distance)


# Get cluster tendency
# Assess clustering tendency of the data using Hopkins statistics.
clust_tendency <- get_clust_tendency(scaleDF, 30, graph = TRUE) 
clust_tendency$hopkins_stat
#  dataset is far above the 0.5 threshole so its highly clusterable
# (Reference: https://www.datanovia.com/en/lessons/assessing-clustering-tendency/)


# COMPUTING KMEANS CLUSTERING--------------------
# We dont know the best number of clusters to use so I will first use the
# fviz_nbclust function to get a rough estimate of possible k values and
# compute kmeans use these value in other to pick the best value using a 
# combination of three evalutaion techniques

# ESTIMATING THE POSSIBLE VALUES OF K
set.seed(123)
fviz_nbclust(scaleDF, kmeans, method = "wss", k.max = 15)

# THIS SUGGESTS 2:5 AS THE POSSIBLE VALUES OF K

# computing kmeans when k=2
# nstart = number of initialization of centriod
km2 <- kmeans(scaleDF, centers = 2, nstart = 25)
km3 <- kmeans(scaleDF, centers = 3, nstart = 25)
km4 <- kmeans(scaleDF, centers = 4, nstart = 25)
km5 <- kmeans(scaleDF, centers = 5, nstart = 25)


# plots to compare
p1 <- fviz_cluster(km2, geom = "point", data = scaleDF) + ggtitle("k = 2")
p2 <- fviz_cluster(km3, geom = "point",  data = scaleDF) + ggtitle("k = 3")
p3 <- fviz_cluster(km4, geom = "point",  data = scaleDF) + ggtitle("k = 4")
p4 <- fviz_cluster(km5, geom = "point",  data = scaleDF) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# The strongly suggest that k=2 and k=3 is the best possible value of k

# Determining Optimal Clusters----------------
# Elbow method
set.seed(123)
fviz_nbclust(scaleDF, kmeans, method = "wss")

# suggest between 2 and 3 but 4 also seems to be a possibility

# Average Silhouette Method

fviz_nbclust(scaleDF, kmeans, method = "silhouette")

# compute the average shihoutte width for k=2, k=3 and k=4
# Average silhouette when k=2
km2.silh <- silhouette(km2$cluster, dist(scaleDF))
ASW1 <- fviz_silhouette(km2.silh) + ggtitle("k = 2")

# Average silhouette when k=3
km3.silh <- silhouette(km3$cluster, dist(scaleDF))
ASW2 <- fviz_silhouette(km3.silh)+ ggtitle("k = 3")

# Average silhouette when k=4
km4.silh <- silhouette(km4$cluster, dist(scaleDF))
ASW3 <- fviz_silhouette(km4.silh) + ggtitle("k = 4")

grid.arrange(ASW1, ASW2, ASW3, nrow = 2)

# k=2 resulted in an average width of 0.73 which is the highest
# k=3 and k=4 both has an average width of 0.52 which is still above the threshole

# Gap Statistic Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(scaleDF, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

# Suggest k=3

# visualize gap statistics
fviz_gap_stat(gap_stat)

# Revisualize k=2, k=3 and k=4 to decide on the value of k
grid.arrange(p1, p2, p3, nrow = 2)


# K=3 is choosen as the optimal value for k

# FINAL KMEANS ANALYSIS USING K=3
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(scaleDF, 3, nstart = 25)
print(final)

# Visualize result
fviz_cluster(final, data = scaleDF)



# ADD THE CLUSTERS TO OUR INITIAL "clusteringDF" DATA AND DO SOME DESCRIPTIVE STATISTICS
head(clusteringDF)

# Add clusters to the original dataset
clusteringDF$clusters <- final$cluster 

# Getting the neighbourhoods in each cluster
cluster1 <- clusteringDF[clusteringDF$clusters == 1, ] 
unique(cluster1$Neighbourhood)

cluster2 <- clusteringDF[clusteringDF$clusters == 2, ] 
unique(cluster2$Neighbourhood)

cluster3 <- clusteringDF[clusteringDF$clusters == 3, ] 
unique(cluster3$Neighbourhood)

# Cluster statistics
cluster_stat <- clusteringDF[-1] %>% group_by(clusters) %>% 
  summarise_all(c("mean", "min", "max"))

cluster_stat

print(clusteringDF[-1] %>% group_by(clusters) %>% 
  summarise_all("mean"))
print(clusteringDF[-1] %>% group_by(clusters) %>% 
        summarise_all("max"))
print(clusteringDF[-1] %>% group_by(clusters) %>% 
        summarise_all("min"))

# DATA ANALYSIS------------------------------------------------------------------------------------------
# What are the major crimes in 2014-2019?
# group crimes by MCI
MCI_group <- group_by(crimesDF, MCI)
MCI_count <- dplyr::summarise(MCI_group, n=n())
MCI_count <- MCI_count[order(MCI_count$n, decreasing = TRUE),]
names(MCI_count)[2] <- "no_of_occur"
MCI_count
# plot the result with ggplot
ggplot(MCI_count, aes(reorder(MCI, no_of_occur), no_of_occur)) + 
  geom_bar(stat = "identity", fill = "blue", data = MCI_count, width = 0.7) + 
  geom_text(aes(label = no_of_occur), stat = 'identity', data = MCI_count, hjust = -0.1, size = 3.5) +
  coord_flip() +
  xlab('Major Crimes') +
  ylab('Number of Occurrences') +
  ggtitle("Major Crime Toronto 2014-2019")
  


# Crime count by offense type
offence_group <- group_by(crimesDF, offence)
crime_by_offence <- dplyr::summarise(offence_group, n=n())
crime_by_offence <- crime_by_offence[order(crime_by_offence$n, decreasing = TRUE), ]
head(crime_by_offence)
ggplot(crime_by_offence, aes(reorder(offence, n), n)) + 
  geom_bar(stat = "identity", fill = "blue", data = crime_by_offence, width = 0.7) + 
  geom_text(aes(label = n), stat = 'identity', data = crime_by_offence, hjust = -0.1, size = 3.5) +
  coord_flip() +
  xlab('Major Crimes') +
  ylab('Number of Occurrences') +
  ggtitle("Offence Types in Toronto 2014-2019")



# Lets look at the frequency different types of breaking and entering 
brk_entr <- crimesDF[crimesDF$MCI == 'Break and Enter', ]
brk_entr_group <- group_by(brk_entr, offence)
brk_entr_by_offence <- dplyr::summarise(brk_entr_group, n=n())
brk_entr_by_offence <- brk_entr_by_offence[order(brk_entr_by_offence$n, decreasing = TRUE), ]
head(brk_entr_by_offence)
ggplot(crime_by_offence, aes(x = reorder(offence, n), y= n)) + 
  geom_bar(stat = "identity", fill = "blue", data = brk_entr_by_offence, width = 0.5) + 
  geom_text(aes(label = n), stat = 'identity', data = brk_entr_by_offence, hjust = -0.1, size = 3.5) +
  coord_flip() +
  xlab('Types of Breaking and Entering') +
  ylab('Number of Occurrences') +
  ggtitle('Breaking and Entering Crimes Toronto 2014 to 2019')



# Total crime count by time of the day?
hour_group <- group_by(crimesDF, occurrencehour)
crime_hour <- dplyr::summarise(hour_group, n=n())
ggplot(aes(x=occurrencehour, y=n), data = crime_hour) + geom_line(size = 2.5, alpha = 0.7, color = "blue", group=1) + 
  geom_point(size = 0.5) + 
  ggtitle('Total Crimes by Hour of Day in Toronto 2014 to 2019') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))

# Count of the occurrence of the different crimes by hour of the day
hour_crime_group <- group_by(crimesDF, occurrencehour, MCI)
hour_crime <- dplyr::summarise(hour_crime_group, n=n())
ggplot(aes(x=occurrencehour, y=n, color=MCI), data = hour_crime) + 
  geom_line(size=1.5) + 
  ggtitle('Crime Types by Hour of Day in Toronto 2014 to 2019') +
  ylab('Number of Occurrences') +
  xlab('Hour(24-hour clock)') +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))



# Where in Toronto were those crimes most likely to occur?

location_group <- group_by(crimesDF, Neighbourhood)
crime_by_location <- dplyr::summarise(location_group, n=n())
crime_by_location <- crime_by_location[order(crime_by_location$n, decreasing = TRUE), ]
# Visualize Top 20
crime_by_location_top20 <- head(crime_by_location, 20)
ggplot(aes(x = reorder(Neighbourhood, n), y = n), data = crime_by_location_top20) +
  geom_bar(stat = 'identity', fill = "blue", width = 0.6) +
  geom_text(aes(label = n), stat = 'identity', data = crime_by_location_top20, hjust = -0.1, size = 3) +
  coord_flip() +
  xlab('Neighbourhoods') +
  ylab('Number of Occurrences') +
  ggtitle('Neighbourhoods with Most Crimes - Top 20') +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))


# Top 10 safest neighborhoods in Toronto?
tail(crime_by_location, 10)


# Comparing neighbourhoods with the top offense types
offence_location_group <- group_by(crimesDF, Neighbourhood, offence)
offence_type_by_location <- dplyr::summarise(offence_location_group, n=n())
offence_type_by_location <- offence_type_by_location[order(offence_type_by_location$n, decreasing = TRUE), ]
offence_type_by_location_top20 <- head(offence_type_by_location, 20)
ggplot(aes(x = Neighbourhood, y=n, fill = offence), data=offence_type_by_location_top20) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  xlab('Neighbourhood') +
  ylab('Number of Occurrence') +
  ggtitle('Offence Type vs. Neighbourhood Toronto 2014 to 2019') + theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

# No of occurrence of these crimes by month in 2016 to 2019
monthly_crime_group <- group_by(crimesDF, occurrencemonth, MCI)
monthly_crime <- dplyr::summarise(monthly_crime_group, n=n())
names(monthly_crime)[3] <- "no_of_occur"
# Order the month
monthly_crime$occurrencemonth <- ordered(monthly_crime$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
# Visualize monthly crime rate
ggplot(aes(x=occurrencemonth, y=no_of_occur, group=MCI), data = monthly_crime) + 
  geom_line(aes(color=MCI), size=1.5) + 
  ggtitle('Crime Types by Month of Year in Toronto 2014-2019') +
  ylab('Number of Occurrences') +
  xlab('Month') +
  theme_bw() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))

# Crime count by day of the week
day_count <- crimesDF %>% group_by(occurrencedayofweek, MCI) %>% dplyr::summarise(Total = n())
ggplot(day_count, aes(occurrencedayofweek, MCI, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Major Crime Types by Day of Week 2014 to 2019") +
  xlab('Day of Week') +
  theme(plot.title = element_text(size = 16), 
        axis.title = element_text(size = 12, face = "bold"))


# Making a Map of Toronto Crimes-----------------------
lat <- crimesDF$Lat
lon <- crimesDF$Long
crimes <- crimesDF$MCI
to_map <- data.frame(crimes, lat, lon)
colnames(to_map) <- c('crimes', 'lat', 'lon')
sbbox <- make_bbox(lon = crimesDF$Long, lat = crimesDF$Lat, f = 0.01)
my_map <- get_map(location = sbbox, maptype = "satellite", scale = 10, color="bw", zoom = 10)


# Map showing the distribution of MCI across Toronto
ggmap(my_map) +
  geom_point(data=to_map, aes(x = lon, y = lat, color = "#27AE60"), 
             size = 0.5, alpha = 0.05) +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle('Crime Distribution in Toronto 2016-2019') +
  guides(color=FALSE) +
  facet_wrap(~ crimes, nrow = 2)



