### Step 2. Exploring and preparing the data
#### loading the data
utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]



# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df,
          method = "euclidean")


#### preprocessing the data
# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df)

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[, c(6,8)],
               method = "euclidean")


### Step 3. Performing clustering
# performing clustering and plotting the dendrogram
# compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm,
               method = "euclidean")


# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm,
              method = "single")
plot(hc1,
     hang = -1,
     ann = FALSE)

hc2 <- hclust(d.norm,
              method = "average")
plot(hc2,
     hang = -1,
     ann = FALSE)

hc3 <- hclust(d.norm,
              method = "complete")
plot(hc3,
     hang = -1,
     ann = FALSE)

hc4 <- hclust(d.norm,
              method = "median")
plot(hc4,
     hang = -1,
     ann = FALSE)



#### membership assignment through cutting the dendrogram
memb <- cutree(hc1, k = 6)
memb

memb <- cutree(hc2, k = 6)
memb

rect.hclust(hc2, k=6)

#### Step.4 Prducing heatmap
# set labels as cluster membership and utility name
row.names(utilities.df.norm) <- paste(memb, ": ",
                                      row.names(utilities.df),
                                      sep = "")



# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm),
        Colv = NA,
        hclustfun = hclust,
        col = rev(paste("gray", 1:99, sep="")))
