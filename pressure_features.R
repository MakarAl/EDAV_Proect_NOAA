#read nc file
#read floods table
load("floods.RData")
load("pressure.RData")

#leave meaningful columns, remove degenerate cases
floods <- floods[(floods$began > as.Date("1985-01-01")) & (floods$centroid.y >= 35) & (floods$centroid.y <= 70),
                 c(1,4:27)]
floods <- floods[is.na()]

get_features <- function(x, y, began, ended, interval_length, unit_length) {
    
    index.x1 <- ifelse(floor((x+180)/2.5) + 1 - unit_length > 0,
                       floor((x+180)/2.5) + 1 - unit_length,
                       1)
    index.x2 <- ceiling((x+180)/2.5) + 1 + unit_length
    
    index.y1 <- ifelse(28-floor((y)/2.5) - unit_length > 0, 
                       28-floor((y)/2.5) - unit_length,
                       1)
    index.y2 <- ifelse(28-floor((y)/2.5)+1 + unit_length < 16,
                       28-floor((y)/2.5)+1 + unit_length,
                       15)
    
    #convert floods$began date to phi index
    if (interval_length == 0) {
        index.start <- as.numeric(began - as.Date("1985-01-01"))
        index.end <- as.numeric(ended - as.Date("1985-01-01"))
    } else {
        index.start <- ifelse(as.numeric(began - as.Date("1985-01-01")) - interval_length > 0,
                              as.numeric(began - as.Date("1985-01-01")) - interval_length,
                              1)
        index.end <- as.numeric(began - as.Date("1985-01-01"))   
    }
    
     avg <- mean(phi[index.x1:index.x2, index.y1:index.y2, index.start:index.end])
     std <- sd(phi[index.x1:index.x2, index.y1:index.y2, index.start:index.end])
     return(c(avg, std))
}

#for each flood extract the presure that was several days before within a certain range

floods$avg.int1.1 <- NA
floods$std.int1.1 <- NA
floods$avg.int1.2 <- NA
floods$std.int1.2 <- NA
floods$avg.int2.1 <- NA
floods$std.int2.1 <- NA
floods$avg.int2.2 <- NA
floods$std.int2.2 <- NA
floods$avg.int3.1 <- NA
floods$std.int3.1 <- NA
floods$avg.int3.2 <- NA
floods$std.int3.2 <- NA
floods$avg.int4.1 <- NA
floods$std.int4.1 <- NA
floods$avg.int4.2 <- NA
floods$std.int4.2 <- NA

for (i in 1:nrow(floods)) {
    if (!(is.na(floods$centroid.x[i]))) {
        int1.1 <- with(floods, 
                        get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 0, 0))
        floods$avg.int1.1[i] <- int1.1[1]
        floods$std.int1.1[i] <- int1.1[2]
        
        int1.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 0, 1))
        floods$avg.int1.2[i] <- int1.2[1]
        floods$std.int1.2[i] <- int1.2[2]
        
        int2.1 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 5, 0))
        floods$avg.int2.1[i] <- int2.1[1]
        floods$std.int2.1[i] <- int2.1[2]
        
        int2.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 5, 1))
        floods$avg.int2.2[i] <- int2.2[1]
        floods$std.int2.2[i] <- int2.2[2]
        
        int3.1 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 10, 0))
        floods$avg.int3.1[i] <- int3.1[1]
        floods$std.int3.1[i] <- int3.1[2]
        
        int3.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 10, 1))
        floods$avg.int3.2[i] <- int3.2[1]
        floods$std.int3.2[i] <- int3.2[2]
        
        int4.1 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 15, 0))
        floods$avg.int4.1[i] <- int4.1[1]
        floods$std.int4.1[i] <- int4.1[2]
        
        int4.2 <- with(floods,
                       get_features(centroid.x[i], centroid.y[i], began[i], ended[i], 15, 1))
        floods$avg.int4.2[i] <- int4.2[1]
        floods$std.int4.2[i] <- int4.2[2]
    }
}
