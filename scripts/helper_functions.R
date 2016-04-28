
# Attribute join function


# Using a join-attribute table function from the following link

#http://permalink.gmane.org/gmane.comp.lang.r.geo/20914#

join_attribute_table <- function(x, y, xcol, ycol) {
    # Merges data frame to SpatialPolygonsDataFrame, keeping the correct
    # order. Code from suggestions at:
    # https://stat.ethz.ch/pipermail/r-sig-geo/2008-January/003064.html
    # Args:
    #   x: SpatialPolygonsDataFrame
    #   y: Name of data.frame to merge
    #   xcol: Merge column name
    #   ycol: Merge column name
    # Returns: Shapefile with merged attribute table
    
    x$sort_id <- 1:nrow(as(x, "data.frame"))  # Column containing
    #original row order for later sorting
    
    x.dat <- as(x, "data.frame")  # Create new data.frame object
    x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol)  # Merge
    x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  # Reorder back to original
    x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  # Make new set of
    #polygons, dropping those which aren't in merge
    x2.dat <- as(x2, "data.frame")  # Make update x2 into a data.frame
    row.names(x.dat2.ord) <- row.names(x2.dat)  # Reassign row.names
    # from original data.frame
    x2@data <- x.dat2.ord  # Assign to shapefile the new data.frame
    return(x2)
}



# Calculate modal value ---------------------------------------------------


# Of course mode() is about the object class rather than something statistical. 

# Using code from:
#http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode

calc_mode <- function(x, na.rm = FALSE) {
    if(na.rm){
        x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}

