convert_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- proj4string(shp)
  
  gIntersection(shp, b_poly, byid = T)
}

change_names <- function(df, from, to, reminder = TRUE) {
  
  ## error checking
  # check if from exist in df
  
  if (!all(from %in% colnames(df))) {
    stop ("undefined column names selected")
  }
  
  # check if the length of the mapping match
  len.from <- length(from)
  len.to <- length(to)
  
  if (len.from != len.to) {
    stop("argument imply differing length of vectors:", len.from, ",", len.to)
  }
  
  org.from <- from
  org.to <- to
  
  ## process to modify the specified names
  field.names <- names(df)
  field.names <- field.names[!(field.names %in% from)]
  
  to <- c(to, field.names)
  from <- c(from, field.names)
  
  names(to) <- from
  names(df) <- to[names(df)]
  
  if (reminder) {
    cat("the column name(s) have been modified:\n")
    indicator <- paste(org.from, " to ", org.to, "\n", sep = "")
    indicator[1] <- paste(" ", indicator[1], sep = "")
    cat(indicator)
  }  
  
  return(df)
  
}