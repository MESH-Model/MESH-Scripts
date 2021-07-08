
create_mask <- function(nrow, ncol, res, xmin, ymin) {
    
    # nrow  <- 64  
    # ncol  <- 81
    # res   <- 0.125
    # 
    # xmin  <- -128.125
    # ymin  <- 48.5
    # outdir <- "D:/Fraser/Basin_Boundary/Basin_outline/FRB_outline_mask" (optional) 
      
    xmax  <- xmin + ncol * res
    ymax  <- ymin + nrow * res
    ###  construct a mask for a region of interest----------------
    mask           <- raster(matrix(1, nrow, ncol, byrow = T))
    extent(mask)   <-c(xmin, xmax, ymin, ymax)
    res(mask)      <-c(res, res)
    dim(mask)      <-c(nrow , ncol)
    crs(mask)      <-CRS("+proj=longlat +ellps=GRS80 +datum=NAD83
                         +no_defs+ towgs84=0,0,0")
    
    # display
    plot(mask)
    
    # save as a geotiff 
    # if required, the mask can be saved 
    #writeRaster(mask, outdir,"GTiff", overwrite = TRUE)
    
    return(mask)
}