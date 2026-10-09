## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Shared Utility Functions         ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Load all utility functions from the ReactiveAtlantis package
##' This ensures modules have access to all helper functions

##' NCtime calculation
##' @param ncfile Netcdf file
##' @return Vector of dates
time_calc <- function(ncfile){
  orign <- unlist(strsplit(ncdf4::ncatt_get(ncfile, 't')$units, ' ', fixed = TRUE))
  if(orign[1] == 'seconds') {
    Time <- ncdf4::ncvar_get(ncfile, 't') / 86400
  } else {
    Time <- ncdf4::ncvar_get(ncfile, 't')
  }
  Time <- as.Date(Time, origin = orign[3])
  return(Time)
}

##' Box information
##' @param bgm.file BGM file, Atlantis input
##' @param depths Cumulative depths
##' @return Dataframe with box information
boxes.prop <- function(bgm.file, depths){
  bgm       <- readLines(bgm.file, warn = FALSE)
  vert      <- text2num(bgm, 'bnd_vert ', Vector = TRUE, lineal = TRUE)
  centroids <- text2num(bgm, '.inside', Vector = TRUE, lineal = TRUE)
  dynamic   <- as.numeric(apply(centroids, 1, function(x) .point_in_polygon(vert, x)))
  boxes     <- text2num(bgm, 'nbox', FG = 'look')
  out       <- NULL
  
  if(all(depths[1:2] == 0)){
    depths <- depths[-1]
  } else {
    depths <- depths[-which(depths == 0)]
  }
  
  max.nlyrs <- length(depths)
  vol       <- array(NA, dim = c(boxes$Value, length(depths)))
  
  for(b in 1:boxes$Value){
    area        <- text2num(bgm, paste0('box', b - 1,'.area'), FG = 'look')
    area$Value  <- area$Value * dynamic[b]
    z           <- text2num(bgm, paste0('box', b - 1,'.botz'), FG = 'look')
    box.lyrs    <- sum(depths < -z$Value)
    box.lyrs    <- pmin(box.lyrs, max.nlyrs)
    out         <- rbind(out, data.frame(Boxid = b - 1, Area = area$Value, 
                                        Volumen = area$Value * -z$Value,
                                        Depth = -z$Value, Layers = box.lyrs))
    vol[b, 1:box.lyrs] <- area$Value * depths[1:box.lyrs]
  }
  
  vol                <- cbind(out$Area, vol)
  vol                <- t(vol[, ncol(vol):1])
  vol[1, ]           <- 0
  vol2               <- vol
  vol2[nrow(vol2), ] <- 0
  
  out[c(1, which(out$Depth <= 0)), 2:ncol(out)] <- 0
  out$dynamic      <- dynamic
  out$Depth_Bound  <- out$Depth * out$dynamic
  
  out <- list(info = out, Vol = vol2, VolInf = vol)
  return(out)
}

##' Point in polygon test
##' @param polygon Array representation of the polygon
##' @param point Array representation of the point
##' @return Boolean indicating if point is in polygon
.point_in_polygon <- function(polygon, point){
  odd = FALSE
  i = 0
  j = nrow(polygon) - 1
  
  while(i < nrow(polygon) - 1){
    i = i + 1
    if (((polygon[i,2] > point[2]) != (polygon[j,2] > point[2]))
        && (point[1] < ((polygon[j,1] - polygon[i,1]) * (point[2] - polygon[i,2]) / 
                        (polygon[j,2] - polygon[i,2])) + polygon[i,1])){
      odd = !odd
    }
    j = i
  }
  return(odd)
}

##' Parameter file reader
##' @param text Biological parameter file for Atlantis
##' @param pattern Text pattern to search for
##' @param FG Name of functional groups
##' @param Vector Logical, if data is in vectors
##' @param pprey Logical, if data is pprey matrix
##' @param lineal Logical, if data is linear vector
##' @return Matrix with values from .prm file
text2num <- function(text, pattern, FG = NULL, Vector = FALSE, pprey = FALSE, lineal = FALSE){
  if(!isTRUE(Vector)){
    text <- text[grep(pattern = pattern, text)]
    if(length(text) == 0) warning(paste0('\n\nThere is no ', pattern, ' parameter in your file.'))
    txt  <- gsub(pattern = '[[:space:]]+', '|', text)
    col1 <- col2 <- vector()
    
    for(i in 1:length(txt)){
      tmp <- unlist(strsplit(txt[i], split = '|', fixed = TRUE))
      if(grepl('#', tmp[1])) next
      tmp2 <- unlist(strsplit(tmp[1], split = '_'))
      
      if(FG[1] == 'look') {
        col1[i] <- tmp2[1]
      } else {
        id.co <- which(tmp2 %in% FG)
        if(sum(id.co) == 0) next
        col1[i] <- tmp2[id.co]
      }
      col2[i] <- as.numeric(tmp[2])
    }
    
    if(is.null(FG)) col1 <- rep('FG', length(col2))
    out.t <- data.frame(FG = col1, Value = col2)
    if(any(is.na(out.t[, 1]))){
      out.t <- out.t[-which(is.na(out.t[, 1])), ]
    }
    return(out.t)
  } else {
    l.pat <- grep(pattern = pattern, text)
    nam   <- gsub(pattern = '[[:space:]]+', '|', text[l.pat])
    fg    <- vector()
    pos   <- 1
    
    for(i in 1:length(nam)){
      tmp <- unlist(strsplit(nam[i], split = '|', fixed = TRUE))
      if(grepl('#', tmp[1]) || (!grepl('^pPREY', tmp[1]) && pprey == TRUE)) next
      fg[pos] <- tmp[1]
      
      if(isTRUE(lineal)){
        t.text <- gsub('+[[:space:]]+', ' ', text[l.pat[i]])
      } else {
        t.text <- gsub('+[[:space:]]+', ' ', text[l.pat[i] + 1])
      }
      
      oldw <- getOption("warn")
      options(warn = -1)
      pp.tmp <- matrix(as.numeric(unlist(strsplit(t.text, split = ' +', fixed = FALSE))), nrow = 1)
      options(warn = oldw)
      
      if(pos == 1) {
        pp.mat <- pp.tmp
      } else {
        if(ncol(pp.mat) != ncol(pp.tmp)) {
          stop('\nError: The pPrey vector for ', tmp[1], ' has ', ncol(pp.tmp), 
               ' columns and should have ', ncol(pp.mat))
        }
        pp.mat <- rbind(pp.mat, pp.tmp)
      }
      pos <- pos + 1
    }
    
    if(all(is.na(pp.mat[, 1]))) pp.mat <- pp.mat[, -1]
    row.names(pp.mat) <- fg
    return(pp.mat)
  }
}

##' Infer cumulative water-column depths from a NetCDF
##' @param nc Open ncdf4 object
##' @return Numeric vector starting with 0, or NULL if depths cannot be inferred
infer_cum_depths <- function(nc){
  if(!('nominal_dz' %in% names(nc$var))) return(NULL)
  ndz <- ncdf4::ncvar_get(nc, 'nominal_dz')
  if(is.null(dim(ndz)) || length(dim(ndz)) != 2) return(NULL)
  z_len <- if(!is.null(nc$dim$z)) nc$dim$z$len else NA
  if(is.finite(z_len)){
    if(ncol(ndz) == z_len && nrow(ndz) != z_len) ndz <- t(ndz)
  } else if(nrow(ndz) > ncol(ndz)){
    ndz <- t(ndz)
  }
  n_z <- nrow(ndz)
  if(n_z < 2) return(NULL)
  water <- ndz[seq_len(n_z - 1), , drop = FALSE]
  box_score <- apply(water, 2, function(x){
    x[!is.finite(x)] <- 0
    sum(x > 0) * sum(x, na.rm = TRUE)
  })
  deep <- which.max(box_score)
  thick <- water[, deep]
  thick <- thick[is.finite(thick) & thick > 0]
  if(length(thick) == 0) return(NULL)
  c(0, cumsum(rev(thick)))
}

##' Count water layers implied by a cumulative depth vector
##' @param depths Numeric cumulative depths
##' @return Integer count of water layers
n_water_layers <- function(depths){
  depths <- depths[is.finite(depths)]
  if(length(depths) >= 2 && depths[1] == 0 && depths[2] == 0){
    depths <- depths[-1]
  } else if(any(depths == 0)){
    depths <- depths[depths != 0]
  }
  length(depths)
}
