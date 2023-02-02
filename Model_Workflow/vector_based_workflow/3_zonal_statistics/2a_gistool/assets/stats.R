# Geospatial Dataset Processing Workflow
# Copyright (C) 2022, University of Saskatchewan
#
# This file is part of the Geospatial Dataset Processing Workflow
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License 
# along with this program.  If not, see <http://www.gnu.org/licenses/>. 

# reading arguments
args <- commandArgs();

# assigning variables to input arguments
temp_install_path <- args[6];
exactextractr_cache_path <- args[7]; 
renv_source_package <- args[8]; 
virtual_env_path <- args[9];
working_dir_path <- args[10];
lockfile_path <- args[11];
vrt_path <- args[12];
shapefile_path <- args[13];
output_path <- args[14];
stats <- args[15];
quantiles <- args[16];

# set the working directory path
setwd(working_dir_path)

# set cache and initialize the environment, i.e., `renv`
Sys.setenv("RENV_PATHS_CACHE"=exactextractr_cache_path);
Sys.getenv("R_LIBS_USER")
install.packages(renv_source_package, repos=NULL, type="source", quiet=TRUE);
renv::activate(virtual_env_path);
renv::restore(lockfile=lockfile_path, prompt=FALSE);

# load necessary libraries
library(dplyr, quietly=TRUE)

# produce necessary stats and print a csv file
if (tools::file_ext(vrt_path) == 'nc') {
  r <- terra::rast(vrt_path)
} else {
  r <- raster::raster(vrt_path);
}
p <- sf::st_read(shapefile_path, quiet=TRUE);

# check the CRS of the shapefile
if (is.na(sf::st_crs(p)$epsg)){
  sf::st_crs(p) = 4326;
  print('Assuming EPSG is 4326');
} else {
  sf::st_transform(p, 4326);
  print('Transforming EPSG to 4326');
}

# extract quantiles
q <- as.double(unlist(strsplit(quantiles, ",")));
# extract stats
s <- unlist(strsplit(stats, ","));
# check for coordinate values
coord_var <- 'coords'
if (coord_var %in% s) {
  s <- s[!s %in% c(coord_var)]
  include_coords = TRUE
} else {
  include_coords = FALSE
}
# extract ID column name
id_col <- names(p[1])[1]

# run exactextractr and calculate necessary stats
df <- exactextractr::exact_extract(r, p, s, quantiles=q, append_cols=id_col); # assuming first column indicates ID

# extract centroid coordinates and prepend to `df`
if (include_coords == TRUE) {
  # mutate the `p` sf object to include the coordinates
  p3 <- p %>%
    mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
           lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  # extract the `lat` and `lon` columns
  coords <- p3 %>% select(all_of(c(id_col, 'lat', 'lon'))) %>% sf::st_drop_geometry() %>% as.data.frame()
  # merge `df` with `coords`
  df <- merge(coords, df, by=id_col)
}

# print the final .csv file
write.csv(df, output_path, row.names=FALSE, quote=FALSE)

