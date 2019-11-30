# this script pulls TIGER/Line shapefiles for outlines of each census geography for the guide

packages <- c('tigris', 'ggplot2', 'sf', 'dplyr', 'svglite')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_class = 'sf')

# county ---------------
ramsey <- counties("MN", 2016)
ramsey <- ramsey %>% 
  filter(NAME == "Ramsey")

a <- ggplot(ramsey) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

ggsave(file = "svgs/county.svg", plot=a, width = 17, height = 22)

# blocks ---------------
blocks <- blocks("MN", "Ramsey", 2016)
# 8548 blocks

# saveRDS(blocks, 'data/blocks.RDS')

b <- ggplot(blocks) + 
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

ggsave(file = "svgs/blocks.svg", plot=b, width = 17, height = 22)

# block groups ---------
block_groups <- block_groups("MN", "Ramsey", 2016)
# 401

#saveRDS(block_groups, 'data/block_groups.RDS')

c <- ggplot(block_groups) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

ggsave(file = "svgs/block_groups.svg", plot=c, width = 17, height = 22)

# tracts ---------------
tracts <- tracts("MN", "Ramsey", 2016)
# 137

#saveRDS(tracts, 'data/tracts.RDS')

d <- ggplot(tracts) + 
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

ggsave(file = "svgs/tracts.svg", plot=d, width = 17, height = 22)

# zctas ----------------
zctas <- zctas()

r_zctas <- st_intersection(zctas, ramsey)
# 38

e <- ggplot(r_zctas) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

#saveRDS(r_zctas, 'data/r_zctas.RDS')

ggsave(file = "svgs/zcta.svg", plot=e, width = 17, height = 22)

# taz ------------------
taz <- st_read("data/tl_2011_27_taz10/tl_2011_27_taz10.shp")

r_taz <- st_intersection(taz, ramsey)

f <- ggplot(r_taz) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

#saveRDS(r_taz, 'data/r_taz.RDS')

ggsave(file = "svgs/taz.svg", plot=f, width = 17, height = 22)
