# this script pulls TIGER/Line shapefiles for outlines of each census geography for the guide

packages <- c('tigris', 'ggplot2', 'sf', 'dplyr', 'svglite')

miss_pkgs <- packages[!packages %in% installed.packages()[,1]]

if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

invisible(lapply(packages, library, character.only = TRUE))

rm(miss_pkgs, packages)

options(tigris_class = 'sf')

## "zoom in" location: roots roasting
roots <- st_as_sf(data.frame(lat = 44.934075, lon = -93.166042), coords = c('lon', 'lat'))
st_crs(roots) <- 4269 


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

rootsb <- st_intersection(blocks, roots)

# block containing roots
roots_b <- blocks %>% dplyr::filter(GEOID10 == rootsb$GEOID10)

b1 <- ggplot(roots_b) +
  geom_sf(fill = "white", lwd = 0.5) +
  geom_sf(data = roots) +
  theme_void()

ggsave(file = "svgs/roots_blocks.svg", plot=b1, width = 17, height = 22)

# block groups ---------
block_groups <- block_groups("MN", "Ramsey", 2016)
# 401

#saveRDS(block_groups, 'data/block_groups.RDS')

c <- ggplot(block_groups) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

ggsave(file = "svgs/block_groups.svg", plot=c, width = 17, height = 22)

rootsbg <- st_intersection(block_groups, roots)
roots_bg <- block_groups %>% dplyr::filter(GEOID == rootsbg$GEOID)
# blocks in bg
roots_bs1 <- st_intersection(blocks, roots_bg)

b2 <- ggplot(roots_bg) +
  geom_sf(fill = "transparent", lwd = 0.5) +
  geom_sf(data = roots_bs1, fill = "transparent", lwd = 0.25) +
  geom_sf(data = roots) +
  theme_void()

ggsave(file = "svgs/roots_block_group.svg", plot=b2, width = 17, height = 22)

# tracts ---------------
tracts <- tracts("MN", "Ramsey", 2016)
# 137

#saveRDS(tracts, 'data/tracts.RDS')

d <- ggplot(tracts) + 
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

ggsave(file = "svgs/tracts.svg", plot=d, width = 17, height = 22)

rootst <- st_intersection(tracts, roots)
roots_t <- tracts %>% dplyr::filter(GEOID == rootst$GEOID)

# block groups in tract
roots_bg1 <- st_intersection(block_groups, roots_t)

# block groups in tract
roots_bs2 <- st_intersection(blocks, roots_t)

b3 <- ggplot(roots_t) +
  geom_sf(fill = "transparent", lwd = 0.5) +
  geom_sf(data = roots_bg1, fill = "transparent", lwd = 0.25) +
  geom_sf(data = roots_bs2, fill = "transparent", lwd = 0.125) +
  geom_sf(data = roots) +
  theme_void()

ggsave(file = "svgs/roots_tract.svg", plot=b3, width = 17, height = 22)

# zctas ----------------
zctas <- zctas()

r_zctas <- st_intersection(zctas, ramsey)
# 38

e <- ggplot(r_zctas) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

#saveRDS(r_zctas, 'data/r_zctas.RDS')

ggsave(file = "svgs/zcta.svg", plot=e, width = 17, height = 22)

rootsz <- st_intersection(zctas, roots)
roots_z <- zctas %>% dplyr::filter(GEOID10 == rootsz$GEOID10)

# tracts in zcta
roots_t1 <- st_intersection(tracts, roots_z)

# block groups in tract
roots_bg2 <- st_intersection(block_groups, roots_z)

# block groups in tract
roots_bs3 <- st_intersection(blocks, roots_z)

b4 <- ggplot(roots_z) +
  geom_sf(fill = "transparent", lwd = 0.5) +
  geom_sf(data = roots_t1, fill = "transparent", lwd = 0.25) +
  geom_sf(data = roots_bg2, fill = "transparent", lwd = 0.125) +
  geom_sf(data = roots_bs3, fill = "transparent", lwd = 0.0625) +
  geom_sf(data = roots) +
  theme_void()

ggsave(file = "svgs/roots_zcta.svg", plot=b4, width = 17, height = 22)

# taz ------------------
taz <- st_read("data/tl_2011_27_taz10/tl_2011_27_taz10.shp")

r_taz <- st_intersection(taz, ramsey)

f <- ggplot(r_taz) +
  geom_sf(fill = "white", lwd = 0.5) +
  theme_void()

#saveRDS(r_taz, 'data/r_taz.RDS')

ggsave(file = "svgs/taz.svg", plot=f, width = 17, height = 22)

rootstz <- st_intersection(taz, roots)
roots_tz <- taz %>% dplyr::filter(GEOID10 == rootstz$GEOID10)

# tracts in taz
roots_t2 <- st_intersection(tracts, roots_tz)

# block groups in taz
roots_bg3 <- st_intersection(block_groups, roots_tz)

# block groups in tract
roots_bs4 <- st_intersection(blocks, roots_tz)

b5 <- ggplot(roots_tz) +
  geom_sf(fill = "transparent", lwd = 0.5) +
  geom_sf(data = roots_t2, fill = "transparent", lwd = 0.25) +
  geom_sf(data = roots_bg3, fill = "transparent", lwd = 0.125) +
  geom_sf(data = roots_bs4, fill = "transparent", lwd = 0.0625) +
  geom_sf(data = roots) +
  theme_void()

ggsave(file = "svgs/roots_taz.svg", plot=b5, width = 17, height = 22)
