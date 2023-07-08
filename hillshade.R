# Packages ----
library(elevatr)
library(ggblend)
library(ggnewscale)
library(giscoR)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(units)
library(whitebox)

# Data ----
fin  <- gisco_get_countries(country = "Finland", resolution = "03")

plot(
    fin,
    max.plot = 1
)

## import the lakes boundaries ----
lakes  <- st_read("data/Jarvi10.shp")

## filter the largest ones ----
lakes <- lakes %>%
    mutate(
        areakm = set_units(pAla_ha, "hectare") %>% set_units("km2")
    ) %>% 
    arrange(desc(areakm)) %>%
    filter(valtio == "FI") %>%
    slice_head(n = 20)

plot(
    lakes,
    max.plot = 1
)

# Digital Elevation Model (DEM) ----

## get the DEM with ----
mdt  <- get_elev_raster(fin, z = 8)

## mdt_suiz ----
mdt

## plot(mdt_suiz) ----
plot(mdt)

## convert to terra and mask area of interest ----
mdt <- mdt %>%
    rast() %>%
    mask(vect(fin))

## reproject ----
mdt <- mdt %>% 
    project(crs(lakes))

## reproject vect ----
fin <- fin %>% 
    st_transform(st_crs(lakes))

fin

## convert the raster into a data.frame of xyz ----
mdtdf <- as.data.frame(mdt, xy = TRUE) %>% 
    as_tibble()

names(mdtdf)[3] <- "alt"

## map ----
p1 <- mdtdf %>%
    ggplot() +
    geom_raster(aes(x, y, fill = alt)) +
    geom_sf(
        data = lakes,
        fill = "#c6dbef",
        color = NA
    ) +
    scale_fill_hypso_tint_c(
        breaks = c(
            100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
        )
    ) +
    guides(
        fill = guide_colorsteps(
            barwidth       = .5,
            barheight      = 20,
            title          = "m",
            title.position = "top",
            direction      = "vertical"
        )
    ) +
    coord_sf() +
    theme_void()

p1

ggsave(
    "01_finland_DEM.png",
    p1,
    width  = 8.3,
    height = 11.7,
    unit   = "in",
    device = png,
    type   = "cairo",
    bg     = "white"
)


# Calculate the hillshade ----

## estimate the slope ----
sl <- terrain(mdt, "slope", unit = "radians")
plot(sl)


## estimate the aspect or orientation ----
asp <- terrain(mdt, "aspect", unit = "radians")
plot(asp)

## calculate the hillshade effect with 45º of elevation ----
hill_single <- shade(
    sl,
    asp,
    angle = 45,
    direction = 300,
    normalize= TRUE
)

## final hillshade ----
plot(hill_single, col = grey(1:100/100))


# Combine the relief and shadow effect ----

## convert the hillshade to xyz ----
hilldf_single <- as.data.frame(hill_single, xy = TRUE) %>% 
    as_tibble()

# map
p2 <- hilldf_single %>% 
    ggplot() +
    geom_raster(
        aes(x, y, fill = hillshade),
        show.legend = FALSE
    ) +
    scale_fill_distiller(palette = "Greys") +
    new_scale_fill() +
    geom_raster(
        data = mdtdf,
        aes(x, y, fill = alt),
        alpha = .7
    ) +
    scale_fill_hypso_tint_c(
        breaks = c(
            100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
        )
    ) +
    geom_sf(
        data = lakes,
        fill = "#c6dbef",
        color = NA
    ) +
    guides(
        fill = guide_colorsteps(
            barwidth       = .5,
            barheight      = 20,
            title          = "m",
            title.position = "top",
            direction      = "vertical"
        )
    ) +
    coord_sf() +
    theme_void()

p2

ggsave(
    "02_finland_relief_and_shadow.png",
    p2,
    width  = 8.3,
    height = 11.7,
    unit   = "in",
    device = png,
    type   = "cairo",
    bg     = "white"
)


# Multidirectional shadows ----

## pass multiple directions to shade() ----
hillmulti <- map(c(270, 15, 60, 330), function(dir){ 
    
    shade(
        sl,
        asp, 
        angle = 45, 
        direction = dir,
        normalize= TRUE
    )
})

## create a multidimensional raster and reduce it by summing up ----
hillmulti <- rast(hillmulti) %>%
    sum()

## multidirectional ----
plot(hillmulti, col = grey(1:100/100))

## unidirectional ----
plot(hill_single, col = grey(1:100/100))

## convert the hillshade to xyz
hillmultidf <- as.data.frame(hillmulti, xy = TRUE) %>%
    as_tibble()

# map
p3 <- ggplot() +
    geom_raster(
        data = hillmultidf,
        aes(x, y, fill = sum),
        show.legend = FALSE
    ) +
    scale_fill_distiller(palette = "Greys") +
    new_scale_fill() +
    geom_raster(
        data = mdtdf,
        aes(x, y, fill = alt),
        alpha = .7
    ) +
    scale_fill_hypso_tint_c(
        breaks = c(
            100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
        )
    ) +
    geom_sf(
        data = lakes,
        fill = "#c6dbef",
        color = NA
    ) +
    guides(
        fill = guide_colorsteps(
            barwidth       = .5,
            barheight      = 20,
            title          = "m",
            title.position = "top",
            direction      = "vertical"
        )
    ) +
    coord_sf() +
    theme_void()

p3

ggsave(
    "03_finland_multidirectional.png",
    p3,
    width  = 8.3,
    height = 11.7,
    unit   = "in",
    device = png,
    type   = "cairo",
    bg     = "white"
)


## map ----
p4 <- ggplot() +
    list(
        geom_raster(
            data = hillmultidf,
            aes(x, y, fill = sum),
            show.legend = FALSE
        ),
        scale_fill_distiller(palette = "Greys"),
        new_scale_fill(),
        geom_raster(
            data = mdtdf,
            aes(x, y, fill = alt),
            alpha = .7
        ),
        scale_fill_hypso_tint_c(
            breaks = c(
                100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
            )
        )
    ) %>% blend("multiply") +
    geom_sf(
        data  = lakes,
        fill  = "#c6dbef",
        color = NA
    ) +
    guides(
        fill = guide_colorsteps(
            barwidth       = .5,
            barheight      = 20,
            title          = "m",
            title.position = "top",
            direction      = "vertical"
        )
    ) +
    coord_sf() +
    theme_void()

ggsave(
    "04_finland_multidirectional_ggblend.png",
    p4,
    width  = 8.3,
    height = 11.7,
    unit   = "in",
    device = png,
    type   = "cairo",
    bg     = "white"
)


# Another alternative for multidirectional shadows ----

## instal whitebox ----
install_whitebox()

## export the DEM ----
writeRaster(mdt, "mdt.tiff", overwrite = TRUE)

## launch whitebox ----
wbt_init()

## create the hillshade ----
wbt_multidirectional_hillshade(
    "mdt.tiff",
    "hillshade.tiff"
)

## re-import the hillshade ----
hillwb <- rast("hillshade.tiff")
plot(hillwb)

## remask ----
hillwb <- mask(hillwb, vect(fin))
plot(hillwb)

# convert the hillshade to xyz ----
hillwbdf <- as.data.frame(hillwb, xy = TRUE) %>%
    as_tibble()

# map
p5 <- ggplot() +
    geom_raster(
        data = hillwbdf,
        aes(x, y, fill = hillshade),
        show.legend = FALSE
    ) +
    scale_fill_distiller(palette = "Greys") +
    new_scale_fill() +
    geom_raster(
        data = mdtdf,
        aes(x, y, fill = alt),
        alpha = .7
    ) +
    scale_fill_hypso_tint_c(
        breaks = c(
            100, 200, 300, 400, 500, 600, 700, 800, 900, 1000
        )
    ) +
    geom_sf(
        data  = lakes,
        fill  = "#c6dbef",
        color = NA
    ) +
    guides(
        fill = guide_colorsteps(
            barwidth       = .5,
            barheight      = 20,
            title          = "m",
            title.position = "top",
            direction      = "vertical"
        )
    ) +
    coord_sf() +
    theme_void()

p5

ggsave(
    "05_finland_multidirectional_whitebox.png",
    p5,
    width  = 8.3,
    height = 11.7,
    unit   = "in",
    device = png,
    type   = "cairo",
    bg     = "white"
)
