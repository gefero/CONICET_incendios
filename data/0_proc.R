library(tidyverse)
library(sf)


f <- function(x){1 / (1 + exp(-x))}
soft_sign_func <- function(x){ x / (1 + abs(x))  }

incendios <- read_csv('./data/raw/arg_focos_grilla.csv') %>%
                rename(wkt = the_geom) %>%
                mutate(across(starts_with("A_"), ~replace_na(.x, 0))) %>%
                mutate(focos_media = focos / anos )
                
incendios <- incendios %>%
        select(-starts_with("A_")) %>%
        filter(focos > 0 ) %>%
        st_as_sf(wkt="wkt")

incendios %>%
        ggplot() +
        geom_sf(aes(fill=soft_sign_func(focos)), colour=NA) +
        scale_fill_viridis_c() +
        theme_minimal()
        
incendios %>%
        ggplot() +
        geom_sf(aes(fill=(focos_media)), colour=NA) +
        scale_fill_viridis_c() +
        theme_minimal()


frontera_depto <- read_sf('../CONICET_estr_agr/proc_data/tablas_finales/frontera_depto.geojson')
st_crs(incendios_centr)

incendios_centr <- incendios %>%
        st_centroid() %>%
        st_set_crs(4326)

ggplot() +
        geom_sf(data=incendios_centr, aes(color=soft_sign_func(focos))) +
        scale_color_viridis_c() +
        geom_sf(data=frontera_depto, fill=NA, color='white') +
        theme_minimal()


incendios_centr <- incendios_centr %>%
        st_join(frontera_depto %>%
                        select(link), join=st_intersects)

frontera_depto %>%
        left_join(
                incendios_centr %>%
                st_set_geometry(NULL) %>%
                group_by(link) %>%
                summarise(focos = sum(focos))
        )
        
                