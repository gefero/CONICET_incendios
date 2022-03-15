library(tidyverse)
library(sf)

f <- function(x){1 / (1 + exp(-x))}
soft_sign_func <- function(x){ x / (1 + abs(x))  }

incendios <- read_csv('./data/raw/arg_focos_grilla.csv') %>%
                select(-c(A_2022:A_2025)) %>%
                rename(wkt = the_geom) %>%
                mutate(across(starts_with("A_"), ~replace_na(.x, 0))) %>%
                mutate(focos_media = focos / anos )

frontera_depto <- read_sf('../CONICET_estr_agr/proc_data/tablas_finales/frontera_depto.geojson')

incendios %>%
        select(-anos) %>%
        pivot_longer(starts_with('A'), names_to = 'ano') %>%
        mutate(ano = as.numeric(str_remove(ano, 'A_'))) %>%
        group_by(ano) %>%
        summarise(n = sum(value)) %>%
        ggplot() +
                geom_line(aes(x=ano, y=n), group=1) +
                theme_minimal()

incendios_geom <- incendios %>%
        select(-starts_with("A_")) %>%
        filter(focos > 0 ) %>%
        st_as_sf(wkt="wkt")

incendios_geom %>%
        ggplot() +
        geom_sf(aes(fill=(focos)), color=NA) +
        scale_fill_viridis_c() +
        theme_minimal()
        
incendios_geom %>%
        ggplot() +
        geom_sf(aes(fill=(focos_media)), colour=NA) +
        scale_fill_viridis_c() +
        theme_minimal()

incendios_geom %>%
        st_set_geometry(NULL) %>%
        summarise(mean = mean(focos),
                  sd = sd(focos),
                  cv = sd/mean,
                  q1 = quantile(focos, probs=0.25),
                  q2 = quantile(focos, probs=0.5),
                  q3 = quantile(focos, probs=0.75),
                  p70 = quantile(focos, probs=0.7),
                  p80 = quantile(focos, probs=0.80),
                  p90 = quantile(focos, probs=0.9),
                  p99 = quantile(focos, probs=0.99),
                  mad = mad(focos))




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
        
                