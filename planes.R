library(geosphere)
library(ggplot2)
library(data.table)
library(magrittr)
library(rworldmap)

a <- 6371000
lonlat2xy <- function(lon, lat) {
   A <- pi/2*a*(1 - lat/90)
   rot <- 0
   x <- cos((lon + rot)*pi/180)*A
   y <- sin((lon + rot)*pi/180)*A
   return(list(x = x, y = y))
}

xy2lonlat <- function(x, y) {
   A <- sqrt(x^2 + y^2)
   lat <- -90*(A*2/(a*pi) - 1)
   lon <- ConvertLongitude(180/pi*atan2(y, x), from = 180)
   return(list(lon = lon, lat = lat))
}


# Mapa
BuildMap <- function(res = 1, smooth = 1, pm = 180,
                     countries = FALSE, ...) {
   # Caraga datos de mapas y cambia el meridiano principal, suaviza y cambia
   # resolución para plotear más rápido.
   # Entra:
   #   res: resolución (cuantos puntos se eliminan entre los que quedan)
   #   smooth: factor de suavizado (ventana del promedio corrido)
   #   pm: longitud del meridiano central
   #   countries: ¿datos a nivel país?
   # Sale:
   #   un data.table con las coordenadas de cada polígono y su grupo
   

   data(coastsCoarse)
   m <- coastsCoarse
   m <- as.data.table(fortify(m))
   m[, group := as.numeric(group)]
   
   # Cierro la ant
   ant.extra <- data.table(group = 99, id = 98, piece = 1, 
                           long = seq(180, -180, length.out = 100),
                           lat = -90)
   ant.extra[, order := .I + m[group == 99, max(order)]]
   m <- rbind(m, ant.extra)
   
   # Cambio el prime meridian.
   m2 <- copy(m)
   m2[, long := long + 360]
   mg <- m[, max(group)]
   m2[, group := group + max(group) + 1]
   m <- rbind(m, m2)
   m <- m[long >= pm - 180 & long <= 180 + pm]
   
   m[, MO := max(order), by = group]
   
   # Suavizo.
   if (smooth > 1) {
      cut <- max(smooth, res)
      notsmooth <- m[, .N, by = group][N < cut + 4, group]
      m <- m[!(group %in% notsmooth)]    # saco los grupos muy chicos
      
      m[order != 1 & order != MO,
        `:=`(long = zoo::rollmean(long, smooth, fill = "extend"),
             lat = zoo::rollmean(lat, smooth, fill = "extend")),
        by = group]
   }
   # Bajo la resolución.
   suppressWarnings(m[, keep := c(1, rep(0, res - 1)), by = group])
   m <- m[order == 1 | order == MO | keep == 1, .SD, by = group]
   
   return(m)
}

ConvertLongitude <- function(lon, from = c(360, 180)) {
   # Pasa la longitud entre convenciones.
   # Entra:
   #   lon: un vector de longitudes
   #   from: la convención desde la cual se convierte
   #   (360 = 0:360, 180 = -180:180)
   # Sale:
   #   un vector con la longitud convertida a la otra convención.
   # Ojo que no hay ningún chequeo de los argumentos. Si se pasa un vector
   # en convención 0:360 y se le dice que está en -180:180, lo "convierte"
   # igual y tira cualquier batata.
   if (from[1] == 360) {
      lon <- ifelse(lon <= 180, lon, lon - 360)
   } else if (from[1] == 180) {
      lon <- ifelse(lon < 0, lon + 360, lon)
   }
   return(lon)
}

w <- setDT(map_data("world2"))
setnames(w, "long", "lon")
ant.extra <- data.table(group = 121, subregion = NA, region = "Antarctica",
                        lat = w[region == "Antarctica" & is.na(subregion), min(lat)],
                        lon = seq(360, 0, length = 100))
ant.extra[, order := .I + 6943]
w <- w[order < 6946 | order > 6955] %>%
   .[order > 6955, order := order + nrow(ant.extra)]
w <- rbind(w, ant.extra)[order(order)]
# 
# w <- BuildMap(res = 3)
# setnames(w, "long", "lon")

w[, c("x", "y") := lonlat2xy(lon, lat)]
# w[, c("long", "lati") := xy2lonlat(x, y)]

base <- list(geom_polygon(data = w, aes(group = group), size = 0.2, 
                          color = "gray40",
                          fill = "gray80"),
             theme_void())

geomap <- function(start, end) {
   X0 <- lonlat2xy(start[1], start[2])
   Xf <- lonlat2xy(end[1], end[2])
   
   esphere <- as.data.table(gcIntermediate(start, end))
   esphere[, lon := ConvertLongitude(lon, from = 180)]
   esphere[, c("x", "y") := lonlat2xy(lon, lat)]
   
   flat <- as.data.table(approx(c(X0$x, Xf$x), c(X0$y, Xf$y), n = 100))
   flat[, c("lon", "lat") := xy2lonlat(x, y)]
   
   paths <- list(geom_path(data = esphere, 
                           color = "red", size = 1), 
                 geom_line(data = flat,
                           color = "blue", size = 1) 
   )
   dist <- distance(start, end)
   
   # dist <- paste0("Distancia en esfera = ", round(dist[1]/1000, 0), "km", 
                  # "    Distancia en disco = ", round(dist[2]/1000, 0), "km")
   d <- pi*a
   b <- 1.07
   ggplot(mapping = aes(x, y)) + base + paths + 
      # labs(caption = dist) +
      annotate("text", x = 0, y = d*b, label = paste0("Distancia en esfera = ", round(dist[1]/1000), "km"),
               hjust = 0.5, color = "red", size = 7) +
      annotate("text", x = 0, y = -d*b, label = paste0("Distancia en disco = ", round(dist[2]/1000), "km"),
               hjust = 0.5, color = "blue", size = 7) +
      annotate("point", x = X0$x, y = X0$y, size = 3) +
      annotate("point", x = Xf$x, y = Xf$y, size = 3) +
      coord_equal() +
      theme(plot.caption = element_text(hjust = 0.5, size = 14))
}
# ggplot(mapping = aes(lon, lat)) + base + paths + 
#    coord_map("perspective", parameters = c(dist = 3),
#              orientation = c(-60, -130, 10)) +
#    theme(panel.grid = element_line())

distance <- function(start, end) {
   sphere <- geosphere::alongTrackDistance(start, end, end)[1]

   start <- unlist(lonlat2xy(start[1], start[2]))
   end <- unlist(lonlat2xy(end[1], end[2]))
   d <- start - end
   flat <- sqrt(sum(d^2))
   return(c(sphere = sphere, flat = flat))
}

