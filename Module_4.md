Assignment 6
================
Erik Strand
12.6.2021

``` r
knitr::opts_chunk$set(
  error = FALSE,
  message = FALSE,
  warning = FALSE, 
  cache = TRUE
)
```

``` r
library(tidyverse)
library(sf)
library(lubridate)
library(raster)
library(tmap)
library(glmnet)
library(useful)
library(coefplot)
library(caret)
```

``` r
typedcols <- c( "A11", "F9", "F10", "F7", "X1","A2",
                "X1","A30", "X1", "A3", "X1", "A3", "X1", "A5" )

stns <- read.fortran("../data/GHCND/ghcnd-stations.txt", 
                     typedcols, comment.char="") %>% 
        tibble()
```

``` r
countries <- 
  read_sf("../Data/Nations")

usa <- 
  countries %>% 
  filter(ADMIN == "United States of America") %>% 
  st_transform(crs = 4326) %>% 
  st_crop(xmin = -130, xmax = -60, ymin = 18, ymax = 50)
```

``` r
koppen <- 
  st_read("../data/WC05_1975H_Koppen_Shapefile") %>% 
  st_make_valid()
```

    ## Reading layer `WC05_1975H_Koppen' from data source 
    ##   `/Users/erikstrand/Documents/EDA/Module 4/Data/WC05_1975H_Koppen_Shapefile' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 25174 features and 2 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -180 ymin: -59.5 xmax: 180 ymax: 83.66667
    ## Geodetic CRS:  WGS 84 + Unknown VCS from ArcInfo Workstation

``` r
koppen_usa <- 
  koppen %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  st_intersection(usa)

koppen_dfa <- 
  koppen_usa %>% 
  filter(Koppen == "Dfa")

ggplot(koppen_usa) + 
  geom_sf(data = usa) + 
  geom_sf(aes(color = Koppen)) + 
  theme_minimal()
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(koppen_dfa) + 
  geom_sf(data = usa) + 
  geom_sf(aes(color = Koppen)) + 
  theme_minimal()
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
hdrs <- c("ID", "LAT", "LON", "ELEV", "ST", "NAME","GSN", "HCN", "WMOID")
names(stns) <- hdrs
stnsFilter <- stns %>%
    filter(str_detect(ID, "^US"), !ST %in% c("AK", "HI", "PI", "UM"))

invcols <- c( "A11", "X1", "F8", "X1", "F9", "X1","A4",
              "X1","I4", "X1", "I4" )
inv <- read.fortran("../data/GHCND/ghcnd-inventory.txt",
                    invcols, comment.char="") %>% 
    tibble()
```

``` r
invhdrs <- c("ID", "LAT", "LON", "ELEM" , "FIRST", "LAST")
names(inv) <- invhdrs

invFilter <- inv %>% 
    filter(
      str_detect(ID,"^US"), str_detect(ELEM,"TMAX"), FIRST < 2016, LAST > 2016
    )
```

``` r
joinDF <- 
  inner_join(stnsFilter, invFilter) %>% 
  mutate(
    fileNameDLY = paste0(ID, ".dly"),
    fileNameCSV = paste0(ID,".csv")
  ) %>% 
  st_as_sf(coords = c("LON", "LAT")) %>% 
  st_set_crs(4326) %>% 
  st_intersection(koppen_dfa)

joinDF %>% 
  ggplot() + 
  geom_sf(data = usa) + 
  geom_sf(data = koppen_dfa, aes(color = Koppen)) + 
  geom_sf(color = "darkblue", alpha = 0.5) + 
  labs(
    title = "US Temperature Stations within Koppen Dfa Classification Zones"
  ) + 
  theme_minimal()
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
cols <- c( "A11", "I4", "I2", "A4",
           rep( c( "I5", "A1", "A1", "A1"), 31) )
tmp <- c("Val","xxM","xxQ","xxS")
vhdrs <- paste(   rep(tmp,31),   rep(1:31,each=4), sep="")
hdrs <- c("ID", "year", "month", "element", vhdrs)

avgTmpDiffFunc <- function(fileName){
  df <- 
    suppressMessages(
      suppressWarnings(
        read_csv(paste0("../Data/GHCND/ghcnd-all/", fileName))
      )
    )
  if("TMIN" %in% colnames(df)){ 
    avgTempDiff <- 
      suppressMessages(
        suppressWarnings(
          df %>%
            mutate(year = year(DATE)) %>% 
            filter(year == 2021) %>% 
            group_by(STATION, year) %>% 
            summarize(avgTempDiff = mean(TMAX-TMIN, na.rm = TRUE))))
  } else 
    avgTempDiff <- 
      tibble(STATION = df$STATION[1], year = 2021, avgTempDiff = NaN)
}

avgTempDiffs <- joinDF$fileNameCSV %>% map_dfr(~avgTmpDiffFunc(.x))

avgTempDiffs <- avgTempDiffs %>% rename("ID" = "STATION")

stationsSF <- joinDF %>% left_join(avgTempDiffs, by = c("ID" = "ID"))

stationsSF %>% 
  ggplot() + 
  geom_sf(data = usa) + 
  geom_sf(data = koppen_dfa) + 
  geom_sf(aes(color = avgTempDiff)) + 
  scale_color_viridis_c() + 
  theme_void()
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
grump <- 
  raster("../Data/GRUMP/usa/usaurextents") %>% 
  crop(usa)
LCZ <- 
  raster("../Data/WUDAPT/CONUS_LCZ_map_NLCD_v1.0_epsg4326.tif")
lights <- 
  raster("../Data/lights.75N180W.rade9.tif") %>% 
  crop(usa)

plot(grump)
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(LCZ)
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
plot(lights)
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
stations_raster <- 
  stationsSF %>% 
  dplyr::select(c(ID, ELEV, avgTempDiff)) %>% 
  mutate(
    grump = extract(grump, .),
    LCZ = extract(LCZ, .),
    lights = extract(lights, .)
  )

stations_raster_fix <- 
  stations_raster %>% 
  mutate(
    grump = if_else(grump == 1, "rural", "urban"),
    LCZ = as.character(LCZ)
  )

stations_raster_fix %>% 
  ggplot(aes(x=ELEV, y = avgTempDiff, color = grump)) +
  geom_point()
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
stations_raster_fix %>% 
  ggplot(aes(x=ELEV, y = avgTempDiff, color = grump)) +
  geom_point() +
  facet_grid(LCZ~grump)
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
scale_this <- function(x) as.vector(scale(x))

modelDF <- 
  stations_raster_fix %>% 
  drop_na() %>% 
  mutate_at(vars(c(ELEV,lights)), scale_this)

formula <- avgTempDiff ~ LCZ  + ELEV + grump + lights

x <- build.x(formula, modelDF, contrasts=FALSE, sparse=TRUE)
y <- build.y(formula, modelDF) %>% as.numeric()

myControl <- 
  trainControl(method = "cv", number = 10)

myGrid <- expand.grid(
    alpha = seq(0,1,.1), lambda = seq(0,10,1)
)

set.seed(123)
out <- train(formula, 
             data = modelDF,
             method = "glmnet",
             tuneGrid = myGrid,
             tfControl = myControl)

alpha <-  out$bestTune$alpha
lambda <- out$bestTune$lambda

mod <- cv.glmnet(x=x,
                 y=y,
                 alpha = alpha,
                 family='gaussian', 
                 nfolds= 10, 
                 standardize = FALSE)

coefplot(mod, sort='magnitude',lambda=lambda, intercept = FALSE) + 
        theme_minimal()
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
prediction <- modelDF %>% bind_cols(predict = as.vector(predict(mod,x)))
residual <- prediction %>% mutate(residual = predict - y) 
MSE  <- mod$cvm[mod$lambda==mod$lambda.1se]
```

``` r
ctSF <-  
  stations_raster_fix %>% 
  left_join(residual, by = c("ID" = "ID")) 

tm_shape(usa) +
    tm_borders()  + 
    tm_shape(ctSF) +
    tm_dots(col = "residual",
            title= "Residual",size = .1)+
    tm_style("watercolor",frame = FALSE,
             title= "Residuals" , title.size = 1) + 
    tm_scale_bar(position = c("right", "bottom"))
```

![](Assignment_6_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Model information:

-   Koppen Classification: Dfa
-   Model MSE: 153.4
-   Model alpha: 0.2
-   Model lambda: 1
