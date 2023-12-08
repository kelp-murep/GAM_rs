# Load required packages
library(mgcv)
library(dplyr)

# https://htmlpreview.github.io/?https://cdn.jsdelivr.net/gh/eric-pedersen/mgcv-esa-workshop@master/example-forest-health.html
# https://noamross.github.io/gams-in-r-course/chapter3

# plotting video
# https://campus.datacamp.com/courses/nonlinear-modeling-with-generalized-additive-models-gams-in-r/spatial-gams-and-interactions?ex=4

# use ordered categorical = ocat for SACFOR


# load in shapefile with sacfor, lat, lon, depth, and pixel spectra

file <- 'sacfor_data.csv'
data <- read.csv(file)

# choose 8 bands to simplify the s(spectra) term in gam
reduced_data <- data %>% select('Name', 'Elev.m.', 'kelp_num','veg_num_corr',
                                'x','y','wl_461','wl_659','wl_668','wl_619',
                                'wl_673','wl_671','wl_700','wl_702')
reduced_data$kelp_num = reduced_data$kelp_num + 1

# spectral data
sacfor_model <- gam(kelp_num ~ s(x,y) + s(Elev.m.) + s(wl_461) + s(wl_659) + s(wl_668)
                    + s(wl_619) + s(wl_673) + s(wl_671) + s(wl_700) + s(wl_702),
                 data = reduced_data,
                 family = ocat(R=6), 
                 method = 'REML')

summary(sacfor_model)
plot(sacfor_model,shade = TRUE, scheme = 2)

# sidescan model

sidescan_data <- data %>% select('Name', 'Elev.m.', 'Slope.deg.','Rugosity', 
                                 'kelp_num','veg_num_corr','x', 'y')


sidescan_data$kelp_num = sidescan_data$kelp_num + 1

sidescan_model <- gam(kelp_num ~ s(x,y) + s(Elev.m.) + s(Slope.deg.) + s(Rugosity),
                      data = sidescan_data,
                      family = ocat(R=6),
                      method = 'REML')

summary(sidescan_model)

plot(sidescan_model,shade = TRUE, scheme = 2)
## try with x(x,y,z) as an interaction? 


# plt the results spatially




# Notes:
# can GAM be used to select the wavelengths that are most useful and then put into the RF?
# model from sidescan data using elevation, slope, rugosity, depth


