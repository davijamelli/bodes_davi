
#' title: "TestVignette"
#' author: "John Fieberg"
#' date: ""
#' ---
#' 
#' ## Purpose
#' 
#' - test whether you have everything set up for the workshop
#' - retrieve and explore animal movement data from Movebank
#' - illustrate how to work with functions in the animal movement tools package (amt)
#' - write out available points for further annotating in EnvDATA
#' 
#' 
#' #### Preamble
#' 
#' Load libraries
#+warning=FALSE, message=FALSE
#install.packages("knitr","lubridate","maptools","raster","move","amt","ggmap","tibble", "leaflet","dplyr")

library(knitr)
library(lubridate)
library(maptools)
library(raster)
library(move)
library(amt) 
library(ggmap)
library(tibble)
library(leaflet)
library(dplyr)
options(width=165,digits.secs = 3)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = F)

#' Record time for running all code
ptm<-proc.time()

#' Set the seed for the random number generator, so it will be possible
#' to reproduce the random points
set.seed(10299)

#### Definir pasta do arquivo como Working directory ####
#install.packages("rstudioapi")
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path))
# you can make sure you are in the right directory
print( getwd() )

####Load data from a study ####
data_capr<- read.csv(file="caprinos_move.csv", header=TRUE, sep=";")

bode_move <- move(x=data_capr$location.long, y=data_capr$location.lat,
                  time=as.POSIXct(data_capr$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                  data=data_capr,
                  proj=CRS("+proj=longlat +ellps=WGS84"),
                  animal=data_capr$ID)
summary(bode_move)

#For now, not necessary
#list <- list(b2_move, b3_move)
#stack <- moveStack(list)
#' Create a data frame from the MoveStack object
#stack.dat <- as(stack, "data.frame")

#' Note: If there are duplicate animal-timestamp records, you will get a warning. 
#' You can exclude duplicate records on import using removeDuplicatedTimestamps=T. If you are 
#' a data manager for a study in Movebank you can also filter them out directly in the study 
#' so they are excluded by default in downloads (see https://www.movebank.org/node/27252).

#### Data cleaning ####

#' Delete observations where missing lat or long or a timestamp.  There are no missing
#' observations in this data set, but it is still good practice to check.
ind_capr<-complete.cases(data_capr[,c("location.lat", "location.long", "timestamp")])
data_capr<-data_capr[ind_capr==TRUE,]

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier). There are no duplicate
#' observations in this data set, but it is still good practice to check.
ind2_capr<-data_capr %>% select(timestamp, location.long, location.lat, ID) %>%
  duplicated
sum(ind2_capr) # no duplicates
data_capr<-data_capr[ind2_capr!=TRUE,]

#' Make timestamp a date/time variable (already done on move function)
data_capr$timestamp <- as.POSIXct(data_capr$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#' Look at functions in the move package.
plot(bode_move)
show(bode_move)
summary(bode_move)

####Plots of the data####
#' 
#' Note: there are lots of ways to plot data using R.  I have included code that 
#' illustrates 2 simple plotting methods for a single individual:
#' 
#' - using ggmap (with google maps)
#' - using leaflet
#' 
#' Both can become cumbersome and slow with too many observations.
#' Lets look at the data from F2. Note: When reading data from Movebank always be 
#' sure to refer to the animal-id and individual-local-identifer
#' and not the tag-id or tag-local-identifier in order to exclude pre- and 
#' post-deployment records and correctly separate out individual animals when the 
#' study includes redeployments.

data_caprB2<-data_capr %>% filter(ID=="B02")

#' We can plot the data using ggmap and ggplot;
#' ggmap extends ggplot2 by adding maps from e.g. google maps as background

#' Get the map that covers the study area. Zoom out a bit from what calc_zoom
#' suggests.  For your own data, you may have to change the zoom.

#z<-(calc_zoom(location.long, location.lat, data_caprB2))
#map <- get_map(location = c(lon = mean(data_caprB2$location.long), 
 #                           lat = mean(data_caprB2$location.lat)), zoom = 12,
#               maptype = "hybrid", source = "google")

#ggmap(map) + 
#  geom_point(data=data_caprB2, aes(x=location.long, y=location.lat), size=2.5)

# plot the track on the map

#' Now, using leaflet
leaflet(data_caprB2)%>%addTiles()%>%
  addCircles(data_caprB2$location.long, data_caprB2$location.lat)

#' ### Using ggplot without a background
#' 
#' Use separate axes for each individual (add scales="free" to facet_wrap)
#+fig.height=12, fig.width=12
#Only one animal
ggplot(data_caprB2, aes(x=location.long, y=location.lat))+geom_point()

#more than one  
ggplot(data_capr, aes(x=location.long, y=location.lat))+geom_point()+
  facet_wrap(~ID, scales="free")

#' Now, all on 1 plot
#+fig.height=6, fig.width=12
data_capr<-data_capr[-grep("B04",data_capr$ID ),] # bodes presos
data_capr<-data_capr[-grep("B09",data_capr$ID ),] # bodes presos
data_capr<-data_capr[-grep("B08",data_capr$ID ),] # definir porque tiramos esses
data_capr<-data_capr[-grep("B12",data_capr$ID ),] # definir porque tiramos esses

## Gr√°fico potencial para material suplementar
ggplot(data_capr, aes(x=location.long, y=location.lat, color=as.factor(ID)))+
  geom_point() 

####Creating a track in amt####
#' 
#' Before we can use the amt package to calculate step lengths, turn angles, and bearings
#' for fisher data, we need to add a class (track) to the data. Then, we can summarize 
#' the data by individual, month, etc. First, create a track using utms and the timestamp.
#' 
#' If we have a data set with locations in utms, we could use:
#trk.temp <- make_track(fisher.dat, .x=utm.easting, .y=utm.northing, .t=timestamp, id = individual_local.identifier)
#trk.temp

#' Note: we did not need to explicitly specify x, y and t (but probably good to do so).
#' This would also have worked
#' trk <- make_track(fisher.dat, utm.easting, utm.northing, timestamp, id = local_identifier)

#' We can also use lat, long, which will allow us to determine
#' time of day 
trk_capr <- mk_track(data_capr, .x=location.long, .y=location.lat, .t=timestamp, id = ID, 
                     crs = CRS("+init=epsg:4326"))

# Now it is easy to calculate day/night with either movement track
trk_capr <- trk_capr %>% time_of_day()

#' Now, we can transform back to geographic coordinates
#trk_capr <- transform_coords(trk_capr, CRS("+init=epsg:32724"))

#' If we create trk.temp as above, we can make sure that we got back the same coordinates 
#' by comparing to the original track
#mean(trk$x_ - trk.temp$x_) # look good, there is virtually no difference
#mean(trk$y_ - trk.temp$y_) # same for y

#' Save the class here (and apply it later after adding columns to the 
#' object)
trk_capr.class<-class(trk_capr)

####Movement Characteristics####
#' 
#' - dir_abs will calculate absolute angles for steps
#' - dir_rel will calculate turning angles (relative angles)
#' - step_lengths will calculate distances between points
#' - nsd = Net Squared Displacement (distance from first point)
#' 
#' Arguments direction_abs:
#' 
#' - full_circle will calculate between 0 and 360 (rather than -180 and 180)
#' - zero gives the direction = 0 for absolute angle
#' 
#' Note:  we have to calculate these characteristics separately for each 
#' individual (to avoid calculating a distance between say the last observation
#' of the first individual and the first observation of the second individual).
#' 
#' 
#' To do this, we could loop through individuals, calculate these
#' characteristics for each individual, then rbind the data 
#' back together.  Or, use nested data frames and the map function
#' in the purrr library to do this with very little code. 
#' 
#' To see how nesting works, we can create a nested object by individual
nesttrk_capr<-trk_capr%>%nest(-id)
nesttrk_capr

#' Each row contains data from an individual.  For example, we can access data
#' from the first individual using:
nesttrk_capr$data[[1]]

#' We could calculate movement characteristics by individual using:
temp_capr<-direction_rel(nesttrk_capr$data[[1]])
head(temp_capr)

#Error
#' or:
#temp_capr<-trk_capr %>% filter(id=="B2") %>% direction_rel
head(temp_capr)

#' Or, we can add a columns to each nested column of data using purrr::map
trk_capr<-trk_capr %>% nest(-id) %>% 
  mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd))%>%unnest()

#' Now, calculate month, year, hour, week of each observation and append these to the dataset
#' Unlike the movement charactersitics, these calculations can be done all at once, 
#' since they do not utilize successive observations (like step lengths and turn angles do).
trk_capr<-trk_capr%>% 
  mutate(
    week=week(t_),
    month = month(t_, label=TRUE), 
    year=year(t_),
    hour = hour(t_)
  )

#' Now, we need to again tell R that this is a track (rather 
#' than just a data frame)
class(trk_capr)
class(trk_capr)<-trk_capr.class

#' Lets take a look at what we created
trk_capr

####Some plots of movement characteristics####

#' ### Absolute angles (for each movement) relative to North 
#' We could use a rose diagram (below) to depict the distribution of angles. 
#+fig.height=12, fig.width=12
ggplot(trk_capr, aes(x = dir_abs, y=..density..)) + geom_histogram(breaks = seq(0,360, by=20))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, by=20), 
                     labels = seq(0, 360, by=20))+
  facet_wrap(~id)

#' ### Turning angles 
#' 
#' Note: a 0 indicates the animal continued to move in a straight line, a 180 
#' indicates the animal turned around (but note, resting + measurement error often can
#' make it look like the animal turned around).
#+fig.height=12, fig.width=12
ggplot(trk_capr, aes(x = dir_rel, y=..density..)) + geom_histogram(breaks = seq(-180,180, by=20))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous("", limits = c(-180, 180), breaks = seq(-180, 180, by=20), 
                     labels = seq(-180, 180, by=20))+
  facet_wrap(~id)

#' ### Turning angles as histograms
#+fig.height=12, fig.width=12
ggplot(trk_capr, aes(x = dir_rel)) +  geom_histogram(breaks = seq(-180,180, by=20))+
  theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Angles Relative") + 
  scale_x_continuous("", limits = c(-180, 180), breaks = seq(-180, 180, by=20),
                     labels = seq(-180, 180, by=20))+facet_wrap(~id, scales="free")

#' ### Net-squared displacement over time for each individual
#+fig.height=12, fig.width=12
ggplot(subset(trk_capr, id=="B02"), aes(x = t_, y=nsd_)) + geom_point()+ ## mudar id para cada bicho
  facet_wrap(~id, scales="free")

ggplot(trk_capr, aes(x = hour, y=nsd_)) + geom_point()+ ## todos os bichos
  facet_wrap(~id, scales="free")

summary(trk_capr$t_)




####Explore movement characteristics by (day/night, hour, month)####
#' 
#' ### step length distribution by day/night
#' 
#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
#ggplot(trk_capr, aes(x = tod_, y = log(sl))) + 
 # geom_boxplot()+geom_smooth()+facet_wrap(~id)

##There is no sense for this goat data
#' ### Space use (MCP or KDE) by week, month, and year
#' 
#' Note:  this code will only work for your critters if you
#' have multiple observations for each combination of
#' (month, year).  If you don't have many observations, you could
#' try:  nest(-id, -year) and unnest(id,year)
#mcps.week_capr<-trk_capr %>% nest(-id,-year,  -month, -week) %>%  
#  mutate(mcparea = map(data_capr, ~hr_mcp(., levels = c(0.95)) %>% hr_area)) %>% 
#  select(id, year, month, week, mcparea) %>% unnest()

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
#ggplot(mcps.week_capr, aes(x = week, y = area, colour=as.factor(year))) + geom_point()+
#  geom_smooth()+ facet_wrap(~id, scales="free")

#' Same for KDE
#kde.week_capr<-trk_capr %>% nest(-id,-year,  -month, -week) %>%  
#  mutate(kdearea = map(data_capr, ~hr_kde(., levels=c(0.95)) %>% hr_area)) %>%
#  select(id, year, month, week,  kdearea) %>% unnest()

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
#ggplot(kde.week, aes(x = week, y = kdearea, colour=as.factor(year))) + geom_point()+
#  geom_smooth()+ facet_wrap(~id, scales="free")


#### RSF prep ####
#' 
#' Generate random points within MCPs for a single individual using amt functions.
#' Notes:
#' 
#' - It is common to generate points randomly, but other options are possible. 
#' - In particular, it can beneficial to generate a systematically placed sample
#' - Samples can also be generated using the *spsample* function in the sp library or
#' using a GIS (note: amt uses the spsample function within its function random_points)
#' - Other home range polygons could be used (e.g., kernel density, local convex hull
#' etc.)
#' 

which(is.na(trk_capr))
colSums(is.na(trk_capr))

#' #### Random points: illustrate for 1 individual
trk_capr %>% filter(id=="B02") %>%
  random_points(.,factor = 100) %>% plot

#Tentar inserir pontos randÙmicos na ·rea de vida Kernel e n„o do MCP. 
#MCP È o padr„o da funÁ„o random_points
trk_capr %>% filter(id=="B12") %>%
 random_points(.,factor = 100, hr="kde") %>% plot

#' Illustrate systematic points (to do this, we need to create the mcp first)
#trk_capr%>%filter(id=="B02") %>% 
  #random_points(., factor = 20, type="regular") %>% 
 # plot() 

#' Now, lets generate points for all individuals. We can do this
#' efficiently by making use of pipes (%>%),nested data frames, and
#' then by adding a new column -- a list-column -- to trks
##POR QUE TYPE = REGULAR? POR QUE N√O RANDOM?
avail.pts_capr <- trk_capr %>% nest(-id) %>% 
  mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="regular"))) %>% 
  select(id, rnd_pts) %>%  # you dont want to have the original point twice, hence drop data
  unnest()

#' Or, we could do this using a loop (commented out, below)
#avail.pts<-NULL
#uid<-unique(trk$id) # individual identifiers
#luid<-length(uid) # number of unique individuals
#for(i in 1:luid){
# random_points will generate random points within mcp
# Add on the individual id and combine all data
#  temp<-cbind(id=uid[i],trk%>%filter(id==uid[i])%>%random_points)
#  avail.pts<-rbind(avail.pts, temp)
#}
#avail.pts<-as_tibble(avail.pts)
#avail.pts

#### Write out data for further annotating ####
#' 
#' Need to rename variables so everything is in the format Movebank requires for annotation of generic time-location 
#' records (see https://www.movebank.org/node/6608#envdata_generic_request). This means, we need the following variables:
#' 
#' - location-lat (perhaps with addition of Easting/Northing in UTMs)
#' - location-long (perhaps with addition of Easting/Northing in UTMs)
#' - timestamp (in Movebank format)
#' 
#' Need to project to lat/long, while also keeping lat/long. Then rename
#' variables and write out the data sets.
avail_capr <- SpatialPointsDataFrame(avail.pts_capr[,c("x_","y_")], avail.pts_capr, 
                                     proj4string=CRS("+proj=utm +zone=24S +datum=WGS84"))  
avail.df_capr <- data.frame(spTransform(avail_capr, CRS("+proj=longlat +datum=WGS84")))[,1:6]

names(avail.df_capr)<-c("idr", "case_", "utm.easting", "utm.northing", "location-long", "location-lat")

#' Check to make sure everything looks right
test_capr<-subset(avail.df_capr, case_==TRUE)
test_capr %>% select('location-lat', 'location-long', utm.easting, utm.northing) %>% 
  summarise_all(mean) # s√≥ vamos usar das duas primerias colunas

data_capr %>% summarize(meanloc.lat=mean(location.lat), 
                        meanloc.long=mean(location.long))

#' Add a timestamp to annotate these data with environmental covariates in Movebank using Env-DATA (https://www.movebank.org/node/6607).
#' Here we just use the first timestamp, however meaningful timestamps are needed if annotating variables that vary in time.
avail.df_capr$timestamp<-data_capr$timestamp[1]

#' These points then need to be annotated prior to fitting rsfs. Let's 
#' write out 2 files:
#' 
#' - CaprinosRSF2018.csv will contain all points and identifying information. 
#' - CaprinosRSFannotate.csv will contain only the columns used to create the annotation.
#' 
#' The latter file will take up less space, making it easier to annotate (and also possible to upload to github)
#write.csv(avail.df_capr, file="E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/CaprinosRSF2018.csv, row.names = FALSE)

#avail.df_capr<-avail.df_capr %>% select("timestamp", "location-long", "location-lat")
#avail.df_capr<-avail.df_capr %>% select("timestamp", "utm.easting", "utm.northing")
#write.csv(avail.df_capr, file="E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/CaprinosRSFannotate.csv", row.names = FALSE)

#### SSF prep  ####
#' 
#' SSFs assume that data have been collected at regular time intervals.
#' We can use the track_resample function to regularize the trajectory so that
#' all points are located within some tolerence of each other in time. To figure
#' out a meaningful tolerance range, we should calculate time differences between
#' locations & look at as a function of individual.
#(timestats_capr<-trk_capr %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
#   select(id, sr) %>% unnest)

#' Time intervals range from every 2 to 15 minutes on average, depending
#' on the individual.  Lets add on the time difference to each obs.
#trk_capr<-trk_capr %>% group_by(id) %>% mutate(dt_ = t_ - lag(t_, default = NA))

#' Let's illustrate track regularization with ID = F2. Let's
#' go from every 2 minutes to every 10.
#tempB2<-trk_capr %>% filter(id=="B2") %>% track_resample(rate=minutes(10), tolerance=minutes(2))
#tempB2 %>% select(id, x_, y_, t_, burst_)

#' Now loop over individuals and do the following:
#' 
#' - Regularize trajectories using an appropriate time window (see e.g., below) 
#' - calculate new dt values
#' - Create bursts using individual-specific time intervals
#' - Generate random steps within each burst
#' 
#' The random steps are generated using the following approach:
#' 
#' 1. Fit a gamma distribution to step lenghts
#' 2. Fit a von mises distribution to turn angles
#' 3. Use these distribution to draw new turns and step lengths, form new simulated steps
#' and generate random x,y values.
#' 

#+warning=FALSE
#ssfdat_capr<-NULL
#temptrk_capr<-with(trk_capr, track(x=x_, y=y_, t=t_, id=id))
#uid_capr<-unique(trk_capr$id) # individual identifiers
#luid_capr<-length(uid_capr) # number of unique individuals
#for(i in 1:luid_capr){
  # Subset individuals & regularize track
#  temp_capr<-temptrk_capr%>% filter(id==uid_capr[i]) %>% 
#    track_resample(rate=minutes(round(timestats_capr$median[i])), 
#                   tolerance=minutes(max(10,round(timestats_capr$median[i]/5))))
  
  # Get rid of any bursts without at least 2 points
#  temp_capr<-filter_min_n_burst(temp_capr, 2)
  
  # burst steps
#  stepstemp_capr<-steps_by_burst(temp_capr)
  
  # create random steps using fitted gamma and von mises distributions and append
#  rnd_stps_capr <- stepstemp_capr %>%  random_steps(n = 15)
  
  # append id
#  rnd_stps_capr<-rnd_stps_capr%>%mutate(id=uid_capr[i])
 # append new data to data from other individuals
#  ssfdat_capr<-rbind(rnd_stps_capr, ssfdat_capr)
#}
#ssfdat_capr<-as_tibble(ssfdat_capr)
#ssfdat_capr


#' Now, lets plot the data for random and matched points
#' 
#+fig.height=12, fig.width=12, warning=FALSE
#ggplot(ssfdat_capr, aes(x2_, y2_, color=case_))+geom_point()+facet_wrap(~id, scales="free")


#' Relabel as utms 
#ssfdat$utm.easting_capr<-ssfdat_capr$x2_
#ssfdat_capr$utm.northing<-ssfdat_capr$y2_

#' ## Write out data for further annotating
#' 
#' Need to rename variables so everything is in the format Movebank requires for annotation of generic time-location 
#' records (see https://www.movebank.org/node/6608#envdata_generic_request). This means, we need the following variables:
#' 
#' - location-lat (perhaps with addition of Easting/Northing in UTMs)
#' - location-long (perhaps with addition of Easting/Northing in UTMs)
#' - timestamp (in Movebank format)
#' 
#' Need to project to lat/long, while also keeping lat/long. Then rename
#' variables and write out the data sets. With the SSFs, we have the extra complication of
#' having a time and location at both the start and end of the step.  
#' 
#' For the time being, we will assume we want to annotate variables at the end of the step
#' but use the starting point of the step as the timestamp.
#' 
#' You could also calculate the midpoint of the timestep like this:
#' data$timestamp.midpoint <- begintime + (endtime-begintime)/2

ssfdat2_capr <- SpatialPointsDataFrame(ssfdat_capr[,c("x2_","y2_")], ssfdat_capr, 
                                       proj4string=CRS("+proj=utm +zone=24S +datum=WGS84"))  
ssf.df_capr <- data.frame(spTransform(ssfdat2_capr, CRS("+proj=longlat +datum=WGS84"))) 
names(ssf.df_capr)[c(13,16,17)] <-c("individual.local.identifier", "location-long", "location-lat")
ssf.df_capr$timestamp<-ssf.df_capr$t1_
ssf.df_capr %>% select('location-lat', utm.easting, x1_, x2_, y1_, y2_, 'location-long', utm.northing) %>% head


#' These points then need to be annotated prior to fitting ssfs. Let's 
#' write out 2 files:
#' 
#' - 2CaprinostesteSSF2018.csv will contain all points and identifying information. 
#' - 2CaprinostesteSSFannotate.csv will contain only the columns used to create the annotation.
#' 
#' The latter file will take up less space, making it easier to annotate (and also possible to upload to github)
write.csv(ssf.df_capr, file="E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/AllStepsCaprinos2018.csv", row.names=FALSE)
ssf.df_capr<-ssf.df_capr %>% select("timestamp", "location-long", "location-lat")
write.csv(ssf.df_capr, file="E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/CaprinosSSFannotate.csv", row.names = FALSE)


#' ## Using nested data frames
#' 
#' This works if all animals are sampled at a constant sampling rate.
#' Again, you need to create a new column (using `mutate`) where you save the random steps to
#' Not all animals have a 15 min sampling rate, so we might drop the first
#' animal that has a 15 minute sampling rate. 
#trk %>% nest(-id) %>% mutate(sr = map(.$data, summarize_sampling_rate)) %>% 
#  select(id, sr) %>% unnest()

# Then, we could avoid a loop with:
#ssfdat <- trk %>% filter(id != "M1") %>% nest(-id) %>% 
#  mutate(ssf = map(data, function(d) {
#    d %>%
#      track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
#      filter_min_n_burst(min_n = 3) %>% 
#      steps_by_burst() %>% random_steps()
#  })) %>% select(id, ssf) %>% unnest()

#' ## Document Footer	
#' 	
#' Session Information:	
#' 	
sessionInfo()	  

proc.time()-ptm


######################### recome√ßar aqui ##############################33

#### Merge data ####
#' ---
#' title: "Merge Data"
#' author: "John Fieberg"
#' date: ""
#' ---
#' 
#' 
#' ### Preamble
#' 
#' **Purpose**: Merge together original Use/available data with annotated Env-Data.
#'  
#' Load libraries
#+warning=FALSE, message=FALSE
library(ezknitr)
library(knitr)
library(lubridate)
library(raster)
library(move)
library(amt) 
library(tidyverse)
options(width=150)

#### Merge RSF data####

#' Read in original data (used and available points) and merge on environmental
#' data.
rsfdattemp_capr<-read.csv("CaprinosRSF2018.csv")
names(rsfdattemp_capr)[3]<-paste("location.long")
names(rsfdattemp_capr)[4]<-paste("location.lat")
names(rsfdattemp_capr)[5]<-paste("utm.easting")
names(rsfdattemp_capr)[6]<-paste("utm.northing")
rsfdattemp_capr<- rsfdattemp_capr[,c(1,2,5,6,3,4,7)]


#Planilha com dados ambientais (vari√°veis explicativas) - arquivo CaprinosRSFannotate_variaveis.csv com as vari√°veis 
annotated_capr<-read.csv("CaprinosRSFannotate_ambientais.csv")
names(annotated_capr)[2]<-paste("location.long")
names(annotated_capr)[3]<-paste("location.lat")
head(annotated_capr)

#' Now, MERGE these by "timestamp", "location-long", "location-lat")
rsfdat_capr<-merge(rsfdattemp_capr, annotated_capr)
summary(rsfdat_capr)

#' Write out RSF data for use in FisherRSF2018.R
write.csv(rsfdat_capr, "CaprinosRSF2018-EnvDATA-results.csv", row.names = FALSE)


####Merge SSF data#### PASSAR

#' Read in original data (used and available points) and merge on environmental
#' data.
ssfdattemp_capr <- read.csv("E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/AllStepsCaprinos2018.csv")
annotated_capr<-read.csv("E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/CaprinosSSFannotate.csv")

#' Now, merge these by "timestamp", "location-long", "location-lat")
ssfdat_capr<-merge(ssfdattemp_capr, annotated_capr)

##### Write out SSF data for use in FisherSSF2018.R ####
write.csv(ssfdat_capr, file="E:/Davi_Drive/SIG/BODES/RSF_caprinos/Animove_scripts/data/AllStepsCaprinos2018-EnvDATA-results.csv", row.names=FALSE)



#### Preparing RSF Fitting #### EXECUTAR

#' ---
#' title: "Fisher RSF"
#' author: "John Fieberg"
#' date: ""
#' ---

#' ### Preamble
#' 
#' Load  libraries
#+warning=FALSE, message=FALSE
library(ezknitr)
library(knitr)
library(lubridate)
library(raster)
library(move)
library(amt) 
library(broom)
library(nlme)
library(lme4)
library(tidyverse)
options(width=165)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = F)

#' Read in annotated data. 
rsfdat_capr<-read.csv("CaprinosRSF2018-EnvDATA-results.csv")
summary(rsfdat_capr)
nrow(rsfdat_capr)
rsfdat_capr<-rsfdat_capr[-grep("B04",rsfdat_capr$idr ),]
rsfdat_capr<-rsfdat_capr[-grep("B09",rsfdat_capr$idr ),]
rsfdat_capr<-rsfdat_capr[-grep("B08",rsfdat_capr$idr ),]
rsfdat_capr<-rsfdat_capr[-grep("B12",rsfdat_capr$idr ),]
summary(rsfdat_capr)


#' Simplify some variable names
names(rsfdat_capr)[c(4,8,9, 10)]<-c("id", "CostDist", "LandClass", "EuclDist")
names(rsfdat_capr)
rsfdat_capr$case_<-as.numeric(rsfdat_capr$case_)
str(rsfdat_capr)

#' Create landcover classes (as suggested by Scott Lapoint :)
rsfdat_capr$LandClass<-as.character(rsfdat_capr$LandClass)
rsfdat_capr<-rsfdat_capr %>% mutate(landC = fct_collapse(LandClass,
                                                         exposed.soil = c("1"),
                                                         low.density =c("2"),
                                                         medium.density= c("3"),
                                                         high.density = c("4")))

#' Center and scale variables (faz sentido para s√≥ uma vari√°vel num√©rica?)
rsfdat_capr<-rsfdat_capr %>% mutate(cost_dist=as.numeric(scale(CostDist)), dis_euc=as.numeric(scale(EuclDist)))
summary(rsfdat_capr)


#' ## Explore the data:

#' Look Distribution of variables for used and available
#+fig.width=8, fig.height=8
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

# Para Dist?ncia de custo
summary(rsfdat_capr)
ggplot(rsfdat_capr,aes(x=CostDist, y=case_))+
  stat_smooth(method="glm", method.args = list(family = "binomial"))+
  binomial_smooth(formula = y ~ splines::ns(x, 5), colour="red")+
  facet_wrap(~id, scales="free")

ggplot(rsfdat_capr,aes(x=cost_dist, y=case_))+
  stat_smooth(method="glm", method.args = list(family = "binomial"))+
  binomial_smooth(formula = y ~ splines::ns(x, 5), colour="red")


# Para Dist?ncia euclideana
ggplot(rsfdat_capr,aes(x=EuclDist, y=case_))+
  stat_smooth(method="glm", method.args = list(family = "binomial"))+
  binomial_smooth(formula = y ~ splines::ns(x, 5), colour="red")+
  facet_wrap(~id, scales="free")


ggplot(rsfdat_capr, aes(x=landC, y=..prop..,group=case_, colour=case_))+
  geom_bar(position="dodge", aes(fill=case_))+facet_wrap(~id, scales="free")

ggplot(rsfdat_capr, aes(x=landC, y=..prop..,group=case_, colour=case_))+ # TODOS os BODES
  geom_bar(position="dodge", aes(fill=case_))

#### RSF fitting ####

#' Weight available data 
#' precisamos saber da propor√ß√£o entre zeros e uns para atribuir peso
str(rsfdat_capr)
rsfdat_capr$case_fac<-as.factor(rsfdat_capr$case_)
summary(rsfdat_capr$case_fac) # agora seiu a propor√ß√£o de zeros e uns
47899/2421 # esse deve ser o peso para os zeros
rsfdat_capr$w<-ifelse(rsfdat_capr$case_==1, 1, 19.78)
head(rsfdat_capr)
#' We can fit an RSF model to a single animal using logistic regression
#mod_capr_costB2 <- glm(case_ ~ CostDist+landC, data = subset(rsfdat_capr, id=="B02"), weight=w,family = binomial) #Entender o Weight
mod_capr_costB2 <- glm(case_ ~ CostDist+landC, data = subset(rsfdat_capr, id=="B02"), weight=w,family = binomial)
mod_capr_euclB2 <- glm(case_ ~ EuclDist+landC, data = subset(rsfdat_capr, id=="B02"),weight=w, family = binomial)
summary(mod_capr_costB2)
summary(mod_capr_euclB2)

# Todos os caprinos
mod_capr_cost <- glm(case_ ~ CostDist+landC, data = subset(rsfdat_capr), weight=w,family = binomial)
mod_capr_eucl <- glm(case_ ~ EuclDist+landC, data = subset(rsfdat_capr), weight=w,family = binomial)
mod_capr_eucl2 <- glm(case_ ~ EuclDist*landC, data = subset(rsfdat_capr), weight=w, family = binomial)
summary(mod_capr_cost)
summary(mod_capr_eucl)
summary(mod_capr_eucl2)
aov(mod_capr_eucl2)

plot(mod_capr_eucl)

# Comparar com modelos alternativos e avaliar AIC
mod_capr_eucl2 <- glm(case_ ~ EuclDist*landC, data = subset(rsfdat_capr), family = binomial)
mod_capr_eucl3 <- glm(case_ ~ EuclDist, data = subset(rsfdat_capr), family = binomial)
mod_capr_eucl4 <- glm(case_ ~ landC, data = subset(rsfdat_capr), family = binomial)
mod_capr_euclnulo <- glm(case_ ~ 1, data = subset(rsfdat_capr), family = binomial)

AIC(mod_capr_eucl, mod_capr_eucl2, mod_capr_eucl3, mod_capr_eucl4, mod_capr_euclnulo)


## Modelos Mistos
library(lme4)
mixmod_capr_cost <- glmer(case_ ~ cost_dist:landC + (1|id), data = rsfdat_capr, weight=w,family = binomial)
summary(mod_capr_cost) # com todos os bodes dist?ncia de custo

mixmod_capr_eucl <- glmer(case_ ~ landC + (1|id), data = rsfdat_capr, weight=w,family = binomial)
summary(mod_capr_eucl) # com todos os bodes

##################nao usar######################################
#' Note, this individual did not experience all landcover classes
#rsfdat_capr %>% filter(id=="B02") %>% with(table(case_, landC))  
#rsfdat_capr$used<-as.factor(rsfdat_capr$case_)
#rsfdat_capr$used<-fct_recode(rsfdat_capr$used, "avail"="0", "used"="1")

#+fig.width=6, fig.height=4
#ggplot(subset(rsfdat_capr, id=="B02"),  aes(x=landC,group=used))+
# geom_bar(position=position_dodge(), aes(y=..prop.., fill = used), stat="count") +
#scale_fill_brewer(palette="Paired")+
#geom_text(aes( label = scales::percent(..prop..),
#              y= ..prop.. ), stat= "count", vjust = -.3, position=position_dodge(0.9)) +
#labs(y = "Proportion", fill="used", x="Landcover") 
###################################################################3

#' Now, fit an RSF model to data from each animal.  Since not all animals experience
#' all habitat types, lets just explore EXPOSED SOIL versus FOREST
rsfdat_capr$expsoil<-ifelse(rsfdat_capr$landC=="exposed.soil", 1, 0)
rsfdat_capr$lowdens<-ifelse(rsfdat_capr$landC=="low.density", 1, 0)
rsfdat_capr$medens<-ifelse(rsfdat_capr$landC=="medium.density", 1, 0)
rsfdat_capr$highens<-ifelse(rsfdat_capr$landC=="high.density", 1, 0)

fit_rsf_capr <- function(data){
  mod <- summary(glmer(case_ ~ CostDist+expsoil + (1|id), data = rsfdat_capr, weight=w,family = binomial))
  return(mod)
}
rsffits_capr <- rsfdat_capr %>%   
  dplyr::mutate(mod = purrr::map(data, fit_rsf_capr))


#' This stores a list of model fits (ERROR)
# rsffits_capr 

#' Look at first model
rsffits_capr$mod[[1]]

#' Now, use tidy to extract information about the model fits into a nested
#' data frame
rsffits_capr <- rsffits_capr %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow) %>% simplify())
#rsffits_capr 
rsffits_capr$tidy

#' Now, create data frame w/ the coefficients, etc
rsf_coefs_capr <- rsffits_capr  %>%
  tidyr::unnest(tidy) %>%
  dplyr::select(-(std.error:p.value))

rsf_coefs_capr %>% tidyr::spread(term, estimate)

#' Plot coefficients
#+fig.width=12, fig.heigh=4
rsf_coefs_capr %>% filter(term!="(Intercept)") %>%
  ggplot(., aes(x=1, y=estimate)) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)+
  facet_wrap(~term, scales="free")

#' Write out coefficients for MultipleAnimals.R
save(rsf_coefs, file="/rsfcoefs_capr.Rdata")

#' ## Document Footer	

#' Session Information:	
#' 	
sessionInfo()

