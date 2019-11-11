rm(list = ls(all = TRUE))
setwd("/Users/patrickhe/Desktop/MSBA/Social Network/Assignment 3")

library(data.table)
library(igraph)
library(stringr)
library(splitstackshape)
library(plotly)
library(network)
library(lubridate)
library(dplyr)
library(reshape)
library(MASS)
library(plm)
library(pglm)
library(ggplot2)

#load datasets
rain = fread(file = "rain_information.csv", header = TRUE)
dist = fread(file = "district_information.csv", header = TRUE)
bord = fread(file = "border_information.csv", header = TRUE)
pty = fread(file = "new_parties_in_each_district_by_candidate.csv", header = TRUE)
  
new_p = dist[,c(2,3,4,5,6,15,16,17,23)]

#create a list of year ranges
ele = unique(dist$year)
ele = sort(append(ele, 1945))
ele_yr = lapply(seq_along(ele)[-1], function(i) seq(ele[i -1] + 1, ele[i]))
ele = ele[-1]

#extreme weather
rain[, extreme := as.numeric(spi > 1 | spi < -1)]

#create a list of tables based on the year range created
#calculate the sum of rain and the mean of spi in each table by districts 
ele_p = lapply(seq_along(ele_yr), function(i) rain[year%in%ele_yr[[i]]])
ele_p = lapply(seq_along(ele_p), function(i) ele_p[[i]][, rain := sum(rain), by = district])
ele_p = lapply(seq_along(ele_p), function(i) ele_p[[i]][, spi := mean(spi), by = district])
ele_p = lapply(seq_along(ele_p), function(i) ele_p[[i]][, extreme := sum(extreme, na.rm=TRUE), by = district])
ele_p = lapply(seq_along(ele_p), function(i) ele_p[[i]][, length := length(ele_yr[[i]]), by = district])
#Many district only have one year data in the election, so we just use the one year data as the calculation result for rain and spi
ele_p = lapply(seq_along(ele_p), function(i) ele_p[[i]][, year := max(ele_yr[[i]]), by = district])

#verify if the mean and sum are correctly calculated
ele_p[[1]][1:6]
sum(rain[1:6,rain])

#create a new table including only the rows with election years
a = rbindlist(ele_p)
a = unique(a)
newp = new_p[order(year),] 

#Merge the table with mean value and sum value with the dist table
#b = left_join(a,new_p,by=c("year","district")), cannot find columns of b with this method
b = merge(a,new_p)

#Not all districts with new parties have rain data, so we omit the NAs

#border info
focal = unique(bord$focal_district)
nbr = lapply(seq_along(focal), function(i) bord[focal_district%in%focal[[i]]])

d = lapply(seq_along(nbr), function(i) b[district%in%unlist(nbr[[i]][,2])])
d_sum = lapply(seq_along(d), function(i) d[[i]][, lagg_rain := (sum(rain)/count(unique(d[[i]][,2]))), by = year])
d_mean = lapply(seq_along(d), function(i) d[[i]][, lagg_spi := (sum(spi)/count(unique(d[[i]][,2]))), by = year])
d_extreme = lapply(seq_along(d), function(i) d[[i]][, lagg_extreme := (sum(extreme)/count(unique(d[[i]][,2]))), by = year])

d_all = lapply(seq_along(d), function(i) d[[i]][, nbr := unlist(nbr[[i]][,1])[1]])

d1 = rbindlist(d_all)
d1 = d1[,c("year","nbr","lagg_rain","lagg_spi","lagg_extreme")]
setnames(d1, old="nbr", new="district")
d1 = unique(d1)

d1 = lapply(seq_along(ele), function(i) d1[year%in%ele[[i]]])
d_self_rain = lapply(seq_along(d1), function(i) d1[[i]][, lagg_self_rain := lapply(seq_along(seq(nrow(d1[[i]]))), function(x) b[which(b$year==d1[[i]][x,year]&b$district==d1[[i]][x,district]), rain])])
d_self_spi = lapply(seq_along(d1), function(i) d1[[i]][, lagg_self_spi := lapply(seq_along(seq(nrow(d1[[i]]))), function(x) b[which(b$year==d1[[i]][x,year]&b$district==d1[[i]][x,district]), spi])])
d_self_extreme = lapply(seq_along(d1), function(i) d1[[i]][, lagg_self_extreme := lapply(seq_along(seq(nrow(d1[[i]]))), function(x) b[which(b$year==d1[[i]][x,year]&b$district==d1[[i]][x,district]), extreme])])
#Change the year list so the spi and rain of current election year will be assigned to the previous  election year
ele1 = sort(append(ele, 1945))
ele1 = ele1[-length(ele1)]
d1 = lapply(seq_along(ele), function(i) d1[[i]][,year := ele1[[i]]])
d1 = d1[-1]
d1 = rbindlist(d1)
d1 = d1[lagg_self_rain != 0]

#For Question 5
pty = pty[,c(1,3,5)]
pty = unique(pty)
pty_by_dist = lapply(seq_along(nbr), function(i) pty[district%in%focal[i]])
pty_by_nbr = lapply(seq_along(nbr), function(i) pty[district%in%unlist(nbr[[i]][,2])])
pty_by_dist1 = pty_by_dist

mmp = lapply(seq_along(pty_by_dist), function(i) split(pty_by_dist[[i]], f = pty_by_dist[[i]]$year))
mmp1 = lapply(seq_along(mmp), function(i) mmp[[i]] = lapply(seq_along(pty_by_dist[[i]][,year]), function(x) pty_by_nbr[[i]][year < sort(unique(pty_by_dist[[i]][,year]))[x]]))
nnq = lapply(seq_along(pty_by_dist), function(i) split(pty_by_dist[[i]], f = pty_by_dist[[i]]$year))
nnq1 = lapply(seq_along(nnq), function(i) nnq[[i]] = lapply(seq_along(nnq[[i]]),
                                                            function(x) nnq[[i]][[x]][, diffused := party_name %in% mmp1[[i]][[x]]$party_name]))

nnq1 = rbindlist(lapply(seq_along(nnq1), function(i) rbindlist(nnq1[[i]])))

nnq1_yr = unique(nnq1$year)

nnq1 = lapply(seq_along(nnq1_yr), function(i) nnq1[year%in%nnq1_yr[[i]]])
nnq1 = lapply(seq_along(nnq1), function(i) nnq1[[i]][, diffuse := sum(diffused), by = district])
nnq1 = rbindlist(nnq1)
nnq1 = nnq1[,c(1,2,5)]
nnq1 = unique(nnq1)

lagg2 = merge(lagg1, nnq1, by=c("year","district"))
lagg2[, non_diffused := new_parties - diffuse]






#Question 1a
#Plot mean/sum and the new parties
cor_rain = ggplot(b, aes(rain, new_parties)) + geom_smooth(method = "loess", se = F) + labs(x = "Rainfall", y = "New Patries")
cor_spi = ggplot(b, aes(spi, new_parties)) + geom_smooth(method = "loess", se = F) + labs(x = "spi", y = "New Parties")
plot(cor_rain)
plot(cor_spi)


#Question 1b
lagg = merge(b,d1, by=c('year','district'))
lagg$lagg_self_rain = unlist(lagg$lagg_self_rain)
lagg$lagg_self_spi = unlist(lagg$lagg_self_spi)
lagg$lagg_self_extreme = unlist(lagg$lagg_self_extreme)

# Check relations
summary(plm(rain ~ lagg_rain + lagg_self_rain, data = lagg, effect = "twoways", model = "within", index = "district"))
summary(plm(spi ~ lagg_spi + lagg_self_spi, data = lagg, effect = "twoways", model = "within", index = "district"))

#Question 1c
summary(pglm(extreme ~ lagg_extreme + lagg_self_extreme, data = lagg, effect = "twoways", model = "within", index = "district", family = "poisson"))

#Question 2
summary(pglm(new_parties ~ extreme + length, data = lagg, effect = "individual", model = "within", index = "district", family = "poisson"))
#which is more likely
summary(pglm(new_parties_caste ~ extreme + length, data = lagg, effect = "individual", model = "within", index = "district", family = "poisson"))

summary(pglm(new_parties_socialist ~ extreme + length, data = lagg, effect = "individual", model = "within", index = "district", family = "poisson"))

#Question 3
ele2 = sort(append(ele, 1945))
ele2 = sort(append(ele2, 1944))
ele2 = ele2[-length(ele2)]
ele2 = ele2[-length(ele2)]
d2 = lapply(seq_along(ele), function(i) d1[year%in%ele[[i]]])
d2 = lapply(seq_along(ele), function(i) d2[[i]][,year := ele2[[i]]])
d2 = d2[-1]
d2 = d2[-1]
d2 = rbindlist(d2)
d2 = d2[lagg_self_rain != 0]
lagg1 = merge(b,d2, by=c('year','district'))
lagg1$lagg_self_rain = unlist(lagg1$lagg_self_rain)
lagg1$lagg_self_spi = unlist(lagg1$lagg_self_spi)
lagg1$lagg_self_extreme = unlist(lagg1$lagg_self_extreme)

summary(pglm(new_parties ~ extreme + lagg_extreme + length, data = lagg1, effect = "individual", model = "within", index = "district", family = "poisson"))

#Question 4a
summary(pglm(new_parties_national_scope ~ extreme + lagg_extreme + length, data = lagg1, effect = "individual", model = "within", index = "district", family = "poisson"))
summary(pglm(new_parties_state_scope ~ extreme + lagg_extreme + length, data = lagg1, effect = "individual", model = "within", index = "district", family = "poisson"))
summary(pglm(new_parties_regional_scope ~ extreme + lagg_extreme + length, data = lagg1, effect = "individual", model = "within", index = "district", family = "poisson"))

#Question 4b
summary(pglm(political_concentration ~ extreme + lagg_extreme + length, data = lagg1, effect = "individual", model = "within", index = "district", family = "poisson"))

#Question 5
summary(pglm(diffuse ~ extreme + lagg_extreme + length, data = lagg2, effect = "individual", model = "within", index = "district", family = "poisson"))
summary(pglm(non_diffused ~ extreme + lagg_extreme + length, data = lagg2, effect = "individual", model = "within", index = "district", family = "poisson"))
