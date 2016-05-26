
#### Year 3 valuation model
## The Demand Curve is simulated 1000 times 
## The supply curve is constructed using marginal cost, plus expected CO2 permit markup
## Market clearing price/ unit is where Supply and Demand meet. 
## The outputs of this model are for both mean values, but also allow for tests against different quantiles of demand

## loading libraries
setwd("/home/dan/Documents/EEM")
library(plyr)
library(dplyr)
library(ggplot2)
set.seed(101)

##loading portfolio details 
ports <- read.csv("ESGPortfolios_forR.csv", header = TRUE)


## Adding fields for cumlative capacity and fixed costs
ports$cumu <- cumsum(ports$Capacity)
ports$totalfixed <- ports$fixed *3

#### Input variables 
## simulation
runs <- 1000
## used to test market power, not used at presesnt
mktpwr <- 
## EX ANTE: this figure is meant to be the incremental value that each team will place on their plants' marginal cost
CO2_cost <- 80
####
permits.bought <- 15000
## EX POST: Actual CO2 price in bilateral trades
Actual.CO2.Cost <- 115
## Test for different levels of demand 
Quantile <- .5

## Adjusting MC/ Supply curve to incorporate expected CO2 permit cost
ports$MC_part <- ports$MC_part + (CO2_cost * ports$CO2e)
ports <- arrange(ports, MC_part)
ports$cumu <- cumsum(ports$Capacity)

supply_curve <- rep(ports$MC_part, ports$Capacity)

permit.cost <- CO2_cost * permits.bought
permit.revenue <- Actual.CO2.Cost * permits.bought

permit.revenue - permit.cost

####MC Day 1

##Intercepts and slopes - not added together exactly right, but should work within the relevant range of demand

D1_intN <- c(4184, 3842, 7986, 5209)
D1_intS <- c(7176, 9874, 9901, 9760)
D1_intT <- D1_intN + D1_intS

D1_mN <- c(-2.34, -2.58, -2.54, -2.67)
D1_mS <- c(-2.03, -2.80, -2.49, -2.41)
D1_mT <- D1_mN + D1_mS

D1_yint <- -D1_mT * D1_intT

MargCost <- ports$Capacity * ports$MC_part

############## Day 1

hour <- 1

## MC sim of demand
x_intercepts_calc11 <- rnorm(runs,  D1_intT[hour], D1_intT[hour] * .03)
y_intercepts_calc11 <- -D1_mT * x_intercepts_calc11

##demand curves
demand_curves11 <- sapply(y_intercepts_calc11, function(x) x + D1_mT[hour] * 1:22050)




### equilibrium quantity and price

equil_quant11 <- apply(demand_curves11, 2, function(x) which.min(abs(supply_curve - x)))
nets11 <- sapply(equil_quant11, function(x) ports$cumu - x)
marginal11 <- apply(nets11, 2, function(x) min(which(x > 0)))
clear_price11 <- ports$MC_part[marginal11]



## Revenue: quantile test
revenue11 <- ports$Capacity * quantile(clear_price11, Quantile) - ports$Capacity * ports$MC_part
ports$revenue11 <- ifelse(revenue11 < 0, 0, revenue11) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal11), Quantile)]
quantile(equil_quant11, .5)

Humboldt1 <- marginal11 == 23
Humboldt1.dispatched <- length(Humboldt1[Humboldt1 == TRUE])

HP1 <- marginal11 == 24
HP1.dispatched <- length(HP1[HP1 == TRUE])

HH.clear.prob.hr1 <- (Humboldt1.dispatched + HP1.dispatched) / runs

## Revenue: mean
rev11 <- sapply(ports$Capacity, function(x) mean(x * clear_price11))
NetRev11 <- rev11 - MargCost 
ports$mean.rev11 <- ifelse(NetRev11 < 0, 0, NetRev11)

## Emissions
ports$emissions11 <- c(ifelse(ports$revenue11[1:quantile(marginal11, Quantile)] >= 0, 
                            ports$Capacity * ports$CO2e, 0), 1+quantile(marginal11, Quantile):41 == 0)



ports$emissions11[quantile(marginal11, Quantile)] <-    ((ports$CO2e[quantile(marginal11, Quantile)] *
                                                       ((ports$cumu[quantile(marginal11, Quantile)]))) 
                                                      - (ports$CO2e[quantile(marginal11, Quantile)] *
                                                        quantile(equil_quant11, Quantile)))

sum(ports$emissions11)

qplot(seq_along(supply_curve), supply_curve, geom = "step", xlab = "quantity", ylab = "price") +
  geom_vline(mapping = x_intercepts_calc11, xintercept = quantile(x_intercepts_calc11, Quantile))
  ggtitle("D1 Hour1")

hour <- 2

## MC sim of demand
x_intercepts_calc12 <- rnorm(runs,  D1_intT[hour], D1_intT[hour] * .03)
y_intercepts_calc12 <- -D1_mT * x_intercepts_calc12

##demand curves
demand_curves12 <- sapply(y_intercepts_calc12, function(x) x + D1_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant12 <- apply(demand_curves12, 2, function(x) which.min(abs(supply_curve - x)))
nets12 <- sapply(equil_quant12, function(x) ports$cumu - x)
marginal12 <- apply(nets12, 2, function(x) min(which(x > 0)))
clear_price12 <- ports$MC_part[marginal12]

## Revenue: quantile test
revenue12 <- ports$Capacity * quantile(clear_price12, Quantile) - ports$Capacity * ports$MC_part
ports$revenue12 <- ifelse(revenue12 < 0, 0, revenue12) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal12), Quantile)]
quantile(equil_quant12, .5)

Humboldt2 <- marginal12 == 23
Humboldt2.dispatched <- length(Humboldt2[Humboldt2 == TRUE])

HP2 <- marginal12 == 24
HP2.dispatched <- length(HP2[HP2 == TRUE])

HH.clear.prob.hr2 <- (Humboldt2.dispatched + HP2.dispatched) / runs

## Revenue: mean
rev12 <- sapply(ports$Capacity, function(x) mean(x * clear_price12))
NetRev12 <- rev12 - MargCost 
ports$mean.rev12 <- ifelse(NetRev12 < 0, 0, NetRev12)

## Emissions
ports$emissions12 <- c(ifelse(ports$revenue12[1:quantile(marginal12, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal12, Quantile):41 == 0)



ports$emissions12[quantile(marginal12, Quantile)] <-    ((ports$CO2e[quantile(marginal12, Quantile)] *
                                                            ((ports$cumu[quantile(marginal12, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal12, Quantile)] *
                                                              quantile(equil_quant12, Quantile)))

sum(ports$emissions12)

qplot(seq_along(supply_curve), supply_curve, geom = "step", xlab = "quantity", ylab = "price") +
  geom_vline(mapping = x_intercepts_calc12, xintercept = quantile(x_intercepts_calc12, Quantile)) +
  ggtitle("D1 Hour2")

hour <- 3

## MC sim of demand
x_intercepts_calc13 <- rnorm(runs,  D1_intT[hour], D1_intT[hour] * .03)
y_intercepts_calc13 <- -D1_mT * x_intercepts_calc13

##demand curves
demand_curves13 <- sapply(y_intercepts_calc13, function(x) x + D1_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant13 <- apply(demand_curves13, 2, function(x) which.min(abs(supply_curve - x)))
nets13 <- sapply(equil_quant13, function(x) ports$cumu - x)
marginal13 <- apply(nets13, 2, function(x) min(which(x > 0)))
clear_price13 <- ports$MC_part[marginal13]

hist(clear_price13)

## Revenue: quantile test
revenue13 <- ports$Capacity * quantile(clear_price13, Quantile) - ports$Capacity * ports$MC_part
ports$revenue13 <- ifelse(revenue13 < 0, 0, revenue13) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal13), Quantile)]
quantile(equil_quant13, .5)

Humboldt3 <- marginal13 == 23
Humboldt3.dispatched <- length(Humboldt3[Humboldt3 == TRUE])

HP3 <- marginal13 == 24
HP3.dispatched <- length(HP3[HP3 == TRUE])

HH.clear.prob.hr3 <- (Humboldt3.dispatched + HP3.dispatched) / runs

marginal13 == 40

## Revenue: mean
rev13 <- sapply(ports$Capacity, function(x) mean(x * clear_price13))
NetRev13 <- rev13 - MargCost 
ports$mean.rev13 <- ifelse(NetRev13 < 0, 0, NetRev13)

## Emissions
ports$emissions13 <- c(ifelse(ports$revenue13[1:quantile(marginal13, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal13, Quantile):41 == 0)


ports$emissions13[quantile(marginal13, Quantile)] <-    ((ports$CO2e[quantile(marginal13, Quantile)] *
                                                            ((ports$cumu[quantile(marginal13, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal13, Quantile)] *
                                                              quantile(equil_quant13, Quantile)))

sum(ports$emissions13)

qplot(seq_along(supply_curve), supply_curve, geom = "step", xlab = "quantity", ylab = "price") +
  geom_vline(mapping = x_intercepts_calc13, xintercept = quantile(x_intercepts_calc13, Quantile)) +
  ggtitle("D1 Hour3")

hour <- 4

## MC sim of demand
x_intercepts_calc14 <- rnorm(runs,  D1_intT[hour], D1_intT[hour] * .03)
y_intercepts_calc14 <- -D1_mT * x_intercepts_calc14

##demand curves
demand_curves14 <- sapply(y_intercepts_calc14, function(x) x + D1_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant14 <- apply(demand_curves14, 2, function(x) which.min(abs(supply_curve - x)))
nets14 <- sapply(equil_quant14, function(x) ports$cumu - x)
marginal14 <- apply(nets14, 2, function(x) min(which(x > 0)))
clear_price14 <- ports$MC_part[marginal14]

## Revenue: quantile test
revenue14 <- ports$Capacity * quantile(clear_price14, Quantile) - ports$Capacity * ports$MC_part
ports$revenue14 <- ifelse(revenue14 < 0, 0, revenue14) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal14), Quantile)]
quantile(equil_quant14, .5)

Humboldt4 <- marginal14 == 23
Humboldt4.dispatched <- length(Humboldt4[Humboldt4 == TRUE])

HP4 <- marginal14 == 24
HP4.dispatched <- length(HP4[HP4 == TRUE])

HH.clear.prob.hr4 <- (Humboldt4.dispatched + HP4.dispatched) / runs

## Revenue: mean
rev14 <- sapply(ports$Capacity, function(x) mean(x * clear_price14))
NetRev14 <- rev14 - MargCost 
ports$mean.rev14 <- ifelse(NetRev14 < 0, 0, NetRev14)

## Emissions
ports$emissions14 <- c(ifelse(ports$revenue14[1:quantile(marginal14, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal14, Quantile):41 == 0)



ports$emissions14[quantile(marginal14, Quantile)] <-    ((ports$CO2e[quantile(marginal14, Quantile)] *
                                                            ((ports$cumu[quantile(marginal14, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal14, Quantile)] *
                                                              quantile(equil_quant14, Quantile)))

sum(ports$emissions14)

qplot(seq_along(supply_curve), supply_curve, geom = "step", xlab = "quantity", ylab = "price") +
  geom_vline(mapping = x_intercepts_calc12, xintercept = quantile(x_intercepts_calc12, Quantile)) +
  ggtitle("D1 Hour4")

## Total Day 1
ports$total_day1 <- ports$revenue11 + ports$revenue12 + ports$revenue13 + ports$revenue14 - ports$fixed
ports$mean.rev_day1 <- ports$mean.rev11 + ports$mean.rev12 + ports$mean.rev13 + ports$mean.rev14 - ports$fixed
ports$emissions_day1 <- ports$emissions11 + ports$emissions12 + ports$emissions13 + ports$emissions14

##Consider Delete ##############
Day1 <- ports %>%
  group_by(Portf) %>%
  summarise(total = sum(total_day1),
            C02 = sum(emissions_day1)) 

sum(ports$emissions_day1) * 3

Day1

ports$UNIT.NAME[quantile((marginal11), Quantile)]
quantile(equil_quant11, .15)

ports$UNIT.NAME[quantile((marginal12), Quantile)]
quantile(equil_quant12, .9)

ports$UNIT.NAME[quantile((marginal13), Quantile)]
quantile(equil_quant13, .3)

ports$UNIT.NAME[quantile((marginal14), Quantile)]
quantile(equil_quant14, .4)

##Day1rev <- ports %>%
##  group_by(Portf) %>%
##  summarise(total = sum(rev_day1))

## Day1rev

Day1CO2 <- ports %>%
  group_by(Portf) %>%
  summarize(CO2 = sum(emissions_day1))

Day1CO2

############### Day 2

######Old stuff from Year 2
SouthSupply <- filter(ports, Location == "South")
NorthSupply <- filter(ports, Location == "North")

MargCostS <- SouthSupply$Capacity * SouthSupply$MC_tot
MargCostN <- NorthSupply$Capacity * NorthSupply$MC_tot
test_dataS <- SouthSupply$Capacity
test_dataN <- NorthSupply$Capacity
#######

D2_intN <- c(4577, 4161, 6134, 4843)
D2_intS <- c(6709, 7891, 9044, 8212)
D2_intT <- D2_intN + D2_intS


D2_mN <- c(-2.54, -2.59, -2.91, -2.52)
D2_mS <- c(-2.38, -2.81, -2.86, -2.51)
D2_mT <- D2_mN + D2_mS

hour <- 1

## MC sim of demand
x_intercepts_calc21 <- rnorm(runs,  D2_intT[hour], D2_intT[hour] * .03)
y_intercepts_calc21 <- -D2_mT * x_intercepts_calc21

##demand curves
demand_curves21 <- sapply(y_intercepts_calc21, function(x) x + D2_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant21 <- apply(demand_curves21, 2, function(x) which.min(abs(supply_curve - x)))
nets21 <- sapply(equil_quant21, function(x) ports$cumu - x)
marginal21 <- apply(nets21, 2, function(x) min(which(x > 0)))
clear_price21 <- ports$MC_part[marginal21]

## Revenue: quantile test
revenue21 <- ports$Capacity * quantile(clear_price21, Quantile) - ports$Capacity * ports$MC_part
ports$revenue21 <- ifelse(revenue21 < 0, 0, revenue21) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal21), Quantile)]

## Revenue: mean
rev21 <- sapply(ports$Capacity, function(x) mean(x * clear_price21))
NetRev21 <- rev21 - MargCost 
ports$mean.rev21 <- ifelse(NetRev21 < 0, 0, NetRev21)

## Emissions
ports$emissions21 <- c(ifelse(ports$revenue21[1:quantile(marginal21, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal21, Quantile):41 == 0)



ports$emissions21[quantile(marginal21, Quantile)] <-    ((ports$CO2e[quantile(marginal21, Quantile)] *
                                                            ((ports$cumu[quantile(marginal21, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal21, Quantile)] *
                                                              quantile(equil_quant21, Quantile)))

sum(ports$emissions21)

hour <- 2

## MC sim of demand
x_intercepts_calc22 <- rnorm(runs,  D2_intT[hour], D2_intT[hour] * .03)
y_intercepts_calc22 <- -D2_mT * x_intercepts_calc22

##demand curves
demand_curves22 <- sapply(y_intercepts_calc22, function(x) x + D2_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant22 <- apply(demand_curves22, 2, function(x) which.min(abs(supply_curve - x)))
nets22 <- sapply(equil_quant22, function(x) ports$cumu - x)
marginal22 <- apply(nets22, 2, function(x) min(which(x > 0)))
clear_price22 <- ports$MC_part[marginal22]

## Revenue: quantile test
revenue22 <- ports$Capacity * quantile(clear_price22, Quantile) - ports$Capacity * ports$MC_part
ports$revenue22 <- ifelse(revenue22 < 0, 0, revenue22) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal22), Quantile)]

## Revenue: mean
rev22 <- sapply(ports$Capacity, function(x) mean(x * clear_price22))
NetRev22 <- rev22 - MargCost 
ports$mean.rev22 <- ifelse(NetRev22 < 0, 0, NetRev22)

## Emissions
ports$emissions22 <- c(ifelse(ports$revenue22[1:quantile(marginal22, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal22, Quantile):41 == 0)



ports$emissions22[quantile(marginal22, Quantile)] <-    ((ports$CO2e[quantile(marginal22, Quantile)] *
                                                            ((ports$cumu[quantile(marginal22, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal22, Quantile)] *
                                                              quantile(equil_quant22, Quantile)))

sum(ports$emissions22)


hour <- 3

## MC sim of demand
x_intercepts_calc23 <- rnorm(runs,  D2_intT[hour], D2_intT[hour] * .03)
y_intercepts_calc23 <- -D2_mT * x_intercepts_calc23

##demand curves
demand_curves23 <- sapply(y_intercepts_calc23, function(x) x + D2_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant23 <- apply(demand_curves23, 2, function(x) which.min(abs(supply_curve - x)))
nets23 <- sapply(equil_quant23, function(x) ports$cumu - x)
marginal23 <- apply(nets23, 2, function(x) min(which(x > 0)))
clear_price23 <- ports$MC_part[marginal23]

## Revenue: quantile test
revenue23 <- ports$Capacity * quantile(clear_price23, Quantile) - ports$Capacity * ports$MC_part
ports$revenue23 <- ifelse(revenue23 < 0, 0, revenue23) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal23), Quantile)]

## Revenue: mean
rev23 <- sapply(ports$Capacity, function(x) mean(x * clear_price23))
NetRev23 <- rev23 - MargCost 
ports$mean.rev23 <- ifelse(NetRev23 < 0, 0, NetRev23)

## Emissions
ports$emissions23 <- c(ifelse(ports$revenue23[1:quantile(marginal23, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal23, Quantile):41 == 0)



ports$emissions23[quantile(marginal23, Quantile)] <-    ((ports$CO2e[quantile(marginal23, Quantile)] *
                                                            ((ports$cumu[quantile(marginal23, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal23, Quantile)] *
                                                              quantile(equil_quant23, Quantile)))

sum(ports$emissions23)


hour <- 4

## MC sim of demand
x_intercepts_calc24 <- rnorm(runs,  D2_intT[hour], D2_intT[hour] * .03)
y_intercepts_calc24 <- -D2_mT * x_intercepts_calc24

##demand curves
demand_curves24 <- sapply(y_intercepts_calc24, function(x) x + D2_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant24 <- apply(demand_curves24, 2, function(x) which.min(abs(supply_curve - x)))
nets24 <- sapply(equil_quant24, function(x) ports$cumu - x)
marginal24 <- apply(nets24, 2, function(x) min(which(x > 0)))
clear_price24 <- ports$MC_part[marginal24]

## Revenue: quantile test
revenue24 <- ports$Capacity * quantile(clear_price24, Quantile) - ports$Capacity * ports$MC_part
ports$revenue24 <- ifelse(revenue24 < 0, 0, revenue24) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal24), Quantile)]

## Revenue: mean
rev24 <- sapply(ports$Capacity, function(x) mean(x * clear_price24))
NetRev24 <- rev24 - MargCost 
ports$mean.rev24 <- ifelse(NetRev24 < 0, 0, NetRev24)

## Emissions
ports$emissions24 <- c(ifelse(ports$revenue24[1:quantile(marginal24, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal24, Quantile):41 == 0)



ports$emissions24[quantile(marginal24, Quantile)] <-    ((ports$CO2e[quantile(marginal24, Quantile)] *
                                                            ((ports$cumu[quantile(marginal24, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal24, Quantile)] *
                                                              quantile(equil_quant24, Quantile)))

sum(ports$emissions24)

## Total Day 2
ports$total_day2 <- ports$revenue21 + ports$revenue22 + ports$revenue23 + ports$revenue24 - ports$fixed
ports$mean.rev_day2 <- ports$mean.rev21 + ports$mean.rev22 + ports$mean.rev23 + ports$mean.rev24 - ports$fixed
ports$emissions_day2 <- ports$emissions21 + ports$emissions22 + ports$emissions23 + ports$emissions24

##Consider Delete ##############
Day2 <- ports %>%
  group_by(Portf) %>%
  summarise(total = sum(total_day1))

Day2

##Day2rev <- ports %>%
##  group_by(Portf) %>%
##  summarise(total = sum(rev_day1))

##Day2rev

Day2CO2 <- ports %>%
  group_by(Portf) %>%
  summarize(CO2 = sum(emissions_day2))

Day2CO2

####Day 3

D3_intN <- c(4301, 4455, 7453, 4996)
D3_intS <- c(7432, 9988, 10170, 9111)
D3_intT <- D3_intN + D3_intS

D3_mN <- c(-2.85, -2.69, -2.93, -2.92)
D3_mS <- c(-2.48, -2.77, -2.87, -2.60)
D3_mT <- D3_mN + D3_mS

hour <- 1

## MC sim of demand
x_intercepts_calc31 <- rnorm(runs,  D3_intT[hour], D3_intT[hour] * .03)
y_intercepts_calc31 <- -D3_mT * x_intercepts_calc31

##demand curves
demand_curves31 <- sapply(y_intercepts_calc31, function(x) x + D3_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant31 <- apply(demand_curves31, 2, function(x) which.min(abs(supply_curve - x)))
nets31 <- sapply(equil_quant31, function(x) ports$cumu - x)
marginal31 <- apply(nets31, 2, function(x) min(which(x > 0)))
clear_price31 <- ports$MC_part[marginal31]

## Revenue: quantile test
revenue31 <- ports$Capacity * quantile(clear_price31, Quantile) - ports$Capacity * ports$MC_part
ports$revenue31 <- ifelse(revenue31 < 0, 0, revenue31) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal31), Quantile)]

## Revenue: mean
rev31 <- sapply(ports$Capacity, function(x) mean(x * clear_price31))
NetRev31 <- rev31 - MargCost 
ports$mean.rev31 <- ifelse(NetRev31 < 0, 0, NetRev31)

## Emissions
ports$emissions31 <- c(ifelse(ports$revenue31[1:quantile(marginal31, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal31, Quantile):41 == 0)



ports$emissions31[quantile(marginal31, Quantile)] <-    ((ports$CO2e[quantile(marginal31, Quantile)] *
                                                            ((ports$cumu[quantile(marginal31, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal31, Quantile)] *
                                                              quantile(equil_quant31, Quantile)))

sum(ports$emissions31)


hour <- 2

## MC sim of demand
x_intercepts_calc32 <- rnorm(runs,  D3_intT[hour], D3_intT[hour] * .03)
y_intercepts_calc32 <- -D3_mT * x_intercepts_calc32

##demand curves
demand_curves32 <- sapply(y_intercepts_calc32, function(x) x + D3_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant32 <- apply(demand_curves32, 2, function(x) which.min(abs(supply_curve - x)))
nets32 <- sapply(equil_quant32, function(x) ports$cumu - x)
marginal32 <- apply(nets32, 2, function(x) min(which(x > 0)))
clear_price32 <- ports$MC_part[marginal32]

## Revenue: quantile test
revenue32 <- ports$Capacity * quantile(clear_price32, Quantile) - ports$Capacity * ports$MC_part
ports$revenue32 <- ifelse(revenue32 < 0, 0, revenue32) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal32), Quantile)]

## Revenue: mean
rev32 <- sapply(ports$Capacity, function(x) mean(x * clear_price32))
NetRev32 <- rev32 - MargCost 
ports$mean.rev32 <- ifelse(NetRev32 < 0, 0, NetRev32)

## Emissions
ports$emissions32 <- c(ifelse(ports$revenue32[1:quantile(marginal32, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal32, Quantile):41 == 0)



ports$emissions32[quantile(marginal32, Quantile)] <-    ((ports$CO2e[quantile(marginal32, Quantile)] *
                                                            ((ports$cumu[quantile(marginal32, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal32, Quantile)] *
                                                              quantile(equil_quant32, Quantile)))

sum(ports$emissions32)


hour <- 3

## MC sim of demand
x_intercepts_calc33 <- rnorm(runs,  D3_intT[hour], D3_intT[hour] * .03)
y_intercepts_calc33 <- -D3_mT * x_intercepts_calc33

##demand curves
demand_curves33 <- sapply(y_intercepts_calc33, function(x) x + D3_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant33 <- apply(demand_curves33, 2, function(x) which.min(abs(supply_curve - x)))
nets33 <- sapply(equil_quant33, function(x) ports$cumu - x)
marginal33 <- apply(nets33, 2, function(x) min(which(x > 0)))
clear_price33 <- ports$MC_part[marginal33]

## Revenue: quantile test
revenue33 <- ports$Capacity * quantile(clear_price33, Quantile) - ports$Capacity * ports$MC_part
ports$revenue33 <- ifelse(revenue33 < 0, 0, revenue33) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal33), Quantile)]

## Revenue: mean
rev33 <- sapply(ports$Capacity, function(x) mean(x * clear_price33))
NetRev33 <- rev33 - MargCost 
ports$mean.rev33 <- ifelse(NetRev33 < 0, 0, NetRev33)

## Emissions
ports$emissions33 <- c(ifelse(ports$revenue33[1:quantile(marginal33, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal33, Quantile):41 == 0)



ports$emissions33[quantile(marginal33, Quantile)] <-    ((ports$CO2e[quantile(marginal33, Quantile)] *
                                                            ((ports$cumu[quantile(marginal33, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal33, Quantile)] *
                                                              quantile(equil_quant33, Quantile)))

sum(ports$emissions33)



hour <- 4

## MC sim of demand
x_intercepts_calc34 <- rnorm(runs,  D3_intT[hour], D3_intT[hour] * .03)
y_intercepts_calc34 <- -D3_mT * x_intercepts_calc34

##demand curves
demand_curves34 <- sapply(y_intercepts_calc34, function(x) x + D3_mT[hour] * 1:22050)

### equilibrium quantity and price

equil_quant34 <- apply(demand_curves34, 2, function(x) which.min(abs(supply_curve - x)))
nets34 <- sapply(equil_quant34, function(x) ports$cumu - x)
marginal34 <- apply(nets34, 2, function(x) min(which(x > 0)))
clear_price34 <- ports$MC_part[marginal34]

## Revenue: quantile test
revenue34 <- ports$Capacity * quantile(clear_price34, Quantile) - ports$Capacity * ports$MC_part
ports$revenue34 <- ifelse(revenue34 < 0, 0, revenue34) 

##Quantile dispatch 
ports$UNIT.NAME[quantile((marginal34), Quantile)]

## Revenue: mean
rev34 <- sapply(ports$Capacity, function(x) mean(x * clear_price34))
NetRev34 <- rev34 - MargCost 
ports$mean.rev34 <- ifelse(NetRev34 < 0, 0, NetRev34)

## Emissions
ports$emissions34 <- c(ifelse(ports$revenue34[1:quantile(marginal34, Quantile)] >= 0, 
                              ports$Capacity * ports$CO2e, 0), 1+quantile(marginal34, Quantile):41 == 0)



ports$emissions34[quantile(marginal34, Quantile)] <-    ((ports$CO2e[quantile(marginal34, Quantile)] *
                                                            ((ports$cumu[quantile(marginal34, Quantile)]))) 
                                                         - (ports$CO2e[quantile(marginal34, Quantile)] *
                                                              quantile(equil_quant34, Quantile)))

sum(ports$emissions34)


#### Total Day 3

ports$total_day3 <- ports$revenue31 + ports$revenue32 + ports$revenue33 + ports$revenue34 - ports$fixed
ports$mean.rev_day3 <- ports$mean.rev31 + ports$mean.rev32 + ports$mean.rev33 + ports$mean.rev34 - ports$fixed
ports$emissions_day3 <- ports$emissions31 + ports$emissions32 + ports$emissions33 + ports$emissions34

Day3 <- ports %>%
  group_by(Portf) %>%
  summarise(gross = sum(total_day3))

Day3

##Day3rev <- ports %>%
##  group_by(Portf) %>%
##  summarise(totald3 = sum(rev_day3))

Day3CO2 <- ports %>%
  group_by(Portf) %>%
  summarize(CO2 = sum(emissions_day3))

Day3CO2

#####Year totals
## CO2 in one game
Year3CO2 <- ports %>%
  group_by(Portf) %>%
  summarize(CO2 = sum(emissions_day1, emissions_day2, emissions_day3))

Year3CO2
sum(Year3CO2$CO2)

## Revenue by portfolio

Year3rev <- ports %>%
  group_by(Portf) %>%
  summarize(Quantile.Revenue = sum(total_day1, total_day2, total_day3),
      Mean.Revenue = sum(mean.rev_day1, mean.rev_day2, mean.rev_day3),
      CO2 = sum(emissions_day1, emissions_day2, emissions_day3))
 
  
      
      
          #Net.Revenue = Revenue - CO2 * CO2_cost_test)
      
##### CO2 cost estimate
### emissions permits
permits_1game <- c(7500, 7500, 7500, 7500, 7500, 0, 3500)
permits_total <- sum(permits_1game) * 3 + 62000

#### Portfolio Value

Year3rev$CO2.permits <- c(7500, 7500, 7500, 7500, 7500, 0, 3500)
Year3rev$CO2.Cost <- cbind((Year3rev$CO2 - Year3rev$CO2.permits) * (CO2_cost - Actual.CO2.Cost))
Year3rev$Mod.Rev <- Year3rev$CO2.Cost + Year3rev$Quantile.Revenue
Year3rev$bid <- Year3rev$Quantile.Revenue + Year3rev$Quantile.Revenue[3] - 114000


### emissions
CO2_total <- sum(Year3CO2$CO2) * 3

## if this is close to zero, the market is near equilibrium. 
## bilateral contracts may prevent this outcome
permit_gap <- permits_total - CO2_total

##CO2ports <- Year3CO2 %>%
  ##cbind(permits)
  
##Gap


##CO2ports$gap <- CO2ports$permits - CO2ports$CO2 

##CO2ports
print(CO2_cost)

Year3rev
CO2_total
permit_gap



