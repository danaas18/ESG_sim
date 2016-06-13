#ESG production simulation
##I'm doing most of the experimentation in Jupyter. When something works I'll add it here.

## load packages, set seed so outputs can be reproduced and determine number of demand curves to calculate
using DataFrames
using Distributions
srand(246)
draws = 1000

#Define user assumptions: price of carbon (tax in this version), quantile of demand to run sensitivities
CO2_cost = 10
quantile = .5

#Read the file, delete the MC_tot variable, create new MC variable, sort in order and calculate cumulative capacity
#This should change the generator stack based on changse in CO2 price. Could add other factors like fuel cost changes
port = readtable("/home/dan/MEGAsync/SummerCoding/ESG_sim/ESGPortfolios_forJJulia.csv")
port = delete!(port,:MC_tot)
port[:MC] = port[:MC_part] .+ (port[:CO2e] .* port[:CO2cost])
sort!(port[:MC])
port[:cumu] = cumsum(port[:Capacity])

#defining demand for each hour.
##note: ideally, a function could import any arrays of intercepts and slopes

D1_intN = [4184, 3842, 7986, 5209]
D1_intS = [7176, 9874, 9901, 9760]
D1_intT = D1_intN + D1_intS

D1_mN = [-2.34, -2.58, -2.54, -2.67]
D1_mS = [-2.03, -2.80, -2.49, -2.41]
D1_mT = D1_mN + D1_mS

x_intercepts = rand(Normal(D1_intT[1], D1_intT[1] * .03), draws)
y_intercepts = x_intercepts .* -D1_mT[1]

#calculating demand curves
##this seems to do what I want. It creates 1000 vectors of length 22050. However, it does not create a matrix???? ndims(demand_curves) returns 1???
demand_curves = map(x -> x + D1_mT[1] * collect(1:maximum(port[:cumu])), y_intercepts)
