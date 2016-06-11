#ESG production simulation
##I'm doing most of the experimentation in Jupyter. When something works I'll add it here. 

using DataFrames
using Distributions

#Define user assumptions: price of carbon (tax in this version), quantile of demand

CO2_cost = 10
quantile = .5

#Read the file, delete the MC_tot variable, create new MC variable, sort in order and calculate cumulative capacity
port = readtable("/home/dan/SpiderOak Hive/EEM/ESGPortfolios_forR.csv")
port = delete!(port,:MC_tot)
port[:MC] = port[:MC_part] .+ (port[:CO2e] .* port[:CO2cost])
sort!(port[:MC])
port[:cumu] = cumsum(port[:Capacity])

port

#defining demand for each hour.
##note: ideally, this would calculate

D1_intN = [4184, 3842, 7986, 5209]
D1_intS = [7176, 9874, 9901, 9760]
D1_intT = D1_intN + D1_intS

D1_mN = [-2.34, -2.58, -2.54, -2.67]
D1_mS = [-2.03, -2.80, -2.49, -2.41]
D1_mT = D1_mN + D1_mS
