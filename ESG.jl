#ESG production simulation
##I'm doing most of the experimentation in Jupyter. When something works I'll add it here.

## load packages, set seed so outputs can be reproduced and determine number of demand curves to calculate
using DataFrames
using Distributions
using DataArrays

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

#creating a supply curve
supply_curve = rep(port[:MC], port[:Capacity])

#defining demand for each hour.
##note: ideally, a function could import any arrays of intercepts and slopes
##change these to add a given array of slopes and intercepts

D1_intN = [4184, 3842, 7986, 5209]
D1_intS = [7176, 9874, 9901, 9760]
D1_intT = D1_intN + D1_intS

D1_mN = [-2.34, -2.58, -2.54, -2.67]
D1_mS = [-2.03, -2.80, -2.49, -2.41]
D1_mT = D1_mN + D1_mS

##I'll want the function(s) to call a given hour, and return: marginal units, clearing prices, and revenues

x_intercepts = []
for i in collect(1:length(D1_intT))
  x_intercepts_add = rand(Normal(D1_intT[i], D1_intT[i] * .03), draws)
  push!(x_intercepts, x_intercepts_add)
end

y_intercepts = []
for i in collect(1:length(D1_mT))
  y_intercepts_add = x_intercepts[i] .* -D1_mT[i]
  push!(y_intercepts, y_intercepts_add)
end

demand_curves = []
for i in collect(1:length(y_intercepts))
  demand_curves_add = map(x -> x + D1_mT[i] * collect(1:maximum(port[:cumu])), y_intercepts[i])
  push!(demand_curves, demand_curves_add)
end

equil_quantities = []
for i in collect(1:length(demand_curves))
  equil_calc = map(x -> indmin(abs(supply_curve - x)), demand_curves[i])
  push!(equil_quantities, equil_calc)
end

#############start here
nets = []
for i in collect(1:length(equil_quantities))
  nets_calc = map(x -> port[:cumu] - x, equil_quantities[i])
  push!(nets, nets_calc)
end

nets

marginal_unit = []
for i in collect(1:length(nets))
  marginal_calc = map(x -> indmin(abs(x)),nets[i])
  push!(marginal_unit, marginal_calc)
end

clear_price = []
for i in collect(1:length(marginal_unit))
  clear_price_calc = map(x -> port[:MC][x], marginal_unit[i])
  push!(clear_price, clear_price_calc)
end


revenue = []
for i in collect(1:length(marginal_unit))
  revenue_calc = port[:Capacity] .* quantile!(clear_price[i], quantile) .- port[:Capacity] .* port[:MC]
  push!(revenue, revenue_calc)
end

<<<<<<< HEAD
revenue

###start here
## get rid of negative revenues
=======
###start here
## get rid of negative revenues 
>>>>>>> f583112582fccd0832b65a9afd013d518d1fa7e6


#### old stuff

x_intercepts = rand(Normal(D1_intT[3], D1_intT[3] * .03), draws)
y_intercepts = x_intercepts .* -D1_mT[3]
<<<<<<< HEAD
=======

######### Start here
demand_curves

x_intercepts = rand(Normal(D1_intT[1], D1_intT[1] * .03), draws)
y_intercepts = x_intercepts .* -D1_mT[1]
>>>>>>> 314727236ede4e52a1fc4f06dfee0ee384190fd8
=======
>>>>>>> f583112582fccd0832b65a9afd013d518d1fa7e6

#calculating demand curves
##this seems to do what I want. It creates 1000 vectors of length 22050. However, it does not create a matrix???? ndims(demand_curves) returns 1???
demand_curves = map(x -> x + D1_mT[3] * collect(1:maximum(port[:cumu])), y_intercepts)
demand_curves

#calculating equilibrium quanitity on supply curves
equil_quantities = map(x -> indmin(abs(supply_curve - x)), demand_curves)
equil_quantities
#calculating marginal unit and clearing price
nets1 = map(x -> port[:cumu] - x, equil_quantities)
marginal_unit = map(x -> indmin(abs(x)), nets1)
clear_price = map(x -> port[:MC][x], marginal_unit)

revenue = port[:Capacity] .* quantile!(clear_price, quantile) .- port[:Capacity] .* port[:MC]
revenue = revenue[revenue .>= 0]
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======


port
>>>>>>> 3d1be4689fe0ecdf872c096ad4b5de47b66f608e
>>>>>>> 314727236ede4e52a1fc4f06dfee0ee384190fd8
=======
>>>>>>> f583112582fccd0832b65a9afd013d518d1fa7e6
