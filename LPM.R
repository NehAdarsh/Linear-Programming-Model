#linear programmiing in R
install.packages("lpSolve")
library(lpSolve)

#____________________________________
#    Item            |  Cost ($)     |
#____________________________________
#  Pressure Washer   |    330        |
#  Go-Kart           |    370        |
#  Generator         |    410        |
#  Water Pumps       |    635        |
#____________________________________

#____________________________________
#    Item            |  Decision Var |
#____________________________________
#  Pressure Washer   |    x1         |
#  Go-Kart           |    x2         |
#  Generator         |    x3         |
#  Water Pumps       |    x4         |
#____________________________________

#_______________________________________________________________________________
#    Item            |  Cost ($)            |   Selling Price   |   Profit      |
#_______________________________________________________________________________
#  Pressure Washer   |    330               |     499.99        |    169.99     |
#  Go-Kart           |    370               |     729.99        |    359.99     |
#  Generator         |    410               |     700.99        |    290.99     |
#  Water Pumps       |    127  (635/5)      |     269.99        |    142.99     |
#_______________________________________________________________________________
#   Total            |    1237              |     2200.96       |    963.96     |
#_______________________________________________________________________________

#__________________________________________________________________
# Maximize Profit (Z) = 169.99x1 + 359.99x2 + 290.99x3 + 142.99x4  |
#__________________________________________________________________

#assigning decision variables
x = c('x1', 'x2', 'x3', 'x4')

#Objective parameters
c = c(169.99, 359.99, 290.99, 142.99)
objective = c

#Constraints

#_____________________________________________________________________________________________________________________
#   Constraint     |     Constraints values                          |   Calculated Constraint Values                 |
#_____________________________________________________________________________________________________________________
#  Constraint 1    |    330x1 + 370x2 + 410x3 + 127x4 ≤ 170,000      |     330x1 + 370x2 + 410x3 + 127x4 ≤ 170,000    |
#  Constraint 2    |    5x1 + 8x2 + 5x3 + (1/5)(1/4)5x4 ≤ (82)(30)   |     5x1 + 8x2 + 5x3 + 0.25x4 ≤ 2460            |
#  Constraint 3    |    5x1 + 8x2 ≥ (82)(30)(0.30)                   |     5x1 + 8x2 ≥ 738                            |
#  Constraint 4    |    x3 ≥ 2x4                                     |     x3 - 2x4 >= 0                              |
#_____________________________________________________________________________________________________________________


const1 = c(330, 370, 410, 127)
const2 = c(5,8,5,0.25)
const3 = c(5,8,0,0)
const4 = c(0,0,1,-2)

constraints = matrix(c(const1, const2, const3, const4), 
                     ncol = 4, byrow = T)
#constraints inequality types
directions = c('<=', '<=', '>=', '>=')

#constraints RHS
b = c(170000, 2460, 738, 0)

#Solving the model
lm_ = lp("max", objective.in = objective, const.mat = constraints, const.dir = directions, const.rhs = b, 
                 compute.sens = T)
lm_

Linear_prog = lp("max", objective.in = objective, const.mat = constraints, const.dir = directions, const.rhs = b, 
         all.int = T, compute.sens = T)
Linear_prog

#Individual solutions
data_frame1 = data.frame("Product" = x, "Optimal Decisions" = round(Linear_prog$solution, 0))
data_frame1

# Printing it out:
cat("The optimal solution is:", Linear_prog$solution, "\nAnd the optimal objective function value is:", Linear_prog$objval)


Linear_prog$solution   #decision variables values
Linear_prog$objective   #objective parameters
Linear_prog$duals     #duals of constraints and reduced cost of variables
Linear_prog$objval    #optimized Z value



#sensitivity analysis results
Linear_prog$duals.from
Linear_prog$duals.to
Linear_prog$sens.coef.from
Linear_prog$sens.coef.to

rbind(Linear_prog$duals,Linear_prog$duals.from,Linear_prog$duals.to)


#more analysis 
install.packages('linprog')
library(linprog)

val = solveLP(cvec = objective,
        bvec = b,
        Amat = constraints,
        maximum = TRUE,
        const.dir = directions)
val


#summary - Linear Program Function
summary(Linear_prog)








