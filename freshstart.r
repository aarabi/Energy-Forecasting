library(gdata)
library(lpSolveAPI)
library(ggplot2)


mydata = read.xls("pvdata3.xls", sheet = 1, header = TRUE)
demand <- mydata$demand
pv_irradiance <- mydata$pv
wind_speed <- mydata$wind
hydro_rate <- mydata$hydro
sun_hours <- mydata$sunhours[1:4]
grid_connected <- mydata$grid[1:4]
cost_pv <- mydata$cost[1:4]

#lpsolve algorithm for sizing the system

#assigning cost of installation
Installation_cost <- c(4183,2213,2936,400,1000,6599)
Consumer_price <- c(max(cost_pv),0.18,0.10,0.2656,0.10,0.2924)

#sizing the PV system
pv_capacity  <- max(demand)/min(sun_hours)
pv_capacity <- pv_capacity/0.72
pv_capacity <- pv_capacity*2
no_of_pvpanels <- pv_capacity/0.25

#sizing the wind turbine
wind_capacity <- ceiling(max(demand))
size_turbine <- 500
no_wind_turbines <- wind_capacity/size_turbine

if(min(sun_hours) <= 2)
{
  wind_capacity = wind_capacity*2
}

wind_turbine_diameter <- function(x)
{
  
  if(x<=200){
    return(29)
  } else if(x>200 && x<=400) {
    return(35)
    
  } else if(x>400 && x<=500) {
    return(47)
    
  } else if(x>500 && x<=600) {
    return(46)
    
  } else {
    
    return(54)
  }
}

#hourly output rates for each power source
pv_output <- pv_capacity*pv_irradiance*0.9*0.98
wind_output <- 0.625*3.14*0.001*no_wind_turbines*((wind_turbine_diameter(size_turbine)/2)^2)*wind_speed
hydro_output <- 0.75*hydro_rate*9.8*10
prices <-  c(min(cost_pv),0.18,0.12,0.2156,0.10,0.2424)
optimization <- matrix(data = NA, nrow = 96, ncol = 6, byrow = FALSE,
                       dimnames =NULL)
#Looping the entire optimization for each of the 24 hours
for (i in 1:96)
{
  #creating an object for solving the mixed integer problem
  # each hour in 24 hours has a constraint to meet supply with demand
  # the number of variables is 6 (pv,wind,hydro,generator,battery,grid)
  lprec <- make.lp(1,6)
  
  #assuming standard price through out the day in order of (pv,wind,hydro,generator,battery,grid)
  
  
  set.column (lprec,1, c(1))
  set.column (lprec,2, c(1))
  set.column (lprec,3, c(1))
  set.column (lprec,4, c(1))
  set.column (lprec,5, c(1))
  set.column (lprec,6, c(1))
  
  set.objfn (lprec,   c(min(cost_pv),0.18,0.10,0.2156,0.10,0.2424))
  set.constr.type(lprec,c(">="))
  rowNames <- c("hour1")
  colNames<- c("pv","wind","hydro","generator","battery","grid")
  dimnames(lprec) <- list(rowNames,colNames)
  set.bounds(lprec,lower=pv_output[i],columns =c(1))
  set.bounds(lprec,lower=wind_output[i],columns =c(2))
  set.bounds(lprec,lower=hydro_output[i],columns =c(3))
 
#   set.bounds(lprec,upper=pv_output[i],columns =c(1))
#   set.bounds(lprec,upper=wind_output[i],columns =c(2))
#   set.bounds(lprec,upper=hydro_output[i],columns =c(3))
  remaining_supply = wind_output[i]+pv_output[i]+hydro_output[i]
remaining_supply = demand[i] - remaining_supply 

   if(grid_connected[1] == "0")
  {
    set.bounds(lprec,upper=max(0),columns =c(6))
    if(remaining_supply > 0)
    
      {
    set.bounds(lprec,lower=(remaining_supply)/2,columns =c(5))
    set.bounds(lprec,lower=(remaining_supply)/2,columns =c(4))
    
    }
    else
    {
      set.bounds(lprec,upper=max(0),columns =c(5))
      set.bounds(lprec,upper=max(0),columns =c(4))
      
    }
  } 
  else{
  if(remaining_supply > 0)
  {
    set.bounds(lprec,lower=(remaining_supply)/3,columns =c(4))
    set.bounds(lprec,lower=(remaining_supply)/3,columns =c(5))
    set.bounds(lprec,lower=(remaining_supply)/3,columns =c(6))
    
  }
  else
  {
    set.bounds(lprec,upper=max(0),columns =c(6))
    set.bounds(lprec,upper=max(0),columns =c(5))
    set.bounds(lprec,upper=max(0),columns =c(4))
    
  }
  }
  set.rhs(lprec,demand[i])
  solve(lprec)
  get.objective(lprec)
  optimization[i,] <- get.variables(lprec)

}


optimization <-  cbind2(optimization,demand) 
optimization <- cbind2(optimization,1:96)

rownames(optimization) <- c("hour1","hour2","hour3","hour4","hour5","hour6","hour7","hour8","hour9",
                            "hour10","hour11","hour12","hour13","hour14","hour15","hour16","hour17","hour18",
                            "hour19","hour20","hour21","hour22","hour23","hour24",
                            "hour1","hour2","hour3","hour4","hour5","hour6","hour7","hour8","hour9",
                            "hour10","hour11","hour12","hour13","hour14","hour15","hour16","hour17","hour18",
                            "hour19","hour20","hour21","hour22","hour23","hour24",
                            "hour1","hour2","hour3","hour4","hour5","hour6","hour7","hour8","hour9",
                            "hour10","hour11","hour12","hour13","hour14","hour15","hour16","hour17","hour18",
                            "hour19","hour20","hour21","hour22","hour23","hour24",
                            "hour1","hour2","hour3","hour4","hour5","hour6","hour7","hour8","hour9",
                            "hour10","hour11","hour12","hour13","hour14","hour15","hour16","hour17","hour18",
                            "hour19","hour20","hour21","hour22","hour23","hour24")

colnames(optimization) <- c("pv","wind","hydro","generator","battery","grid","demand","hours")

print(optimization)
plots <- list()

demandplot <- as.data.frame(matrix(0,96,2))
demandplot$demand <- optimization[,7]
demandplot$hours <- optimization[1:24,8]
print(plots[[1]] <-ggplot(demandplot, aes(demandplot)) +
  geom_line(aes(x= hours[1:24], y= demand[1:24], color = "Demand in Summer"),)  +
  geom_line(aes(x= hours[25:48], y= demand[25:48], color = "Demand in Fall"),)  +
  geom_line(aes(x= hours[49:72], y= demand[49:72], color = "Demand in Winter"),)  +
  geom_line(aes(x= hours[73:96], y= demand[73:96], color = "Demand in Spring"),)  +
  xlab("hours") +
  ylab("demand")+
  theme(legend.position="right",legend.title=element_blank()))
 

demandplot3 <- as.data.frame(matrix(0,96,8))
demandplot3$demand <- optimization[,7]
demandplot3$pv <- optimization[,1]
demandplot3$wind <- optimization[,2]
demandplot3$hours <- optimization[,8]
demandplot3$hydro <- optimization[,3]
demandplot3$nr <- optimization[,4] + optimization[,5] + optimization[,6]



print(plots[[2]] <-ggplot(demandplot3, aes(demandplot3)) +
  geom_line(aes(x= hours[1:24], y= demand[1:24], color = "Demand in Summer"),)  +
  geom_line(aes(x= hours[1:24], y= pv[1:24], color = "PV Supply"),)  +
  geom_line(aes(x= hours[1:24], y= wind[1:24], color = "Wind Supply"),)  +
  geom_line(aes(x= hours[1:24], y= nr[1:24], color = "Non-RE Supply"),)  +
  geom_line(aes(x= hours[1:24], y= hydro[1:24], color = "Hydro Supply"),)  +
  xlab("hours") +
  ylab("demand & Supply")+
theme(legend.position="right",legend.title=element_blank()))

print(plots[[3]] <-ggplot(demandplot3, aes(demandplot3)) +
  geom_line(aes(x= hours[25:48], y= demand[25:48], color = "Demand in Fall"),)  +
  geom_line(aes(x= hours[25:48], y= pv[25:48], color = "PV Supply"),)  +
  geom_line(aes(x= hours[25:48], y= wind[25:48], color = "Wind Supply"),)  +
  geom_line(aes(x= hours[25:48], y= nr[25:48], color = "Non-RE Supply"),)  +
  geom_line(aes(x= hours[25:48], y= hydro[25:48], color = "Hydro Supply"),)  +
  xlab("hours") +
  ylab("demand & Supply")+
theme(legend.position="right",legend.title=element_blank()))

print(plots[[4]] <-ggplot(demandplot3, aes(demandplot3)) +
  geom_line(aes(x= hours[49:72], y= demand[49:72], color = "Demand in Winter"),)  +
  geom_line(aes(x= hours[49:72], y= pv[49:72], color = "PV Supply"),)  +
  geom_line(aes(x= hours[49:72], y= wind[49:72], color = "Wind Supply"),)  +
  geom_line(aes(x= hours[49:72], y= nr[49:72], color = "Non-RE Supply"),)  +
  geom_line(aes(x= hours[49:72], y= hydro[49:72], color = "Hydro Supply"),)  +
  xlab("hours") +
  ylab("demand & Supply")+
theme(legend.position="right",legend.title=element_blank()))

print(plots[[5]] <-ggplot(demandplot3, aes(demandplot3)) +
  geom_line(aes(x= hours[73:96], y= demand[73:96], color = "Demand in Spring"),)  +
  geom_line(aes(x= hours[73:96], y= hydro[73:96], color = "Hydro Supply"),)  +
  geom_line(aes(x= hours[73:96], y= pv[73:96], color = "PV Supply"),)  +
   geom_line(aes(x= hours[73:96], y= nr[73:96], color = "Non-RE Supply"),)  +
  geom_line(aes(x= hours[73:96], y= wind[73:96], color = "Wind Supply"),)  + 
  xlab("hours") +
  ylab("demand & Supply")+
theme(legend.position="right",legend.title=element_blank()))



output <- matrix(ncol=3, nrow=16)


    
    output[1,] <- c("summer",sum(demandplot3$pv[1:24]),"pv")
   
   
    output[2,] <- c("summer",sum(demandplot3$wind[1:24]),"wind")
  
    output[3,] <- c("summer",sum(demandplot3$hydro[1:24]),"hydro")
  
    output[4,] <- c("summer",sum(demandplot3$nr[1:24]),"non-renewable")
   
    output[5,] <- c("fall",sum(demandplot3$pv[25:48]),"pv")
   
    output[6,] <- c("fall",sum(demandplot3$wind[25:48]),"wind")
   
    output[7,] <- c("fall",sum(demandplot3$hydro[25:48]),"hydro")
   
    output[8,] <- c("fall",sum(demandplot3$nr[25:48]),"non-renewable")
    
 
    output[9,] <- c("winter",sum(demandplot3$pv[49:72]),"pv")
   
    output[10,] <- c("winter",sum(demandplot3$wind[49:72]),"wind")
   
    output[11,] <- c("winter",sum(demandplot3$hydro[49:72]),"hydro")

    output[12,] <- c("winter",sum(demandplot3$nr[49:72]),"non-renewable")
    
 
    output[13,] <- c("spring",sum(demandplot3$pv[73:96]),"pv")

    output[14,] <- c("spring",sum(demandplot3$wind[73:96]),"wind")
  
    output[15,] <- c("spring",sum(demandplot3$hydro[73:96]),"hydro")
 
    output[16,] <- c("spring",sum(demandplot3$nr[73:96]),"non-renewable")
    


seasonal <- data.frame(output)
class(seasonal)

print(plots[[6]] <-ggplot(data = seasonal, aes(x = seasonal[,1] , y = seasonal[,2], fill = seasonal[,3])) + 
  geom_bar(stat="identity")+ coord_flip())

  invisible(
    lapply(
      seq_along(plots), 
      function(x) ggsave(filename=paste0("myplot", x, ".png"), plot=plots[[x]])
    ) )
#ggsave(file="length-hist.pdf")

