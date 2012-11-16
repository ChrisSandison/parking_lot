parkinglotarray = c(" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", #row 1
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",#island1, row2
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #row3
                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #island2
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #island3
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "
                    )

parkinglot = matrix(parkinglotarray, nrow=10, ncol=20, byrow=TRUE)
totalcars = 60

#picks a random car from the island to leave, returned as (row, car)
#note -- if we eventually run out of busy spots this will loop forever
whichcarleaves <- function(parkinglot){
  run = TRUE
  while(run){
   test = c(sample(c(2,3,5,6,8,9), 1, replace=FALSE), sample(1:20, 1, replace=FALSE))
   if(parkinglot[test[1], test[2]] == "X"){ #make sure it has a car there
     parkinglot[test[1], test[2]] = " "
     totalcars = totalcars - 1
     return(parkinglot)
   }
  }
}

parkcar <- function(car, coordinates, parkinglot){
  spot = c(-1,-1)
  #check above and below for a parking space
  if(coordinates[1] != 10){ if(parkinglot[coordinates[1] + 1,coordinates[2]] == " "){
    spot = c(coordinates[1]+1,coordinates[2])
  }} else if(coordinates[1] != 1){ if(parkinglot[coordinates[1] - 1,coordinates[2]] == " "){
    spot = c(coordinates[1]-1,coordinates[2])
  }}
  return(spot)
}

nextfreeasce <- function(row,starting){
  free = FALSE
  while((starting <= 20) && (free == FALSE)){
    if(parkinglot[row,starting] == " "){
      free = TRUE
    } else {
      starting = starting + 1
    }
  }
  if(starting == 21){
    return(nextfreedesc(row + 3,20))
  } else {
    return(c(row, starting))
  }
}

nextfreedesc <- function(row,starting){
  free = FALSE
  while((starting >= 1) && (free == FALSE)){
    if(parkinglot[row,starting] == " "){
      free = TRUE
    } else {
      starting = starting - 1
    }
  }
  if(starting == 0){
    return(nextfreeasce(row - 3,1))
  } else {
    return(c(row,starting))
  }
}

movecar1 <- function(carcoordinates, parkinglot){
  if((carcoordinates[1] == 1) && (carcoordinates[2] == 20)){
    freespot = nextfreedesc(4,20)
  } else if((carcoordinates[1] == 4) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(1,1)
  } else if(carcoordinates[1] == 1){
    freespot = nextfreeasce(1,carcoordinates[2]+1)
  } else if(carcoordinates[1] == 4){
    freespot = nextfreedesc(4,carcoordinates[2]-1)
  } else {
    #something is wrong, reset car
    freespot = nextfreeasce(1,1)
  }
  return(freespot)
}

movecar2 <- function(carcoordinates, parkinglot){
  if((carcoordinates[1] == 1) && (carcoordinates[2] == 20)){
    freespot = nextfreedesc(4, 20)
  } else if((carcoordinates[1] == 4) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(7, 1)
  } else if((carcoordinates[1] == 7) && (carcoordinates[2] == 20)){
    freespot = nextfreedesc(10, 20)
  } else if((carcoordinates[1] == 10) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(1,1)
  } else if(carcoordinates[1] == 1){
    freespot = nextfreeasce(1, carcoordinates[2]+1)
  } else if(carcoordinates[1] == 4){
    freespot = nextfreedesc(4, carcoordinates[2]-1)
  } else if(carcoordinates[1] == 7){
    freespot = nextfreeasce(7, carcoordinates[2]+1)
  } else if(carcoordinates[1] == 10){
    freespot = nextfreedesc(10, carcoordinates[2]-1)
  } else {
    freespot = nextfreeasce(1,1)
  }
  return(freespot)
}

movecar3 <- function(carcoordinates, parkinglot){
  if((carcoordinates[1] == 7) && (carcoordinates[2] == 20)){
    freespot = nextfreedesc(10,20)
  } else if((carcoordinates[1] == 10) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(7,1)
  } else if(carcoordinates[1] == 7){
    freespot = nextfreeasce(7,carcoordinates[2]+1)
  } else if(carcoordinates[1] == 10){
    freespot = nextfreedesc(10,carcoordinates[2]-1)
  } else {
    freespot = nextfreeasce(7,1)
  }
  return(freespot)
}

getatime <- function(){
  return(round(rexp(1, 0.01), 0))
}

placecars <- function(){
  #place car 1
  car1 = c(sample(c(1,4), 1), sample(c(1:20), 1))
  car3 = c(sample(c(1,4,7,10), 1), sample(c(1:20), 1))
  matching = TRUE
  while(matching){
    car2 = c(sample(c(1,4,7,10), 1), sample(c(1:20), 1))
    if((car2 != car1) && (car2 != car3)){
      matching = FALSE
    }
  }
  return(c(car1, car2, car3))
}

placecarinlot <- function(car, carcoordinates, parkinglot, oldspot){
  if(oldspot != 0){
    parkinglot[oldspot[1],oldspot[2]] = " "
  }
  parkinglot[carcoordinates[1],carcoordinates[2]] = car
  return(parkinglot)
}

simulate <- function(parkinglot){

  car1 = 1  #carA only circles island 1
  car2 = 2  #carB snakes around all islands
  car3 = 3  #carC only circles island 3
  car1coordinates = car2coordinates = car3coordinates = c(0,0)
  carplaces = placecars()
  car1coordinates = c(carplaces[1], carplaces[2]); car2coordinates = c(carplaces[3],carplaces[4]); car3coordinates = c(carplaces[5],carplaces[6])
  parkinglot = placecarinlot(car1,car1coordinates,parkinglot, 0)
  parkinglot = placecarinlot(car2,car2coordinates,parkinglot, 0)
  parkinglot = placecarinlot(car3,car3coordinates,parkinglot, 0)
  time = car1time = car2time = car3time = 0
  car1parked = car2parked = car3parked = allparked = FALSE;
  movetime = getatime()
  #this occurs every second
  while(!(allparked)){
    if((time == movetime) && (totalcars > 0)){
      #remove a car
      parkinglot = whichcarleaves(parkinglot)
      movetime = getatime() + 1 #incase of 0, which happened in testing
      time = 0
    }
    #move each car, see if we can park
    if(!(car1parked)){
      newspot = movecar1(car1coordinates, parkinglot)
      oldspot = car1coordinates
      car1coordinates = newspot
      parkinglot = placecarinlot(car1, car1coordinates, parkinglot, oldspot)
      parkingspot = parkcar(car1, car1coordinates, parkinglot)
      if(parkingspot != c(-1,-1)){
        parkinglot[parkingspot[1],parkingspot[2]] = "1"
        parkinglot[newspot[1],newspot[2]] = " "
        car1parked = TRUE
      }
      car1time = car1time + 1
    }
    if(!(car2parked)){
      newspot = movecar2(car2coordinates, parkinglot)
      oldspot = car2coordinates
      car2coordinates = newspot
      parkinglot = placecarinlot(car2, car2coordinates, parkinglot, oldspot)
      parkingspot = parkcar(car2, car2coordinates, parkinglot)
      if(parkingspot != c(-1,-1)){
        parkinglot[parkingspot[1],parkingspot[2]] = "2"
        parkinglot[newspot[1],newspot[2]] = " "
        car2parked = TRUE
      }
      car2time = car2time + 1
    }
    if(!(car3parked)){
      newspot = movecar3(car3coordinates, parkinglot)
      oldspot = car3coordinates
      car3coordinates = newspot
      parkinglot = placecarinlot(car3, car3coordinates, parkinglot, oldspot)
      parkingspot = parkcar(car3, car3coordinates, parkinglot)
      if(parkingspot != c(-1,-1)){
        parkinglot[parkingspot[1],parkingspot[2]] = "3"
        parkinglot[newspot[1],newspot[2]] = " "
        car3parked = TRUE
      }
      car3time = car3time + 1
    }
    allparked = car1parked && car2parked && car3parked
    if(allparked){
      print(parkinglot)
      return(c(car1time, car2time, car3time))
    }
    time = time + 1
  }
}

getmeans <- function(parkinglot){
  car1total = car2total = car3total = 0
  for(i in 1:1000){
    times = simulate(parkinglot)
    car1total = car1total + times[1]
    car2total = car2total + times[2]
    car3total = car3total + times[3]
  }
  car1mean = car1total / 1000
  car2mean = car2total / 1000
  car3mean = car3total / 1000
  cat("Car1 took an average of", car1mean, "seconds to park\nCar2 took an average of", car2mean, "seconds to park\nCar3 took an average of", car3mean, "seconds to park\n")
}