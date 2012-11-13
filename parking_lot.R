parkinglotarray = c(" ", " ", " ", " ", " ", " ", " ", " ", " ", " ", #row 1
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #island1, row2
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #row3
                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #island2
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", #island3
                    "X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                    " ", " ", " ", " ", " ", " ", " ", " ", " ", " "
                    )
parklinglot = matrix(parkinglotarray, nrow=10, ncol=10, byrow=TRUE)

totalcars = 60

#for distributions of islands
lambda = 100

#picks a random car from the island to leave, returned as (row, car)
#note -- if we eventually run out of busy spots this will loop forever
whichcarleaves <- function(){
  run = TRUE
  while(run){
   test = c(sample(c(2,3,5,6,8,9), 1, replace=FALSE), sample(1:10, 1, replace=FALSE))
   if(parkinglot[test[1], test[2]] == "X"){ #make sure it has a car there
     totalcars = totalcars - 1
     return(test)
   }
  }
}

parkcar(car, coordinates){
  parked = FALSE
  #check above and below for a parking space
  if((coordinates[1] != 10) && (parkinglot[coordinates[1] + 1,coordinates[2]] == " ")){
    parkinglot[coordinates[1], coordinates[2]] = " "
    parkinglot[coordinates[1] + 1,coordinates[2]] = car
    coordinates = c(coordinates[1] + 1,coordinates[2])
    parked = TRUE
  } else if((coordinates[1] != 1) && (parkinglot[coordinates[1] - 1,coordinates[2]] == " ")){
    parkinglot[coordinates[1], coordinates[2]] = " "
    parkinglot[coordinates[1] - 1,coordinates[2]] = car
    coordinates = c(coordinates[1] - 1,coordinates[2])
    parked = TRUE
  }
  return(parked)
}

nextfreeasce <- function(row,starting){
  free = FALSE
  while((starting <= 10) && (free == FALSE)){
    if(parkinglot[row,starting] == " "){
      free = TRUE
    } else {
      starting = starting + 1
    }
  }
  if(starting == 11){
    return(nextfreedesc(row + 3,10))
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

movecar1 <- function(carcoordinates){
  if((carcoordinates[1] == 1) && (carcoordinates[2] == 10)){
    freespot = nextfreedesc(4,10)
  } else if((carcoordinates[1] == 4) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(1,1)
  } else if(carcoordinates[1] == 1){
    freespot = nextfreeasce(1,carcoordinates[2])
  } else if(carcoordinates[1] == 4){
    freespot = nextfreedesc(4,carcoordinates[2])
  } else {
    #something is wrong, reset car
    freespot = nextfreeasc(1,1)
  }
  parkinglot[carcoordinates[1],carcoordinates[2]] = " "
  parkinglot[freespot[1],freespot[2]] = "1"
  carcoordinates = freespot
}

movecar2 <- function(carcoordinates){
  if((carcoordinates[1] == 1) && (carcoordinates[2] == 10)){
    freespot = nextfreedesc(4, 10)
  } else if((carcoordinates[1] == 4) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(7, 1)
  } else if((carcoordinates[1] == 7) && (carcoordinates[2] == 10)){
    freespot = nextfreedesc(10, 10)
  } else if((carcoordinates[1] == 10) && (carcoordinates[2] == 1)){
    freespot = nextfreeasc(1,1)
  } else if(carcoordinates[1] == 1){
    freespot = nextfreeasc(1, carcoordinates[2])
  } else if(carcoordinates[1] == 4){
    freespot = nextfreedesc(4, carcoordinates[2])
  } else if(carcoordinates[1] == 7){
    freespot = nextfreeasce(7, carcoordinates[2])
  } else if(carcoordinates[1] == 10){
    freespot = nextfreedesc(10, carcoordinates[2])
  } else {
    freespot = nextfreeasce(1,1)
  }
  parkinglot[carcoordinates[1],carcoordinates[2]] = " "
  parkinglot[freespot[1], freespot[2]] = "2"
  carcoordinates = freespot
}

movecar3 <- function(carcoordinates){
  if((carcoordinates[1] == 7) && (carcoordinates[2] == 10)){
    freespot = nextfreedesc(10,10)
  } else if((carcoordinates[1] == 10) && (carcoordinates[2] == 1)){
    freespot = nextfreeasce(7,1)
  } else if(carcoordinates[1] == 7){
    freespot = nextfreeasce(7,carcoordinates[2])
  } else if(carcoordinates[1] == 10){
    freespot = nextfreedesc(10,carcoordinates[2])
  } else {
    freespot = nextfreeasc(7,1)
  }
  parkinglot[carcoordinates[1],carcoordinates[2]] = " "
  parkinglot[freespot[1],freespot[2]] = "3"
  carcoordinates = freespot
}

getatime <- function(){
  return(rpois(1, lambda))
}

placecars <- function(car1, car2, car3){
  #place car 1
  car1 = c(sample(c(1,4), 1), sample(c(1:10, 1)))
  car3 = c(sample(c(7,10), 1), sample(c(1:10, 1)))
  matching = TRUE
  while(matching){
    car2 = c(sample(c(1,4,7,10), 1), sample(1:10, 1))
    if((car2 != car1) && (car2 != car2)){
      matching = FALSE
    }
  }
  return()
}

simulate <- function(){

  car1 = 1  #carA only circles island 1
  car2 = 2  #carB snakes around all islands
  car3 = 3  #carC only circles island 3
  car1coordinates = car2coordinates = car3coordinates = c(0,0)
  
  placecars(car1coordinates, car2coordinates, car3car3coordinates)
  time = 0
  car1parked = car2parked = car3parked = allparked = FALSE;
  movetime = getatime()
  #this occurs every second
  while(!(allparked)){
    if((time == movetime) && (totalcars > 0)){
      #remove a car
      deletecar = whichcarleaves()
      movetime = getatime()
      time = 0
    }
    #move each car, see if we can park
    if(!(car1parked)){
      movecar1(car1coordinates)
      if(parkcar(car1, car1coordinates)){
        car1parked = TRUE
      }
    }
    if(!(car2parked)){
      movecar2(car2coordinates)
      if(parkcar(car2,car2coordinates)){
        car2parked = TRUE
      }
    }
    if(!(car3parked)){
      movecar3(car3coordinates)
      if(parkcar(car3,car3coordinates)){
        car3parked = TRUE
      }
    }
    allparked = car1parked && car2parked && car3parked
    if(allparked){
      return(time)
    }
    time = time + 1
  }
}