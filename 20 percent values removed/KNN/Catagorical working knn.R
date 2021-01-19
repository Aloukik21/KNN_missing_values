toyota <- read.csv("ToyotaCorolla.csv")
df <- as.data.frame(toyota$FuelType)
#colnames(df) <- c("Index","FuelType")
df <- data.frame(cbind(df, toyota))
print(df)
toyota.missing_100<-0
mat_bind_fuel<-0










Fueltype_num<-0#------------------------------------------------------------converting catagorical value to Numerical
for (i in 1:nrow(df)) {
  
  if(df$toyota.FuelType[i]=="Diesel"){
    Fueltype_num[i]<-1
    
    
  }
  if(df$toyota.FuelType[i]=="Petrol"){
    Fueltype_num[i]<-2
    
    
  }
  if(df$toyota.FuelType[i]=="CNG"){
    Fueltype_num[i]<-3
    
    
  }
  

  
  
  
  
  
}
#---------------------------------------------end of converting values

df<- data.frame(cbind(df, Fueltype_num))
print(df)
df <- as.data.frame(df[, -c(1,2,3,4,5,6,7)])
print(df)

















toyota.fuel_missing<-as.data.frame(lapply(df, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.80, 0.20), size = length(cc), replace = TRUE) ]))
print(toyota.fuel_missing)

mat_bind_fuel<- data.frame(cbind(toyota.fuel_missing, toyota))
colnames(mat_bind_fuel) <- c("FuelType", "Price", "Age","KM","FuelType_orignal","CC","Weight")
print(mat_bind_fuel)

toyota.missing_100<-as.data.frame(mat_bind_fuel[1:150,])#-----------------------------taking number of instances
print(toyota.missing_100)

library(dplyr)




summary(df)




accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){  
       
    if(is.na(test_data[i,1])){
      test_data[i,1]<-0
      
    }
    
                                     #--------------------------------------creating accuracy function
    if(test_data[i,1] == test_data[i,8]){
      correct = correct+1
    }
    
  }
  accu = correct/nrow(test_data) * 100
  print(accu)
}






Fueltype_correct<-0#------------------------------------------------------------converting catagorical value to Numerical
for (i in 1:nrow(toyota.missing_100)) {
  
  if(toyota.missing_100$FuelType_orignal[i]=="Diesel"){
    Fueltype_correct[i]<-1


  }
  if(toyota.missing_100$FuelType_orignal[i]=="Petrol"){
    Fueltype_correct[i]<-2


  }
  if(toyota.missing_100$FuelType_orignal[i]=="CNG"){
    Fueltype_correct[i]<-3


  }




}

toyota.missing_100 <- data.frame(cbind(toyota.missing_100, Fueltype_correct))

print(toyota.missing_100)



















#################################################


manhattan_dist <- function(rating1, rating2){#-----------------------creating manhattan distance
  distance <- abs(rating1-rating2)
  distance <- sum(distance)
  return(distance)
}


euclideanDist <- function(a, b){ #------------------------------------creating eucliean distance function
  d = 0
  
  d = d + (a-b)^2
  
  d = sqrt(d)
  return(d)
}

cosine_dist <- function(m,n) #--------------------------------------------creating cosine distance
{ 
  A<-0
  B<-0
  z<-0
  A = m
  B = n
  z<-( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
  return(z)
  
}   

###############################################

modes <- function(x) {#--------------------------------------------------------creating mode function for coloumn 3 in dataframe
  ux <- unique(x[,3])
  tab <- tabulate(match(x[,3], ux))
  ux[tab == max(tab)]
  #return(ux)
}


################################################
k=5#-------------------selecting value of k
k=k+1#------------------first row is Na row itself, as it will be removed later, so results in 1 nearest neighbour only

############################################Creating KNN
distance=0
distance_price=0
ordered_distance=0
distance=0
distance_km=0
index_for_accu<-0
distace_fuel=0
distance_cc=0
distance_weight_cars=0
index_price=0
index_km<-0
index_fuel<-0
i=0


fuel_total=0

q<-1

for (row in 1:nrow(toyota.missing_100)) {
  price <- toyota.missing_100[row, "Price"]
  age  <- c(toyota.missing_100[row, "Age"])
  km  <- toyota.missing_100[row, "KM"]
  fuel  <- toyota.missing_100[row, "FuelType"]
  print(fuel)
  cc  <- toyota.missing_100[row, "CC"]
  weight_cars  <- toyota.missing_100[row, "Weight"]
  
  price_inside_price<-0
  distance_price<-0
  index_price<-0
  index_km<-0
  index_fuel<-0
  
  if(is.na(fuel)) {
    fuel_mean_round<-0
    index_for_accu[q]<-row
    q = q+1
    
    print(paste("On fuel", fuel, 
                "the price was", price))
    
    
    price_inside_price<-0
    price_compare_price<-0
    
    
    for (ja in 1:nrow(toyota.missing_100)){ #---------------------------getting feature 1 which is price
      #print("hello lello")
      
     # if(!is.na(km) && !is.na(price) && !is.na(cc) && !is.na(weight_cars)){
      
      if(!is.na(price)) {
        #print(ja)
        price_inside_price <- toyota.missing_100[ja, "Price"]
        
        
        price_compare_price<-toyota.missing_100[row, "Price"]
        distance_price[ja]<-c(manhattan_dist(price_compare_price, price_inside_price))
        #print(price_inside_price)
        index_price[ja]<-ja
        #print(index_price)
        
        #print(index_price[ja])
        #print(distance_price[ja])
        
      } 
      
        
      
       if(!is.na(km)) {
        #print(ja)
        price_inside_km <- toyota.missing_100[ja, "KM"]


        price_compare_km<-toyota.missing_100[row, "KM"]
        distance_km[ja]<-c(euclideanDist(price_compare_km, price_inside_km))
        #print(distance_km[ja])
        index_km[ja]<-ja

      }

     #  ##########
     #  
      # if(!is.na(fuel)) {
      #   ##print(ja)
      #   price_inside_fuel <- toyota.missing_100[ja, "Fueltype_num"]
      # 
      # 
      #  price_compare_fuel<-toyota.missing_100[row, "Fueltype_num"]
      #  distance_fuel[ja]<-c(euclideanDist(price_compare_fuel, price_inside_fuel))
      #  index_fuel[ja]<-ja
      #  #print(distance_fuel[ja])
      #  #print(distance[ja])
      # 
      # }
     #  ####################
     #  
     #  if(!is.na(cc)) {
     #    #print(ja)
     #    price_inside_cc <- toyota.missing_100[ja, "CC"]
     #    
     #    
     #    price_compare_cc<-toyota.missing_100[row, "CC"]
     #    distance_cc[ja]<-c(euclideanDist(price_compare_cc, price_inside_cc))
     #    #print(distance[ja])
     #    
     #  } 
     #  ######################
     #  if(!is.na(weight_cars)) {
     #    #print(ja)
     #    price_inside_weight_cars <- toyota.missing_100[ja, "Weight"]
     #    
     #    
     #    price_compare_weight_cars<-toyota.missing_100[row, "Weight"]
     #    distance_weight_cars[ja]<-c(euclideanDist(price_compare_weight_cars, price_inside_weight_cars))
     #    #print(distance_weight_cars[ja])
     #    
     #  } 
      
      }#if else 
      
      
      
      
      
    
      
      
    
    
    fuel_predict<-0
    fuel_predictt<-0
    i<-1
    k=5
    k=k+1#------------------first row is Na row itself, as it will be removed later, so results in 1 nearest neighbour only
    mat_distance_price <-0
    ordered_distance<-0
    ordered_distance_new<-0
    ordered_distance_k<-0
    j<-0
    fuel_mean<-0
    fuel_mean_round<-0
    mat_distance_price1<-0
    
    mat_distance_price <- cbind(index_price, distance_price, toyota.missing_100$FuelType)
    mat_distance_km<-cbind(index_km, distance_km, toyota.missing_100$FuelType)
    #mat_distance_fuel<-cbind(index_km, distance_fuel, toyota.missing_100$Age)
    colnames(mat_distance_price) <- c("Index", "Distance_price", "fuel_predict")
    colnames(mat_distance_km) <- c("Index", "Distance_km", "fuel_predict")
    c#olnames(mat_distance_fuel) <- c("Index", "Distance_fuel", "Age_predict")
    
     # print(mat_distance_price)
     # print(mat_distance_km)
     # print(mat_distance_fuel)
    # print(distance_price)
     
     
     
     
     #mat_distance_price_ordered<-mat_distance_price[order(mat_distance_price[,2], decreasing = FALSE),]
     #mat_distance_km_ordered<-mat_distance_km[order(mat_distance_km[,2], decreasing = FALSE),]
     #mat_distance_fuel_ordered<-mat_distance_fuel[order(mat_distance_fuel[,2], decreasing = FALSE),]
     # print(mat_distance_km)
     # print(mat_distance_fuel)
     # 
     # print(mat_distance_price)
     
    # print(ordered_distance)
     sum_distance<-mat_distance_price + mat_distance_km
     print(sum_distance)
     mat_distance_total<-cbind(mat_distance_price, mat_distance_km,sum_distance)
     print(mat_distance_total)
     mat_distance_total <- mat_distance_total[, -c(4,6,7,9)]
     colnames(mat_distance_total) <- c("Index", "Distance_price" ,"fuel_predict", "Distance_km","Total_distance")
     
     mat_distance_total<-mat_distance_total[order(mat_distance_total[,5], decreasing = FALSE),]
     print(mat_distance_total)
     
     
    ordered_distance<- mat_distance_total
    print(ordered_distance)
    
   # ordered_distance<---sum of all distance
    
    
    
    
    #---------------------------------removing first because it contains the value of NA
    
    #mat_distance_price[order(mat_distance_price[,2], decreasing = FALSE),]
    #print(Age_predictt)
    # print(ordered_distance)
  
    
    
    
    l<-0
    ###############################
    for(i in 1:nrow(ordered_distance))
    {
      fuel_predictt  <- c(ordered_distance[i, "fuel_predict"])
      # print(Age_predictt)
      
      # print(i)
      if(is.na(fuel_predictt))
      {
        # print("NA removed")
        #--------------------------------------------------------------------couting number of na
        l<-l+1
      }
      # print(i)
    }
    #print(l)
    m<-0
    m<-nrow(toyota.missing_100)-l
    #print(m)
    
    #
    
    for(i in 1:m)
    {
      fuel_predictt  <- c(ordered_distance[i, "fuel_predict"])
      # print(Age_predictt)
    
      # print(i)
      if(is.na(fuel_predictt))
      {
        print("NA removed")
        ordered_distance<- ordered_distance[-c(i), ]#-------------------removing values of NA in ordered set
        
      }
      # print(i)
      
    }
    
    k=5#------------------------------------------------------------putting  value of K
    k=k+1#------------------first row is Na row itself, as it will be removed later, so results in 1 nearest neighbour only
    #ordered_distance_new[-c(1), ]
    print(ordered_distance)
  
    ordered_distance_k<-ordered_distance[1:k,] #----------------------selecting value of K
    # print(ordered_distance_k)
    
    
    #--------------------mean of k nearest neighbours
    
    j<-modes(ordered_distance_k)
    print(j)
    # print("starting ordered distance")
    print(ordered_distance_k[,3])
    fuel_mean<-j[3]
    fuel_mean_round<-print(round(j))
    
    #print(mat_distance_price)
    mat_distance_price1<-data.frame(mat_distance_price)
    #print(mat_distance_price1)
    #print(mat_distance_price1$Age_predict)
    
    #for(q in 1:nrow(toyota.missing_100) ){
      
      #if(is.na(toyota.missing_100$Age[row])) {
     #print("row which contain na, ", row)
     toyota.missing_100$FuelType[row][is.na(toyota.missing_100$FuelType[row])] <- fuel_mean_round
     toyota.missing_100$FuelType[row] <- fuel_mean_round#----------------------------------updating value of age in database
    #print(toyota.missing_100$Age[row])
    # print(row)
    # print(toyota.missing_100)
    
    #break()
    #}
      
    #}
    
    
   
 #Age#print(age_total[1:k])  
 
    
    
    
    print("if loop closed")
    
    
    
  }#-------------------------------age if loop ending
  
  
  
    print("main for loop closed")

   
    }#------------------closing main for loop(do not touch)
print(toyota.missing_100) 

x<-0
predicted_value<-0

for(n in c(1:length(index_for_accu))){ 
  for(m in c(1:nrow(toyota.missing_100))){   #-------------making a loop to select only NA value rows, to find accuracy
                                              #--------------selecting only predicted value to compare in accuracy function
    if(index_for_accu[n] == m){
      predicted_value <- toyota.missing_100[m,]
      x <- data.frame(rbind(x,predicted_value))
      #print(predicted_value[n])
      print(m)
    }
    
    
  }
}


print(x)

accuracy(x)
print(paste("Accuracy is", accuracy(x)))
accu_d <- accuracy(x)
write.csv(x,'W:\\cases\\Question 1\\20 percent values removed\\KNN\\MyData_catagorical_knn_20percent.csv', row.names = TRUE)
write.table(accu_d, 'W:\\cases\\Question 1\\20 percent values removed\\KNN\\MyData_catagorical_knn_20percent.csv', col.names=FALSE, sep=",", append=TRUE)

