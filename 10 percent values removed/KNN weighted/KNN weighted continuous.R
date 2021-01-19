toyota <- read.csv("ToyotaCorolla.csv")
print(toyota.new)
df <- data.frame(toyota$Age)

toyota.age_missing<-as.data.frame(lapply(df, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.90, 0.10), size = length(cc), replace = TRUE) ]))
print(toyota.age_missing)

mat_bind_age <- data.frame(cbind(toyota.age_missing, toyota))
colnames(mat_bind_age) <- c("Age", "Price", "Age_correct","KM","FuelType","CC","Weight")
print(mat_bind_age)

toyota.missing_100<-as.data.frame(mat_bind_age[1:150,])
print(toyota.missing_100)
summary(toyota.missing)
library(dplyr)
library(matlib)




summary(df)










Fueltype_num<-0#------------------------------------------------------------converting catagorical value to Numerical
for (i in 1:nrow(toyota.missing_100)) {
  
  if(toyota.missing_100$FuelType[i]=="Diesel"){
    Fueltype_num[i]<-1
    
    
  }
  if(toyota.missing_100$FuelType[i]=="Petrol"){
    Fueltype_num[i]<-2
    
    
  }
  if(toyota.missing_100$FuelType[i]=="CNG"){
    Fueltype_num[i]<-3
    
    
  }
  
  
  
}


toyota.missing_100 <- data.frame(cbind(toyota.missing_100, Fueltype_num))
print(toyota.missing_100)



















#################################################


manhattan_dist <- function(rating1, rating2){#-----------------------creating manhattan distance
  distance <- abs(rating1-rating2)
  distance <- sum(distance)
  return(distance)
}



accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){           #--------------------------------------creating accuracy function
    if(test_data[i,1] == test_data[i,3]){
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100
  return(accu)
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



get_mse<- function(test_data){
  
  mse1<-0
  mse<-0
  
  for(i in c(1:nrow(test_data))){    #--------------------------------------creating accuracy function
    
    
    mse = mse + (test_data[i,1] - test_data[i,3])**2
    mse = (1/i)*mse
    #print(mse)
  }
  
  
  #mse = mean((test_data[i+1,1] - test_data[i+1,3])**2)
  print(mse)
  
}


print(toyota.missing_100)



euclideanDist <- function(a, b){ #------------------------------------creating eucliean distance function
  d = 0
  
  d = d + (a-b)^2
  
  d = sqrt(d)
  return(d)
}


################################################
k=7

############################################Creating KNN
distance=0
index_for_accu<-0
distance_price=0
ordered_distance=0
distance=0
distance_km=0

distace_fuel=0
distance_cc=0
distance_weight_cars=0
index_price=0
index_km<-0
index_fuel<-0
i=0


age_total=0

q<-1

for (row in 1:nrow(toyota.missing_100)) {
  price <- toyota.missing_100[row, "Price"]
  age  <- c(toyota.missing_100[row, "Age"])
  km  <- toyota.missing_100[row, "KM"]
  fuel  <- toyota.missing_100[row, "Fueltype_num"]
  cc  <- toyota.missing_100[row, "CC"]
  weight_cars  <- toyota.missing_100[row, "Weight"]
  
  price_inside_price<-0
  distance_price<-0
  distance_fuel<-0
  index_price<-0
  index_km<-0
  
  index_fuel<-0
  
  
  if(is.na(age)) {
    
    index_for_accu[q]<-row
    q = q+1
    age_mean_round<-0
    
    print(paste("On age", age, 
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
      if(!is.na(fuel)) {
        ##print(ja)
        price_inside_fuel <- toyota.missing_100[ja, "Fueltype_num"]
        
        
        price_compare_fuel<-toyota.missing_100[row, "Fueltype_num"]
        distance_fuel[ja]<-c(cosine_dist(price_compare_fuel, price_inside_fuel))
        index_fuel[ja]<-ja
        #print(distance_fuel[ja])
        #print(distance[ja])
        
      }
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
    
    
    
    
    
    
    
    
    
    
    Age_predict<-0
    Age_predictt<-0
    i<-1
    k=7
    mat_distance_price <-0
    mat_distance_fuel<-0
    mat_distance_km<-0
    ordered_distance<-0
    ordered_distance_new<-0
    ordered_distance_k<-0
    j<-0
    age_mean<-0
    age_mean_round<-0
    mat_distance_price1<-0
    
    mat_distance_price <- cbind(index_price, distance_price, toyota.missing_100$Age)
    mat_distance_km<-cbind(index_km, distance_km, toyota.missing_100$Age)
    mat_distance_fuel<-cbind(index_fuel, distance_fuel, toyota.missing_100$Age)
    colnames(mat_distance_price) <- c("Index", "Distance_price", "Age_predict")
    colnames(mat_distance_km) <- c("Index", "Distance_km", "Age_predict")
    colnames(mat_distance_fuel) <- c("Index", "Distance_fuel", "Age_predict")
    
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
    sum_distance<-mat_distance_price + mat_distance_km + mat_distance_fuel
    print(sum_distance)
    mat_distance_total<-cbind(mat_distance_price, mat_distance_km,mat_distance_fuel,sum_distance)
    mat_distance_total <- mat_distance_total[, -c(4,6,7,9,10,12)]
    colnames(mat_distance_total) <- c("Index", "Distance_price" ,"Age_predict", "Distance_km","Distance_fuel","Total_distance")
    
    mat_distance_total<-mat_distance_total[order(mat_distance_total[,6], decreasing = FALSE),]
    # print(mat_distance_total)
    
    
    ordered_distance<- mat_distance_total
    # print(ordered_distance)
    
    # ordered_distance<---sum of all distance
    
    
    
    
    #---------------------------------removing first because it contains the value of NA
    
    mat_distance_price[order(mat_distance_price[,2], decreasing = FALSE),]
    #print(Age_predictt)
    # print(ordered_distance)
    
    
    
    
    l<-0
    ###############################
    for(i in 1:nrow(ordered_distance))
    {
      Age_predictt  <- c(ordered_distance[i, "Age_predict"])
      # print(Age_predictt)
      
      # print(i)
      if(is.na(Age_predictt))
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
    for(i in 1:m){
      for(i in 1:m)
      {
        Age_predictt  <- c(ordered_distance[i, "Age_predict"])
        # print(Age_predictt)
        
        # print(i)
        if(is.na(Age_predictt))
        {
          print("NA removed")
          ordered_distance<- ordered_distance[-c(i), ]#-------------------removing values of NA in ordered set
          
        }
        # print(i)
        
      }
    }
    
    k=5#------------------------------------------------------------putting  value of K
    #ordered_distance_new[-c(1), ]
    # print(ordered_distance)
    
    ordered_distance_k<-ordered_distance[1:k,] #----------------------selecting value of K
    print(ordered_distance_k)
    
    
    
    
    
    
    
    
    ja<-1
    summ<-0
    inverse_total_distance<- 0
    for (ja in 1:nrow(ordered_distance_k)){
      
      inverse_total_distance[ja]  <- 1/(ordered_distance_k[ja, "Total_distance"])
      print(inverse_total_distance)
      summ<- summ +inverse_total_distance[ja]
      
      
      
    } -----------------------------------------####Finding distance inverse
      
      print(inverse_total_distance[])
    print(summ)
    
    
    
    ba<-1
    summ1<-0
    classes_weights=0
    for (ba in 1:nrow(ordered_distance_k)){
      
      classes_weights[ba] <- (inverse_total_distance[ba])/summ
      summ1<- summ1 + classes_weights[ba] #-------------------------------iff summ1 = 1 then weights are correct
      print(ba)
      
    }
    
    # 
    # 
    print(ordered_distance_k)
    print(classes_weights)
    ordered_distance_k<- data.frame(cbind(ordered_distance_k, classes_weights))
    # print(summ1)
    # 
    # 
    # 
    
    print(ordered_distance_k)
    # colnames(ordered_distance_k) <- c("Index", "Distance_price", "fuel_predict","Distance_km","Total_distance","classes_weights")
    
    age_multiply_weights<-0
    ba<-1
    for (ba in 1:nrow(ordered_distance_k)){
      
      
      age_multiply_weights[ba] <- ordered_distance_k[ba, "Age_predict"] * ordered_distance_k[ba, "classes_weights"]
      
    }
    print(age_multiply_weights)
    ordered_distance_k<- data.frame(cbind(ordered_distance_k, age_multiply_weights))
    print(ordered_distance_k)
    
    ba<-1
    sum_final<-0
    for (ba in 1:nrow(ordered_distance_k)){
      
      
      sum_final<- sum_final + ordered_distance_k[ba, "age_multiply_weights"]
      print(sum_final)
      
    }
    
    
    
    print(round(sum_final))
    
    
    # #--------------------mean of k nearest neighbours
    # j<-colMeans(ordered_distance_k,na.rm=T)
    # # print("starting ordered distance")
    # # print(ordered_distance_k)
    # age_mean<-j[3]
    # age_mean_round<-print(round(age_mean))
    # 
    # #print(mat_distance_price)
    # mat_distance_price1<-data.frame(mat_distance_price)
    # #print(mat_distance_price1)
    # #print(mat_distance_price1$Age_predict)
    # 
    # #for(q in 1:nrow(toyota.missing_100) ){
    #   
    #   #if(is.na(toyota.missing_100$Age[row])) {
    #  #print("row which contain na, ", row)
    #  toyota.missing_100$Age[row][is.na(toyota.missing_100$Age[row])] <- age_mean_round
    toyota.missing_100$Age[row] <- round(sum_final)#----------------------------------updating value of age in database
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
predicted_value<-0
x<-0


#index_for_accu <- as.data.frame((index_for_accu))
#index_for_accu1 <- data.frame(r1=names(index_for_accu), t(index_for_accu))
print(index_for_accu[2])
#colnames(predicted_value) <- c("Age", "Price", "Age_correct","KM","FuelType","CC","Weight")
#predicted_value<-as.data.frame((predicted_value))

for(n in c(1:length(index_for_accu))){ 
  for(m in c(1:nrow(toyota.missing_100))){   
    
    if(index_for_accu[n] == m){
      predicted_value <- toyota.missing_100[m,]
      x <- data.frame(rbind(x,predicted_value))
      xn<-x[-1,]
      
      #print(predicted_value[n])
      
    }
    
    
  }
}

print(xn)
print(toyota.missing_100)


#print(index_for_accu[])
#accuracy(xn)
get_mse(xn)
print(paste("Mean Square Error is", get_mse(xn)))
msse <- get_mse(xn)
write.csv(xn,'W:\\cases\\Question 1\\10 percent values removed\\KNN weighted\\MyData_continuous_1nn_10percent.csv', row.names = TRUE)
write.table(msse, 'W:\\cases\\Question 1\\10 percent values removed\\KNN weighted\\MyData_continuous_1nn_10percent.csv', col.names=FALSE, sep=",", append=TRUE)

#################################################----predicting age of the car

