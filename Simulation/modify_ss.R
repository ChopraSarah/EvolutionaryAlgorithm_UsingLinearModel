#equation: y = 1 + 2.x1 + 1.x2b + 1.x2c + 1.x2d + 5.x3e
#                       + 3.x3b + 3.x3c + 3.x3d
pickCat2 <- function(mat_data_test) {
  available_val = which(mat_data_test$flag == "N")
  to_take = available_val[sample(1:length(available_val), 1)]
  value = mat_data_test[to_take,1]
  mat_data_test[to_take,2] = "Y"
  output<-list(a = value, b = mat_data_test)
  return(output)
}

#sample(available_val, 1, replace=TRUE)
set.seed(10)
LevelList <- c("Level11", "Level12","Level13","Level14","Level15")
LevelList2 <- c("Level21","Level22","Level23","Level24","Level25")
#LevelList2 <- c("Level21","Level22")

matrix_coef = matrix(nrow = 5, ncol = 2)
true_coeff_matrix = data.frame(matrix_coef)
true_coeff_matrix[1,1] = "Level11"
true_coeff_matrix[2,1] = "Level12"
true_coeff_matrix[3,1] = "Level13"
true_coeff_matrix[4,1] = "Level14"
true_coeff_matrix[5,1] = "Level15"
true_coeff_matrix[1,2] = 1
true_coeff_matrix[2,2] = 1
true_coeff_matrix[3,2] = 2
true_coeff_matrix[4,2] = 2
true_coeff_matrix[5,2] = "k"
true_coeff_matrix[6,1] = "Level21"
true_coeff_matrix[7,1] = "Level22"
true_coeff_matrix[8,1] = "Level23"
true_coeff_matrix[9,1] = "Level24"
true_coeff_matrix[10,1] = "Level25"
true_coeff_matrix[11,1] = "Cont"
true_coeff_matrix[6,2] = 1
true_coeff_matrix[7,2] = 3
true_coeff_matrix[8,2] = 1
true_coeff_matrix[9,2] = 4
true_coeff_matrix[10,2] = 6
true_coeff_matrix[11,2] = 2
#sampleSize = 1000
#k = 5
#sampleSizeList = c(3000,4000,5000,6000,7000,8000,10000,15000,30000)

sampleSizeList = c(25,50,100,200)
matrix_empty_datasetnames = matrix(nrow = 0, ncol = 2)
dataset_names_ss = data.frame(matrix_empty_datasetnames)

for(sampleSize in sampleSizeList)
{
  #sampleSize = 20
  k = 3
  matrix_empty = matrix(nrow = 0, ncol = 4)

  data = data.frame(matrix_empty)

  names(data)[1] <- "Y"
  names(data)[2] <- "X1"
  names(data)[3] <- "X2"
  sigma_square = 0.1
  for(j in LevelList)
  {

    #assign(paste("mat_data",j), mat_data)
    #mat_data = rbind(dataset_names_k, c(paste("data_k",k))

    mat_data = matrix(nrow = sampleSize/length(LevelList), ncol = 2)
    #assign(paste("mat_data",j,sep="_"), mat_data)
    #mat_data_df = data.frame(get(paste("mat_data",j,sep="_")))
    mat_data_df = data.frame(mat_data)
    names(mat_data_df)[1] <- "cat_value"
    names(mat_data_df)[2] <- "flag"
    mat_data_df$flag = "N"
    divid_fac = (sampleSize/length(LevelList))/length(LevelList2)
    mat_data_df$cat_value = rep(LevelList2,divid_fac)


    #   for(p in LevelList2)
    # {
    for(i in 1:(sampleSize/length(LevelList))) #For the first categorical variable, each level will have.
      # So first we get 10 continuous values for variable, then for first categorical variable
    {
      cont_var = rnorm(1, 5, 10)
      if(j == "Level15")
      { output<-pickCat2(mat_data_df)

      cat2<- output$a
      mat_data_df <- output$b
      if(cat2 == "Level21")
      {y = as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Level11",2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Cont",2])*(cont_var) + k + rnorm(1, mean=0, sd=sigma_square) }
      if(cat2 != "Level21")

      {y = as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Level11",2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Cont",2])*(cont_var) + k + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]==cat2,2]) + rnorm(1, mean=0, sd=sigma_square)}
      }
      if(j == "Level11")
      {
        output<-pickCat2(mat_data_df)

        cat2<- output$a
        mat_data_df <- output$b
        if(cat2 == "Level21")
        {y = as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Level11",2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Cont",2])*(cont_var) + rnorm(1, mean=0, sd=sigma_square)
        }

        if(cat2 != "Level21")
        {y = as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Level11",2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Cont",2])*(cont_var) +  as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]==cat2,2] ) + rnorm(1, mean=0, sd=sigma_square)
        }


      }
      if(j!="Level11" && j!="Level15")
      {
        output<-pickCat2(mat_data_df)

        cat2<- output$a
        mat_data_df <- output$b
        if(cat2 == "Level21")
        {
          y = as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Level11",2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Cont",2])*(cont_var)  + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]==j,2]) + rnorm(1, mean=0, sd=sigma_square)
        }
        if(cat2 != "Level21")
        {
          y = as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Level11",2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]=="Cont",2])*(cont_var)  + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]==j,2]) + as.integer(true_coeff_matrix[true_coeff_matrix[,"X1"]==cat2,2]) + rnorm(1, mean=0, sd=sigma_square)


        }
      }



      df2 <- data.frame(Y=c(y),
                        X1=c(cont_var),
                        X2=c(j),
                        X3=c(cat2))
      data = rbind(data, df2)

    }

  }

  # }

  #print((data))
  assign(paste("data_ss",sampleSize,sep="_"), data)
  dataset_names_ss = rbind(dataset_names_ss, c(paste("data_ss",sampleSize,sep="_"),sampleSize))

  model = lm(Y~., data= data)
  print(model)

}

colnames(dataset_names_ss)[1]  <- "names"
colnames(dataset_names_ss)[2]  <- "values"
#dataset_names_ss
#table(data_ss_100$X3)

