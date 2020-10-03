##################################################################################################
######################################SUPPORTING FUNCTIONS #######################################
##################################################################################################
make_data = function()
{
  dataset = list()
  filename = paste0("data/DN01.xlsx")
  dataset[[1]] = read_excel(filename, sheet = paste0("DN01"), col_names = TRUE,na = "NA")[,-1]
  dataset[[1]]$ES = NA
  
  # Swap ES and Deps
  dataset[[1]] = dataset[[1]][,c("screening","Age","Weight","Height","Alcohol","ES","Deps")]
  
  # Generate Team label
  dataset[[1]]$Team = rep(1,dim(dataset[[1]])[1])
  
  for(i in 2:12)
  {
    filename = paste0("data/DN0",i,".xlsx")
    dataset[[i]] = read_excel(filename, sheet = paste0("DN0",i), col_names = TRUE,na = "NA")[,-1]
    dataset[[i]]$Team = rep(i,dim(dataset[[i]])[1]) 
  }
  name_list = paste0("DN0",seq(1,12,1))
  names(dataset) = name_list
  return(dataset)
}
##################################################################################################
# Generate dummies
##################################################################################################
generate_dummies = function()
{
  data = list()
  
  for(i in 2:12)
  {
    data = rbind(data,data0[[i]])
  }
  
  # Generate is_screen dummy
  data$is_screen = ifelse(data$screening =="Y",1,0)
  # Generate employment status dummy
  ES_dummies = dummy_cols(data$ES)[,-1]
  colnames(ES_dummies) = c("Full","Part","Retired")
  data = cbind.data.frame(data,ES_dummies[,1:2])
  
  # Represent the first dataset
  data1 = data0[[1]]
  data1$is_screen = ifelse(data1$screening =="Y",1,0)
  data1$Full = NA
  data1$Part = NA
  data = rbind(data1,data)
  return(data)
}

##################################################################################################
# summary data
##################################################################################################
summary_data = function()
{
  for(i in 1:12)
  {
    data1 = subset(data,Team == i)
    na_index = apply(apply(data1,2,is.na),2,which)
    obs_matrix[i,1] = dim(data1)[1]
    # Calculate na matrix
    for(c in 1:7)
    {
      if(length(na_index[[c]])!=0){na_matrix[i,c] = length(na_index[[c]])}
    }
    
    # Calculate mean matrix with na removed
    for(c in c(2:5))
    {
      if(na_matrix[i,c]==0){mean_matrix[i,c] = mean(data1[[c]])}
      else{mean_matrix[i,c] = mean(data1[[c]],na.rm = TRUE)}
    }

    if(na_matrix[i,1]==0){mean_matrix[i,1] = mean(data1[[9]])}
    else{mean_matrix[i,1] = mean(data1[[9]],na.rm = TRUE)}
    
    if(na_matrix[i,6]==0){mean_matrix[i,7] = mean(data1[[10]]);mean_matrix[i,8] = mean(data1[[11]])}
    else{mean_matrix[i,7] = mean(data1[[11]],na.rm = TRUE);mean_matrix[i,8] = mean(data1[[11]],na.rm = TRUE)}
    
    if(na_matrix[i,7]==0){mean_matrix[i,6] =  mean(data1[[7]])}
    else{mean_matrix[i,6] =  mean(data1[[7]],na.rm = TRUE)}
  }
  row.names(mean_matrix) = seq(1:12)
  row.names(na_matrix) = seq(1:12)
  row.names(obs_matrix) = seq(1:12)
  mean_matrix = round(mean_matrix,2)
  na_matrix = round(na_matrix,2)
  
  write.csv(mean_matrix,"output/mean_matrix.csv")
  write.csv(na_matrix,"output/na_matrix.csv")
  write.csv(obs_matrix,"output/obs_matrix.csv")
}
##################################################################################################
# Plot forest graph
##################################################################################################
plot_forest = function(var)
{
  mean_list = numeric(12)
  lower_list = numeric(12)
  upper_list = numeric(12)
  for(i in 1:12)
  {
    mean_list[i] = mean(data[data$Team == i,][[var]],na.rm = TRUE)
    lower_list[i] = quantile(data[data$Team == i,][[var]],probs = 0.25,na.rm = TRUE)
    upper_list[i] = quantile(data[data$Team == i,][[var]],probs = 0.75,na.rm = TRUE)
  }
  
  mean_list = c(NA,NA,mean_list,NA,mean(data[[var]],na.rm = TRUE))
  lower_list = c(NA,NA,lower_list,NA,quantile(data[[var]],probs = 0.25,na.rm = TRUE))
  upper_list = c(NA,NA,upper_list,NA,quantile(data[[var]],probs = 0.75,na.rm = TRUE))
  
  input_data = structure(
    list(mean = mean_list, lower = lower_list, upper = upper_list),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -11L), 
      class = "data.frame")
  
  tabletext = cbind(
    c("", "Clinical Team", "1", "2","3","4","5","6","7","8","9","10","11","12",NA,"Population"),
    c(colnames(data)[var],"(mean)",round(mean_list[3:16],2))
  )
  pdf(paste0("plot/heteroeneity_in_",colnames(data)[var],".pdf"),width = 3.93701,height = 3.14961,
      pointsize = 10)
  forestplot(tabletext, 
             input_data,new_page = FALSE,
             is.summary=c(TRUE,TRUE,rep(FALSE,13),TRUE),
             col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),graph.pos = "right",
             zero = 0.8*min(lower_list,na.rm = TRUE),
             title = paste0("Heterogeneity in ",colnames(data)[var], " Distribution"))
  dev.off()
}

##################################################################################################
# Check representativeness
##################################################################################################
check_representativeness = function()
{
  p_matrix = matrix(0,nrow = 12, ncol = 8)
  colnames(p_matrix) = c("is_screen","Age","Weight","Height","Alcohol","Deps","Full","Part")
  for(i in 1:12)
  {
    p_matrix[i,1] = t.test(data[data$Team==i,][[9]],data$is_screen)$p.value
    for(j in 2:5){p_matrix[i,] = t.test(data[data$Team==i,][[j]],data[[j]])$p.value}
    p_matrix[i,6] = t.test(data[data$Team==i,][[7]],data[[7]])$p.value
    if(i==1){p_matrix[1,7:8] = NA}else{for(j in 7:8){p_matrix[i,] = t.test(data[data$Team==i,][[j+3]],data[[j+3]])$p.value}}
  }
  p_matrix = round(p_matrix,2)
  write.csv(p_matrix, "output/representative_test.csv")
  return(p_matrix)
}
##################################################################################################
# Generate Imputed Dataset
##################################################################################################
make_imputed_data = function()
{
  set.seed(sample(seq(1,10000,1),1))
  na_matrix = read.csv("output/na_matrix.csv",header = TRUE)[,-1]
  ES_imputed = sample(data_naomit$ES, sum(na_matrix$ES))
  Alcohol_imputed = sample(data_naomit$Alcohol, sum(na_matrix$Alcohol))
  Height_imputed = sample(data_naomit$Height, sum(na_matrix$Height))
  
  dataset = list()
  filename = paste0("data/DN01.xlsx")
  dataset[[1]] = read_excel(filename, sheet = paste0("DN01"), col_names = TRUE,na = "NA")[,-1]
  dataset[[1]]$ES = ES_imputed
  
  # Swap ES and Deps
  dataset[[1]] = dataset[[1]][,c("screening","Age","Weight","Height","Alcohol","ES","Deps")]
  
  # Generate Team label
  dataset[[1]]$Team = rep(1,dim(dataset[[1]])[1])
  
  for(i in 2:12)
  {
    filename = paste0("data/DN0",i,".xlsx")
    dataset[[i]] = read_excel(filename, sheet = paste0("DN0",i), col_names = TRUE,na = "NA")[,-1]
    dataset[[i]]$Team = rep(i,dim(dataset[[i]])[1]) 
  }
  name_list = paste0("DN0",seq(1,12,1))
  names(dataset) = name_list
  
  data = list()
  
  for(i in 1:12)
  {
    data = rbind(data,dataset[[i]])
  }
  
  data$Alcohol[which(is.na(data$Alcohol)==TRUE)] = Alcohol_imputed
  data$Height[which(is.na(data$Height)==TRUE)] = Height_imputed
  
  # Generate Dummies
  
  # Generate is_screen dummy
  data$is_screen = ifelse(data$screening =="Y",1,0)
  
  # Generate employment status dummy
  ES_dummies = dummy_cols(data$ES)[,-1]
  colnames(ES_dummies) = c("Full","Part","Retired")
  data = cbind.data.frame(data,ES_dummies[,1:2])
  data$BMI = data$Weight/(data$Height/100)^2
  # Add Team Dummies
  Team_dummies = dummy_cols(data$Team)[,-1]
  colnames(Team_dummies) = c("Team10","Team11","Team12",paste0("Team",seq(2,9,1)))
  data = cbind.data.frame(data,Team_dummies[,c(1:2,4:11)])
  return(data)
}
