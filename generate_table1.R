obs_matrix = read.csv("output/obs_matrix.csv",header = TRUE)[,-1]
mean_matrix = read.csv("output/mean_matrix.csv",header = TRUE)[,-1]
na_matrix = read.csv("output/na_matrix.csv",header = TRUE)[,-1]
p_matrix = read.csv("output/representative_test.csv",header = TRUE)[,-1]
for(i in 1:12)
{
  cat(i,"&",obs_matrix[i],"&",mean_matrix[i,1],"(",na_matrix[i,1],")(",p_matrix[i,1],")&",
      mean_matrix[i,2],"(",na_matrix[i,2],")(",p_matrix[i,2],")&",
      mean_matrix[i,3],"(",na_matrix[i,3],")(",p_matrix[i,3],")&",
      mean_matrix[i,4],"(",na_matrix[i,4],")(",p_matrix[i,4],")&",
      mean_matrix[i,5],"(",na_matrix[i,5],")(",p_matrix[i,5],")&",
      mean_matrix[i,6],"(",na_matrix[i,7],")(",p_matrix[i,6],")&",
      mean_matrix[i,7],"(",na_matrix[i,6],")(",p_matrix[i,7],")&",
      mean_matrix[i,8],"(",na_matrix[i,6],")(",p_matrix[i,8],")\\\\",sep = "")
  cat("\n")
}

for(i in 1:12)
{
  cat(i,"&",obs_matrix[i],"&",
      mean_matrix[i,2],"(",p_matrix[i,2],")&", # age
      mean_matrix[i,3],"(",p_matrix[i,3],")&", # weight
      mean_matrix[i,4],"(",na_matrix[i,4],")(",p_matrix[i,4],")&", # height
      mean_matrix[i,5],"(",na_matrix[i,5],")(",p_matrix[i,5],")&", # alcohol
      mean_matrix[i,6],"(",p_matrix[i,6],")&", # deps
      mean_matrix[i,7],"(",na_matrix[i,6],")(",p_matrix[i,7],")&",
      mean_matrix[i,8],"(",p_matrix[i,8],")\\\\",sep = "")
  cat("\n")
}