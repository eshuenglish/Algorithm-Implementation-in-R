#loading the data
df = as.matrix(read.table('/home/smith/Downloads/nci.data.txt', header = FALSE , sep = "", dec = "."))
df2 = as.matrix(read.table('/home/smith/Downloads/label.txt', header = FALSE , sep = "", dec = "."))
scaled_data = scale(t(df))
r =nrow(scaled_data)
c = nrow(scaled_data)
#computing the pairwise differences
pair_diff = matrix(Inf ,nrow = r , ncol= c)
for (i in 1:r){
    for (j in 1:c){
        if(i!=j){
            pair_diff[i,j] = sqrt(sum((scaled_data[i,] - scaled_data[j,])^2))
        }
    }
}
upper_tri = upper.tri(pair_diff)
pair_diff[upper_tri] = Inf   #making the reapeated distances infinity

cluster_matrix = matrix(ncol =nrow(scaled_data), nrow = nrow(scaled_data))
cluster_matrix[,1] = 1:64  #cluster matrix has all the cluster assignments from n to 1 cluster
sl_pair_diff = pair_diff
branch_length = vector(length = r-1)
#This for loop find the cluster assignments for single-linkage 
for(m in 2:r){
    cluster_matrix[,m] = cluster_matrix[,m-1]
    min_diff = min(sl_pair_diff)
    r_min = which(sl_pair_diff ==min_diff , arr.ind =TRUE)[1,1]
    c_min = which(sl_pair_diff == min_diff, arr.ind =TRUE)[1,2]
    while(cluster_matrix[c_min,m] == cluster_matrix[r_min,m]){
        sl_pair_diff[r_min,c_min] = Inf
        min_diff = min(sl_pair_diff)
        r_min = which(sl_pair_diff ==min_diff , arr.ind =TRUE)[1,1]
        c_min = which(sl_pair_diff == min_diff, arr.ind =TRUE)[1,2]
    }
    branch_length[m-1] = min_diff
    sl_pair_diff[r_min,c_min] = Inf
    cluster_matrix[,m][cluster_matrix[,m] == cluster_matrix[r_min,m]] = cluster_matrix[c_min,m]
    cluster_matrix[,m] = as.numeric(factor(cluster_matrix[,m]))

}
sl_cluster_matrix = cluster_matrix
print(sl_cluster_matrix)  #This is all cluster assignments at each level
print(branch_length)   #This is the height at each of N-1 steps



#The next part is for implementaion of Compllete Linkage
# computing pair-wise distance again, this time using NaN value for unneeded redundancies
pair_diff = matrix(NaN ,nrow = r , ncol= c)
for (i in 1:r){
    for (j in 1:c){
        if(i!=j){
            pair_diff[i,j] = sqrt(sum((scaled_data[i,] - scaled_data[j,])^2))
        }
    }
}
cluster_matrix = matrix(ncol =nrow(scaled_data), nrow = nrow(scaled_data))
cluster_matrix[,1] = 1:64
branch_length = vector(length = r-1)
cl_pair_diff = pair_diff
#This for loop find the cluster assignments for complete-linkage 
for (m in 2:r){
    cluster_labels = as.data.frame(table(cluster_matrix[,m-1]))
    already_clustered = cluster_labels[,1]
    for(l in 1:length(already_clustered)){
        for(k in 1:length(already_clustered)){
            if(l!=k){
               cluster_value_1 = which(cluster_matrix[,m-1]==already_clustered[l])
               cluster_value_2 = which(cluster_matrix[,m-1]==already_clustered[k])
               comp_var =c(cluster_value_1,cluster_value_2)
               temp = max(cl_pair_diff[comp_var,comp_var], na.rm =TRUE)
               index = which(pair_diff ==temp,arr.ind =TRUE)
               cl_pair_diff[comp_var,comp_var] = NaN
               cl_pair_diff[index[1],index[2]]= temp        
            } 
        }        
    }
    
    cluster_matrix[,m] = cluster_matrix[,m-1]
    min_diff = min(cl_pair_diff, na.rm =TRUE)
    r_min = which(cl_pair_diff ==min_diff , arr.ind =TRUE)[1,1]
    c_min = which(cl_pair_diff == min_diff, arr.ind =TRUE)[1,2]
    branch_length[m-1] = min_diff
    cl_pair_diff[r_min,c_min] = NaN
    cl_pair_diff[c_min,r_min] = NaN
    cluster_matrix[,m][cluster_matrix[,m] == cluster_matrix[r_min,m]] = cluster_matrix[c_min,m]
    cluster_matrix[,m] = as.numeric(factor(cluster_matrix[,m]))

        
}
cl_cluster_matrix = cluster_matrix
print(cl_cluster_matrix)  #This is all cluster assignments at each level
print(branch_length)   #This is the height at each of N-1 steps



#The next part is for implementaion of Average Linkage

branch_length = vector(length = r-1)
cl_pair_diff = pair_diff
#This is an optional step of making redundant values zero,however, the loop already take cares of these values
upper_tri = upper.tri(cl_pair_diff)
cl_pair_diff[upper_tri] = NaN 
#This for loop find the cluster assignments for average-linkage 
for (m in 2:r){
    cluster_labels = as.data.frame(table(cluster_matrix[,m-1]))
    already_clustered = cluster_labels[,1]
    for(l in 1:length(already_clustered)){
        for(k in 1:length(already_clustered)){
            if(l!=k){
               cluster_value_1 = which(cluster_matrix[,m-1]==already_clustered[l])
               cluster_value_2 = which(cluster_matrix[,m-1]==already_clustered[k])
               cl_pair_diff[cluster_value_1,cluster_value_1] =NaN
               cl_pair_diff[cluster_value_2,cluster_value_2] =NaN
               comp_var =c(cluster_value_1,cluster_value_2)
               temp = (sum(cl_pair_diff[comp_var,comp_var], na.rm =TRUE))/(length(cluster_value_1)*length(cluster_value_2))
               #temp = temp/(length(cluster_value_1)*length(cluster_value_2))
               cl_pair_diff[comp_var,comp_var] = NaN
               cl_pair_diff[cluster_value_1,cluster_value_2] = temp
            } 
        }        
    }
    cluster_matrix[,m] = cluster_matrix[,m-1]
    min_diff = min(cl_pair_diff, na.rm =TRUE)
    r_min = which(cl_pair_diff ==min_diff , arr.ind =TRUE)[1,1]
    c_min = which(cl_pair_diff == min_diff, arr.ind =TRUE)[1,2]
    
    branch_length[m-1] = min_diff
    cl_pair_diff[r_min,c_min] = NaN
    cluster_matrix[,m][cluster_matrix[,m] == cluster_matrix[r_min,m]] = cluster_matrix[c_min,m]
    cluster_matrix[,m] = as.numeric(factor(cluster_matrix[,m]))       
}
al_cluster_matrix = cluster_matrix
print(al_cluster_matrix)  #This is all cluster assignments at each level
print(branch_length)   #This is the height at each of N-1 steps



#The next part is for implementaion of Centroid Linkage
#function to calculate distance
mydist<- function(scaled_data){
    r =nrow(scaled_data)
    c = nrow(scaled_data)
    pair_diff = matrix(NaN ,nrow = r , ncol= c)
    for (i in 1:r){
     for (j in 1:c){
        if(i!=j){
            pair_diff[i,j] = sqrt(sum((scaled_data[i,] - scaled_data[j,])^2 ,na.rm=TRUE))
        }
    }
}
    return(pair_diff)
}

#Next for loop find the cluster assignments for centroid-linkage 
centroid_data = scaled_data
nullified = matrix(FALSE,r,c)  #This boolean matrix will keep track of the distances already used
for (m in 2:r){
    all_distance = mydist(centroid_data)
    diag(all_distance)= NaN
    upper_tri = upper.tri(all_distance)
    all_distance[upper_tri] = NaN
    all_distance[nullified] = NaN

    cluster_matrix[,m] = cluster_matrix[,m-1]
    min_diff = min(all_distance, na.rm = TRUE)
    r_min = which(all_distance ==min_diff , arr.ind =TRUE)[1,1]
    c_min = which(all_distance == min_diff, arr.ind =TRUE)[1,2]

    while(cluster_matrix[c_min,m] == cluster_matrix[r_min,m]){
        all_distance[r_min,c_min] = NaN
        min_diff = min(all_distance, na.rm = TRUE)
        r_min = which(all_distance == min_diff , arr.ind =TRUE)[1,1]
        c_min = which(all_distance == min_diff, arr.ind =TRUE)[1,2]
    }
    branch_length[m-1] = min_diff
    all_distance[r_min,c_min] = NaN
    cluster_matrix[,m][cluster_matrix[,m] == cluster_matrix[r_min,m]] = cluster_matrix[c_min,m]
    cluster_matrix[,m] = as.numeric(factor(cluster_matrix[,m]))
    nullified = is.nan(all_distance)

    cluster_labels = as.data.frame(table(cluster_matrix[,m-1]))
    already_clustered = as.numeric(as.matrix(cluster_labels[,1][cluster_labels[,2]>1]))

    for(l in 1:length(already_clustered)){
        cluster_value = which(cluster_matrix[,m-1]==already_clustered[l])
        if(length(cluster_value)>1)
        {
        total = colSums(scaled_data[cluster_value,])
        temp = total/length(cluster_value)
        centroid_data[cluster_value,] = temp
        }
      
    }
    
}

cent_cluster_matrix = cluster_matrix
print(cent_cluster_matrix)  #This is all cluster assignments at each level
print(branch_length)   #This is the height at each of N-1 steps


#applying K-means to our dataset
km_out = kmeans(scaled_data , 3 , nstart = 20)
table(df2 , km_out$cluster)

km_out = kmeans(scaled_data , 4 , nstart = 20)
table(df2 , km_out$cluster)
km_out = kmeans(scaled_data , 5 , nstart = 20)
table(df2 , km_out$cluster)
km_out = kmeans(scaled_data , 6 , nstart = 20)
table(df2 , km_out$cluster)
km_out = kmeans(scaled_data , 7 , nstart = 20)
table(df2 , km_out$cluster)
km_out = kmeans(scaled_data , 15 , nstart = 20)
table(df2 , km_out$cluster)


#comparing complete linkage and K-means both with clusters = 5
km_out = kmeans(scaled_data , 4, nstart = 20)
km_cluster = km_out$cluster
hc_cluster = cl_cluster_matrix[,61]
table(km_cluster, hc_cluster)