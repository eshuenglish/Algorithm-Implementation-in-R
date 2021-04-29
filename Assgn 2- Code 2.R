#I have used common assignment operator(=) instead of old arrow operator(<-) for assignment of values cos It feels more natural to me.

library(MASS)
fix(Boston)
attach(Boston)
X = cbind(rm , lstat)
y = medv
smp_size = floor(0.5 * nrow(X))
set.seed(1028)
train_ind = sample(seq_len(nrow(X)), size = smp_size)
X_train = X[train_ind, ]
X_test = X[-train_ind, ]
y_train = y[train_ind  ]
y_test = y[-train_ind]
s2 = seq(from = 1.8, to = 37.9, by = 0.1)
s1 = seq(from = 3.6, to = 8.7, by = 0.1)

r_train = y_train
fx = matrix(ncol =4, nrow = 1000)
eta = 0.01
for (b in 1:1000){
    best_error = Inf
    best_split = 0
    for (i in s2){
        split1 = X_train[,2]<i
        split2 = X_train[,2]>=i
        m1 = mean(r_train[split1])
        m2 = mean(r_train[split2])
        train_error = (1/nrow(X_train))*(sum((r_train[split1] - m1)^2)+sum((r_train[split2] - m2)^2))
        if (train_error< best_error){
            best_error = train_error
            best_split = i
            best_m1 = m1
            best_m2 = m2
            best_feature = 2

        }
    }
    for (j in s1){
        split1 = X_train[,1]<j
        split2 = X_train[,1]>=j
        m1 = mean(r_train[split1])
        m2 = mean(r_train[split2])
        train_error = (1/nrow(X_train))*(sum((r_train[split1] - m1)^2)+sum((r_train[split2] - m2)^2))
        if (train_error< best_error){
            best_error = train_error
            best_split = j
            best_m1 = m1
            best_m2 = m2
            best_feature = 1
        }
        
    }
    #fx is 1000*4 matrix which stores best_feature, best_split, and mean values for both branches after the split    
    fx[b,1] = best_feature
    fx[b,2] = best_split
    fx[b,3] = best_m1
    fx[b,4] = best_m2
    best_split1 = X_train[,best_feature]<best_split  #storing index for both branches after split to use in updation of residual
    best_split2 = X_train[,best_feature]>= best_split
    r_train = r_train - eta*((best_split1*best_m1)+(best_split2*best_m2))
}

y_pred = matrix(nrow= nrow(X_test),ncol = 1000)
for (b in 1:1000){                          #calculating prediction values for each of the 1000 stumps for the test set
    if(fx[b,1]==1){
        y_pred[,b][X_test[,1]<fx[b,2]] = fx[b,3]
        y_pred[,b][X_test[,1]>=fx[b,2]] = fx[b,4]
    }
    else{
        y_pred[,b][X_test[,2]<fx[b,2]] = fx[b,3]
        y_pred[,b][X_test[,2]>=fx[b,2]] = fx[b,4]
    }
}

test_mse = mean((y_test - eta*rowSums(y_pred))^2)
print(test_mse)