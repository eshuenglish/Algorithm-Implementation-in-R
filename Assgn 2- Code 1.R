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
best_error = Inf
best_split = 0
for (i in s2){                     #exploring best split in feature 2
    split1 = X_train[,2]<i
    split2 = X_train[,2]>=i
    m1 = mean(y_train[split1])
    m2 = mean(y_train[split2])
    train_error = (1/nrow(X_train))*(sum((y_train[split1] - m1)^2)+sum((y_train[split2] - m2)^2))
    if (train_error< best_error){
        best_error = train_error
        best_split = i
        best_m1 = m1
        best_m2 = m2
        best_feature = 2

    }
}
for (j in s1){                     #exploring best split in feature 2
    split1 = X_train[,1]<j
    split2 = X_train[,1]>=j
    m1 = mean(y_train[split1])
    m2 = mean(y_train[split2])
    train_error = (1/nrow(X_train))*(sum((y_train[split1] - m1)^2)+sum((y_train[split2] - m2)^2))
    if (train_error< best_error){
        best_error = train_error
        best_split = j
        best_m1 = m1
        best_m2 = m2
        best_feature = 1
    }
}
#storing index for both branches after split to use in calculation of test error
split1 = X_test[,best_feature]<best_split
split2 = X_test[,best_feature]>=best_split
test_error = (1/nrow(X_test))*(sum((y_test[split1] - best_m1)^2)+sum((y_test[split2] - best_m2)^2))
print(test_error)