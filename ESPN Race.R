## Race/Eth Script

SampleSize <- c(seq(100,1000,10))
df <- data.frame(SampleSize)

# CNN_MAR_15 <- read.delim("CNN_MAR_15.txt")
# CNN_APR_15 <- read.delim("CNN_APR_15.txt")
# CNN_MAY_15 <- read.delim("CNN_MAY_15.txt")

ESPN_MAR_15 <- read.delim("ESPN_MAR_15.txt")
# ESPN_APR_15 <- read.delim("ESPN_APR_15.txt")
# ESPN_MAY_15 <- read.delim("ESPN_MAY_15.txt")

# CNN_MAR_UA <- CNN_MAR_15[rep(seq_len(nrow(CNN_MAR_15)), CNN_MAR_15[,8]), ]
# CNN_APR_UA <- CNN_APR_15[rep(seq_len(nrow(CNN_APR_15)), CNN_APR_15[,8]), ]
# CNN_MAY_UA <- CNN_MAY_15[rep(seq_len(nrow(CNN_MAY_15)), CNN_MAY_15[,8]), ]

ESPN_MAR_UA <- ESPN_MAR_15[rep(seq_len(nrow(ESPN_MAR_15)), ESPN_MAR_15[,13]), ]
# ESPN_APR_UA <- ESPN_APR_15[rep(seq_len(nrow(ESPN_APR_15)), ESPN_APR_15[,8]), ]
# ESPN_MAY_UA <- ESPN_MAY_15[rep(seq_len(nrow(ESPN_MAY_15)), ESPN_MAY_15[,8]), ]

# test = CNN_MAR_UA
# test = CNN_APR_UA
# test = CNN_MAY_UA

test = ESPN_MAR_UA
# test = ESPN_APR_UA
# test = ESPN_MAY_UA

segment = test$race_eth_recode

truth_test = prop.table(table(segment))

######################################################################

######################################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run1 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run2 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run3 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run4 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run5 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run6 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run7 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run8 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run9 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run10 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run11 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run12 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run13 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run14 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run15 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run16 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run17 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run18 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run19 <- results$CV

#####################################################

N = length(test[,1])
test$rand = sample(1:N,N, replace=FALSE)

samplesizes = seq(100,1000,10)

results = NULL 
results = rbind(c(N, truth_test),results)
results = as.data.frame(results)
colnames(results) = c("Sample Size",seq(1,6,1)) 

## Make sure segment reflects the one chosen
for(num in samplesizes){
  samp = subset(test, test$rand<= num) 
  segment = samp$race_eth_recode
  truth_samp = prop.table(table(segment)) 
  results = rbind(results, c(num,truth_samp)) 
  samp = truth_samp = NULL 
}

results$weight1 = 1/(results[,2]/results[1,2])
results$weight2 = 1/(results[,3]/results[1,3])
results$weight3 = 1/(results[,4]/results[1,4])
results$weight4 = 1/(results[,5]/results[1,5])
results$weight5 = 1/(results[,6]/results[1,6])
results$weight6 = 1/(results[,7]/results[1,7])



results$mean = 0 
results$sd = 0 


M = length(results[,1])
for(i in 1:M){
  marg_weights = c(rep(results$weight1[i], results[1,1]*results[1,2]), rep(results$weight2[i], results[1,1]*results[1,3]), rep(results$weight3[i], results[1,1]*results[1,4]), rep(results$weight4[i], results[1,1]*results[1,5]), rep(results$weight5[i], results[1,1]*results[1,6]), rep(results$weight6[i], results[1,1]*results[1,7]))  #create the marginal weights by multiplying times original segment size
  results$mean[i] = mean(marg_weights)
  results$sd[i] = sd(marg_weights)
  marg_weights = NULL
}

results$CV = results$sd/results$mean

results <- results[-1,]

df$run20 <- results$CV
#####################################################

