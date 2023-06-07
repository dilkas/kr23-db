require(ggplot2)
require(ggrepel)
require(tikzDevice)

TRAINING_N <- 25
NAME <- "Quartic"
BEST_DEGREE <- 4

df0 <- read.csv('runtimes.csv')
df <- df0[order(df0$n),]

#randomly shuffle data
df.shuffled <- df[sample(nrow(df)),]

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 10

#create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse <- matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(time ~ poly(n,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$time)^2) 
  }
}

#find MSE for each degree 
means <- colMeans(mse)
means

best <- lm(time ~ poly(n, BEST_DEGREE), data = df)
linear <- lm(time ~ n, data = head(df, TRAINING_N))

#cubic <- lm(time ~ poly(n, 3), data = head(df, TRAINING_N))
#exponential <- lm(log(time) ~ n, data = df)
#summary(exponential)
#exponential.df <- data.frame(n = df$n, time = exp(fitted(exponential)))

colours <- c("#1b9e77", "#d95f02")
for_labels <- data.frame(x = max(df$n),
                         y = c(predict(best, data.frame(n = max(df$n))),
                               predict(linear, data.frame(n = max(df$n)))),
                         label = c(NAME, "Linear"))
tikz(file = "../../doc/paper/plot.tex", width = 3.31, height = 2.05,
     standAlone = TRUE)
ggplot(df, aes(x = n, y = time)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, formula = y ~ poly(x, BEST_DEGREE),
              aes(color = NAME), fill = colours[2]) +
  stat_smooth(method = 'lm', formula = y ~ x, data = head(df, TRAINING_N),
              se = FALSE, fullrange = TRUE, aes(color = "Linear")) +
  xlab('Domain size') +
  ylab('Runtime (s)') +
  theme_set(theme_gray(base_size = 9)) +
  theme_minimal() +
  scale_color_manual(name = "Model fit", values = colours, guide = FALSE) +
  geom_label_repel(data = for_labels, aes(x = x, y = y, label = label,
                                          color = label),
                   min.segment.length = Inf)
dev.off()

tikz(file = "../../doc/talks/3_long/plot.tex", width = 2.13, height = 1.32,
     standAlone = TRUE)
ggplot(df, aes(x = n, y = time)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE, formula = y ~ poly(x, BEST_DEGREE),
              aes(color = NAME), fill = colours[2]) +
  xlab('Domain size') +
  ylab('Runtime (s)') +
  theme_set(theme_gray(base_size = 9)) +
  theme_minimal() +
  scale_color_manual(name = "Model fit", values = colours, guide = FALSE)
dev.off()
