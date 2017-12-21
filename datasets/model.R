library(readr)
library(rstan)
library(rethinking)

## Building Model 1

### Import Data
# read files
data1 = read.csv(file.path("datasets", "M1.data.new.csv"))

### Preprocess Data
# factor to numeric
data1$crime_cases = as.numeric(as.character(data1$Total_cases_Number))
# fix NAs
data1[1,"crime_cases"] = 115879

# make log version of outcome
data1$log_crime = log(data1$crime_cases)

# construct centered predictor 
data1$Bx1_c<- data1$Bx1 - mean(data1$Bx1)
data1$Bx2_c<- data1$Bx2 - mean(data1$Bx2)
data1$Bx3_c<- data1$Bx3 - mean(data1$Bx3)

# trim data frame
d1 = data1[ , c("log_crime","Bx1_c","Bx2_c","Bx3_c")]

# define model fit by map
m1 <- map(
  alist(
    log_crime ~ dnorm(mu, sigma),
    mu <- a +b1*Bx1_c + b2*Bx2_c + b3*Bx3_c,
    a ~ dnorm(11,100),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data <- d1)

# define model fit by map2stan
m1.stan <- map2stan(
  alist(
    log_crime ~ dnorm(mu, sigma),
    mu <- a +b1*Bx1_c + b2*Bx2_c + b3*Bx3_c,
    a ~ dnorm(11,100),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    sigma ~ dcauchy(0,2)
  ),
  data <- d1)

## Building Model 2
### Import Data
# read files
data2 = read.csv(file.path("datasets", "M2.data.new.csv"))

### Preprocess Data
# factor to numeric
data2$crime_cases = as.numeric(as.character(data2$Total_cases_Number))
# fix NAs
data2[1,"crime_cases"] = 115879

# make log version of outcome
data2$log_crime = log(data2$crime_cases)

# trim data frame
d2 = data2[ , c("log_crime","log_box_office","score.s","Bscore.s")]

# define model fit by map
m2 <- map(
  alist(
    log_crime ~ dnorm(mu, sigma),
    mu <- a +b1*log_box_office + b2*score.s + b3*Bscore.s,
    a ~ dnorm(11,100),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data <- d2)

m2.stan <- map2stan(
  alist(
    log_crime ~ dnorm(mu, sigma),
    mu <- a +b1*log_box_office + b2*score.s + b3*Bscore.s,
    a ~ dnorm(11,100),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    sigma ~ dcauchy(0,2)
  ),
  data <- d2)

# combine d1, d2
d = merge(d1,d2)