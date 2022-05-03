setwd("/Users/lamyathei/Desktop/2021-22 Term 1/STAT344/project")

### SRS
# Read data from the csv file
data <- read.csv("SRS.csv")
# Discard the samples not in the population
sample <- data[data$Family == "No", ]
# Indicator of sample rent larger than $1,000
sample$exp <- ifelse(sample$Rent > 1000, 1,0)
n <- as.numeric(nrow(sample))

# sample mean of the monthly rent
avg_srs <- mean(sample$Rent)
avg_srs
# Standard Error (SE) of the estimate
se_srs <- sd(sample$Rent)/sqrt(n)
round(se_srs,2)
# 95% C.I. of the sample mean
CI_srs <- c(avg_srs - 1.96*se_srs, avg_srs + 1.96*se_srs)
round(CI_srs,2)

# p_hat
prop_srs <- mean(sample$exp)
round(prop_srs,4)
# SE of p_hat
se_prop_srs <- sqrt(prop_srs*(1-prop_srs)/n)
round(se_prop_srs,4)
# 95% C.I. of p_hat
CI_prop_srs <- c(prop_srs - 1.96*se_prop_srs, prop_srs + 1.96*se_prop_srs)
round(CI_prop_srs,4)


### Code for estimating the population size and within-strata variances for stratified sampling
# Number of off-campus students paying their own rent in the sample
offcampus_rent <- sum(ifelse(data$Family=="No" & data$Oncampus. == "No",1,0))
# Number of off-campus students in the sample
offcampus <- sum(ifelse(data$Oncampus. == "No",1,0))
# Estimated proportion of off-campus students paying their own rent
prop_offrent <- offcampus_rent/offcampus
prop_offrent

# Total number of UBC Vancouver students
ttl <- 56936  #Source: https://www.ubc.ca/about/facts.html
# Total number of on-campus students paying their own rent
N_on <- 13000  #Source: https://vancouver.housing.ubc.ca
# Estimated total number of off-campus students paying their own rent
N_off <- round((ttl-N_on)*prop_offrent,0)
# Population size
N <- N_on + N_off
N

# Guessed sample SD of rent paid by on-campus students
oncampus_sd <- sd(sample[sample$Oncampus. == "Yes","Rent"])
oncampus_sd
# Guessed sample SD of rent paid by off-campus students
offcampus_sd <- sd(sample[sample$Oncampus. == "No","Rent"])
offcampus_sd

# Optimal allocation
n_on <- round(n*(oncampus_sd*N_on)/(oncampus_sd*N_on + offcampus_sd*N_off),0)
n_off <- n - n_on
satrified_n <- c(n_on, n_off)
satrified_n


### Stratified Sampling
set.seed(1214)
data_on <- read.csv("oncampus.csv")
data_on <- data_on[data_on$Family == "No", ]
sample_on_int <- sample.int(nrow(data_on), size = n_on, replace = FALSE)
sample_on <- data_on[sample_on_int,]
sample_on$exp <- ifelse(sample_on$Rent > 1000, 1,0)


data_off <- read.csv("offcampus.csv")
data_off <- data_off[data_off$Family == "No", ]
data_off <- data_off[data_off$Rent != "0", ]
sample_off_int <- sample.int(nrow(data_off), size = n_off, replace = FALSE)
sample_off <- data_off[sample_off_int,]
sample_off$exp <- ifelse(sample_off$Rent > 1000, 1,0)


avg_str <- (n_on/n)*mean(sample_on$Rent)+(n_off/n)*mean(sample_off$Rent)
avg_str
se_str <- sqrt((N_on/N)^(2)*var(sample_on$Rent)/n_on + (N_off/N)^(2)*var(sample_off$Rent)/n_off)
se_str
CI_str <- c(avg_str - 1.96*se_str, avg_str + 1.96*se_str)
CI_str


prop_str <- (n_on/n)*mean(sample_on$exp)+(n_off/n)*mean(sample_off$exp)
prop_str
se_prop_str <- sqrt((N_on/N)^(2)*mean(sample_on$exp)*(1-mean(sample_on$exp))/n_on + (N_off/N)^(2)*mean(sample_off$exp)*(1-mean(sample_off$exp))/n_off)
se_prop_str
CI_prop_str <- c(prop_str - 1.96*se_prop_str, prop_str + 1.96*se_prop_str)
CI_prop_str

# Histogram of two sampling methods
hist(sample$Rent, main="Histogram (Under SRS)", xlab = "Rent", xlim = c(400,2400), ylim = c(0,30))

sample_str <- rbind(sample_on, sample_off)
hist(sample_str$Rent, main="Histogram (Under Stratified Sampling)", xlab = "Rent", xlim = c(400,2400), ylim = c(0,30))
