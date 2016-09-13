#install.packages(c("rjson", "chron", "Hmisc", "arules", "homals", "devtools", "e1071"))
#library(devtools)
#install_github("dimant/murmurPCA/murmurPCA")

library(rjson)
library(chron)
library(Hmisc)
library(arules)
library(homals)
library(murmurPCA)
library(e1071)

marathon <- fromJSON(file = "./results-utf8.json")
marathon <- do.call(rbind, lapply(marathon, data.frame, stringsAsFactors = FALSE))
row.names(marathon) <- marathon[, 1]
marathon <- marathon[-1]
marathon$bib <- as.numeric(marathon$bib)

secPerDay = 24 * 60 * 60

paces <- as.vector(times(unlist(sapply(marathon$pace, function(x) { paste("00:", x, sep = ""); }))) * secPerDay)

# all finishing times in seconds
allTimes <- as.numeric(times(marathon$net_time)) * secPerDay
allQuantiles <- cut2(allTimes, g = 10)
allAgeGrades <- unlist(lapply(marathon$age_grade, function(x) { as.numeric(substring(x, 0, nchar(x) - 1)) }))
allAgeGradeQt <- cut2(allAgeGrades, g = 10)



genders <- unlist(lapply(marathon$sex_age, substring, 1, 1))

getState <- function(location) {
    l <- nchar(location)
    substring(location, l - 1, l)
}

getCharFreq <- function(name) {
    table(strsplit(tolower(gsub(" |-", "", name)), ""))
}

getLetterFreq <- function(l, t) {
    c <- t[l]
    if (is.na(c)) 0 else c
    }

getAllLettersFreq <- function(name) {
    t <- getCharFreq(name)
    f <- unlist(lapply(letters, getLetterFreq, t))
    names(f) <- letters
    as.integer(f)
}

normalize <- function(v) {
    vpos <- v + abs(min(v))
    vpos / sum(vpos)
}

nameLetterFreqs <- do.call(rbind, lapply(marathon$name, getAllLettersFreq))
colnames(nameLetterFreqs) <- letters

predictWithLetters <- function(freqs, outcome) {
    pca <- mrmrPCA(freqs, outcome)
    expl <- normalize(pca$loadings[, 1])
    names(expl) <- letters
    expl
}

letterTime <- predictWithLetters(nameLetterFreqs, allQuantiles)

letterAgeGrade <- predictWithLetters(nameLetterFreqs, allAgeGradeQt)

ages <- as.integer(unlist(lapply(marathon$sex_age, getState)))

ageGroupsUSA <- read.table(file = "2015-age-group-estimate.txt")
row.names(ageGroupsUSA) <- ageGroupsUSA[, 1]
ageGroupsUSATotal <- ageGroupsUSA[1, 2]
ageGroupsUSAAbs <- ageGroupsUSA[-1, -1]
ageGroupsUSARel <- ageGroupsUSAAbs / ageGroupsUSATotal

ageGroupsMarathonAbs <- table(cut(ages, breaks = seq(0, 90, 5)))
ageGroupsMarathonRel <- ageGroupsMarathonAbs / sum(ageGroupsMarathonAbs)

ageBuckets <- arules::discretize(ages, categories = 4)

ageSexEstimateUSA <- read.table(file = "2015-age-sex-estimate.txt", header = TRUE)
ageSexEstimateMarathon <- cut(ages, breaks = c(0, 14, 24, 54, 64, 150))
ageSexEstimateMarathon <- table(ageSexEstimateMarathon[genders == "M"])

ageSexEstimates <- rbind(t(as.vector(ageSexEstimateUSA[2])), as.vector(ageSexEstimateMarathon))
row.names(ageSexEstimates) <- c("USA", "Marathon")
ageSexEstimates[1,] <- ageSexEstimates[1,] / sum(ageSexEstimates[1,])
ageSexEstimates[2,] <- ageSexEstimates[2,] / sum(ageSexEstimates[2,])

states <- unlist(lapply(marathon$location, getState))

# mean, median
# summary(allTimes)
# standard deviation
# sqrt(var(allTimes))
# skewness(allTimes)
# kurtosis(allTimes)

# normal distribution
# plot(dnorm, ylab = "p", from = -3, to = 3)

# barplot(table(genders))

#par(mfrow = c(2, 2))
#hist(allTimes, main = "5 bins", xlab = "Time", breaks = 5)
#hist(allTimes, main = "10 bins", xlab = "Time", breaks = 10)
#hist(allTimes, main = "100 bins", xlab = "Time", breaks = 100)
#par(mfrow = c(1, 1))

# boxplot(allTimes, ylab="Time")

# tabular representation
# table(states)
# table(genders,ageBuckets)
# barplot(sort(table(states), decreasing = TRUE)[1:10])

# correlation bib number and time
# cor.test(marathon$bib, allTimes)

# finishing time vs gender - if we adjust for age, the genders are exactly equal
# this is likely due to the imbalance of gender groups within the participants
# boxplot(allTimes ~ genders, notch=TRUE)
# boxplot(allAgeGrades ~ genders, notch=TRUE)
# this outlier probably didn 't provide his age
# marathon[2125,]

# gender ratios compared with CIA factbook estimate for USA in 2015
# https://www.cia.gov/library/publications/the-world-factbook/fields/2018.html
# total gender ratio for USA is 97 males to each 100 females
# binom.test(sum(genders == "M"), length(genders), 97 / (97 + 100))
# barplot(table(genders))

# age versus gender
# boxplot(ages ~ genders, notch = TRUE)

# gender ratios broken down by age group and compared with CIA factbook
# chisq.test(rbind(t(as.vector(ageSexEstimateUSA[2])), as.vector(ageSexEstimateMarathon)))
# barplot(ageSexEstimates[2,] - ageSexEstimates[1,])

# performance by age - unless we bucket the data no pattern is apparent
# boxplot(ages ~ allQuantiles, notch=TRUE)
# boxplot(ages ~ allAgeGradeQt, notch = TRUE)
# plot(ageBuckets ~ allQuantiles)

# age group sizes relative to census buerau expectation for USA in 2015
# chisq.test(as.table(rbind(ageGroupsUSAAbs, ageGroupsMarathonAbs)))
# barplot(ageGroupsMarathonRel - ageGroupsUSARel)

# the red herring
# letter frequency correlated with finishing time
# seems like a red herring especially because if we adjust for age, the correlation is reversed
#par(mfrow=c(2,1))
#barplot(sort(letterTime, decreasing = TRUE), ylab = "% of variance explained", main="% variance of time explained by letter.")
#barplot(sapply(split(nameLetterFreqs[, 1], allQuantiles), median), xlab = "bins", names = 1:10, ylab = "median frequency of letter a", main = "Frequency of letter a by time.")
#par(mfrow = c(1, 1))

#par(mfrow = c(2, 1))
#barplot(sort(letterAgeGrade, decreasing = TRUE), ylab = "% of variance explained", main = "% variance of age grade explained by letter.")
#barplot(sapply(split(nameLetterFreqs[, 1], allAgeGradeQt), median), xlab = "bins", names = 1:10, ylab = "median frequency of letter a", main = "Frequency of letter a by age grade.")
#par(mfrow = c(1, 1))

