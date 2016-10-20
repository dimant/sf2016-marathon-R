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

allAges <- as.integer(unlist(lapply(marathon$sex_age, getState)))
ages <- allAges[!is.na(allAges)]

ageGroupsUSA <- read.table(file = "2015-age-group-estimate.txt")
row.names(ageGroupsUSA) <- ageGroupsUSA[, 1]
ageGroupsUSATotal <- ageGroupsUSA[1, 2]
ageGroupsUSAAbs <- ageGroupsUSA[-1, -1]
ageGroupsUSARel <- ageGroupsUSAAbs / ageGroupsUSATotal

ageGroupsMarathonAbs <- table(cut(ages, breaks = seq(0, 90, 5)))
ageGroupsMarathonRel <- ageGroupsMarathonAbs / sum(ageGroupsMarathonAbs)

ageBuckets <- arules::discretize(ages, categories = 4)

ageSexEstimateUSA <- read.table(file = "2015-age-sex-estimate.txt", header = TRUE)
ageSexEstimateUSA[, 3] <- ageSexEstimateUSA[, 1] - ageSexEstimateUSA[, 2]
colnames(ageSexEstimateUSA)[3] <- "F"
ageSexEstimateMarathon <- cut(ages, breaks = c(0, 14, 24, 54, 64, 150))
ageSexEstimateMarathon <- table(ageSexEstimateMarathon[genders == "M"])

ageSexEstimates <- rbind(t(as.vector(ageSexEstimateUSA[2])), as.vector(ageSexEstimateMarathon))
row.names(ageSexEstimates) <- c("USA", "Marathon")
ageSexEstimates[1,] <- ageSexEstimates[1,] / sum(ageSexEstimates[1,])
ageSexEstimates[2,] <- ageSexEstimates[2,] / sum(ageSexEstimates[2,])

states <- unlist(lapply(marathon$location, getState))

##################################################################################################

# 1) mean
mean(ages)

# 2) leverage of single observation
mean(c(ages, 100000))

# 3) median is robust
median(ages)

median(c(ages, 100000))

# 4) var sd sqrt(sd)
var(ages)

sd(ages)

sqrt(var(ages))

# 5) IQR
quantile(ages, probs = c(0, 0.25, 0.5, 0.75, 1))
45 - 28 # <- IQR

# 6) Normal Distribution Marathon Ages Estimates
curve(dnorm(x, mean(ages), sd(ages)), from = min(ages), to = max(ages))

# 7) skewness
skewness(ages)

# 8) kurtosis
kurtosis(ages)

###############################################################
# Presenting Data

# 9) table genders
table(genders)

# 10) ratio genders
4171 / 2163

# 11) barplot genders
barplot(table(genders))

# 12) time spent working - bureau of labor statistics
# https://goo.gl/25XzTn

# easter egg
# https://goo.gl/lJ463D

# 13) histogram ages
hist(ages, 10)

# 14) histogram ages bin size
hist(ages, 5)
hist(ages, 100)

