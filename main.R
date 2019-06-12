setwd("C:/Users/duviv/Documents/oSoc/VLINDER/VLAIO")
library(readxl)
library(dplyr)
library(anytime)


#import and clean NSZ data----
nsz.ela <- read_excel("NSZ.xlsx", sheet = "ELA VLAIO")
nsz.aom <- read_excel("NSZ.xlsx", sheet = "AOM VLAIO")
nsz.cb <- read_excel("NSZ.xlsx", sheet = "C&B VLAIO")


nsz.ela <- select(nsz.ela, 1:5)
nsz.aom <- select(nsz.aom, 1:5)
nsz.cb <- select(nsz.cb, 1:5)


columns <- c("Source", "Type", "Typedetail", "Date", "VAT")
colnames(nsz.ela) <- columns
colnames(nsz.aom) <- columns
colnames(nsz.cb) <- columns


nsz <- rbind(nsz.ela, nsz.aom, nsz.cb)
nsz.clean <- nsz \%>\%
  filter(substr(VAT, start = 1, stop = 4) == "BE 0") \%>\%
  mutate(VAT = gsub(" 0", "", VAT)) \%>\%
  mutate(VAT = gsub("\\.", "", VAT))


nsz.clean$Date <- as.Date(nsz.clean$Date, "\%d/\%m/\%Y")


#import and clean Belfirst data----
belfirst <- read_xlsx("belfirst.xlsx")
belfirst.clean <- belfirst \%>\%
  mutate(Ondernemingsnummer = paste("BE", Ondernemingsnummer, sep =""))


nsz.belfirst <- left_join(nsz.clean, belfirst.clean, by = c("VAT" = "Ondernemingsnummer"))
nsz.belfirst <- select(nsz.belfirst, 1:5)


#import Voka----
voka <- read_excel("Voka.xlsx", sheet = "Ondnrs")
voka <- select(voka, 1:5)
columns <- c("Source", "Type", "Typedetail", "VAT", "Date")
colnames(voka) <- columns
voka.clean <- voka \%>\%
  filter(substr(VAT, start = 1, stop = 8) == "BTW BE 0") \%>\%
  mutate(VAT = gsub("BTW BE 0", "BE", VAT)) \%>\%
  mutate(VAT = gsub("\\.", "", VAT))
voka.clean <- select(voka.clean,1:3,5,4)


voka.clean$Date <- as.Date(voka.clean$Date, "\%Y-\%m-\%d")


#import Vlaio----
vlaio <- read_excel("VLAIO advice.xlsx")
vlaio <- select(vlaio, 1:3, 5, 4)
columns <- c("Source", "Type", "Typedetail", "Date", "VAT")
colnames(vlaio) <- columns
vlaio.clean <- vlaio \%>\%
  filter(!(is.na(VAT))) \%>\%
  mutate(VAT = substr(VAT, start = 2, stop = 12)) \%>\%
  mutate(VAT = gsub("\\.", "", VAT)) \%>\%
  mutate(VAT = paste("BE", VAT, sep=""))


vlaio.clean$Date <- substring(vlaio.clean$Date, 1, 10)


##tried format dates with time but could not be compared with data from previous df (so did line 61 substring)
#should try fixing this not to loose the time data
vlaio.clean$Date[1] < nsz.clean$Date[10] #test if dates comparison work
vlaio.clean$Date[1] <- as.Date(vlaio.clean$Date[1], format="\%Y-\%m-\%d \%H:\%M:\%s")
vlaio.clean$Date[1] <- format(as.POSIXct(vlaio.clean$Date[1],format='\%m-\%d-\%Y \%H:\%M:\%s'),format='\%m/\%d/\%Y')


##import Vlaio financial aid----
vlaiofin <- read_excel("VLAIO financial aid.xlsx")
vlaiofin <- select(vlaiofin, 1:3, 5, 4)
colnames(vlaiofin) <- columns


##Strip leading zeroes
vlaiofin$VAT <- as.integer(vlaiofin$VAT)
vlaiofin.clean <- filter(vlaiofin, !(is.na(VAT))) \%>\%
  mutate(VAT = paste("BE", VAT, sep=""))


vlaiofin.clean$Date <- as.Date(vlaiofin.clean$Date, format="\%Y-\%m-\%d")


alldata <- rbind(nsz.clean, voka.clean, vlaio.clean, vlaiofin.clean)


#order by company and date ----
alldataord <- alldata[order(alldata$VAT, alldata$Date),]
#interaction without date is not taken into account
alldataord <- subset(alldataord, Date!=0)


#algorithm to make sequence of interaction for each company (df 'companies')----
companies <- data.frame(matrix(NA_character_, nrow = length(unique(alldataord$VAT)), ncol = 200), stringsAsFactors=FALSE)
#200 has been defined the maximum length of a company's sequence
companies[,1] <- unique(alldataord$VAT) #generate the df to be filled
companies[,2:(length(companies))] <- as.character(companies[,2:(length(companies))])


companies <- data.frame(matrix(NA_character_, nrow = length(unique(alldataord$VAT)), ncol = 200), stringsAsFactors=FALSE)
companies[,1] <- unique(alldataord$VAT) #generate the df to be filled


i <- 1 #goes through every interaction
j <- 1 #goes through every company in the df 'companies'
k <- 2 #fills the interactions for each company in the 'companies' df, starts at 2 because first column is for VAT number


# /!\ this algorithms requires the interaction df to be ordered by company, by date of interaction /!\ 
while (i<=length(alldataord$VAT))
{
  print(i)
  if(alldataord[i,5] == companies[j,1])
  {
    companies[j,k] <- alldataord[i,1]
    i<-i+1
    k<-k+1
  }
  else
  {
    j<-j+1
    k<-2
    companies[j,k] <- alldataord[i,1]
    k<-k+1
    i<-i+1
  }
  if(i==length(alldataord$VAT))
  {
    break
  }
  if(k==ncol(companies)) #if we arrive at the K'th interaction of a company, go to the next interaction until new company
  {
    while(alldataord[i,5]==alldataord[i-1,5])
    {
      i<-i+1
    }
  }
}


#count occurence of each sequence ----
#this section has been made so the sequences are reduced to the first 5 interactions
sequences <- count(companies, vars = companies$X2, companies$X3, companies$X4, companies$X5, companies$X6) #each var adds another interaction in the sequence
sequences <- data.frame(matrix(unlist(sequences), nrow=length(sequences[[1]]), byrow=F)) #convert to a dataframe
colnames(sequences) <- c("interaction1", "interaction2", "interaction3", "interaction4", "interaction5", "n")


#compute probability for each sequence ----
f <- sequences$n
f2 <- as.numeric(levels(f))[f] #transforms the n column from factor to numeric so it can be summed
sequences$n <- f2


total <- sum(sequences$n)
sequences$probability <- (sequences[,6]) / total


options("scipen" = 10)
options()$scipen #when printing the probabilities, will display the numbers like 0,4 instead of 4e-01


#exploration of sequences----

#sequences with highest probabilities
m <- 30 #number of sequences we want to see
sequences <- sequences[order(sequences$n),]
tail(sequences, m)
captured.proba <- sum(sequences$probability[m:length(sequences$probability)]) #probability captured by the number of sequences selected in 'm'
captured.proba


#probability that VLAIO (or other) is at least once in the interaction sequence
sequences[,1:5] <- lapply(sequences[,1:5], as.character) #convert interactions from factor to character
a <- 'VLAIO'
institution.sequences <- subset(sequences, interaction1 == a | interaction2 == a | interaction3 == a | interaction4 == a | interaction5 == a)


sum(institution.sequences$probability) #gives probability to get at least one interaction with institution defined in 'a'
institution.sequences <- institution.sequences[order(sequences$n),]
institution.sequences <- institution.sequences[rowSums(is.na(institution.sequences)) != ncol(institution.sequences), ] #removes the lines of Na


#Partners following each other wherever in the sequence----
p <- unique(alldataord[,1])
n <- nrow(p)
n #number of different partners
partner_seq <- data.frame(matrix(0, nrow=n, ncol = n))


names(partner_seq) <- (t(p))
row.names(partner_seq) <- t(as.vector(p[,1]))


#import companies not to re-run everything above
# companies <- read.csv("companies.csv")
# companies <- subset(companies[,2:(length(companies))])


i <- 1
j <- 1
k <- 2
l <- 1


while(l <= length(companies[,1]))
{
  print(paste(l/length(companies[,1])*100, "\%"))
  if(!is.na(companies[l,k+1]))
  {
    i <- grep(companies[l,k], row.names(partner_seq))
    j <- grep(companies[l,k+1], colnames(partner_seq))
    partner_seq[i,j] <- partner_seq[i,j] + 1
    k <- k+1
  }
  if(is.na(companies[l,k+1]))
  {
    l <- l+1
    k <- 2
  }
}


proba_partner_seq <- partner_seq/(sum(rowSums(partner_seq)))


#Type of interaction following each other wherever in the sequence----
types <- unique(alldataord[,2])
types


Typedetails <- unique(alldataord[,3])
Typedetails


number_of_types <- as.data.frame(table(unlist(alldataord[,3])))
names(number_of_types) <- c("interaction_type", "frequency")
number_of_types <- number_of_types[order(-number_of_types$frequency),]
number_of_types


na_percentage <- colSums(is.na(alldataord))/length(alldataord[,1])
na_percentage


#create the df to be filled
companies_type <- data.frame(matrix(NA_character_, nrow = length(unique(alldataord$VAT)), ncol = 200), stringsAsFactors=FALSE)
companies_type[,1] <- unique(alldataord$VAT) #generate the df to be filled
alldataord <- subset(alldataord, !is.na(alldataord[,3]))
alldataord$Typedetail <- as.character(alldataord$Typedetail)


#fill the df
i <- 1
j <- 1
k <- 2


# /!\ this algorithms requires the interaction df to be ordered by company, by date of interaction /!\ 
#It is the same as the one used from line 100 except now we fill the df with interaction type and not partner
while (i<=length(alldataord$VAT))
{
  print(i)
  if(alldataord[i,5] == companies_type[j,1])
  {
    companies_type[j,k] <- alldataord[i,3]
    i<-i+1
    k<-k+1
  }
  else
  {
    j<-j+1
    k<-2
    companies_type[j,k] <- alldataord[i,3]
    k<-k+1
    i<-i+1
  }
  if(i==length(alldataord$VAT))
  {
    break
  }
  if(k==ncol(companies_type)) #if we arrive at the K'th interaction of a company, go to the next interaction until new company
  {
    while(alldataord[i,5]==alldataord[i-1,5])
    {
      i<-i+1
    }
  }
}


#the results are different from the ones of the companies df as we did not use the ones where Typedetail is NaN


#create the df
p <- unique(alldataord[,3])
n <- nrow(p)
n #number of different partners
int_types_cross <- data.frame(matrix(0, nrow=n, ncol = n))
names(int_types_cross) <- (t(p))
row.names(int_types_cross) <- t(as.vector(p[,1]))


#fill the df
i <- 1
j <- 1
k <- 2
l <- 1


while(l <= length(companies_type[,1]))
{
  print(paste(l/length(companies_type[,1])*100, "\%"))
  if(!is.na(companies_type[l,k+1]))
  {
    i <- grep(companies_type[l,k], row.names(int_types_cross))
    j <- grep(companies_type[l,k+1], colnames(int_types_cross))
    int_types_cross[i,j] <- int_types_cross[i,j] + 1
    k <- k+1
  }
  if(is.na(companies_type[l,k+1]))
  {
    l <- l+1
    k <- 2
  }
}


proba_int_types_cross <- int_types_cross/(sum(rowSums(int_types_cross)))