#load the libraries
library(haven)
library(ggplot2)
library(dplyr)
library(data.table)

file <- read_sas("medicare_referring_puf.sas7bdat")

length(unique(df.medicare_npi$hcpcs_code))
#hcpcs code - 513 level
#

# the following dataset is created from medicare_ps_puf.
# It is an aggregation of medicare payment amount and submitted charge amount
#  by state and hcpcs code
df.medicare <- read_sas("hcpcs.sas7bdat")
#write the data frame to a csv
write.csv(as.data.table(df.medicare) ,"hcpcs.csv")
length(unique(df.medicare$nppes_provider_state)) #61


str(df.medicare)
#convert the following variables type to factor
df.medicare$hcpcs_code <- as.factor(df.medicare$hcpcs_code)
df.medicare$nppes_provider_state <- as.factor(df.medicare$nppes_provider_state)

#the following gives us the most expensive procdure (hcpcs_code) by state
state_agg <- aggregate(charged_amt ~ nppes_provider_state, data = df.medicare, FUN = max)
medicare_state_charge <- merge(state_agg, df.medicare)
#write the data to a csv file
write.csv(as.data.table(medicare_state_charge), "medicare_state_charge.csv")

medicare_state_charge <- read.csv("medicare_state_charge.CSV")
medicare_state_charge$hcpcs_code <- as.factor(medicare_state_charge$hcpcs_code)
table(medicare_state_charge$hcpcs_code)

#plot the data to see the provider who charges the most
ggplot(data = medicare_state_charge , aes( x = hcpcs_code))+
  geom_bar(aes(fill = hcpcs_code) )



# read the medicare oayment aggregate file
df.medicare_npi <- read_sas("medicare_ps_npi_puf.sas7bdat")
#convert required coulmns to factor type
df.medicare_npi$npi <- as.factor(df.medicare_npi$npi)
df.medicare_npi$nppes_provider_gender <- as.factor(df.medicare_npi$nppes_provider_gender)
df.medicare_npi$nppes_provider_city <- as.factor(df.medicare_npi$nppes_provider_city)
df.medicare_npi$nppes_provider_state <- as.factor(df.medicare_npi$nppes_provider_state)
df.medicare_npi$nppes_provider_country <- as.factor(df.medicare_npi$nppes_provider_country)
df.medicare_npi$provider_type <- as.factor(df.medicare_npi$provider_type)
df.medicare_npi$medicare_participation_indicator <- as.factor(df.medicare_npi$medicare_participation_indicator)
df.medicare_npi$nppes_entity_code <- as.factor(df.medicare_npi$nppes_entity_code)
df.medicare_npi$provider_type <- as.factor(df.medicare_npi$provider_type)

#gives the count of providers by entity code
#individual/ organization
group_by(df.medicare_npi, nppes_entity_code) %>% 
  summarise(count = length(npi))

#get count of providers based on speciality 
providertype <-as.data.frame(table(df.medicare_npi$provider_type))
#order speciality based on providers count
providertype <- providertype[order(providertype$Freq),]

#count of providers by state
stateproviders <- as.data.frame(table(df.medicare_npi$nppes_provider_state))
stateproviders <- stateproviders[order(stateproviders$Freq),]

#get providers of type individual
df.individuals <- df.medicare_npi[which(df.medicare_npi$nppes_entity_code=="I"),]
#get organization providers
df.organizations <- df.medicare_npi[which(df.medicare_npi$nppes_entity_code=="O"),]

#plot payment against the provider types
ggplot(data = df.organizations, aes(x= npi, y = total_medicare_payment_amt))+ geom_point()
ggplot(data = df.individuals, aes(x= npi, y = total_medicare_payment_amt))+ geom_point()

#write the aggregate file into csv
write.csv(dt,"medicare_npi.csv")

#following practices have charged the most (analysis from tableau)
temp1 <- df.medicare_npi[which(df.medicare_npi$provider_type == "Ambulatory Surgical Center"),]
temp2 <- df.medicare_npi[which(df.medicare_npi$provider_type == "Cardiac Surgery"),]
temp1$submitted_service <- temp1$total_med_submitted_chrg_amt/temp1$total_med_services
temp2$submitted_service <- temp2$total_med_submitted_chrg_amt/temp2$total_med_services

temp2 <- temp2[order(temp2$submitted_service, decreasing = TRUE),]
temp1 <- temp1[order(temp1$submitted_service, decreasing = TRUE),]

#get the top 10 providers who charge the most
top101 <- temp1[1:10,]
top102 <- temp2[1:10,]

#plot the providers payment patterns for the above two practices
ggplot(data = top101, aes(x = reorder( npi, -submitted_service), y = submitted_service, fill = npi))+
  geom_bar(stat="identity")
ggplot(data = top102, aes(x = reorder( npi, -submitted_service), y = submitted_service, fill = npi))+
  geom_bar(stat="identity")




