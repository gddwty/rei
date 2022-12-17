
# setting working directory
getwd()
setwd()

# loading libraries
library(tidyverse)
library(ggrepel)

# importing ras dataset
read.csv("ras3.csv", header = TRUE, strip.white = TRUE,
               stringsAsFactors = FALSE, na.strings = c("NA", "")) -> df

# inspect the variables in df
names(df)

# inspecting certain variable to see difference
df$LX11X2014
df$LX11X2014X

# choosing/selecting certain variables/column into a new dataframe

df %>% select(COUNTRY,LX01X2014X,LX03X2014X,LX04X2014X,LX05X2014X,LX06X2014X,
                     LX07X2014X,LX08X2014X,LX09X2014X,LX12X2014X,LX13X2014X,
                     LX15X2014X,LX17X2014X,LX18X2014X,LX19X2014X,LX20X2014X,LX22X2014X,
                     LX23X2014X,LX24X2014X,LX26X2014X,LX31X2014X,LX42X2014X,LX48X2014X,
                     LX50X2014X,LX51X2014X,MX03X2014X,MX21X2014X,MX22X2014X,MX23X2014X,MX29X2014X) -> df2


as_tibble(df2)

# export df2 to csv to recode the value of MX variables to 0/1
write.csv(df2, "C:\\Users\\Thinkpad\\Desktop\\ras2\\rei2014.csv")

# manually recode MX variables in Excel from previously valued 0-3 to 0/1

#importing back the csv with MX variables has been recoded to 0/1
read.csv("rei2014.csv", header = TRUE, strip.white = TRUE,
         stringsAsFactors = FALSE, na.strings = c("NA", "")) -> df3

#checking MX variables
install.packages("vtable")
library(vtable)
summary(df3$MX21X2014X)
summary(df3$MX22X2014X)
summary(df3$MX23X2014X)        
summary(df3$MX29X2014X)        
        
# sum of each rows in the dataframe (to calculate each country's religious legislations/REI score)
REI <- rowSums(df3[, 2:30], na.rm = TRUE)
print(REI)

#append rei as a new column to the data frame
df3$REI <- REI

# exporting df3 to csv
write.csv(df3, "C:\\Users\\Thinkpad\\Desktop\\ras2\\rei2014X.csv")

######## VDEM DATASET WORKS #####

#importing VDem dataset

df4 <- readRDS(file.choose())
names(df4)

# taking only variables of interest 

df4 %>% select(country_name,year,v2x_polyarchy,v2x_libdem)-> df5

# filter only year 2014
df5 %>% filter(year==2014)->vdem2014

# exporting dataframe into csv to be before merged
write.csv(vdem2014, "C:\\Users\\Thinkpad\\Desktop\\ras2\\vdem2014.csv")

##### Merging REI and VDem dataset by COUNTRY manually in Excel (by adding the value from VDem to REI 2014)

# importing new dataframe that has been cleaned and merged with variables from VDem
read.csv("data2.csv", header = TRUE, strip.white = TRUE,
         stringsAsFactors = FALSE, na.strings = c("NA", "")) -> df6

# mutating REI to percentage of possible scores
df6 %>%
  mutate(REIX = REI/29) -> df7

# summarizing variable to get descriptive statistics
install.packages("vtable")
library(vtable)
st(df7)
summary(df7$REI)
summary(df7$regime)

# filtering only democracy and create a df
df7 %>% filter(regime=="Democracy") -> dem

# filtering only autocracies and create a df
df7 %>% filter(regime=="Autocracy") -> auto 

# summarizing democracy
st(dem)
summary(dem)

# summarizing autocracy
st(auto)
summary(auto)

# boxplot comparinng distribution of scores in democracies and autocracies
boxplot(REIX ~ regime, data = df7, 
        xlab = "Regime",
        ylab = "Religion Enforcement")

boxplot(REI ~ regime, data = df7, 
        xlab = "Regime",
        ylab = "Religion Enforcement")


# scatterplot visualization of countries along two dimensions: polyarchy index and REI
ggplot(data = df7,
       mapping = aes(x=REIX,
                     y=v2x_polyarchy,
                     color=regime
       ))+
  geom_point(alpha=.7,
             size=2)+
  geom_text_repel(aes(label=COUNTRY))+
  xlim(0,1)+
  ylim(0,1)+
  xlab("Religion Enforcement Index")+
  ylab("Polyarchy Index")

ggplot(data = df7,
       mapping = aes(x=v2x_polyarchy,
                     y=REIX,
                     color=regime
       ))+
  geom_point(alpha=.7,
             size=2)+
  geom_text_repel(aes(label=COUNTRY))+
  xlim(0,1)+
  ylim(0,1)+
  xlab("Polyarchy Index")+
  ylab("Religion Enforcement Index")






