library(tidyverse)
library(magrittr)
library(readxl)

setwd("~/Downloads")
strawb <- read_xlsx("strawberries-2022oct30-a.xlsx", col_names = TRUE)
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]
unique(strawb[1])
unique(strawb[2])
unique(strawb[3])
unique(strawb[5])
unique(strawb[6])
unique(strawb[7])
unique(strawb[14])
unique(strawb[16])
unique(strawb[17])
unique(strawb[18])
unique(strawb[19])
unique(strawb[20])
unique(strawb[21])
strawb <- subset(strawb,select = -c(4,5,8,9,10,11,12,13,14,15,16))
strawb %<>% arrange(Year, State)
colnames(strawb)
temp1 <- strawb %>% select(`Data Item`) %>% 
  distinct()
strawb2 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "items", "units"),
                               sep = ",",
                               fill = "right")
strawb3 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")
rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")
r_thiram <- grep("THIRAM", strawb$`Domain Category`)
r_thiram_1 <- grep("Thiram", 
                   strawb$`Domain Category`, 
                   ignore.case = T)
df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)
df_Bifenthrin <- grep("Bifenthrin", 
                      strawb$`Domain Category`, ignore.case = T)
df_methyl_bromide <- grep("methyl bromide", 
                          strawb$`Domain Category`, ignore.case = T)
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                               strawb$`Domain Category`, 
                               ignore.case = T)
df_chloropicrin <- grep("chloropicrin", 
                        strawb$`Domain Category`, 
                        ignore.case = T)
df_Telone <- grep("Telone", 
                  strawb$`Domain Category`, 
                  ignore.case = T)
pr_rec <- grep("STRAWBERRIES - PRICE RECEIVED", 
               strawb$Strawberries, 
               ignore.case = T)
type_organic <- grep("organic", 
                     strawb$type, 
                     ignore.case = T)

items_organic <- grep("organic", 
                      strawb$items, 
                      ignore.case = T)  

Domain_organic <- grep("organic", 
                       strawb$Domain, 
                       ignore.case = T)


Domain_Category_organic <- grep("organic", 
                                strawb$`Domain Category`, 
                                ignore.case = T)
same <- (intersect(type_organic, Domain_organic)==
           intersect(type_organic, Domain_organic))
length(same)==length(type_organic)


org_rows <- intersect(type_organic, Domain_organic)

strawb_organic <- strawb %>% slice(org_rows, preserve = FALSE)

strawb_non_organic <- strawb %>% filter(!row_number() %in% org_rows)
chem_rows <- grep("BEARING - APPLICATIONS", 
                  strawb_non_organic$type, 
                  ignore.case = T)
chem_rows_1 <- grep("chemical", 
                    strawb_non_organic$Domain, 
                    ignore.case = T)
ins <- intersect(chem_rows, chem_rows_1)
chem_rows_2 <- grep("chemical", 
                    strawb_non_organic$`Domain Category`, 
                    ignore.case = T)

ins_2 <- intersect(chem_rows, chem_rows_2)
strawb_chem <- strawb_non_organic %>% slice(chem_rows, preserve = FALSE)
rm(x, T, drop_cols, temp1, r_thiram, r_thiram_1,
   df_carbendazim, df_Bifenthrin, df_methyl_bromide, 
   df_1_3_dichloropropene, df_chloropicrin, df_Telone,
   pr_rec, type_organic, items_organic, Domain_organic,
   Domain_Category_organic, same, org_rows, chem_rows,
   chem_rows_1, chem_rows_2, ins, ins_2, cnames, i)
before_cols = colnames(strawb_chem)
T = NULL
x = length(before_cols)

for(i in 1:x){
  b <- length(unlist(strawb_chem[,i] %>% unique()) )
  T <- c(T,b)
}

drop_cols <- before_cols[which(T == 1)]
strawb_chem %<>% select(!all_of(drop_cols))
after_cols = colnames(strawb_chem)
temp1 <- strawb_chem %>% select(units) %>% distinct()
strawb_chem %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")
temp1 <- strawb_chem %>% select(chem_name) %>% unique()
length(unlist(temp1))
aa  <- grep("measured in", 
            strawb_chem$items, 
            ignore.case = T)
length(aa)
sum(strawb_chem$Domain == strawb_chem$dc1) == dim(strawb_chem)[1]
strawb_chem %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_chem %<>% rename(category = units)
strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")
strawb_chem %<>% rename(units = items)
bb  <- grep("CHEMICAL, ", 
            strawb_chem$dc1, 
            ignore.case = T)
length(bb)
chem <- 1:2112

non_chem_rows <- setdiff(chem, bb)
length(non_chem_rows)
temp1 <- strawb_chem %>% slice(non_chem_rows)
fertilizers <- temp1
rm(temp1, temps, temp3, aa, bb)
strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")

strawb_chem$dc1 %>% unique()

strawb_chem %<>% rename(chem_types = dc1)
bb  <- grep("BIFENTHRIN", 
            strawb_chem$chem_name, 
            ignore.case = T)

bifen <- strawb_chem %>% slice(bb)
strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")
strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 
aa <- which(strawb_chem$units == " LB")

bb <- which(is.na(strawb_chem$category))

sum(aa==bb)==length(aa)

# Gradescope problems
# 1 
# 285 CWT = 285*100LB = 28500 LB

# 2
m1 <- mean(c(231304956,1446458,211553703,1221571,19751253,224886))
sd1 <- sd(c(231304956,1446458,211553703,1221571,19751253,224886))
n1 <- 6
se1 <- sd1/sqrt(n1)
alpha = 0.05
t_score = qt(p=alpha/2, df=5,lower.tail=F)
print(t_score)
me1 <- se1*t_score
low1 <- m1-me1
upp1 <- m1+me1
# (-39779720, 194947329)

# 3
# NA

# 4
unique(strawb[10])
chem <- filter(strawb,Domain != 'ORGANIC STATUS' & Domain != 'TOTAL')
c <- grep("TOTAL",chem$`Domain Category`,ignore.case=T)
unique(chem[11])
# 175-36=139

# 5
ca <- filter(strawb, State == 'CALIFORNIA' & Domain != 'ORGANIC STATUS' 
             & Domain != 'TOTAL')
fl <- filter(strawb, State == 'FLORIDA' & Domain != 'ORGANIC STATUS' 
             & Domain != 'TOTAL')
flchem <- grep("TOTAL",fl$`Domain Category`,ignore.case=T)
unique(fl[11])
cachem <- grep("TOTAL",ca$`Domain Category`,ignore.case=T)
unique(ca[11])
# 142-119=23


