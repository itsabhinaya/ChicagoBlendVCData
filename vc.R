install.packages('gsheet')
library(gsheet)
library(dplyr)
library(tidyr)

#Get data
url <- 'docs.google.com/spreadsheets/d/11eBNIVZka9XwNzQ2BxF4S1Z2C6ZsWiUCJkrfu_DxDXQ/edit#gid=1596477396'
data <- gsheet2tbl(url)

#Rename column names
colnames(data) = data[1, ]
data = data[-1, ]   

#Split the first column from "Firm 1 - Employee 1" to "Firm 1" and "Employee 1"
data <- separate(data, Employee, into = c("firm", "employee"), sep = " - ")
data <- type.convert(data)
write.csv(data, file = "all_data.csv")

gender_data <- data
gender_data$gender <- ifelse(gender_data$Female == 1, "Female", "Male")
write.csv(gender_data, file = "gender_data.csv")


# 72% of the industry employees are at VC firms and the rest are at angel groups, family offices and others. 

type_data <- data
type_data$type <- ifelse(type_data$`VC Firm` == 1, "VC Firm",ifelse(type_data$`Angel Group` == 1, "Angel Group",ifelse(type_data$`Family Office` == 1, "Family Office",ifelse(type_data$Other == 1, "Other",""))))
write.csv(type_data, file = "type_data.csv")
type_data %>% group_by(type) %>% tally()
type_data_a = type_data
type_data_a$employee <- NULL #delete a column I don't need
type_data_a$`Size of Most Recent Fund ($MM)` <- NULL
type_data_a$`Size of Most Recent Fund ($MM) Grouping` <- NULL

type_data_a = type_data_a %>% group_by(firm, type) %>% summarise_all(funs(sum(., na.rm = TRUE)))

tech_focus = data[,c(1,2,30:35)]
tech_focus %>% group_by(`Main Focus Midwest?`) %>% tally()
tech_focus %>% group_by(firm, `Main Focus Midwest?`) %>% tally()

tech_focus_group = tech_focus
tech_focus_group$`Main Focus Midwest?` <- ifelse(tech_focus$`Main Focus Midwest?` ==1, "Midwest Focus","National/Global")
tech_focus_group$`First Time Fund?` <- ifelse(tech_focus$`First Time Fund?` ==1, "First Time Fund","Multiple Funds")
tech_focus_group$industry <- ifelse(tech_focus_group$`Industry Focus - Tech` == 1,"Tech",ifelse(tech_focus_group$`Industry Focus - Consumer` == 1, "Consumer",ifelse(tech_focus_group$`Industry Focus - General` == 1,"General",ifelse(tech_focus_group$`Industry Focus - Other` ==1,"Other",""))))
tech_focus_group_viz = tech_focus_group %>% group_by(firm, `Main Focus Midwest?`,`First Time Fund?`,industry) %>% tally()

tech_focus_group_viz$perc = (tech_focus_group_viz$n/354)*100
write.csv(tech_focus_group_viz, file = "tech_focus_group_viz.csv")

tech_type_viz =  tech_focus_group %>% group_by(`First Time Fund?`,`Main Focus Midwest?`) %>% tally()
tech_type_viz$perc = tech_type_viz$n/354*100
tech_type_viz

tech_focus_group %>% group_by(`Main Focus Midwest?`) %>% tally()
tech_focus_group %>% group_by(`First Time Fund?`) %>% tally()

#grouping data by firm and most recent fund
firm_data<- data
firm_data$employee <- NULL #delete a column I don't need
firm_data$`Size of Most Recent Fund ($MM)` <- NULL
firm_data_group = firm_data %>% group_by(firm,`Size of Most Recent Fund ($MM) Grouping`) %>% summarise_all(funs(sum(., na.rm = TRUE)))
write.csv(firm_data_group, file = "firm_size_group_data_by.csv")

firm_data_no_fund_group<- firm_data
firm_data$`Size of Most Recent Fund ($MM) Grouping` <- NULL
firm_data_group_no_size = firm_data %>% group_by(firm) %>% summarise_all(funs(sum(., na.rm = TRUE)))
write.csv(firm_data_group, file = "firm_group_data.csv")



#1. Number of data points 
# There are 71 firms in Chicago that have made at least one venture investment with 354 employees. 
people_sum <- data %>% tally()
people_sum

#Number of firms
count(data %>% group_by(firm) %>% tally())

#Number of firms without a female employee
gender_sum <- data %>% group_by(firm) %>% summarise(female =  sum(as.numeric(Female)),male = sum(as.numeric(Male)))

gender_total = gender_sum %>% summarise(female =  sum(as.numeric(female)),male = sum(as.numeric(male)))

gender_total$female
gender_total$male

paste(round((gender_total$female / people_sum)*100,0), "% Females",round((gender_total$male / people_sum)*100,0), "% Males")


gender_sum$female_only <- ifelse(gender_sum$female>0, "Female", "No Female")
total_gender_sum <- (gender_sum %>% group_by(female_only) %>% tally()) #There are 28 firms with no females
paste(round(total_gender_sum$n [1]*100/sum(total_gender_sum$n),0), "% of firms do not have a female employee")
paste(round(total_gender_sum$n [2]*100/sum(total_gender_sum$n),0), "% of firms have a female employee")

#Number of firms without a male employee
gender_sum$male_only <- ifelse(gender_sum$male>0, "Male", "No Male")
total_gender_sum_male = (gender_sum %>% group_by(male_only) %>% tally()) #There are 6 firms with no males

paste(round(total_gender_sum_male$n [2]*100/sum(total_gender_sum_male$n),0), "% of firms have no male employee")

write.csv(gender_sum, file = "gender_sum.csv")


#Ethnicity
eth <- type.convert(data[,c(1,2,9:13)])
eth$pc <- rowSums(eth[,c(4:7)])
eth$pc_type <- ifelse(eth$pc >= 1, "Person of Color","White")
eth$type <- ifelse(eth$White == 1, "White",ifelse(eth$Black == 1, "Black",ifelse(eth$`Asian/Indian` == 1, "Asian/Indian",ifelse(eth$`Latino/a` == 1, "Latino/a",ifelse(eth$`Other Non-White` == 1, "Other Non-White","")))))

eth_sum <- eth %>% group_by(firm) %>% summarise(white =  sum(White),pc = sum(pc))
eth_sum$type <- ifelse(eth_sum$pc >= 1, "Person of Color", "White")
write.csv(eth_sum, file = "eth_sum.csv")

eth_sum %>% group_by(type) %>% tally()

eth_sum_group_type <- eth %>% group_by(firm, type, pc_type) %>% summarise(white =  sum(White),pc = sum(pc))
write.csv(eth_sum_group_type, file = "eth_su_PC.csv")


eth_sum %>% summarise(white =  sum(white),pc = sum(pc))

total_eth_sum <- (eth %>% group_by(pc_type) %>% tally()) #There are 28 firms with no females
paste(round(total_eth_sum$n [2]*100/sum(total_eth_sum$n),0), "% of firms do not have a people of color employee")
paste(round(total_eth_sum$n [1]*100/sum(total_eth_sum$n),0), "% of firms have a person of color employee")

write.csv(eth, file = "eth.csv")

#Executive Level
exec_data <- cbind(gender_data[,1:6],gender_data$gender,eth[,9:10])
exec_data$exec <- ifelse(exec_data$`Executive Investor`==1,"Executive Investor",ifelse(exec_data$`Executive Non-Investor`==1,"Executive Non-Investor",ifelse(exec_data$`Non-Executive Investor`==1,"Non-Executive Investor",ifelse(exec_data$`Non-Executive Non-Investor`==1,"Non-Executive Non-Investor","N/A"))))
write.csv(exec_data, file = "exec_data.csv")

final_data <- cbind(data[,1:2],exec_data$exec,gender_data$gender,eth[,9:10],type_data$type,tech_focus_group$industry,data[,24:25])
colnames(final_data) <- c("Firm","Employee","Type of Investor","Gender","Ethnicity","Ethnicity Detailed","Type of Firm","Industry Focus","Size of Most Recent Fund ($MM)","Size of Most Recent Fund ($MM) Grouping")
write.csv(final_data, file = "final_data.csv")
