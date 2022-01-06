library(foreign)
library(memisc)


##########################################################################
#
# Functions
#
##########################################################################
# Function to code scales
CodeScales <- function(variable, reverse=FALSE, standardize=TRUE){
    # Args:
    #   variable: The variable to be re-coded
    #   reverse (bool): Whether the question was re-verse coded 
    recoded.variable <- rep(NA, length(variable))
    if(reverse == FALSE){
        recoded.variable[variable == "Strongly agree"] = "1"
        recoded.variable[variable == "Agree"] = "2"
        recoded.variable[variable == "Somewhat agree"] = "3"
        recoded.variable[variable == "Neither agree nor disagree"] = "4"
        recoded.variable[variable == "Somewhat disagree"] = "5"
        recoded.variable[variable == "Disagree"] = "6"
        recoded.variable[variable == "Strongly disagree"] = "7"
        recoded.variable[variable == ""] = NA
    } 
    if(reverse == TRUE){
        recoded.variable[variable == "Strongly agree"] = "7"
        recoded.variable[variable == "Agree"] = "6"
        recoded.variable[variable == "Somewhat agree"] = "5"
        recoded.variable[variable == "Neither agree nor disagree"] = "4"
        recoded.variable[variable == "Somewhat disagree"] = "3"
        recoded.variable[variable == "Disagree"] = "2"
        recoded.variable[variable == "Strongly disagree"] = "1"
        recoded.variable[variable == ""] = NA
    }
    recoded.variable.numeric <- as.numeric(as.character(recoded.variable))
    #if(standardize == TRUE){
    #    recoded.variable.numeric <- (recoded.variable.numeric - mean(recoded.variable.numeric, na.rm=T)) / sd(recoded.variable.numeric, na.rm=T)
    #}
    return(recoded.variable.numeric)
}


VariableScore<-function(vars){
    # Function to combine several seperate variables into a single variable with a numeric score.
    #
    # Args:
    #   vars: A cbind object of the different numeric variables to be combined. 
    #
    # Returns:
    #   score: a numeric vector of the seperate variables combined into a single value.
    mat <- cbind(vars)
    number.answered <- numeric(nrow(mat))
    for(i in 1:nrow(mat)){ 
        number.answered[i]<-sum(is.na(mat[i,]) == FALSE)
    }
    total <- numeric(nrow(mat))
    for(i in 1:nrow(mat)){
        total[i]<-sum(na.omit(mat[i,]))
    }
    score <- numeric(nrow(mat))
    for(i in 1:nrow(mat)){
        score[i]<-total[i]/number.answered[i]
    }
    return(score)		
}


Standardize <- function(variable){
    variable.out <- (variable - mean(variable, na.rm=T)) / sd(variable, na.rm=T)
    return(variable.out)
}
##########################################################################
#
# Pre-Process the Data
#
##########################################################################
# Load the data
raw_data <- read.csv('TC Data July 2020.csv', header=TRUE)
raw_data <- droplevels(raw_data[-c(1,2),])

# Drop those that do no consent or failed the manipulatin check
length(which(raw_data$attention_check == "Strongly Agree,Strongly disagree"))
length(which(!raw_data$attention_check == "Strongly Agree,Strongly disagree"))
raw_data <- subset(raw_data, (raw_data$Q20 == "I would like to participate"
                              & raw_data$attention_check == "Strongly Agree,Strongly disagree"))
raw_data <- droplevels(raw_data)

#################################################
# Reoder factors to convert to numeric
#################################################
raw_data$ideology <- recode(raw_data$ideology,
                            1 <- "Extremely liberal",
                            2 <- "Liberal", 
                            3 <- "Slightly liberal",
                            4 <- "Moderate or middle of the road",
                            5 <- "Slightly conservative",
                            6 <- "Conservative",
                            7 <- "Extremely conservative")
raw_data$ideology <- as.numeric(as.character(raw_data$ideology))

raw_data$pid <- as.numeric(as.character(raw_data$political_party))
raw_data$pid[raw_data$pid == 6] <- 3
raw_data$pid[raw_data$pid == 7] <- 4
raw_data$pid[raw_data$pid == 8] <- 5
raw_data$pid[raw_data$pid == 9] <- 6
raw_data$pid[raw_data$pid == 10] <- 7
raw_data$pid.dem <- ifelse(raw_data$pid < 4, 1, 0)
raw_data$pid.rep <- ifelse(raw_data$pid > 4, 1, 0)

raw_data$education[raw_data$education == '6,5'] <- '5'
raw_data$education <- droplevels(raw_data$education)
raw_data$education <- as.numeric(as.character(raw_data$education))
raw_data$education[raw_data$education < 0] <- NA

raw_data$age <- as.numeric(as.character(raw_data$age))

raw_data$income <- as.numeric(as.character(raw_data$hhi))
raw_data$income[raw_data$income < 0] <- NA

raw_data$race <- as.factor(as.numeric(as.character(raw_data$ethnicity)))
levels(raw_data$race) <- c("White", "Black", "Indigenous", rep("AAPI", 9), "Other", NA)
# levels(raw_data$race) <- c("White", "Black", "Indigenous", rep("AAPI", 11), "Other", NA)
table(raw_data$race, raw_data$ethnicity)

raw_data$hispanic[raw_data$hispanic == '15'] <- NA
raw_data$hispanic <- as.numeric(as.character(raw_data$hispanic))
raw_data$hispanic <- ifelse(raw_data$hispanic > 1, 1, 0)

table(raw_data$race, raw_data$hispanic)

raw_data$race.bin <- ifelse(raw_data$race == "White" & raw_data$hispanic == 0, "White", "POC")

raw_data$r.other <- ifelse(raw_data$race == "Other" | raw_data$race == "Indigenous", 1, 0)
raw_data$r.asian <- ifelse(raw_data$race == "AAPI", 1, 0)
raw_data$r.black <- ifelse(raw_data$race == "Black", 1, 0)
raw_data$r.white <- ifelse(raw_data$race == "White", 1, 0)

raw_data$religion <- as.factor(raw_data$religion)
raw_data$religion.lim <- raw_data$religion
levels(raw_data$religion.lim) <- c(NA, "Other", "Other", "Other", "Other", "Nothing", "Other", "Protestant", "Catholic")
raw_data$rel.other <-ifelse(raw_data$religion.lim == 'Other', 1, 0)
raw_data$rel.noth <-ifelse(raw_data$religion.lim == 'Nothing', 1, 0)
raw_data$rel.prot <-ifelse(raw_data$religion.lim == 'Protestant', 1, 0)
raw_data$rel.cath <-ifelse(raw_data$religion.lim == 'Catholic', 1, 0)

raw_data$italian <- ifelse(raw_data$italian == 'Yes', 1, 0)

raw_data$gender <- as.factor(raw_data$gender)
raw_data$sexuality <- as.factor(raw_data$sexual_orientation)
raw_data$gender.lim <- ifelse(raw_data$gender == "Man" | raw_data$gender == "Transgender Man", 1, 0)
raw_data$lgbt <- ifelse(raw_data$sexuality != "Heterosexual" | 
                            (raw_data$gender != "Man" & raw_data$gender != "Woman"),
                        1, 0)
summary(as.factor(raw_data$lgbt))

raw_data$white_man <- ifelse(raw_data$gender.lim  == 1 & raw_data$race.bin == 'White', 1, 0)
raw_data$white_woman <- ifelse(raw_data$gender.lim  == 0 & raw_data$race.bin == 'White', 1, 0)
raw_data$man_of_color <- ifelse(raw_data$gender.lim  == 1 & raw_data$race.bin != 'White', 1, 0)
raw_data$woman_of_color <- ifelse(raw_data$gender.lim  == 0 & raw_data$race.bin != 'White', 1, 0)

raw_data$race_gender <- NA
raw_data$race_gender[raw_data$white_man == 1] <- 'White Man'
raw_data$race_gender[raw_data$white_woman == 1] <- 'White Woman'
raw_data$race_gender[raw_data$man_of_color == 1 & raw_data$r.black == 0] <- 'NBMOC'
raw_data$race_gender[raw_data$woman_of_color == 1 & raw_data$r.black == 0] <- 'NBWOC'
raw_data$race_gender[raw_data$man_of_color == 1 & raw_data$r.black == 1] <- 'Black Man'
raw_data$race_gender[raw_data$woman_of_color == 1 & raw_data$r.black == 1] <- 'Black Woman'
raw_data$race_gender <- as.factor(raw_data$race_gender)
raw_data$race_gender_lim <- raw_data$race_gender
levels(raw_data$race_gender_lim) <- c("MOC", "WOC", "MOC", "WOC", "White Man", "White Woman")

raw_data$rel.chr <- ifelse(raw_data$rel.prot == 1 | raw_data$rel.cath == 1, 1, 0)

raw_data$region.north <- ifelse(raw_data$region == "1", 1, 0)
raw_data$region.mdwst <- ifelse(raw_data$region == "2", 1, 0)
raw_data$region.south <- ifelse(raw_data$region == "3", 1, 0)
raw_data$region.west <- ifelse(raw_data$region == "4", 1, 0)
#################################################
# Treatments
#################################################
levels(raw_data$sexism_treat) <- c("Control", "White", "Black", "LGBT")
raw_data$sexism_treat_lim <- raw_data$sexism_treat
levels(raw_data$sexism_treat_lim) <- c("Control", "Treatment", "Treatment", "Treatment")
levels(raw_data$resentment_treat) <- c("Control", "Men", "Women", "LGBT")
raw_data$resentment_treat_lim <- raw_data$resentment_treat
levels(raw_data$resentment_treat_lim) <- c("Control", "Treatment", "Treatment", "Treatment")

raw_data$resent.men <- ifelse(raw_data$resentment_treat == 'Men', 1, ifelse(raw_data$resentment_treat == 'Control', 0, NA))
raw_data$resent.women <- ifelse(raw_data$resentment_treat== "Women", 1, ifelse(raw_data$resentment_treat == 'Control', 0, NA))
raw_data$resent.lgbt <- ifelse(raw_data$resentment_treat == "LGBT", 1, ifelse(raw_data$resentment_treat == 'Control', 0, NA))
raw_data$resent.men.women <- ifelse(raw_data$resentment_treat== "Women", 1, ifelse(raw_data$resentment_treat == 'Men', 0, NA))
raw_data$resent.men.lgbt <- ifelse(raw_data$resentment_treat == "LGBT", 1, ifelse(raw_data$resentment_treat == 'Men', 0, NA))

raw_data$sexism.white <- ifelse(raw_data$sexism_treat == "White", 1, ifelse(raw_data$sexism_treat == 'Control', 0, NA))
raw_data$sexism.black <- ifelse(raw_data$sexism_treat == "Black", 1, ifelse(raw_data$sexism_treat == 'Control', 0, NA))
raw_data$sexism.lgbt <- ifelse(raw_data$sexism_treat == "LGBT", 1, ifelse(raw_data$sexism_treat == 'Control', 0, NA))
raw_data$sexism.white.black <- ifelse(raw_data$sexism_treat == "Black", 1, ifelse(raw_data$sexism_treat == 'White', 0, NA))
raw_data$sexism.white.lgbt <- ifelse(raw_data$sexism_treat == "LGBT", 1, ifelse(raw_data$sexism_treat == 'White', 0, NA))

raw_data$treat_full <- paste(raw_data$resentment_treat, raw_data$sexism_treat)
raw_data$treat_full <- as.factor(raw_data$treat_full)

raw_data$treat_full_order <- NA
raw_data$treat_full_order[raw_data$question_order == '0'] <- paste(raw_data$resentment_treat[raw_data$question_order == '0'], raw_data$sexism_treat[raw_data$question_order == '0'])
raw_data$treat_full_order[raw_data$question_order == '1'] <- paste(raw_data$sexism_treat[raw_data$question_order == '1'], raw_data$resentment_treat[raw_data$question_order == '1'])
raw_data$treat_full_order <- as.factor(raw_data$treat_full_order)

#################################################
# Treatments: In and Out Group
#################################################
raw_data$resentment_treat_groups <- as.character(raw_data$resentment_treat)
raw_data$resentment_treat_groups[raw_data$resentment_treat == 'Men' & raw_data$gender.lim == 1] <- "In"
raw_data$resentment_treat_groups[raw_data$resentment_treat == 'Men' & raw_data$gender.lim == 0] <- "Out"
raw_data$resentment_treat_groups[raw_data$resentment_treat == 'Women' & raw_data$gender.lim == 0] <- "In"
raw_data$resentment_treat_groups[raw_data$resentment_treat == 'Women' & raw_data$gender.lim == 1] <- "Out"
raw_data$resentment_treat_groups[raw_data$resentment_treat == 'LGBT' & raw_data$lgbt == 1] <- "In"
raw_data$resentment_treat_groups[raw_data$resentment_treat == 'LGBT' & raw_data$lgbt == 0] <- "Out"
raw_data$resentment_treat_groups <- factor(raw_data$resentment_treat_groups)

raw_data$sexism_treat_groups <- ""
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'Control'] <- "Control"
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'White' & raw_data$race.bin == 'White'] <- "In"
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'White' & raw_data$race.bin != 'White'] <- "Out"
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'Black' & raw_data$race.bin != 'White'] <- "In"
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'Black' & raw_data$race.bin == 'White'] <- "Out"
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'LGBT' & raw_data$lgbt == 1] <- "In"
raw_data$sexism_treat_groups[raw_data$sexism_treat == 'LGBT' & raw_data$lgbt == 0] <- "Out"
raw_data$sexism_treat_groups <- factor(raw_data$sexism_treat_groups)


raw_data$resentment_dif <- NA
raw_data$resentment_dif[raw_data$r.black == 0 & raw_data$resentment_treat == "Control"] <- "Out-Group"
raw_data$resentment_dif[raw_data$gender.lim == 0 & raw_data$resentment_treat == "Men"] <- "Out-Group"
raw_data$resentment_dif[raw_data$gender.lim == 1 & raw_data$resentment_treat == "Women"] <- "Out-Group"
raw_data$resentment_dif[raw_data$lgbt == 0 & raw_data$resentment_treat == "LGBT"] <- "Out-Group"
raw_data$resentment_dif[raw_data$r.black == 1 & raw_data$resentment_treat == "Control"] <- "In-Group"
raw_data$resentment_dif[raw_data$gender.lim == 1 & raw_data$resentment_treat == "Men"] <- "In-Group"
raw_data$resentment_dif[raw_data$gender.lim == 0 & raw_data$resentment_treat == "Women"] <- "In-Group"
raw_data$resentment_dif[raw_data$lgbt == 1 & raw_data$resentment_treat == "LGBT"] <- "In-Group"
raw_data$resentment_dif_num <- 0
raw_data$resentment_dif_num <- ifelse(
    raw_data$r.black == 1,
    raw_data$resentment_dif_num + 1,
    raw_data$resentment_dif_num + 0
)
raw_data$resentment_dif_num <- ifelse(
    raw_data$gender.lim == 1 & raw_data$resentment_treat == 'Men',
    raw_data$resentment_dif_num + 1,
    raw_data$resentment_dif_num + 0
)
raw_data$resentment_dif_num <- ifelse(
    raw_data$gender.lim == 0 & raw_data$resentment_treat == 'Women',
    raw_data$resentment_dif_num + 1,
    raw_data$resentment_dif_num + 0
)
raw_data$resentment_dif_num <- ifelse(
    raw_data$lgbt == 1 & raw_data$resentment_treat == 'LGBT',
    raw_data$resentment_dif_num + 1,
    raw_data$resentment_dif_num + 0
)
raw_data$resentment_dif_num_comb <- ifelse(
    raw_data$r.black == 0 & raw_data$race.bin == 'POC',
    raw_data$resentment_dif_num + 1,
    raw_data$resentment_dif_num + 0
)

raw_data$sexism_dif <- NA
raw_data$sexism_dif[raw_data$gender.lim == 1 & raw_data$sexism_treat == "Control"] <- "Out-Group"
raw_data$sexism_dif[raw_data$r.white == 0 & raw_data$sexism_treat == "White"] <- "Out-Group"
raw_data$sexism_dif[raw_data$r.black == 0 & raw_data$sexism_treat == "Black"] <- "Out-Group"
raw_data$sexism_dif[raw_data$lgbt == 0 & raw_data$sexism_treat == "LGBT"] <- "Out-Group"
raw_data$sexism_dif[raw_data$gender.lim == 0 & raw_data$sexism_treat == "Control"] <- "In-Group"
raw_data$sexism_dif[raw_data$r.white == 1 & raw_data$sexism_treat == "White"] <- "In-Group"
raw_data$sexism_dif[raw_data$r.black == 1 & raw_data$sexism_treat == "Black"] <- "In-Group"
raw_data$sexism_dif[raw_data$lgbt == 1 & raw_data$sexism_treat == "LGBT"] <- "In-Group"
raw_data$sexism_dif_num <- 0
raw_data$sexism_dif_num <- ifelse(
    raw_data$gender.lim == 0,
    raw_data$sexism_dif_num + 1,
    raw_data$sexism_dif_num + 0
)
raw_data$sexism_dif_num <- ifelse(
    raw_data$race.bin == 'White' & raw_data$sexism_treat == 'White',
    raw_data$sexism_dif_num + 1,
    raw_data$sexism_dif_num + 0
)
raw_data$sexism_dif_num <- ifelse(
    raw_data$r.black == 1 & raw_data$sexism_treat == 'Black',
    raw_data$sexism_dif_num + 1,
    raw_data$sexism_dif_num + 0
)
raw_data$sexism_dif_num <- ifelse(
    raw_data$lgbt == 1 & raw_data$sexism_treat == 'LGBT',
    raw_data$sexism_dif_num + 1,
    raw_data$sexism_dif_num + 0
)
raw_data$sexism_dif_num_comb <- ifelse(
    raw_data$r.black == 0 & raw_data$race.bin != 'White' & raw_data$sexism_treat == 'Black',
    raw_data$sexism_dif_num + 1,
    raw_data$sexism_dif_num + 0
)
#################################################
# Manipulation Check
#################################################
raw_data$manip.bp <- 0
raw_data$manip.bm <- 0
raw_data$manip.bw <- 0
raw_data$manip.bl <- 0
raw_data$manip.w <- 0
raw_data$manip.ww <- 0
raw_data$manip.wl <- 0
for(i in 1:nrow(raw_data)){
    manip_list <- unlist(strsplit(as.character(raw_data$manip_check[i]), ","))
    if("Black people" %in% manip_list){
        raw_data$manip.bp[i] <- 1
    }
    if("Black men" %in% manip_list){
        raw_data$manip.bm[i] <- 1
    }
    if("Black women" %in% manip_list){
        raw_data$manip.bw[i] <- 1
    }
    if(" and trans gender Black people" %in% manip_list){
        raw_data$manip.bl[i] <- 1
    }
    if("Women" %in% manip_list){
        raw_data$manip.w[i] <- 1
    }
    if("White women" %in% manip_list){
        raw_data$manip.ww[i] <- 1
    }
    if(" and trans gender women" %in% manip_list){
        raw_data$manip.wl[i] <- 1
    }
}
#table(raw_data$manip_check, raw_data$resentment_treat)
#table(raw_data$manip_check,raw_data$sexism_treat)

table(raw_data$resentment_treat, raw_data$manip.bp)
table(raw_data$resentment_treat, raw_data$manip.bm)
table(raw_data$resentment_treat, raw_data$manip.bw)
table(raw_data$resentment_treat, raw_data$manip.bl)
table(raw_data$sexism_treat, raw_data$manip.w)
table(raw_data$sexism_treat, raw_data$manip.ww)
table(raw_data$sexism_treat, raw_data$manip.bw)
table(raw_data$sexism_treat, raw_data$manip.wl)

raw_data$manip.correct <- ifelse(
    (raw_data$resentment_treat == "Control" & raw_data$manip.bp == 1)|
        (raw_data$resentment_treat == "Control" & raw_data$manip.bm == 1 & raw_data$manip.bw == 1)|
        (raw_data$resentment_treat == "Men" & raw_data$manip.bm == 1)|
        (raw_data$resentment_treat == "Women" & raw_data$manip.bw == 1)|
        (raw_data$resentment_treat == "Women" & raw_data$manip.w == 1)|
        (raw_data$resentment_treat == "LGBT" & raw_data$manip.bl == 1)|
        (raw_data$sexism_treat == "Control" & raw_data$manip.w == 1)|
        (raw_data$sexism_treat == "White" & raw_data$manip.ww == 1)|
        (raw_data$sexism_treat == "Black" & raw_data$manip.bw == 1)|
        (raw_data$sexism_treat == "LGBT" & raw_data$manip.wl == 1),
    1, 0
)
raw_data$manip.correct.resentment <- ifelse(
    ((raw_data$resentment_treat == "Control" & raw_data$manip.bp == 1)|
        (raw_data$resentment_treat == "Control" & raw_data$manip.bm == 1 & raw_data$manip.bw == 1)|
        (raw_data$resentment_treat == "Men" & raw_data$manip.bm == 1)|
        (raw_data$resentment_treat == "Women" & raw_data$manip.bw == 1)|
        (raw_data$resentment_treat == "LGBT" & raw_data$manip.bl == 1)),
    1, 0
)
raw_data$manip.correct.sexism <- ifelse(
    ((raw_data$sexism_treat == "Control" & raw_data$manip.w == 1)|
        (raw_data$sexism_treat == "Control" & raw_data$manip.ww == 1 & raw_data$manip.bw == 1)|
        (raw_data$sexism_treat == "White" & raw_data$manip.ww == 1)|
        (raw_data$sexism_treat == "Black" & raw_data$manip.bw == 1)|
        (raw_data$sexism_treat == "LGBT" & raw_data$manip.wl == 1)),
    1, 0
)
raw_data$manip.correct.both <- ifelse(raw_data$manip.correct.resentment == 1 & raw_data$manip.correct.sexism == 1, 1, 0)
raw_data$manip.correct.both.or <- ifelse(raw_data$manip.correct.resentment == 1 | raw_data$manip.correct.sexism == 1, 1, 0)

table(raw_data$sexism_treat, raw_data$resentment_treat, raw_data$manip.correct.both)
table(raw_data$resentment_treat, raw_data$manip.correct.both)
table(raw_data$sexism_treat, raw_data$manip.correct.both)
summary(raw_data$manip.correct)
summary(raw_data$manip.correct.resentment)
summary(raw_data$manip.correct.sexism)
summary(raw_data$manip.correct.both)

table(raw_data$resentment_treat, raw_data$manip.correct.resentment)
table(raw_data$sexism_treat, raw_data$manip.correct.sexism)
#################################################
# Racial Resentment
#################################################
resent.vars <- grepl('resentment', colnames(raw_data))

for(r_var in colnames(raw_data)[resent.vars]){
    raw_data[,r_var][raw_data[,r_var] == ''] <- NA
}

table(raw_data$resentment1_control, raw_data$resentment_treat)
table(raw_data$resentment1_men, raw_data$resentment_treat)
table(raw_data$resentment1_women, raw_data$resentment_treat)
table(raw_data$resentment1_lgbt, raw_data$resentment_treat)

summary(as.factor(CodeScales(raw_data$resentment2_women, standardize=F)))
summary(raw_data$resentment2_women)
summary(as.factor(CodeScales(raw_data$resentment2_control, standardize=F)))
summary(raw_data$resentment2_control)
summary(as.factor(CodeScales(raw_data$resentment2_men, standardize=F)))
summary(raw_data$resentment2_men)
summary(as.factor(CodeScales(raw_data$resentment2_lgbt, standardize=F)))
summary(raw_data$resentment2_lgbt)

raw_data$resentment1_control <- CodeScales(raw_data$resentment1_control, reverse = TRUE, standardize=F)
raw_data$resentment2_control <- CodeScales(raw_data$resentment2_control, standardize=F)
raw_data$resentment3_control <- CodeScales(raw_data$resentment3_control, standardize=F)
raw_data$resentment4_control <- CodeScales(raw_data$resentment4_control, reverse = TRUE, standardize=F)

raw_data$resentment1_men <- CodeScales(raw_data$resentment1_men, reverse = TRUE, standardize=F)
raw_data$resentment2_men <- CodeScales(raw_data$resentment2_men, standardize=F)
raw_data$resentment3_men <- CodeScales(raw_data$resentment3_men, standardize=F)
raw_data$resentment4_men <- CodeScales(raw_data$resentment4_men, reverse = TRUE, standardize=F)

raw_data$resentment1_women <- CodeScales(raw_data$resentment1_women, reverse = TRUE, standardize=F)
raw_data$resentment2_women <- CodeScales(raw_data$resentment2_women, standardize=F)
raw_data$resentment3_women <- CodeScales(raw_data$resentment3_women, standardize=F)
raw_data$resentment4_women <- CodeScales(raw_data$resentment4_women, reverse = TRUE, standardize=F)

raw_data$resentment1_lgbt <- CodeScales(raw_data$resentment1_lgbt, reverse = TRUE, standardize=F)
raw_data$resentment2_lgbt <- CodeScales(raw_data$resentment2_lgbt, standardize=F)
raw_data$resentment3_lgbt <- CodeScales(raw_data$resentment3_lgbt, standardize=F)
raw_data$resentment4_lgbt <- CodeScales(raw_data$resentment4_lgbt, reverse = TRUE, standardize=F)

raw_data$resentment1 <- rowSums(cbind(raw_data$resentment1_control, raw_data$resentment1_men, raw_data$resentment1_women, raw_data$resentment1_lgbt), na.rm=T)
raw_data$resentment2 <- rowSums(cbind(raw_data$resentment2_control, raw_data$resentment2_men, raw_data$resentment2_women, raw_data$resentment2_lgbt), na.rm=T)
raw_data$resentment3 <- rowSums(cbind(raw_data$resentment3_control, raw_data$resentment3_men, raw_data$resentment3_women, raw_data$resentment3_lgbt), na.rm=T)
raw_data$resentment4 <- rowSums(cbind(raw_data$resentment4_control, raw_data$resentment4_men, raw_data$resentment4_women, raw_data$resentment4_lgbt), na.rm=T)

raw_data$resentment2[raw_data$resentment2 == 0] <- NA
raw_data$resentment3[raw_data$resentment3 == 0] <- NA
raw_data$resentment4[raw_data$resentment4 == 0] <- NA
summary(raw_data$resentment2)
summary(raw_data$resentment3)
summary(raw_data$resentment4)
#raw_data$resentment1 <- Standardize(raw_data$resentment1)
#raw_data$resentment2 <- Standardize(raw_data$resentment2)
#raw_data$resentment3 <- Standardize(raw_data$resentment3)
#raw_data$resentment4 <- Standardize(raw_data$resentment4)

length(which(
    raw_data$resentment1 == 4 &
        raw_data$resentment2 == 4 &
        raw_data$resentment3 == 4 &
        raw_data$resentment4 == 4
)) # 19

raw_data$racial_resentment <- VariableScore(
    vars = cbind(raw_data$resentment1,
                 raw_data$resentment2,
                 raw_data$resentment3,
                 raw_data$resentment4)
)
raw_data$racial_resentment.s <- VariableScore(
    vars = cbind(Standardize(raw_data$resentment1),
                 Standardize(raw_data$resentment2),
                 Standardize(raw_data$resentment3),
                 Standardize(raw_data$resentment4))
)
summary(raw_data$racial_resentment)

raw_data$resentment_answers <- as.factor(paste(raw_data$resentment1, raw_data$resentment2, raw_data$resentment3, raw_data$resentment4, sep='-'))
sort(summary(raw_data$resentment_answers), decreasing=T)
ind <- which(raw_data$resentment_answers != '4-4-4-4' & raw_data$racial_resentment == 4)
raw_data$resentment_answers[ind]

length(which(raw_data$resentment_answers == '7-7-7-7'))
length(which(raw_data$resentment_answers == '1-1-1-1'))
#################################################
# Modern Sexism
#################################################
resent.vars <- grepl('sexism', colnames(raw_data))

for(r_var in colnames(raw_data)[resent.vars]){
    raw_data[,r_var][raw_data[,r_var] == ''] <- NA
}
rm(resent.vars)
raw_data$sexism1_control <- CodeScales(raw_data$sexism1_control, reverse = TRUE, standardize=F)
raw_data$sexism2_control <- CodeScales(raw_data$sexism2_control, standardize=F)
raw_data$sexism3_control <- CodeScales(raw_data$sexism3_control, reverse = TRUE, standardize=F)
raw_data$sexism4_control <- CodeScales(raw_data$sexism4_control, reverse = TRUE, standardize=F)
raw_data$sexism5_control <- CodeScales(raw_data$sexism5_control, standardize=F)

raw_data$sexism1_white <- CodeScales(raw_data$sexism1_white, reverse = TRUE, standardize=F)
raw_data$sexism2_white <- CodeScales(raw_data$sexism2_white, standardize=F)
raw_data$sexism3_white <- CodeScales(raw_data$sexism3_white, reverse = TRUE, standardize=F)
raw_data$sexism4_white <- CodeScales(raw_data$sexism4_white, reverse = TRUE, standardize=F)
raw_data$sexism5_white <- CodeScales(raw_data$sexism5_white, standardize=F)

raw_data$sexism1_black <- CodeScales(raw_data$sexism1_black, reverse = TRUE, standardize=F)
raw_data$sexism2_black <- CodeScales(raw_data$sexism2_black, standardize=F)
raw_data$sexism3_black <- CodeScales(raw_data$sexism3_black, reverse = TRUE, standardize=F)
raw_data$sexism4_black <- CodeScales(raw_data$sexism4_black, reverse = TRUE, standardize=F)
raw_data$sexism5_black <- CodeScales(raw_data$sexism5_black, standardize=F)

raw_data$sexism1_lgbt <- CodeScales(raw_data$sexism1_lgbt, reverse = TRUE, standardize=F)
raw_data$sexism2_lgbt <- CodeScales(raw_data$sexism2_lgbt, standardize=F)
raw_data$sexism3_lgbt <- CodeScales(raw_data$sexism3_lgbt, reverse = TRUE, standardize=F)
raw_data$sexism4_lgbt <- CodeScales(raw_data$sexism4_lgbt, reverse = TRUE, standardize=F)
raw_data$sexism5_lgbt <- CodeScales(raw_data$sexism5_lgbt, standardize=F)

raw_data$sexism1 <- rowSums(cbind(raw_data$sexism1_control, raw_data$sexism1_white, raw_data$sexism1_black, raw_data$sexism1_lgbt), na.rm=T)
raw_data$sexism2 <- rowSums(cbind(raw_data$sexism2_control, raw_data$sexism2_white, raw_data$sexism2_black, raw_data$sexism2_lgbt), na.rm=T)
raw_data$sexism3 <- rowSums(cbind(raw_data$sexism3_control, raw_data$sexism3_white, raw_data$sexism3_black, raw_data$sexism3_lgbt), na.rm=T)
raw_data$sexism4 <- rowSums(cbind(raw_data$sexism4_control, raw_data$sexism4_white, raw_data$sexism4_black, raw_data$sexism4_lgbt), na.rm=T)
raw_data$sexism5 <- rowSums(cbind(raw_data$sexism5_control, raw_data$sexism5_white, raw_data$sexism5_black, raw_data$sexism5_lgbt), na.rm=T)
#raw_data$sexism1 <- Standardize(raw_data$sexism1)
#raw_data$sexism2 <- Standardize(raw_data$sexism2)
#raw_data$sexism3 <- Standardize(raw_data$sexism3)
#raw_data$sexism4 <- Standardize(raw_data$sexism4)
#raw_data$sexism5 <- Standardize(raw_data$sexism5)

length(which(
    raw_data$sexism1 == 4 &
        raw_data$sexism2 == 4 &
        raw_data$sexism3 == 4 &
        raw_data$sexism4 == 4 &
        raw_data$sexism5 == 4 
)) # 16

raw_data$modern_sexism <- VariableScore(
    vars = cbind(raw_data$sexism1,
                 raw_data$sexism2,
                 raw_data$sexism3,
                 raw_data$sexism4,
                 raw_data$sexism5)
)
raw_data$modern_sexism.s <- VariableScore(
    vars = cbind(Standardize(raw_data$sexism1),
                 Standardize(raw_data$sexism2),
                 Standardize(raw_data$sexism3),
                 Standardize(raw_data$sexism4),
                 Standardize(raw_data$sexism5))
)

summary(raw_data$modern_sexism)

raw_data$dif <- raw_data$racial_resentment - raw_data$modern_sexism
raw_data$dif.s <- raw_data$racial_resentment.s - raw_data$modern_sexism.s

raw_data$rr.bin <- ifelse(raw_data$racial_resentment.s >=0, "RR High", "RR Low")
raw_data$ms.bin <- ifelse(raw_data$modern_sexism.s >=0, "MS High", "MS Low")


#################################################
# Additional Processing
#################################################
data.ca <- raw_data
rownames(data.ca) <- c(1:nrow(data.ca))

data.ca$race.lim = data.ca$race
levels(data.ca$race.lim) <- c("White", "Black", "NBPOC", "NBPOC", "NBPOC")

data.ca$treat_full2 <- factor(data.ca$treat_full, levels=levels(data.ca$treat_full)[c(2, 1, 4, 3, 10, 9, 12, 11, 14, 13, 16, 15, 6, 5, 8, 7)])
data.ca$treat_full3 <- data.ca$treat_full2
data.ca$treat_full4 <- data.ca$treat_full2
levels(data.ca$treat_full3) <- c('Partial Mix', 'Partial Mix', 'Mix', 'Mix', 
                                 'Mix', 'Partial Mix', 'Mix', 'Mix',
                                 'Partial Mix', 'Same', 'Partial Mix', 'Mix',
                                 'Mix', 'Partial Mix', 'Mix', 'Same')

levels(data.ca$treat_full4) <- c('Partial Mix', 'Partial Mix', 'Mix', NA, 
                                 'Mix', 'Partial Mix', 'Mix', NA,
                                 'Partial Mix', 'Same', 'Partial Mix', NA,
                                 NA, NA, NA, NA)

data.ca$treat_full5 <- data.ca$treat_full2
levels(data.ca$treat_full5) <- c('Control-Control', 'One Same Subgroup', 'Different Subgroups', 'Different Subgroups', 
                                 'Different Subgroups', 'One Same Subgroup', 'Different Subgroups', 'Different Subgroups',
                                 'One Same Subgroup', 'Same Subgroups', 'One Same Subgroup', 'Different Subgroups',
                                 'Different Subgroups', 'One Same Subgroup', 'Different Subgroups', 'Same Subgroups')

data.ca$treat_full5b <- data.ca$treat_full2
levels(data.ca$treat_full5b) <- c('Control-Control', 'One Same Subgroup', 'Different Subgroups', NA, 
                                  'Different Subgroups', 'One Same Subgroup', 'Different Subgroups', NA,
                                  'One Same Subgroup', 'Same Subgroups', 'One Same Subgroup', NA,
                                  NA, NA, NA, NA)

data.ca$is.mix <- data.ca$treat_full3 == 'Mix'


data.ca$resentment_treat_names <-data.ca$resentment_treat
levels(data.ca$resentment_treat_names) <- c('Black People', 'Black Men', 'Black Women', 'LGBT Black People')

data.ca$sexism_treat_names <-data.ca$sexism_treat
levels(data.ca$sexism_treat_names) <- c('Women', 'White Women', 'Black Women', 'LBT Women')

data.ca$RR <- ifelse(data.ca$racial_resentment >= 4, "High RR", "Low RR")
data.ca$MS <- ifelse(data.ca$modern_sexism >= 4, "High MS", "Low MS")
data.ca$RR.MS <- factor(paste(data.ca$RR, data.ca$MS), levels=c("High RR High MS", "High RR Low MS", "Low RR High MS", "Low RR Low MS"))
data.ca$cross <- ifelse(data.ca$RR.MS == "High RR Low MS" | data.ca$RR.MS == "Low RR High MS" , 1, 0)

data.ca$manip.correct.second <- 0
data.ca$manip.correct.second[(data.ca$question_order == "0" & data.ca$manip.correct.sexism == 1)] <- 1
data.ca$manip.correct.second[(data.ca$question_order == "1" & data.ca$manip.correct.resentment == 1)] <- 1

data.ca$manip.correct.first<- 0
data.ca$manip.correct.first[(data.ca$question_order == "0" & data.ca$manip.correct.resentment == 1)] <- 1
data.ca$manip.correct.first[(data.ca$question_order == "1" & data.ca$manip.correct.sexism == 1)] <- 1
data.ca$mp <- paste0(data.ca$manip.correct.first, data.ca$manip.correct.second)

#################################################
# Save, Export
#################################################
rm(raw_data)
rm(ind)
rm(manip_list)
rm(r_var)
save.image("TermsConditionsData.RData")

