library(tidyverse)
library(tesseract)          #WHAT WE WILL USE FOR OCR
library(magick)             #WHAT WE WILL USE TO PROCESS THE IMAGE
library(lubridate)          #MAKES PROCESSING THE DATES EASIER

#SET THE PROJECT DIRECTORY
setwd("~/projects/Covid")

#LOAD THE LIST OF RYAN IMGRUND IMAGE FILES FROM A FOLDER NAMED IMGRUND
#WE WILL RENAME THESE LATER TO AN ORDERED DATE FORMAT AUTOMATICALLY
imgrund_r <- list.files(path = "imgrund", pattern="*.jpg", full.names=TRUE)

#LOAD THE LANGUAGE
eng <- tesseract("eng")

#IMGRUND TABLE WILL BE OUR COMBINED TABLE WITH THE R VALUES FOR THE DATES
#WE WILL LOAD EACH FILE SEQUENTUALLY AND THEN PROCESS THEM WITH OCR INTO A TEMP TABLE
#THAT WILL BE ADDED TO THE MASTER TABLE OF DATA
imgrund_table <- NULL
for(n in 1:length(imgrund_r)) {
  #READ IN THE IMAGE FILE FROM THE LIST OF IMAGE FILES
  curr_img <- image_read(imgrund_r[n])
  
  #PROCESS THE IMAGE FILE:
  #WE WANT TO GET RID OF THE COLOR BOXES AND THEN MAKE THE IMAGE GRAYSCALE
  #TO MAKE IT AS EASY AS POSSIBLE TO USE THE OCR
  curr_img <- curr_img %>%
    image_modulate(brightness=200) %>%
    image_combine(colorspace = "gray")
  
  #FIX SOME COMMON OCR ISSUES
  text <- tesseract::ocr(curr_img, engine = eng)
  text <- str_replace_all(text, "\\( ","")
  text <- str_replace_all(text, " \\)","")
  text <- str_replace_all(text, "\\(","")
  text <- str_replace_all(text, " -","-")
  text <- str_replace_all(text, "- ","-")
  
  #DO SOME Q&D FIXES FOR REGIONS
  text <- str_replace_all(text, "British Columbia","British_Columbia")
  text <- str_replace_all(text, "Fraser Valley","Fraser_Valley")
  text <- str_replace_all(text, "Vancouver Coastal","Vancouver_Coastal")
  text <- str_replace_all(text, "New Brunswick","New_Brunswick")
  text <- str_replace_all(text, "Nova Scotia","Nova_Scotia")
  text <- str_replace_all(text, "Quebec City","Quebec_City")
  
  #SPLIT THE TEXT UP
  text_split <- str_split(text, pattern="\n", simplify = TRUE)
  
  #LOAD THE VALUES INTO THE DATA TABLE
  r_location <- NULL
  r_val <- NULL
  c_low <- NULL
  c_high <- NULL
  week_case <- NULL
  for(i in 4:25) {
    r_location <- append(r_location,str_split(text_split[,i], " ", simplify=TRUE)[1])
    
    r_val_num <- as.numeric(str_split(text_split[,i], " ", simplify=TRUE)[2])
    r_val_num <- ifelse(r_val_num < 100, r_val_num, r_val_num/100)
    r_val_num <- ifelse(r_val_num > 4, r_val_num/100, r_val_num)
    r_val <- append(r_val, r_val_num)
    
    c_low_num <- as.numeric(str_split(str_split(text_split[,i], " ", simplify=TRUE)[3],"-",simplify=TRUE)[1])
    c_low_num <- ifelse(c_low_num < 100, c_low_num, c_low_num/100)
    c_low_num <- ifelse(c_low_num > 4, c_low_num/100, c_low_num)
    c_low <- append(c_low, c_low_num)
    
    c_high_num <- as.numeric(str_split(str_split(text_split[,i], " ", simplify=TRUE)[3],"-",simplify=TRUE)[2])
    c_high_num <- ifelse(c_high_num < 100, c_high_num, c_high_num/100)
    c_high_num <- ifelse(c_high_num > 4, c_high_num/100, c_high_num)
    c_high <- append(c_high, c_high_num)
    
    week_case <- append(week_case, as.numeric(str_split(text_split[,i], " ", simplify=TRUE)[4]))
  }
  
  #CREATE A TEMP TABLE AND THEN ADD IT TO THE MASTER TABLE
  temp_table <- data.frame(DATE = as_date(str_replace(text_split[,2],"Canada for ",""), format="%A, %B %d, %Y"),
                        LOCATION = r_location,
                        R_VAL = r_val, 
                        C_INTERVAL_LOW = c_low,
                        C_INTERVAL_HIGH = c_high,
                        WEEK_CASE_RATE = week_case)
  temp_table$LOCATION <- str_replace_all(temp_table$LOCATION, "_"," ")
  
  imgrund_table <- rbind(imgrund_table, temp_table)
  
  #THEN RENAME THE FILES TO THE DATE WITHIN THE IMAGE
  if(is.Date(temp_table$DATE[1])) {
    if(file.exists(imgrund_r[n])) {
      file.rename(imgrund_r[n], paste("imgrund/imgrund-r ",temp_table$DATE[1],".jpg",sep=""))
    }
  }
  
}

