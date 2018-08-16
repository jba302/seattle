#First Pass, initial looks and such.


#Library imports that will keep on increasing.
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(shiny)



##Core Review of data - Library Collection Inventory
key = read.csv('data/Integrated_Library_System__ILS__Data_Dictionary.csv')

##first run - 2.7M lines x 13 variables.
summary(key)
list(colnames(key))

####The Key is a Collection to "Type of Book" information.
#Code - Ths is the collection in other columns, so primary key.
#Description - Book type grouping (non-fiction, young adult, etc.).
#Code.Type - ItemCollection, ItemLocation, ItemType. Not sure why this is in here just yet.
#Format.Group - Equipment, Internet, Media, Print. Heavily Print and Media.
#Format.Subgroup - Book, Periodical, etc. The actual format of the book.
#Category.Group - Fiction, Nonfiction, etc.
#Category.Subgroup - Biography, ESL,etc.

#I noticed from a first grab that the column names are a bit off. I'm changing the column names to match the data files so that each extraction doesn't need an update.
names(key)[1] = 'Collection'


#Generally there isn't much to do as a stand-alone data look right now. Keep moving.


#Actual book data here.
book_data = read.csv('data/Library_Collection_Inventory.csv')

#same column changing as above. Since these are referential items I'm fixing them to be in line with the checkout data that will come in over time.
names(book_data)[1] = 'BibNumber'
names(book_data)[9] = 'Collection'


#first run - 2.7M lines x 13 variables.
summary(book_data)
list(colnames(book_data))

###Columns are:
#BibNumber = unique book id
#Title = literal title
#Author = literal author
#ISBN = seems to be a version type (paper, hard, cd, etc.) or can be a multi-book series (1,2,3,etc.)
#PublicationYear = can be multiple, but also important -cannot be checked out prior to publication year. This needs to be cleaned up.
##I'm changing this publication year in line here now and will address this another time.
book_data$clean_pubyear_first = str_extract(book_data$PublicationYear,"\\d{4}")
book_data$clean_pubyear_last = str_sub(str_extract(book_data$PublicationYear,"\\d{4}.?$"),1,4)

#Publisher = literal publisher
#Subjects = list of subject relevancy
#ItemType = This can be joined to Integrated_Library_System__ILS_Data_Dictionary for more info:Code,Description,Code Type,Format Group,Format Subgroup,Category Group,Category Subgroup. This might be valuable.

#FloatingItem  = nfc. 84.7% null
#ItemLocation = probably physical location, could be library, city, state level?
#ReportDate = 2 days, 10/1 and 9/1, same year. 
#ItemCount = count of copies probably.

##Overall, looks like a few of these can be stripped, FloatingItem is one of them. I think we can scrape out some of the report dates as well, so look for duplications/updates there to see if that's an accurate thought. I wonder if the publisher will be useful. 



#Next file is the 2017 checkout data. The assumption being that all the yearly data will have the same layout, perhaps not trends but this will be a good start.
checkouts = read.csv('data/Checkouts_By_Title_Data_Lens_2017.csv')

#Maybe there's a good reason for minute/second accuracy of checkouts? Going to give myself the leeway that probably not.
checkouts$date = as.Date(mdy_hms(checkouts$CheckoutDateTime))

#and adding in monthly-level only
checkouts$monthdate = floor_date(checkouts$date,unit="month")




#and add in the key here to have a full start of the 2017 checkouts. this is basically the grouped book data without being the actual book. #This is kind of a round 2, but there isn't much to look at in isolated fashion.
checkouts = merge(checkouts,key,by='Collection')





#example of testing checkout rates by a type, in this case Format.Group. Here we see that print has a much larger overall count, but the Media and Print trends are about dead in line. So we can probably kill this as a useful feature.
checkouts %>% select(monthdate,Format.Group) %>% group_by(monthdate,Format.Group) %>% summarize(counts=length(Format.Group)) %>% ggplot(aes(x=monthdate,y=counts,group=Format.Group,color=Format.Group))+geom_line()



#So now that I'm seeing this one, I'm going to walk back the prior comment a little. There's a distinct spike in June and it dies near the end of the year. I would guess this correlates to schools releasing, students stop pulling from school libraries. This became apparent when I noticed the jump with Fiction specifically. 
checkouts %>% select(monthdate,Category.Group) %>% group_by(monthdate,Category.Group) %>% summarize(counts=length(Category.Group)) %>% ggplot(aes(x=monthdate,y=counts,group=Category.Group,color=Category.Group))+geom_line()

#Another interesting thing here is the pattern of increased language during spring/high summer/declining fall. My small town memory of corn fields is telling me that this could be some kind of immigrant labor (or... just immigration perhaps?) pattern.
#Within the Language group of books, a quick look at the Call Number is showing the first word as a language. Exploring this more.
checkouts$test = word(checkouts$CallNumber,1)
table(checkouts$test[checkouts$Category.Group=='Language'])
checkouts$spec_language = ifelse(checkouts$test %in% c('SPANISH','FRENCH','CHINESE','GERMAN','HINDI','ITALIAN','JAPANESE','SWEDISH'),checkouts$test,'')


#Graphing counts by month by language noted above. French, Chinese, Japanese, Spanish are bigger than the others group. So at least we have something valuable here, certain languages see bumps.
checkouts %>% filter(Category.Group=='Language',spec_language!='') %>% select(monthdate,spec_language) %>% group_by(monthdate,spec_language) %>% summarise(counts=length(spec_language)) %>% ggplot(aes(x=monthdate,y=counts,group=spec_language,color=spec_language,label=spec_language))+geom_line()


checkout_merged = checkouts %>% left_join(book_data,by=c('BibNumber','ItemType','Collection'))
#one difficulty here is that the item's location explodes the join. there's no location identifier in the checkouts. It it isn't residing in the checkouts, then for now i'll cut it out and progress. I have an idea to test later about location heat but that will be a different thought process. Right now I'm just trying to get a better feel for how to trend the checkouts generally.


#Get most recent report dates only for each bibnumber... Should I add more later to this?
books_updated = book_data %>% group_by(BibNumber) %>%  arrange(desc(ReportDate)) %>% top_n(1,ReportDate)





