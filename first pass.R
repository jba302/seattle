#First Pass, initial looks and such.


#Library imports.
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(shiny)



#Core Review of data - Library Collection Inventory
key = read.csv('data/Integrated_Library_System__ILS__Data_Dictionary.csv')

#I noticed from a first grab that the column names are a bit off. I'm changing the column names to match the data files so that each extraction doesn't need an update.
names(key)[1] = 'Collection'


#Actual book data here.
book_data = read.csv('data/Library_Collection_Inventory.csv')
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


#first round of join and review
checkouts = read.csv('data/Checkouts_By_Title_Data_Lens_2017.csv')

#Maybe there's a good reason for second-level checkouts? going to give myself the leeway that probably not.
checkouts$date = as.Date(mdy_hms(checkouts$CheckoutDateTime))

#and adding in monthly-level only
checkouts$monthdate = floor_date(checkouts$date,unit="month")

#and add in the key here to have a full start of the 2017 checkouts. this is basically the grouped book data without being the actual book. 
checkouts = merge(checkouts,key,by='Collection')



checkout_rates = checkouts %>% select(Collection,monthdate) %>% group_by(monthdate,Collection) %>% summarise(CollectionCount=length(Collection))
test = merge(checkout_rates,key,by='Collection')

#example of testing checkout rates by a type, in this case Format.Group. Here we see that print has a much larger overall count, but the Media and Print trends are about dead in line. So we can probably kill this as a useful feature.

checkouts %>% select(monthdate,Format.Group) %>% group_by(monthdate,Format.Group) %>% summarize(counts=length(Format.Group)) %>% ggplot(aes(x=monthdate,y=counts,group=Format.Group,color=Format.Group))+geom_line()



#So now that I'm seeing this one, I'm going to walk back the prior comment a little. There's a distinct spike in June and it dies near the end of the year. I would guess this correlates to schools releasing, students stop pulling from school libraries. This became apparent when I noticed the jump with Fiction specifically. 

checkouts %>% select(monthdate,Category.Group) %>% group_by(monthdate,Category.Group) %>% summarize(counts=length(Category.Group)) %>% ggplot(aes(x=monthdate,y=counts,group=Category.Group,color=Category.Group))+geom_line()

checkout_merged = checkouts %>% left_join(book_data,by=c('BibNumber','ItemType','Collection'))
#one difficulty here is that the item's location explodes the join. there's no location identifier in the checkouts. It it isn't residing in the checkouts, then for now i'll cut it out and progress. I have an idea to test later about location heat but that will be a different thought process. Right now I'm just trying to get a better feel for how to trend the checkouts generally.

#Don't do this.
#checkout_merged = checkouts %>% left_join(book_data,by=c('BibNumber','ItemType','Collection')) %>% select(-ItemLocation) %>% unique()

test = book_data %>% select(-FloatingItem) %>% group_by(BibNumber,Title,Author,ISBN,PublicationYear,Publisher,Subjects,ItemType,Collection,ReportDate) %>%  summarise(locations=length(unique(ItemLocation)),full_count=sum(ItemCount))

books_constrained = book_data %>% select(-ItemLocation,-ReportDate) %>% unique()
checkout_merged = checkouts %>% left_join(books_constrained,by=c('BibNumber','ItemType','Collection')) 

#The publisher year is a mess. 
