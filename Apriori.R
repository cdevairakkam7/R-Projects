# Data loading and Processing
order_detail <- read.csv(file = 'https://www.dropbox.com/s/p46fmc6q7yq7map/B2-3_order_detail.csv?dl=0')
orders <- read.csv(file = 'https://www.dropbox.com/s/m3axouso5lkltgn/B2-3_orders.csv?dl=0')
combined <-sqldf("Select a.order_number as order_number, a.product_name as product_name , b.completed_time as completed_time FROM order_detail a INNER join orders b ON a.order_number = b.order_number")
dataset<-dcast(combined,order_number~product_name, value.var="product_name")
write.csv(dataset,file='~/Desktop/dataset.csv')
freq_data = read.transactions('~/Desktop/dataset.csv',sep=',')
itemFrequencyPlot(freq_data)

# The apriori algorithm 
rules = apriori (data=freq_data,parameter = list(support= 0.04 ,confidence = 0.35, minlen=2 ))
inspect(sort(rules,by = 'lift') )
