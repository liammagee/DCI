library(MonetDB.R)

batfile <- "C:\\Users\\BR\\Documents\\GitHub\\DCI\\MonetDB\\test.bat"
monetdb.server.start( batfile )
dbname <- "test"
dbport <- 50000
monet.url <- paste0( "monetdb://localhost:" , dbport , "/" , dbname )
db <- dbConnect( MonetDB.R() , monet.url , wait = TRUE )
pid <- as.integer( dbGetQuery( db , "SELECT value FROM env() WHERE name = 'monet_pid'" )[[1]] )

# List the tables in the test database
dbListTables( db )

# Read in survey data
data = read.csv("output/sample_values_melted.csv")

# Write data to database, name the table 'data'.
dbWriteTable( db , 'data' , data )

# List the tables in the test database
dbListTables( db )
