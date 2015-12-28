# Install this just once on each computer.
# install.packages("MonetDB.R")
library(MonetDB.R)

# Set this to wherever your project is located.
setwd("~/GitHub/DCI")

# Note: Only run this command once! 
# Do not run again when adding new files.
batfile <-
  monetdb.server.setup(
    
    # set the path to the directory where the initialization batch file and all data will be stored
    database.directory = paste0( getwd() , "/MonetDB" ) ,
    # must be empty or not exist
    
    # find the main path to the monetdb installation program
    monetdb.program.path = 
      ifelse( 
        .Platform$OS.type == "windows" , 
        "C:/Program Files/MonetDB/MonetDB5" , 
        "" 
      ) ,
    # note: for windows, monetdb usually gets stored in the program files directory
    # for other operating systems, it's usually part of the PATH and therefore can simply be left blank.
    
    # choose a database name
    dbname = "test" ,
    
    # choose a database port
    # this port should not conflict with other monetdb databases
    # on your local computer.  two databases with the same port number
    # cannot be accessed at the same time
    dbport = 50000
  )

# For later use, to access the files in the 'test' database:
# batfile <- "C:\\Users\\BR\\Documents\\GitHub\\DCI\\MonetDB\\test.bat"





# # How to use:
# 
# ######################################################################
# # lines of code to hold on to for all other `test` monetdb analyses #
# 
# # first: specify your batfile.  again, mine looks like this:
# batfile <- "C:/My Directory/MonetDB/test.bat"		# # note for mac and *nix users: `test.bat` might be `test.sh` instead
# 
# # second: run the MonetDB server
# monetdb.server.start( batfile )
# 
# # third: your five lines to make a monet database connection.
# # just like above, mine look like this:
# dbname <- "test"
# dbport <- 50000
# 
# monet.url <- paste0( "monetdb://localhost:" , dbport , "/" , dbname )
# db <- dbConnect( MonetDB.R() , monet.url , wait = TRUE )
# 
# # fourth: store the process id
# pid <- as.integer( dbGetQuery( db , "SELECT value FROM env() WHERE name = 'monet_pid'" )[[1]] )
# 
# 
# # # # # run your analysis commands # # # #
# 
# 
# # disconnect from the current monet database
# dbDisconnect( db )
# 
# # and close it using the `pid`
# monetdb.server.stop( pid )
# 
# # end of lines of code to hold on to for all other `test` monetdb analyses #
# #############################################################################


