
scrap_currency_names <- function(){
  my_url <- 'http://www.xe.com/iso4217.php'
  my_css <- '#codeList .tblBrdrLn'


  my_page <- try(xml2::read_html(my_url),silent = T)

  if(class(my_page)[1]=='try-error'){
    cat('Error occurred why reading page. Exiting now ...')
    return(NULL)
  }

  my_element <- rvest::html_nodes(my_page, my_css)
  if(is.null(my_element)){ return(NULL) }

  my_txt <- rvest::html_text(my_element)
  my_len <- length(my_txt)
  my_sym <- my_txt[seq(1,my_len-1,2)]
  my_name <- my_txt[seq(2,my_len,2)]
  my_name <- gsub("'" , "",my_name)
  my_df <- data.frame(symbol = my_sym,description=my_name)

  return(my_df)
}

fx_tbl_tail <- function(tbl='fx_data',n=10, where=NULL){
  fx_utils$new()$get_sq()$table_tail(tbl,n,where)
}
fx_tbl_head <- function(tbl='fx_data',n=10, where=NULL){
  fx_utils$new()$get_sq()$table_head(tbl,n,where)
}

# fx_update_meta_src_euro <- function(){
#   fx_url <- "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip?9e7953460ebd225db8e7991a0968a53d"
#   my_data <- read.csv(csv_path,header = T)
#   my_names <- names(my_data)
#   my_sql <- sprintf("update fx_meta set data_src='%s', data_frq='%s',data_src_url='%s', data_unit='1EUR' where data_code='%s'",
#                     "ECB - European Central Bank",'d',fx_url,my_names
#   )
#
#   my_con <-  DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
#
#   for(i in 1:length(my_sql)){
#     RSQLite::dbSendQuery(my_con,my_sql[i])
#     cat(my_names[i]," done\n")
#   }
#
#   DBI::dbDisconnect(my_con)
# }
#

#Examples
# fx_download$new()$set_data_points(5000)$update_euro('EUR')
# fx_download$new()$set_data_points(30)$update_euro_all()

# fx_series$new('USD,GBP,EUR')$set_date1('2016/01/01')$set_date2('2016/04/30')$set_freq('m')$get_data()
# fx_series$new('USD,GBP,EUR')$set_date1('2016/01/01')$set_date2('2016/04/30')$set_freq('m')$build_sql()
# fx_series$new('USD,GBP,EUR')$set_date1('2015/01/01')$set_date2('2016/04/30')$set_freq('q')$get_data()
# fx_series$new('USD,GBP,EUR',to='GHS')$set_date1('2015/01/01')$set_date2('2016/04/30')$set_freq('q')$get_data()
#
# fx_series$new('USD,GBP,EUR')$set_date1('2008/01/01')$set_date2('2016/04/30')$set_freq('y')$get_data()
# fx_series$new('USD,GBP,EUR')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
#
# fx_series$new('USD,GBP,EUR',to='USD')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
# fx_series$new('EUR')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()

# The following 2 lines produces the same results
# fx_series$new('USD',    to='GBP')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
# fx_series$new('USD,GBP',to='GBP')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$set_filter(T)$get_data()
