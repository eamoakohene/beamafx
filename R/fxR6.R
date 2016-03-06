

fx_utils <- R6::R6Class(
  'fx_utils'

  ,public = list(

    db_name_local ='R:/packages/beamafx/inst/extdata/beamafx.sqlite',
    db_name_pkg = system.file("extdata/beamafx.sqlite",package="beamafx"),
    local_mode = FALSE,

    initialize = function(){

    }
    ,get_db = function(){

      if( self$local_mode ){
            return(self$db_name_local)
      }else{
            return(self$db_name_pkg)
      }
    }
    ,str_pos = function(x,pattern=","){
      my_str <- gregexpr(pattern =pattern,x)
      return(my_str[[1]][1])
    }
  )
  ,private = list(

    get_db_con = function(){
      return(
        DBI::dbConnect(RSQLite::SQLite(), dbname=self$get_db() )
      )
    }

    ,run_sql = function(qry) {
      return(sqldf::sqldf(qry, dbname= self$get_db()))
    }
  )#private
)

fx_download <- R6::R6Class(
  'fx_download',
  inherit = fx_utils,

  public = list(

    zip_ext =  list(  yr=c(1,4), mth=c(6:7),dy=c(9:10)),
    csv_ext =  list(  yr=c(7,10), mth=c(4:5),dy=c(1:2)),

    dp = 30,
    src = NULL,
    src_list = list(
      ecb ="http://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip?9e7953460ebd225db8e7991a0968a53d"
    ),

    initialize = function(src='ECB'){
      self$set_src(src)
    }
    ,set_src = function(value){
       if(!missing(value) && !is.null(value)){
         self$src <- self$src_list[[tolower(value)]]
       }
      invisible(self)
    }

    ,set_data_points = function(value){
      if(!missing(value) && !is.null(value)){
        self$dp <- value
      }
      invisible(self)

    }

    ,download_euro = function() {
      cat('Downloading data from ',self$src,'\n')
      fx_file <- tempfile()
      download.file(self$src,fx_file, mode="wb")
      my_data <- read.csv(unz(fx_file,"eurofxref-hist.csv"),header = T,stringsAsFactors = F)
      #cat('rows = ',nrow(my_data),'\n')
      return(my_data)
    }

    ,update_euro_all = function(is_zip = TRUE){

      my_data <- self$download_euro()
      my_data$EUR <- 1
      my_names <- names(my_data)
      my_dp <- self$dp

      my_ext <- NULL
      if(is_zip){
        my_ext <- self$zip_ext
      }else{
        my_ext <- self$csv_ext
      }


      for(j in 2:length(my_names)){

        my_df <- my_data[,c(1,j)]


        if(nrow(my_df)> my_dp){
          my_df <- my_df[c(1:my_dp),]
        }

        names(my_df) <- c('date','value')
        my_df <- dplyr::filter(my_df,!( trimws(value)=='N/A'))
        my_df$value <- as.numeric(my_df$value)



        my_yr <-  substr(my_df$date, my_ext$yr[1], my_ext$yr[2])
        my_mth <- as.numeric(substr(my_df$date,my_ext$mth[1],my_ext$mth[2]))
        my_dy <- as.numeric(substr(my_df$date,my_ext$dy[1],my_ext$dy[2]))

        my_value <- my_df$value

        my_sql <- sprintf ("insert into fx_data (yr,mth,dy,data_value,data_code) values (%s,%s,%s,%s,'%s');", my_yr,my_mth,my_dy,my_value,my_names[j])
        my_con <-  private$get_db_con()
        my_rows <- length(my_sql)

        for(i in 1:my_rows){
          RSQLite::dbSendQuery(my_con,my_sql[i])
          cat(my_names[j]," row ",i," of ",my_rows ,"\n")
        }

        DBI::dbDisconnect(my_con)
      }

      cat('Now filling in the gaps. Please wait .....\n')
      SQ$new("fx_update_periods")$qry_exec()
      cat('All done!\n')

    }
    ,update_euro = function( cur=c('GBP','USD','CNY','JPY','BRL'),is_zip = TRUE ){

      my_data <- self$download_euro()
      my_data$EUR <- 1

      my_header <- c('Date',cur)

      my_ext <- NULL
      if(is_zip){
        my_ext <- self$zip_ext
      }else{
        my_ext <- self$csv_ext
      }


      # cat('rows = ',nrow(my_data),'\n')
      # cat('my_header = ',paste(my_header,sep='',collapse = ','),'\n')
      # cat('names = ',paste(names(my_data),sep='',collapse = ','),'\n')
      # return(NULL)

      my_data <- my_data[, my_header]
      my_names <- names(my_data)
      my_dp <- self$dp
      if(my_dp < nrow(my_data)){
        my_data <- my_data[c(1:my_dp),]
      }
      #return(my_data)
      #j=2
      for(j in 2:length(my_names)){

        my_df <- my_data[,c(1,j)]

        names(my_df) <- c('date','value')
        my_df <- dplyr::filter(my_df,!( trimws(value)=='N/A'))
        my_df$value <- as.numeric(my_df$value)

        my_yr <-  substr( my_df$date, my_ext$yr[1], my_ext$yr[2])
        my_mth <- as.numeric( substr( my_df$date, my_ext$mth[1], my_ext$mth[2] ))
        my_dy <- as.numeric( substr( my_df$date, my_ext$dy[1], my_ext$dy[2] ))

        my_value <- my_df$value

        my_sql <- sprintf ("insert into fx_data (yr,mth,dy,data_value,data_code) values (%s,%s,%s,%s,'%s');", my_yr,my_mth,my_dy,my_value,my_names[j])
        my_con <-  private$get_db_con()
        my_rows <- length(my_sql)

        for(i in 1:my_rows){
          RSQLite::dbSendQuery(my_con,my_sql[i])
          cat(my_names[j]," row ",i," of ",my_rows ,"\n")
        }
        DBI::dbClearResult(DBI::dbListResults(my_con)[[1]])
        DBI::dbDisconnect(my_con)

      }
      cat('Now filling in the gaps. Please wait .....\n')
      SQ$new("fx_update_periods")$qry_exec()
      cat('All done!\n')

    }

  ),
  private = list(

  )
)

fx_convert <- R6::R6Class(
  'fx_convert',
  inherit = fx_utils,
  public = list(

    fx_from = NULL,
    fx_to = NULL,
    fx_convert = NULL,
    fx_amt = 1,
    yr= NULL,
    mth = NULL,
    dy = NULL,

    initialize = function(from,to='GBP',dt,amt=1){
      self$set_from(from)
      self$set_to(to)
      self$set_date(dt)
      self$set_amt(amt)
    }
    ,set_from = function(value){
      if(!missing(value) && !is.null(value)){
        self$fx_from <- value
      }
      invisible(self)
    }
    ,set_to = function(value){
      if(!missing(value) && !is.null(value)){
        self$fx_to <- value
      }
      invisible(self)
    }
    ,set_date = function(value){

      if(!missing(value) && !is.null(value)){

        my_date <- as.Date(value)
        self$yr <- lubridate::year(my_date)
        self$mth <- lubridate::month(my_date)
        self$dy <- lubridate::day(my_date)
      }
      invisible(self)

    }

    ,set_amt = function(value){
      if(!missing(value) && !is.null(value)){
        self$fx_amt <- value
      }
      invisible(self)
    }

    ,get_data = function(){
       my_sql <- sprintf(
               "select * from fx_data where data_code in ('%s','%s') and yr=%s and mth=%s and dy =%s",
                self$fx_from,self$fx_to,self$yr,self$mth,self$dy
               )

       return(
         private$run_sql(my_sql)
       )
    }

    ,get_rate = function(){
      my_data <- self$get_data()
      my_from <- dplyr::filter( my_data,data_code == self$fx_from )$data_value[1]
      my_to <-   dplyr::filter( my_data,data_code == self$fx_to )$data_value[1]

      #cat(self$fx_from,' = ',my_from,'\n')
      #cat(self$fx_to,' = ',my_to,'\n')

      if(my_to > 0){
        self$fx_convert <-  (as.numeric(my_from) / as.numeric(my_to)) * as.numeric(self$fx_amt)
      }

      return(
          list(
            desc=paste0( round(self$fx_convert,4),' ',self$fx_to ),
            value=self$fx_convert,
            units=self$fx_to
          )
      )
    }
  )#public
  ,private = list(

  )#private
)#class

fx_series <- R6::R6Class(
    'fx_series',
    inherit = fx_utils,

    public = list(
      y1= 2010,
      y2=2020,
      m1=1,
      m2=12,
      d1=1,
      d2=31,
      code = NULL,
      frq = 'd',
      fx_to = NULL,
      dtd1 = NULL,
      dtd2 = NULL,
      filter_on = FALSE,

      initialize = function(code = 'USD,GBP,EUR',to='GBP'){

        my_code <- code
        if( !(self$str_pos(code,to)) > 0){
          my_code <- paste0(code,',',to)
          self$set_filter(TRUE)
        }

        self$set_codes(my_code)
        self$convert_to(to)
      }

      ,set_freq = function(value){
        if(!missing(value) && !is.null(value)){
          self$frq <- value
        }
        invisible(self)
      }

      ,set_filter = function(value){
        if(!missing(value) && !is.null(value)){
          self$filter_on <- value
        }
        invisible(self)
      }


      ,convert_to = function(value){
        if(!missing(value) && !is.null(value)){

          self$fx_to <- value
        }
        invisible(self)
      }
      ,set_date1 = function(value){
        if(!missing(value) && !is.null(value)){
          my_date <- as.Date(value)

          self$set_y1( lubridate::year( my_date))
          self$set_m1( lubridate::month( my_date))
          self$set_d1( lubridate::day( my_date))
          self$set_data_days(1)
        }
        invisible(self)
      }

      ,set_date2 = function(value){
        if(!missing(value) && !is.null(value)){
          my_date <- as.Date(value)

          self$set_y2( lubridate::year( my_date))
          self$set_m2( lubridate::month( my_date))
          self$set_d2( lubridate::day( my_date))
          self$set_data_days(2)
        }
        invisible(self)
      }
      ,set_date_range = function(value1,value2){

         if(!missing(value1) && !is.null(value1)){
           self$set_date1(value1)
         }

         if(!missing(value2) && !is.null(value2)){
           self$set_date2(value2)
         }
         invisible(self)
       }
      ,set_data_days = function(value){

        if(value==1){
          self$dtd1 <- 372 * self$y1 + 31 * self$m1 + self$d1
        }else{
          self$dtd2 <- 372 * self$y2 + 31 * self$m2 + self$d2
        }
        invisible(self)
      }
      ,set_y1 = function(value){
        if(!missing(value) && !is.null(value)){
          self$y1 <- value
        }
        invisible(self)
      }
      ,set_y2 = function(value){
        if(!missing(value) && !is.null(value)){
          self$y2 <- value
        }
        invisible(self)
      }
      ,set_m1 = function(value){
        if(!missing(value) && !is.null(value)){
          self$m1 <- value
        }
        invisible(self)
      }

      ,set_m2 = function(value){
        if(!missing(value) && !is.null(value)){
          self$m2 <- value
        }
        invisible(self)
      }
      ,set_d1 = function(value){
        if(!missing(value) && !is.null(value)){
          self$d1 <- value
        }
        invisible(self)
      }
      ,set_d2 = function(value){
        if(!missing(value) && !is.null(value)){
          self$d2 <- value
        }
        invisible(self)
      }

      ,set_codes = function(value){
        if(!missing(value) && !is.null(value)){
          self$code <- private$split_str( value )
        }
        invisible(self)
      }

     ,build_sql = function(){

         my_sql <- NULL
         group_by <- ""
         order_by <- ""

         my_prd <- self$frq

         if(my_prd=='q'){

           my_sql <- "select yr,qtr*3 as mth,qtr,28 as dy,data_code,avg(data_value)  as data_value from fx_data "
           group_by <- "group by yr,qtr,data_code "
           order_by <- " order by data_code,yr,qtr "

         }else if(my_prd=='m'){

           my_sql <- "select yr,mth,qtr,28 as dy,data_code,avg(data_value)  as data_value from fx_data "
           group_by <- "group by yr,mth,data_code "
           order_by <- " order by data_code,yr,mth "

         }else if(my_prd=='y'){

           my_sql <- "select yr, 12 as mth, 4 as qtr, 28 as dy, data_code,avg(data_value) as data_value from fx_data "
           group_by <- "group by yr,data_code "
           order_by <- " order by data_code,yr "

         }else{
           my_sql <- "select yr,mth,qtr, dy,data_code,data_value from fx_data "
           group_by <- "group by yr,mth,dy,data_code "
           order_by <- " order by data_code,yr,mth,dy "
         }

         self$set_data_days(1)
         self$set_data_days(2)

         qry_where <-" where "
         qw_code <- paste0(" data_code in ",self$code)
         qw_dtd <- paste0(" and (data_days between ",self$dtd1," and ",self$dtd2,")")

         sql_where <- paste0( qry_where, qw_code, qw_dtd)

         my_sql <- paste0( my_sql, sql_where, group_by, order_by )
         return(my_sql)

     }
     ,get_data = function(){

        my_data <- private$run_sql( self$build_sql() )
        if(!is.null(my_data)){

          if( self$str_pos( self$code, self$fx_to)>0 ){
              my_spread <- tidyr::spread( my_data, data_code, data_value)
              ncols <- ncol(my_spread)
              my_spread_val <- my_spread[,5:ncols]
              my_scaler <- my_spread[[ self$fx_to ]]
              COL_OP <- 2
              my_spread_scaled <- apply(my_spread_val,COL_OP,FUN=function(x){x/my_scaler})
              my_spread_scaled <- cbind(my_spread[,1:4],my_spread_scaled)
              my_gather <- tidyr::gather(
                                      data = my_spread_scaled,
                                      key = data_code,
                                      value = calc_value,
                                      5:ncol(my_spread_scaled)
                                      )
              my_data <- dplyr::arrange(my_data,data_code,yr,mth,dy)
              my_gather <- dplyr::arrange(my_gather,data_code,yr,mth,dy)
              my_data$value <- my_gather$calc_value

              if(self$filter_on){
                my_data$data_value <- NULL
                my_data <- dplyr::filter(my_data,!(data_code == self$fx_to))
              }

          }

        }
        return(my_data)
     }

    ),

    private = list(

      split_str = function(q="EUR,GBP,USD"){
        my_str <- base::gsub(",","','",q)
        return (
          paste0("('",my_str,"')")
        )
      }
    )
)

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
  SQ$new()$table_tail(tbl,n,where)
}
fx_tbl_head <- function(tbl='fx_data',n=10, where=NULL){
  SQ$new()$table_head(tbl,n,where)
}

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
