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

get_d <- function(
  fx='EUR',
  from= as.Date(
    paste(
      lubridate::year(Sys.Date()),
      lubridate::month(Sys.Date())-1,
      1,sep='-')
  ),

  to = Sys.Date()

){
  fx_series$new( paste0( trimws(fx),',GBP') )$set_date_range( from ,to )$set_freq( 'd' )$set_filter(T)$get_data()
}

get_m <- function(
     fx='EUR',
     from= as.Date(
         paste(
            lubridate::year(Sys.Date())-1,
            lubridate::month(Sys.Date()),
            1,sep='-')
         ),

     to = Sys.Date()

  ){
  fx_series$new( paste0( trimws(fx),',GBP') )$set_date_range( from ,to )$set_freq( 'm' )$set_filter(T)$get_data()
}

get_y <- function(
  fx='EUR',
  from= as.Date(
    paste(
      lubridate::year(Sys.Date())-10,
      1,
      1,sep='-')
  ),

  to = Sys.Date()

){

  fx_series$new( paste0( trimws(fx),',GBP') )$set_date_range(from,to)$set_freq('y')$set_filter(T)$get_data()
}

get_last <- function(x=10,fx='EUR',prd = c('d','m','y')){

  today <- Sys.Date()
  period <- NULL

  if(missing(prd)){
    period <- 'd'
  }else{
    period <- match.arg(prd)
  }

  from <- switch(period,
          'd'= today-x,
          'm'= lubridate::month(today)-x,
          'y'= lubridate::year(today)-x
  )

  to <- today

  fx_series$new( paste0( trimws(fx),',GBP') )$set_date_range(from,to)$set_freq( period )$set_filter(T)$get_data()
}
