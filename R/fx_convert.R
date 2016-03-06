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
