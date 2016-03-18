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

        if(nrow(my_df) > 0 ){
              my_df$value <- as.numeric(my_df$value)

              my_yr <-  substr(my_df$date, my_ext$yr[1], my_ext$yr[2])
              my_mth <- as.numeric(substr(my_df$date,my_ext$mth[1],my_ext$mth[2]))
              my_dy <- as.numeric(substr(my_df$date,my_ext$dy[1],my_ext$dy[2]))

              my_value <- my_df$value

              my_sql <- sprintf ("insert into fx_data (yr,mth,dy,data_value,data_code) values (%s,%s,%s,%s,'%s');", my_yr,my_mth,my_dy,my_value,my_names[j])
              my_con <-  private$get_db_con()
              my_rows <- length(my_sql)

              for(i in 1:my_rows){
                RSQLite::dbSendQuery( my_con,my_sql[i] )
                cat(my_sql[i],"\n")
                cat(my_names[j]," row ",i," of ",my_rows ,"\n")
              }
              DBI::dbClearResult(DBI::dbListResults(my_con)[[1]])
              DBI::dbDisconnect(my_con)
        }
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

        if( nrow(my_df)> 0 ){
            my_df$value <- as.numeric(my_df$value)

            my_yr <-  substr( my_df$date, my_ext$yr[1], my_ext$yr[2])
            my_mth <- as.numeric( substr( my_df$date, my_ext$mth[1], my_ext$mth[2] ))
            my_dy <- as.numeric( substr( my_df$date, my_ext$dy[1], my_ext$dy[2] ))

            my_value <- my_df$value

            my_sql <- sprintf ("insert into fx_data (yr,mth,dy,data_value,data_code) values (%s,%s,%s,%s,'%s');", my_yr,my_mth,my_dy,my_value,my_names[j])
            #my_con <-  private$get_db_con()
            my_rows <- length(my_sql)

            for(i in 1:my_rows){
              #RSQLite::dbSendQuery(my_con,my_sql[i])
              private$run_sql(my_sql[i])
              cat(my_names[j]," row ",i," of ",my_rows ,"\n")
            }
            #DBI::dbClearResult(DBI::dbListResults(my_con)[[1]])
            #DBI::dbDisconnect(my_con)
        }
      }
      cat('Now filling in the gaps. Please wait .....\n')
      SQ$new("fx_update_periods")$qry_exec()
      cat('All done!\n')

    }

  ),
  private = list(

  )
)
