fx_utils <- R6::R6Class(
  'fx_utils'

  ,public = list(


    get_sq = function(){

      return(
        storedQry::SQ$new( self$get_db() )
      )
    }

    ,get_db = function(
      path=here::here("R","beamafx.sqlite")
    ){

      if( private$local_mode ){
        return(path)
      }else{
        return(private$db_pkg)
      }
    }
    ,str_pos = function(x,pattern=","){
      my_str <- gregexpr(pattern =pattern,x)
      return(my_str[[1]][1])
    }
  )
  ,private = list(


    db_pkg = here::here("R","beamafx.sqlite"),# system.file("beamafx.sqlite",package="beamafx"),
    local_mode = TRUE,

    get_db_con = function(){
      return(
        DBI::dbConnect(RSQLite::SQLite(), dbname=self$get_db() )
      )
    }

    ,run_sql = function(qry) {
      #cat( qry, '\n')
      return(sqldf::sqldf(qry, dbname= self$get_db()))
    }
  )#private
)

fx.get_end_of_month_rates <- function(from = 'USD', to='EUR', d1='2010/01/01', d2 = '2018/12/31', odd_mths = T ){

  aaa <- beamafx::fx_series$new( code = from , to = to)$set_date1(d1)$set_date2(d2)$set_freq('d')$set_filter(TRUE)$get_data()
  aaa$qtr <- NULL

  ccc <- aaa

  if(odd_mths){
    ccc <- dplyr::filter(aaa, mth %% 2 == 1)
  }


  max_day_row <- function(yr = 2018, mth = 7){
     sqldf::sqldf(
        sprintf("select * from ccc where yr=%s and mth = %s order by dy desc limit 1", yr, mth)
     )
  }

  get_unique_months <- function(){
    sqldf::sqldf(
      "select distinct yr,mth from ccc order by yr,mth"
    )
  }

  mths <- get_unique_months()
  df <- max_day_row( yr = mths$yr[ 1 ], mth = mths$mth[ 1 ])

  for(i in 2: nrow(mths)){
    ldf <- max_day_row( yr = mths$yr[ i ], mth = mths$mth[ i ])
    df <- rbind( df, ldf)

  }

  return(df)
}

