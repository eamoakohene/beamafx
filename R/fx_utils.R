fx_utils <- R6::R6Class(
  'fx_utils'

  ,public = list(


    get_sq = function(){

      return(
        storedQry::SQ$new( self$get_db() )
      )
    }

    ,get_db = function(){

      if( private$local_mode ){
        return(private$db_local)
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

    db_local ='R:/packages/beamafx/inst/extdata/beamafx.sqlite',
    db_pkg = system.file("extdata/beamafx.sqlite",package="beamafx"),
    local_mode = TRUE,

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
