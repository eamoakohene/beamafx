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
