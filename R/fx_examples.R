

# beamafx::fx_tbl_tail(n=60, where = "data_code='GBP'", order_by ="yr,mth,dy")
# beamafx::fx_tbl_head(n=15, where = "data_code='EUR'", order_by ="yr,mth,dy")
# beamafx::fx_download$new()$set_data_points(30)$update_euro_all()


  #abc <-  beamafx::fx_series$new('CNY',codes_only=T)$set_date1('2000/01/01')$set_date2('2016/04/30')$set_freq('d')$get_data()
  # beamafx::fx_series$new('USD,GBP,EUR')$set_date1('2016/01/01')$set_date2('2016/04/30')$set_freq('m')$build_sql()
  # beamafx::fx_series$new('USD,GBP,EUR')$set_date1('2015/01/01')$set_date2('2016/04/30')$set_freq('q')$get_data()
  # beamafx::fx_series$new('USD,GBP,EUR',to='GHS')$set_date1('2015/01/01')$set_date2('2016/04/30')$set_freq('q')$get_data()
  #
  # beamafx::fx_series$new('USD,GBP,EUR')$set_date1('2008/01/01')$set_date2('2016/04/30')$set_freq('y')$get_data()
  # beamafx::fx_series$new('USD,GBP,EUR')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
  #
  # beamafx::fx_series$new('USD,GBP,EUR',to='USD')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
  #
  # fx_series$new('USD')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
  # fx_series$new('USD,GBP')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$set_filter(T)$get_data()
  #

# dt1 <- '2016/01/01'
# dt2 <- '2016/12/31'

#fx_series$new('USD,GBP,EUR')$set_date1(dt1)$set_date2(dt2)$set_freq('m')$get_data()

# storedQry::SQ$new( 'R:/packages/beamafx/inst/extdata/beamafx.sqlite' )$set_name("fx_update_periods")$qry_exec()

# coil coatings
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2014/10/20')$set_date2('2014/10/31')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2015/01/20')$set_date2('2015/01/31')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2015/04/20')$set_date2('2015/04/30')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2015/07/20')$set_date2('2015/07/31')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2015/10/20')$set_date2('2015/10/31')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2016/01/20')$set_date2('2016/01/31')$set_freq('d')$set_filter(TRUE)$get_data()

# marine coatings
# beamafx::fx_series$new('EUR',to='GBP')$set_date1('2015/09/20')$set_date2('2015/09/30')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('EUR',to='GBP')$set_date1('2015/11/20')$set_date2('2015/11/30')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('EUR',to='GBP')$set_date1('2016/01/20')$set_date2('2016/01/31')$set_freq('d')$set_filter(TRUE)$get_data()

##
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2015/09/20')$set_date2('2015/09/30')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2015/11/20')$set_date2('2015/11/30')$set_freq('d')$set_filter(TRUE)$get_data()
# beamafx::fx_series$new('USD,GBP',to='EUR')$set_date1('2016/01/20')$set_date2('2016/01/31')$set_freq('d')$set_filter(TRUE)$get_data()

# beamafx::get_d()
# beamafx::get_m()
# beamafx::get_y()

# beamafx::get_d('USD')
# beamafx::get_d('EUR')
