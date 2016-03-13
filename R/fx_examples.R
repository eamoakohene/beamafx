

# fx_tbl_tail(n=15, where = "data_code='GBP'")
# fx_tbl_head(n=15, where = "data_code='EUR'")
# fx_download$new()$set_data_points(5000)$update_euro('EUR')
# fx_download$new()$set_data_points(20)$update_euro_all()


  # fx_series$new('USD,GBP,EUR')$set_date1('2016/01/01')$set_date2('2016/04/30')$set_freq('m')$get_data()
  # fx_series$new('USD,GBP,EUR')$set_date1('2016/01/01')$set_date2('2016/04/30')$set_freq('m')$build_sql()
  # fx_series$new('USD,GBP,EUR')$set_date1('2015/01/01')$set_date2('2016/04/30')$set_freq('q')$get_data()
  # fx_series$new('USD,GBP,EUR',to='GHS')$set_date1('2015/01/01')$set_date2('2016/04/30')$set_freq('q')$get_data()
  #
  # fx_series$new('USD,GBP,EUR')$set_date1('2008/01/01')$set_date2('2016/04/30')$set_freq('y')$get_data()
  # fx_series$new('USD,GBP,EUR')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
  #
  # fx_series$new('USD,GBP,EUR',to='USD')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
  #
  # fx_series$new('USD')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$get_data()
  # fx_series$new('USD,GBP')$set_date_range('2008/01/01','2016/04/30')$set_freq('y')$set_filter(T)$get_data()
  #
