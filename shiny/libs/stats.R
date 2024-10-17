fun_list$fun_stats_test <- function(x,y,method){
  method <- tolower(method)
  message('>>> test using: ',method)
  print('>>> test using(x): ')
  print(x)
  print('>>> test using(y): ')
  print(y)
  if(method == 't-test'){
    res <-  stats::t.test(x = x,y = y)
  }else if(method == 'wilcoxon'){
    res <- stats::wilcox.test(x,y)
  }else{
    stop("methods not supported: ",method)
  }
  return(res$p.value)
}

# #a = c(1,2,3,4,5,6)
# a = c(1,2,3,7,5,5)
# #b = c(4,5,6,7,1,2,3,4,5,6)
# b = c(4,5,6,12)
# funs_list$fun_stat_two(
#   a,b,'t-test'
# )
# funs_list$fun_stat_two(
#   a,b,'wilcoxon'
# )
