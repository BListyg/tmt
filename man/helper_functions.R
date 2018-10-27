#
name_rearrange <- function(x){
  apply(X = do.call(rbind,str_split(string = x, pattern = " "))[,c(2,1)],MARGIN = 1,FUN = paste,sep=' ',collapse=', ')
}

#
'%!in%' <- function(x,y)!('%in%'(x,y))

#
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}