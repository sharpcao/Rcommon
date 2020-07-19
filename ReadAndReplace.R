require(jsonlite)
require(clipr)
# desc = read_clip_tbl(quote="", stringsAsFactors=F,header =F)
readAndReplace <- function(my_file, desc, ...)
{
  dd = read.csv(my_file,stringsAsFactors = F, ...)
  apply(desc,1, function(x){
            txt = dd[[x[1]]]
            if(!is.na(x[3])){
              js = fromJSON(x[3])
              for(it in names(js)) {
                txt = gsub(it,js[[it]],txt)
              }
            }
            if(x[2]=="num"){
              txt = as.numeric(txt)
            }else if(x[2]=="fac"){
              txt = as.factor(txt)
             
            }
            dd[[x[1]]] <<- txt
           
      }
    )
  return(dd[,desc[[1]]])
}
genNAFields <- function(xx)
{
    fields = names(xx)[sapply(xx,function(x) any(is.na(x)))]
    xx = xx[,fields]
    xx[!is.na(xx)] = 0
    xx[is.na(xx)] = 1
    names(xx) = paste(fields,"na",sep="_")
    return(xx)
}
FillNAFields <- function(dd,method="median")
{
  out = sapply(dd,function(x) {
      xm = median(x[!is.na(x)])
      ifelse(is.na(x),xm,x)
      
    } )
  return(out)
}

