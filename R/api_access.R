# KOLADA API Reference Class Object

#' KOLADA API Reference Class Object
#' @param muni data frame of municipality
#' @param kpi data frame of KPI
#' @examples
#' kolada_mod <- kolada_api$new()
#' kolada_mod$search_with_title(search_type = "municipality", input_str = "lund")
#' kolada_mod$search_with_id(search_type = "municipality_groups", input_str = "G124026")
#' kolada_mod$search_data(input_kpi="N00945",input_municipality="1860",input_year="")
#' kolada_mod$search_ou("N15033,N15030","V15E144001301,V15E144001101","2009,2008,2007")
#' @import jsonlite

#' @export kolada_api
#' @exportClass kolada_api
kolada_api<-setRefClass("kolada_api",fields =list(muni="data.frame",kpi="data.frame") 
                        ,methods =list(
                          initialize=function(municipality,kpi){
                            # library(jsonlite)
                            muni<<-get_all_municipality()
                            kpi<<-get_all_KPI()
                          },
                          get_all_municipality=function(){
                            url<-"http://api.kolada.se/v2/municipality"
                            #d<-retreive_data(url)
                            d<-fromJSON(url)
                            return(as.data.frame(d))
                          },
                          get_all_KPI=function(){
                            url<-"http://api.kolada.se/v2/kpi"
                            #d<-retreive_data(url)
                            d<-fromJSON(url)
                            return (as.data.frame(d))
                          },
                          
                          search_with_title=function(search_type,input_str){

                            "user select search type and input the title, function returns a data frame of result"
                            url<-"http://api.kolada.se/v2/"
                            if(search_type=="kpi"){
                              url<-paste(url,"kpi?title=",input_str,sep="")
                            }else if(search_type=="kpi_groups"){
                              url<-paste(url,"kpi_groups?title=",input_str,sep="")
                            }else if(search_type=="municipality"){
                              url<-paste(url,"municipality?title=",input_str,sep="")
                            }else if(search_type=="municipality_groups"){
                              url<-paste(url,"municipality_groups?title=",input_str,sep="")
                            }else if(search_type=="ou"){
                              url<-paste(url,"ou?title=",input_str,sep="")
                            }else{
                              return ("Wrong Search Type!")
                            }
                            url<-URLencode(url)
                            d<-fromJSON(url)
                            if(d$count == 0){
                              return("0 result found.")
                            }
                            if(search_type=="kpi_groups"||search_type=="municipality_groups"){
                              d<-as.data.frame(d)
                              df<-data.frame()
                              for(i in 1:nrow(d)){
                                value<-as.data.frame(d[i,3])
                                id<-d[i,2]
                                title<-d[i,4]
                                value$id<-id
                                value$title<-title
                                df<-rbind(df,value)
                              }
                              return (df)
                            }
                            return(as.data.frame(d))
                          },
                          

                          search_with_id=function(search_type,input_str){
                            #' @param search_type id type that user selected
                            #' @param input_str id input by users
                            "user select search type and input the id, function returns a data frame of result"
                            url<-"http://api.kolada.se/v2/"
                            if(search_type=="kpi"){
                              url<-paste(url,"kpi/",input_str,sep="")
                            }else if(search_type=="kpi_groups"){
                              url<-paste(url,"kpi_groups/",input_str,sep="")
                            }else if(search_type=="municipality"){
                              url<-paste(url,"municipality/",input_str,sep="")
                            }else if(search_type=="municipality_groups"){
                              url<-paste(url,"municipality_groups/",input_str,sep="")
                            }else if(search_type=="ou"){
                              url<-paste(url,"ou/",input_str,sep="")
                            }else{
                              return ("Wrong Search Type!")
                            }
                            url<-URLencode(url)
                            d<-fromJSON(url)
                            if(d$count == 0){
                              return("0 result found.")
                            }
                            if(search_type=="kpi_groups"||search_type=="municipality_groups"){
                              d<-as.data.frame(d)
                              df<-data.frame()
                              for(i in 1:nrow(d)){
                                value<-as.data.frame(d[i,3])
                                id<-d[i,2]
                                title<-d[i,4]
                                value$id<-id
                                value$title<-title
                                df<-rbind(df,value)
                              }
                              return (df)
                            }
                            return(as.data.frame(d))
                          },
                          
                          search_data=function(input_kpi,input_municipality,input_year){
                            "Search data when all three parameters or only two are given.Functions returns a data frame."
                            url<-"http://api.kolada.se/v2/data"
                            if(nchar(input_kpi)!=0){
                              #kpi_str<-paste(as.character(input_kpi),collapse = ",")
                              url<-paste(url,"/kpi/",input_kpi,sep = "")
                            }
                            if(nchar(input_municipality)!=0){
                              # print(input_municipality)
                              # print(length(input_municipality))
                              #municipality_str<-paste(as.character(input_municipality),collapse = ",")
                              url<-paste(url,"/municipality/",input_municipality,sep="")
                            }
                            if(nchar(input_year)!=0){
                              # print(nchar(input_year))
                              # print(input_year)
                              #year_str<-paste(as.character(input_year),collapse = ",")
                              url<-paste(url,"/year/",input_year,sep="")
                            }
                            # print(url)
                            d<-fromJSON(url)
                            if(d$count == 0){
                              return("0 result found.")
                            }
                            d<-as.data.frame(d)
                            df<-data.frame()
                            for(i in 1:nrow(d)){
                              value<-d[i,ncol(d)]
                              info<-d[i,1:ncol(d)-1]
                              value<-cbind(value,info)
                              df<-rbind(df,value)
                            }
                            return (df)
                          },
                          search_ou=function(input_kpi,input_ou,input_year){
                            "Search orgranization unit when all three parameters or only two are given.Functions returns a data frame."
                            url<-"http://api.kolada.se/v2/oudata"
                            if(nchar(input_kpi)!=0){
                              #kpi_str<-paste(as.character(input_kpi),collapse = ",")
                              url<-paste(url,"/kpi/",input_kpi,sep = "")
                            }
                            if(nchar(input_ou)!=0){
                              #ou_str<-paste(as.character(input_ou),collapse = ",")
                              url<-paste(url,"/ou/",input_ou,sep="")
                            }
                            if(nchar(input_year)!=0){
                              #year_str<-paste(as.character(input_year),collapse = ",")
                              url<-paste(url,"/year/",input_year,sep="")
                            }
                            d<-fromJSON(url)
                            if(d$count == 0){
                              return("0 result found.")
                            }
                            d<-as.data.frame(d)
                            df<-data.frame()
                            for(i in 1:nrow(d)){
                              value<-d[i,ncol(d)]
                              info<-d[i,1:ncol(d)-1]
                              value<-cbind(value,info)
                              df<-rbind(df,value)
                            }
                            return (df)
                          }
                        ) )

k1<-kolada_api$new()

#k1$search_area("Stockholms läns landsting")
# 
# search_area=function(area_name){
#   search_area_url<-"http://api.kolada.se/v2/municipality/"
#   #area_name<-gsub(pattern = " ", replacement = "%20", x = area_name)
#   search_area_url<-paste(search_area_url,area_name,sep="")
#   search_area_url<-URLencode(search_area_url)
#   print(search_area_url)
#   d<-retreive_data(search_area_url)
#   return(as.data.frame(d))
# },
# search_KPI=function(KPI_str){
#   search_KPI_url<-"http://api.kolada.se/v1/kpi/"
#   search_KPI_url<-paste(search_KPI_url,KPI_str,sep="")
#   search_KPI_url<-URLencode(search_KPI_url)
#   d<-retreive_data(search_KPI_url)
#   return(as.data.frame(d))
# },
# search_enheter=function(id_str){
#   url<-"http://api.kolada.se/v1/ou/"
#   
#   search_enheter_url<-paste(url,id_str,sep="")
#   d<-retreive_data(search_enheter_url)
#   return(d)
# },
# get_data=function(input_str){
#   url<-"http://api.kolada.se/v1/data/"
#   input<-(unlist(strsplit(input_str,split=" ")))
#   if(input[1]=="exact"){
#     input_kpi<-input[2]
#     input_municipality<-input[3]
#     input_period<-input[4]
#     url<-paste(url,"exact/",input_kpi,"/",input_municipality,"/",input_period,sep="")
#     d<-retreive_data(url)
#     return (d)
#   }else if(input[1]=="peryear"||input[1]=="permunicipality"){
#     input_kpi<-input[2]
#     input_needle<-input[3]
#     url<-paste(url,input[1],"/",input_kpi,"/",input_needle,sep="")
#     d<-retreive_data(url)
#     return (d)
#   }else{
#     return ("error input")
#   }
# },
# get_enhets=function(input_str){
#   url<-"http://api.kolada.se/v1/ou/data/"
#   input<-(unlist(strsplit(input_str,split=" ")))
#   if(input[1]=="exact"){
#     input_kpi<-input[2]
#     input_ou<-input[3]
#     input_period<-input[4]
#     url<-paste(url,"exact/",input_kpi,"/",input_ou,"/",input_period,sep="")
#     d<-retreive_data(url)
#     return (d)
#   }else if(input[1]=="peryear"||input[1]=="perou"){
#     input_kpi<-input[2]
#     input_needle<-input[3]
#     url<-paste(url,input[1],"/",input_kpi,"/",input_needle,sep="")
#     d<-retreive_data(url)
#     return (d)
#   }else{
#     return ("error input")
#   }
# }
d<-c(2009,2010,2011)
d<-as.character(d)
d<-paste(d,collapse = ",")
d