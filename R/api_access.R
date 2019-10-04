kolada_api<-setRefClass("kolada_api",fields =list(muni="data.frame",kpi="data.frame") 
                        ,methods =list(
                          initialize=function(municipality,kpi){
                            library(jsonlite)
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
                              return ("error")
                            }
                            url<-URLencode(url)
                            d<-fromJSON(url)
                            return(as.data.frame(d))
                          },
                          search_with_id=function(search_type,input_str){
                            url<-"http://api.kolada.se/v2/"
                            if(search_type=="kpi"){
                              url<-paste(url,"kpi/",input_str)
                            }else if(search_type=="kpi_groups"){
                              url<-paste(url,"kpi_groups",input_str)
                            }else if(search_type=="municipality/"){
                              url<-paste(url,"municipality/",input_str)
                            }else if(search_type=="municipality_groups"){
                              url<-paste(url,"municipality_groups/",input_str)
                            }else if(search_type=="ou"){
                              url<-paste(url,"ou/",input_str)
                            }else{
                              return ("error")
                            }
                          },
                          search_data=function(input_kpi,input_municipality,input_year){
                            url<-"http://api.kolada.se/v2/data"
                            if(length(input_kpi)!=0){
                              kpi_str<-paste(as.character(input_kpi),collapse = ",")
                              url<-paste(url,"/kpi/",kpi_str,sep = "")
                            }
                            if(length(input_municipality)!=0){
                              municipality_str<-paste(as.character(input_municipality),collapse = ",")
                              url<-paste(url,"/municipality/",municipality_str,sep="")
                            }
                            if(length(input_year)){
                              year_str<-paste(as.character(input_year),collapse = ",")
                              url<-paste(url,"/year/",year_str,sep="")
                            }
                            d<-fromJSON(url)
                            return(as.data.frame(d))
                          },
                          search_ou=function(input_kpi,input_ou,input_year){
                            url<-"http://api.kolada.se/v2/oudata"
                            if(length(input_kpi)!=0){
                              kpi_str<-paste(as.character(input_kpi),collapse = ",")
                              url<-paste(url,"/kpi/",kpi_str,sep = "")
                            }
                            if(length(input_ou)!=0){
                              ou_str<-paste(as.character(input_ou),collapse = ",")
                              url<-paste(url,"/ou/",ou_str,sep="")
                            }
                            if(length(input_year)){
                              year_str<-paste(as.character(input_year),collapse = ",")
                              url<-paste(url,"/year/",year_str,sep="")
                            }
                            d<-fromJSON(url)
                            return(as.data.frame(d))
                          }
                        ) )

k1<-kolada_api$new()
k1$search_data(input_kpi=c(),input_municipality=c(1860),input_year=c(2009,2010))
k1$search_with_title("municipality","lund")

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