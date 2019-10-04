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
                              return ("error")
                            }
                            url<-URLencode(url)
                            d<-fromJSON(url)
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
                            url<-"http://api.kolada.se/v2/data"
                            if(nchar(input_kpi)!=0){
                              #kpi_str<-paste(as.character(input_kpi),collapse = ",")
                              url<-paste(url,"/kpi/",input_kpi,sep = "")
                            }
                            if(nchar(input_municipality)!=0){
                              print(input_municipality)
                              print(length(input_municipality))
                              #municipality_str<-paste(as.character(input_municipality),collapse = ",")
                              url<-paste(url,"/municipality/",input_municipality,sep="")
                            }
                            if(nchar(input_year)!=0){
                              print(nchar(input_year))
                              print(input_year)
                              #year_str<-paste(as.character(input_year),collapse = ",")
                              url<-paste(url,"/year/",input_year,sep="")
                            }
                            print(url)
                            d<-fromJSON(url)
                            
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
#d<-k1$search_with_title("kpi_groups","kostnad")
#f<-k1$search_with_title("municipality_groups","stockholm")
#e<-k1$search_with_id("municipality_groups","G124026")
#g<-k1$search_data(input_kpi=c(),input_municipality=c(1860),input_year=c(2009,2010))
#j<-k1$search_data(input_kpi="N00945",input_municipality = "",input_year="2009,2007")
k<-k1$search_data(input_kpi="N00945",input_municipality = "1860",input_year="")
h<-k1$search_ou(input_kpi = "N15033,N15030",input_ou = "V15E144001301,V15E144001101",input_year = "2009,2008,2007")
k1$search_with_title("municipality","lund")

url<-"http://api.kolada.se/v2/kpi_groups?title=kostnad"
test<-fromJSON(url)
test<-as.data.frame(test)

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