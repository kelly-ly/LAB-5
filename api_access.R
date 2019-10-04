kolada_api<-setRefClass("kolada_api",fields =list(muni="data.frame",kpi="data.frame") 
                        ,methods =list(
                          initialize=function(municipality,kpi){
                            library(jsonlite)
                            muni<<-get_all_municipality()
                            kpi<<-get_all_KPI()
                          },
                          retreive_data=function(url){
                            paging_data<-fromJSON(url)
                            data<-paging_data$value
                            while(TRUE){
                              if(paging_data['next']%in%paging_data){
                                #url<-URLencode(paging_data['next'])
                                url<-paging_data['next']
                                url<-gsub(' ','%20',x=url)
                                #url<-URLencode(url)
                                paging_data<-fromJSON(url[[1]])
                                data<-rbind(data,paging_data$value)
                              }else{
                                return(data)
                                break
                              }
                            }
                          },
                          get_all_municipality=function(){
                            url<-"http://api.kolada.se/v1/municipality"
                            d<-retreive_data(url)
                            return(as.data.frame(d))
                          },
                          get_all_KPI=function(){
                            url<-"http://api.kolada.se/v1/kpi/"
                            d<-retreive_data(url)
                            return (as.data.frame(d))
                          },
                          search_area=function(area_name){
                            search_area_url<-"http://api.kolada.se/v1/municipality/"
                            #area_name<-gsub(pattern = " ", replacement = "%20", x = area_name)
                            search_area_url<-paste(search_area_url,area_name,sep="")
                            search_area_url<-URLencode(search_area_url)
                            print(search_area_url)
                            d<-retreive_data(search_area_url)
                            return(as.data.frame(d))
                          },
                          search_KPI=function(KPI_str){
                            search_KPI_url<-"http://api.kolada.se/v1/kpi/"
                            search_KPI_url<-paste(search_KPI_url,KPI_str,sep="")
                            search_KPI_url<-URLencode(search_KPI_url)
                            d<-retreive_data(search_KPI_url)
                            return(as.data.frame(d))
                          },
                          search_enheter=function(id_str){
                            url<-"http://api.kolada.se/v1/ou/"
                            
                            search_enheter_url<-paste(url,id_str,sep="")
                            d<-retreive_data(search_enheter_url)
                            return(d)
                          },
                          get_data=function(input_str){
                            url<-"http://api.kolada.se/v1/data/"
                            input<-(unlist(strsplit(input_str,split=" ")))
                            if(input[1]=="exact"){
                              input_kpi<-input[2]
                              input_municipality<-input[3]
                              input_period<-input[4]
                              url<-paste(url,"exact/",input_kpi,"/",input_municipality,"/",input_period,sep="")
                              d<-retreive_data(url)
                              return (d)
                            }else if(input[1]=="peryear"||input[1]=="permunicipality"){
                              input_kpi<-input[2]
                              input_needle<-input[3]
                              url<-paste(url,input[1],"/",input_kpi,"/",input_needle,sep="")
                              d<-retreive_data(url)
                              return (d)
                            }else{
                              return ("error input")
                            }
                          },
                          get_enhets=function(input_str){
                            url<-"http://api.kolada.se/v1/ou/data/"
                            input<-(unlist(strsplit(input_str,split=" ")))
                            if(input[1]=="exact"){
                              input_kpi<-input[2]
                              input_ou<-input[3]
                              input_period<-input[4]
                              url<-paste(url,"exact/",input_kpi,"/",input_ou,"/",input_period,sep="")
                              d<-retreive_data(url)
                              return (d)
                            }else if(input[1]=="peryear"||input[1]=="perou"){
                              input_kpi<-input[2]
                              input_needle<-input[3]
                              url<-paste(url,input[1],"/",input_kpi,"/",input_needle,sep="")
                              d<-retreive_data(url)
                              return (d)
                            }else{
                              return ("error input")
                            }
                          }
                        ) )

k1<-kolada_api$new()
k1$search_area("stockholm")
k1$muni
