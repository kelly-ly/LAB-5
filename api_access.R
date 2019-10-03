library(jsonlite)

retreive_data<-function(url){
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
}
# get municipality
get_all_municipality<-function(){
  url<-"http://api.kolada.se/v1/municipality"
  d<-retreive_data(url)
  print(d)
}
#get_all_municipality()


# search area
search_area<-function(){
  search_area_url<-"http://api.kolada.se/v1/municipality/"
  area_name=readline("Input area name:")
  #area_name<-gsub(pattern = " ", replacement = "%20", x = area_name)
  search_area_url<-paste(search_area_url,area_name,sep="")
  search_area_url<-URLencode(search_area_url)
  print(search_area_url)
  d<-retreive_data(search_area_url)
  print(d)
}
search_area()

# get KPI 
get_all_KPI<-function(){
  url<-"http://api.kolada.se/v1/kpi/"
  d<-retreive_data(url)
  print(d)
}
get_all_KPI()

search_KPI<-function(){
  search_KPI_url<-"http://api.kolada.se/v1/kpi/"
  KPI_str<-readline("Input KPI str:")
  search_KPI_url<-paste(search_KPI_url,KPI_str,sep="")
  search_KPI_url<-URLencode(search_KPI_url)
  d<-retreive_data(search_KPI_url)
  print(d)
}
search_KPI()

# search Enheter
search_enheter<-function(){
  url<-"http://api.kolada.se/v1/ou/"
  id_str<-readline("Input ID:")
  search_enheter_url<-paste(url,id_str,sep="")
  d<-retreive_data(search_enheter_url)
  print(d)
}
search_enheter()

get_data<-function(){
  url<-"http://api.kolada.se/v1/data/"
  input_str<-readline("Input search data:")
  input<-(unlist(strsplit(input_str,split=" ")))
  if(input[1]=="exact"){
    kpi<-input[2]
    municipality<-input[3]
    period<-input[4]
    url<-paste(url,"exact/",kpi,"/",municipality,"/",period,sep="")
    d<-retreive_data(url)
    return (d)
  }else if(input[1]=="peryear"||input[1]=="permunicipality"){
    kpi<-input[2]
    needle<-input[3]
    url<-paste(url,input[1],"/",kpi,"/",needle,sep="")
    d<-retreive_data(url)
    return (d)
  }else{
    return ("error input")
  }
}
#get_data()

# Enhetsdata
get_enhets<-function(){
  url<-"http://api.kolada.se/v1/ou/data/"
  input_str<-readline("Input search enhets data:")
  input<-(unlist(strsplit(input_str,split=" ")))
  if(input[1]=="exact"){
    kpi<-input[2]
    ou<-input[3]
    period<-input[4]
    url<-paste(url,"exact/",kpi,"/",ou,"/",period,sep="")
    d<-retreive_data(url)
    return (d)
  }else if(input[1]=="peryear"||input[1]=="perou"){
    kpi<-input[2]
    needle<-input[3]
    url<-paste(url,input[1],"/",kpi,"/",needle,sep="")
    d<-retreive_data(url)
    return (d)
  }else{
    return ("error input")
  }
}
get_enhets()