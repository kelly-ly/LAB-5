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
  return(d)
}
#get_all_municipality()


# search area

search_area<-function(area_name){
  search_area_url<-"http://api.kolada.se/v1/municipality/"
  #area_name<-gsub(pattern = " ", replacement = "%20", x = area_name)
  search_area_url<-paste(search_area_url,area_name,sep="")
  search_area_url<-URLencode(search_area_url)
  print(search_area_url)
  d<-retreive_data(search_area_url)
  return(d)
}
area_name=readline("Input area name:")
search_area(area_name)

# get KPI 
get_all_KPI<-function(){
  url<-"http://api.kolada.se/v1/kpi/"
  d<-retreive_data(url)
  return (d)
}
get_all_KPI()


search_KPI<-function(KPI_str){
  search_KPI_url<-"http://api.kolada.se/v1/kpi/"
  search_KPI_url<-paste(search_KPI_url,KPI_str,sep="")
  search_KPI_url<-URLencode(search_KPI_url)
  d<-retreive_data(search_KPI_url)
  return(d)
}
KPI_str<-readline("Input KPI str:")
search_KPI(KPI_str)

# search Enheter

search_enheter<-function(id_str){
  url<-"http://api.kolada.se/v1/ou/"
  
  search_enheter_url<-paste(url,id_str,sep="")
  d<-retreive_data(search_enheter_url)
  return(d)
}
id_str<-readline("Input ID:")
search_enheter(id_str)


get_data<-function(input_str){
  url<-"http://api.kolada.se/v1/data/"
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
input_str<-readline("Input search data:")
get_data(input_str)

# Enhetsdata
get_enhets<-function(input_str){
  url<-"http://api.kolada.se/v1/ou/data/"
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
input_str<-readline("Input search enhets data:")
get_enhets(input_str)