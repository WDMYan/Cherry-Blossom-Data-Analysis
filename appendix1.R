
setwd("/Users/jianshizhang/stat242_2015/Assignment1/data/")
files = list.files()

#functions to get year
getYear = function(filename){
  strsplit(filename,'10Mile_')[[1]][2]  
}
sapply(files, getYear)

#functions to get gender
getGender = function(filename){
  strsplit(filename, '10Mile_')[[1]][1]
}

#Read the file
readFile = function(filename){
  con = file(filename, open = "rt")
  text = readLines(con) 
  text = gsub("[\u00A0]", " ", text)
  close(con)
  text
}


#remove the table name and other information before the data frame starts
getBody = function(filename){
  con = readFile(filename)
  if( length(which(con=="")) != 0){
    con = con[-which(con=="")] 
  }
  location1 = grep('===', con, useBytes=TRUE)
  location2 = grep('^#|^ #', con, useBytes = TRUE)
  
  #deal with file without comments
  if(length(location2)==0){
    location2 = length(con)+1
  }
  
  
  #deal with file without head
  if(length(location1)==0){
    if(grepl("^w", filename)){
      filename1 = sub("women","men",filename)
    }
    else{
      filename1 = sub("men","women",filename)
    }
    con1=readFile(filename1)
    location.1 = grep('===', con1, useBytes=TRUE)
    add.text = con1[(location.1-1):location.1]
    location.2 = grep('1 ',con, useBytes = T)[1]
    Body = add.text
    Body[3:(location2+2-location.2)] = con[location.2:(location2-1)]
  }
  
  #normal situation
  if(length(location1)!=0){
    Body = con[(location1-1):(location2-1)] 
  }
  
  #to deal with men10Mile_2008
  Body = Body[!grepl("/./",Body)]
  
  ################deal with the Body we already get to easier our "grab"
  
  #uniform the signal line and variable name
  Body[1] = tolower(Body[1])
  
  #deal with "  "
  Body[2] = gsub("  ","= ",Body[2])
  
  #deal with"=========="
  position = gregexpr("net", Body[1])[[1]]
  signal.c = substr(Body[2], position-4, position-1)
  if(signal.c == "===="){
    replace.1 = substr(Body[2], 1, position-2)
    replace.2 = " "
    replace.3 = substr(Body[2],position,nchar(Body[2]))
    replace = paste0(replace.1,replace.2,replace.3)
    Body[2] = replace
  }
  #ending " " to add a = and replace the last " "to = 
  Body[2] = paste0(substr(Body[2], 1, nchar(Body[2])-1),"==") 
  
  #remove the space and vacant lines
  #grep("^\\s+$", Body)
  Body = Body[!grepl("^\\s+$|''", Body)]
  Body
}
a = sapply(files, getBody)

#dealing with the colnames
#getColnames = function(filename){
  #temp = getBody(filename)
  #cont = temp[1]
  #z = unlist(strsplit(cont,' '))
  #z = z[z != '']
#}

#split the Body and build the data frame
splitBody = function(filename){
  temp = getBody(filename)
  signal = temp[2]
  context = temp[1]
  index = gregexpr(' ', signal)[[1]]
  index.start = c(1, index+1)
  index.end = c(index, 1000000L)
  z = substring(context, first = index.start, last = index.end)
  var.name = gsub("\\s+","",z)
  var.name[var.name %in% c("guntim", "gun","time")] = "guntime"
  var.name[var.name %in% c("nettim", "net")] = "nettime"
  var.name[var.name %in% c("ag")] = "age"
  var.name[var.name %in% c("5mi","5mile")] = "5mi"
  Body = matrix(rep(NA, (length(index)+1)*(length(temp)-2)), 
                nrow = length(temp)-2)
  colnames(Body) = var.name
  
  #deal with pace name
  colnames(Body)[which(colnames(Body)=="pace")][-sum(colnames(Body)=="pace")] = 
    paste0(rep("pace",n = sum(colnames(Body)=="pace")-1),seq(1:c(sum(colnames(Body)=="pace")-1)))
 
  for(i in 3:length(temp)){
  Body[i-2, ]= substring(temp[i], first = index.start, last = index.end)
  }
   
  #get rid of spaces
  Body = gsub("^\\s+|\\s+$","",Body)
  
  #divided the div/tot
  temptext = Body[,which(colnames(Body)=='div/tot')]
  Body[,which(colnames(Body)=='div/tot')][temptext==""] = "/"
  text = read.table(text = Body[,which(colnames(Body)=='div/tot')],
                    sep = "/", col.names = c('div','total'),fill = T)
  
  
  #creat a new variable named identity
  if(sum(colnames(Body)=='nettime')==0){
    te = Body[,which(colnames(Body)=='guntime')]
    ind = grepl("\\#|\\*", te)
    identity = vector(length = length(ind))
    identity[ind==T] = substr(te, nchar(te),nchar(te))[ind==T]
    identity[ind!=T] = ""
    Body[,which(colnames(Body)=='guntime')] = 
      gsub("\\#|\\*", "",  Body[,which(colnames(Body)=='guntime')]) 
  }
  if(sum(colnames(Body)=='nettime')!=0){
    te = Body[,which(colnames(Body)=='guntime')]
    tex = Body[,which(colnames(Body)=='nettime')]
    ind = grepl("\\#|\\*", te)
    ind.1 = grepl("\\#|\\*", tex)
    identity = vector(length = length(ind))
    identity[ind==T] = substr(te, nchar(te),nchar(te))[ind==T]
    identity[ind.1==T] = substr(tex, nchar(tex),nchar(tex))[ind.1==T]
    identity[ind!=T & ind!=T] = ""
    Body[,which(colnames(Body)=='guntime')] = gsub("\\#|\\*", "",  Body[,which(colnames(Body)=='guntime')])
    Body[,which(colnames(Body)=='nettime')] = gsub("\\#|\\*", "",  Body[,which(colnames(Body)=='nettime')])
  }
  
  Body = as.data.frame(Body)
  if(dim(text)[1]!=0){
    dat = cbind(Body[,which(colnames(Body)!='div/tot')], text, identity)
  }   
  if(dim(text)[1]==0){
    dat = cbind(Body[,which(colnames(Body)!='div/tot')], identity)
  }
  
  dat[dat==""]  = NA
  dat
}

#methond from http://www.macalester.edu/~kaplan/startingwithr/panel-data.pdf
to.secs = function(time){
  secs = rep(0,length(time))
  if(length(time)!=0){
    time = as.character(time)
    for (k in 1:length(time)) {
      s = strsplit(time[k], ":")[[1]]
      s = as.numeric(s)
      if(length(s)==3){
        secs[k] = s[1]*3600 + s[2]*60 + s[3]
      }   
      else{
        secs[k] = s[1]*60 + s[2]
      }  
  }
  time = secs
}
  time
}

#if a person is from USA, get the state where he from
getState = function(hometown){
   temp = strsplit(as.character(hometown),' ')
   state = rep(NA, length(hometown))
   for(i in 1:length(temp)){
     tmp[i] = temp[[i]][length(temp[[i]])]
     states = c(state.abb,"DC")
     index = match(toupper(tmp[i]),states)
     if(!is.na(index)){
       state[i] = states[index]
     }  
   }
   state
}



#
buildData = function(filename){
  dat = splitBody(filename)
  ###variables we care about
  #add year and gender
  year = rep(getYear(filename),dim(dat)[1])
  gender = rep(getGender(filename), dim(dat)[1])
  dat$year = as.integer(year)
  dat$gender = as.factor(gender)
  dat$state = getState(dat$hometown)
  #transform the time (only includes what we care about for analysis)
  dat[,which(colnames(dat)=='guntime')] = to.secs(dat[,which(colnames(dat)=='guntime')])
  dat[,which(colnames(dat)=='nettime')] = to.secs(dat[,which(colnames(dat)=='nettime')])
  dat[,which(colnames(dat)=='pace')] = to.secs(dat[,which(colnames(dat)=='pace')])
  dat  
}

#set the class of data and merge it into one, drop variables that we are not interested in.
ss = sapply(files,buildData)
cherryrun = Reduce(function(x, y) merge(x, y, all=TRUE), ss)
tttt = Reduce(function(x, y) merge(x, y, all=TRUE), ss)
cherryrun = tttt
variable.care = c("year","gender","place","num","name","age","hometown","guntime","nettime","identity","div","total","pace","state")
cherryrun = cherryrun[,variable.care]
cherryrun$place = as.integer(as.character(cherryrun$place))
cherryrun$year = as.factor(cherryrun$year)
cherryrun$age = as.integer(as.character(cherryrun$age))
cherryrun$num = as.integer(as.character(cherryrun$num))
cherryrun$name = as.character(cherryrun$name)
cherryrun$hometown = as.character(cherryrun$hometown)
cherryrun$state = as.factor(cherryrun$state)
cherryrun = cherryrun[order(cherryrun$year,cherryrun$gender,cherryrun$place),]
setwd("..")
write.table(cherryrun, "cherryrun.txt",sep="\t")







