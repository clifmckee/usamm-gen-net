# Non-default packages that must be downloaded from GitHub
#devtools::install_github('rstudio/shinyapps')
#devtools::install_github('rstudio/DT')

# Load required packages
library(shiny)
library(datasets)
library(ggplot2)
library(mapproj)
library(devtools)
library(DT)
#library(shinyapps)
library(RColorBrewer)
library(gridExtra)

# Load required data
load("USAMM_shiny.RData")
head(county.info)

# Start server based on inputs from app
shinyServer(function(input, output) {  
  
  # Begin reactive plot code
  output$shipment.map <- renderPlot({
    
    # Chooses between separate generated networks
    datasetInput <- reactive({
      switch(input$gen.net,
             "network 1" = Net1,
             "network 2" = Net2,
             "network 3" = Net3,
             "network 4" = Net4,
             "all 1000 networks (mean)"= Net.all
      )})
    gen.dat = datasetInput()
    # Removes first row and renames rows with FIPS codes
    gen.dat = gen.dat[,-1,]
    rownames(gen.dat)=colnames(gen.dat)
    
    # Transposes matrices if choosing in-shipments or out-shipments
    if(input$destination=="out"){gen.dat = gen.dat}
    if(input$destination=="in"){gen.dat = as.data.frame(t(gen.dat))}
    
    # Selects unique FIPS codes if choosing states vs. counties
    st = unique(county.info$region[county.info$FIPS == input$fips.codes])
    if(input$regtype=="state"){fips = unique(county.info$FIPS[county.info$region == st])}
    if(input$regtype=="county"){fips = input$fips.codes}

    # Take all of the shipments for the focal county/state and sum them
    shipments.of.interest = gen.dat[colnames(gen.dat) %in% as.character(fips),]
    shipments.of.interest = shipments.of.interest[,!(colnames(shipments.of.interest) %in% as.character(fips))]
    total = apply(shipments.of.interest, 2, sum)
    # For the 1000 networks, calculate the mean number of shipments
    if(input$gen.net=="all 1000 networks (mean)"){total2=total/1000}

    total.prob = as.data.frame(total / sum(total))
    total.prob = total.prob/sum(total.prob)
    colnames(total.prob) = "Proportion.shipments"
    if(input$gen.net=="all 1000 networks (mean)"){total.prob$Total.shipments = total2}
    else{total.prob$Total.shipments = total}
    total.prob$FIPS = rownames(total.prob)
    Total.sum = sum(total.prob$Total.shipments)
    if(input$gen.net=="all 1000 networks (mean)"){output$Total.sum<-renderText(
      paste("Total mean projected shipments =",as.character(Total.sum)))}
    else{output$Total.sum<-renderText(
      paste("Total projected shipments =",as.character(Total.sum)))}
    
    data.to.table = merge(county.codes, total.prob, by="FIPS", all=TRUE)
    
    if(input$gen.net=="all 1000 networks (mean)"){
      # Repeat data summary for minimum shipments
      min.dat = MIN.dat[,-1,]
      rownames(min.dat)=colnames(min.dat)
      if(input$destination=="out"){min.dat = min.dat}
      if(input$destination=="in"){min.dat = as.data.frame(t(min.dat))}
      min.out = min.dat[colnames(min.dat) %in% as.character(fips),]
      min.out = min.out[,!(colnames(min.out) %in% as.character(fips))]
      min.total = as.data.frame(apply(min.out, 2, sum))
      colnames(min.total) = "Min.shipments"
      min.total$FIPS = rownames(min.total)
      min.table = merge(county.codes, min.total, by="FIPS", all=TRUE)
      data.to.table$Min.shipments = min.table$Min.shipments
      
      # Repeat data summary for maximum shipments
      max.dat = MAX.dat[,-1,]
      rownames(max.dat)=colnames(max.dat)
      if(input$destination=="out"){max.dat = max.dat}
      if(input$destination=="in"){max.dat = as.data.frame(t(max.dat))}
      max.out = max.dat[colnames(max.dat) %in% as.character(fips),]
      max.out = max.out[,!(colnames(max.out) %in% as.character(fips))]
      max.total = as.data.frame(apply(max.out, 2, sum))
      colnames(max.total) = "Max.shipments"
      max.total$FIPS = rownames(max.total)
      max.table = merge(county.codes, max.total, by="FIPS", all=TRUE)
      data.to.table$Max.shipments = max.table$Max.shipments
    }
    
    data.to.table = data.to.table[order(-data.to.table$Total.shipments),]
    data.to.table = data.to.table[,-2]
    
    data.to.plot = merge(county.info, total.prob, by="FIPS", all=TRUE)
    data.to.plot = data.to.plot[order(data.to.plot$order),]
       
diff.val = unique(total.prob$Proportion.shipments)
diff.val = diff.val[order(diff.val)]
bks = as.data.frame(c(-1,0,
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.25),     
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.5),
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.75),
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.9),
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.95),
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.99),
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.999),
        quantile(total.prob$Proportion.shipments[total.prob$Proportion.shipments > 0],0.9999),
        max(diff.val)))
colnames(bks)<-"pct"
rownames(bks)<-c("NA","0","25","50","75","90","95","99","99.9","99.99","100")
bks = unique(bks)
data.to.plot$Bin = cut(data.to.plot$Proportion.shipments, bks$pct, labels=FALSE)

data.to.table$Bin = cut(data.to.table$Proportion.shipments, bks$pct, labels=FALSE)
bin <- (unique(data.to.table$Bin))
breaker=NULL
for(i in 1:(length(bin)-1)){
    breaker[i]=rownames(bks)[bin[i]]
  }
data.to.table$Proportion.shipments<-format(data.to.table$Proportion.shipments, scientific=TRUE)

if(input$gen.net=="all 1000 networks (mean)"){colnames(data.to.table)[4:7]<-
                                                   c("Proportion.shipments","Mean.shipments",
                                                     "Minimum.shipments","Maximum.shipments");
                                              data.to.table<-data.to.table[c(2,3,1,5,6,7,4,8)];
                                              data.to.table<-data.to.table[-8]}
else{colnames(data.to.table)[4:5]<-c("Proportion.shipments","Shipments");
                                              data.to.table<-data.to.table[c(2,3,1,5,4,6)];
                                              data.to.table<-data.to.table[-6]}

output$shipment.table <- renderDataTable(data.to.table,filter = 'bottom',
                                        options = list(
                                          lengthMenu = list(c(10,20,50,100,-1),
                                                            c('10','20','50','100','All')),
                                          pageLength = 20,
                                          autoWidth = TRUE
                                        ))
output$downloadTable <- downloadHandler(
  filename = function() { 
    paste(input$gen.net,'_',input$destination,'_',
          input$regtype,'_',input$fips.codes,'.csv',sep='') 
  },
  content = function(file) {
    write.csv(data.to.table,file)
  }
)

p <- ggplot(data.to.plot,aes(x=long,y=lat,group=group)) + 
      geom_polygon(colour="black",size=.15) +
        aes(fill=Bin)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(), legend.position="right")+
      theme(plot.title = element_text(lineheight=.5,face="bold",size=16))+
      coord_map("polyconic")+
      scale_fill_gradient(low="grey60",high=brewer.pal(9,"Reds"),breaks=bin,
                          name="Bin", na.value="black") +
      guides(fill = guide_legend(
        title.theme = element_text(size=20,angle=0),
        label.theme = element_text(size=16,angle=0),
        label.position="right"))

labs = cbind(paste(1:10),c("no shipments","25th percentile","50th percentile",
                                      "75th percentile","90th percentile","95th percentile",
                                      "99th percentile","99.9th percentile","99.99th percentile",
                                      "100th percentile"))
colnames(labs)<-c("Bin","Percentile")
g = tableGrob(labs)
grid.arrange(p, g, nrow=1, widths=c(3,1))
})
})
















