# Titanic

################################################################################
#   A. PARAMETER                                                               #
################################################################################

# WD
setwd("C:/Users/sambo/Dropbox/R/R Projekte/kaggle/titanic")
# Packages laden
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(caret)
library(mice)
library(FSelector)

################################################################################
#   B. FUNKTIONEN                                                              #
################################################################################

plot_bar <- function(data,x,val="abs",facet=NA,fill=NA,
                      err.col="#404040", fill.col=c("#00CDCD","#666666")){

    if (fill.col[1] == "set1") fill.col <- c("#FFD700","#666666") # gelb
    if (fill.col[1] == "set2") ill.col <- c("#00CDCD","#666666") # grün
    if (fill.col[1] == "set3") ill.col <- c("#404040","#666666") # grau
    if (fill.col[1] == "set4") ill.col <- c("#FFC0CB","#666666") # rosa
    
    if (is.na(fill)) fill <- x
    if (val=="abs") {
        if (is.na(facet)) {
            title_label <- paste(fill,": nach ",x,sep="")
            out <- data %>%
                dplyr::mutate(dmmy=as.numeric(levels(Survived))[Survived]) %>%
                # dplyr::mutate(y=mean_se(dmmy)$y,
                #               ymin=mean_se(dmmy)$ymin,
                #               ymax=mean_se(dmmy)$ymax) %>%
                dplyr::mutate(sur_ges=sum(dmmy)) %>%
                dplyr::group_by_(fill,x) %>%
                dplyr::summarize(sur_ges=unique(sur_ges),
                                 #y=unique(y),
                                 #ymin=unique(ymin),
                                 #ymax=unique(ymax),
                                 cnt=n()) %>%
                dplyr::group_by_(x) %>%
                dplyr::ungroup() %>%
                ggplot2::ggplot(aes_string(x=x,y="cnt",fill=fill)) +
                ggplot2::geom_bar(stat="identity") +
                # ggplot2::geom_errorbar(aes(ymin=y,
                #                            ymax=y),
                #                        color=error.col) +
                ggplot2::theme_bw() +
                #ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_fill_manual(values=fill.col) +
                ggplot2::ggtitle(title_label) +
                ggplot2::labs(x=x,y="") +
                ggplot2::theme(plot.title=element_text(color="#666666",face="bold",
                                                       size=18,hjust=0.5),
                               axis.text=element_text(color="#666666"),
                               axis.title=element_text(color="#666666"))
        } else {
            title_label <- paste(fill,": nach ",x," und ",facet,sep="")
            out <- data %>%
                dplyr::mutate_(fac=facet) %>%
                dplyr::mutate(fac=as.character(fac)) %>%
                dplyr::bind_rows(data %>% 
                                     dplyr::mutate_(fac=facet) %>%
                                     dplyr::mutate(fac=as.character(fac)) %>%
                                     dplyr::mutate(fac="gesamt")) %>%
                dplyr::group_by(fac) %>%
                dplyr::mutate(dmmy=as.numeric(levels(Survived))[Survived]) %>%
                # dplyr::mutate(y=mean_se(dmmy)$y,
                #               ymin=mean_se(dmmy)$ymin,
                #               ymax=mean_se(dmmy)$ymax) %>%
                dplyr::mutate(sur_ges=sum(dmmy)) %>%
                dplyr::group_by_(fill,x,"fac") %>%
                dplyr::summarize(sur_ges=unique(sur_ges),
                                 #y=unique(y),
                                 #ymin=unique(ymin),
                                 #ymax=unique(ymax),
                                 cnt=n()) %>%
                dplyr::group_by_(x,"fac") %>%
                #dplyr::mutate(pct=cnt/sum(cnt)) %>%
                dplyr::ungroup() %>%
                ggplot2::ggplot(aes_string(x=x,y="cnt",fill=fill)) +
                ggplot2::geom_bar(stat="identity") +
                # ggplot2::geom_errorbar(aes(ymin=y,
                #                            ymax=y),
                #                        color=err.col) +
                ggplot2::facet_grid(~fac,scale="free",margins=F) +
                ggplot2::theme_bw() +
                #ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_fill_manual(values=fill.col) +
                ggplot2::ggtitle(title_label) +
                ggplot2::labs(x=x,y="") +
                ggplot2::theme(plot.title=element_text(color="#666666",face="bold",
                                                       size=18,hjust=0.5),
                               axis.text=element_text(color="#666666"),
                               axis.title=element_text(color="#666666"))
        }
    } else {
        if (is.na(facet)) {
            title_label <- paste(fill,": nach ",x,sep="")
            out <- data %>%
                dplyr::mutate(dmmy=as.numeric(levels(Survived))[Survived]) %>%
                dplyr::mutate(y=mean_se(dmmy)$y,
                              ymin=mean_se(dmmy)$ymin,
                              ymax=mean_se(dmmy)$ymax) %>%
                dplyr::mutate(sur_pct_ges=sum(dmmy)/nrow(data)) %>%
                dplyr::group_by_(fill,x) %>%
                dplyr::summarize(sur_pct_ges=unique(sur_pct_ges),
                                 y=unique(y),
                                 ymin=unique(ymin),
                                 ymax=unique(ymax),
                                 cnt=n()) %>%
                dplyr::group_by_(x) %>%
                dplyr::mutate(pct=cnt/sum(cnt)) %>%
                dplyr::ungroup() %>%
                ggplot2::ggplot(aes_string(x=x,y="pct",fill=fill)) +
                ggplot2::geom_bar(stat="identity") +
                ggplot2::geom_errorbar(aes(ymin=y,
                                           ymax=y),
                                       color=error.col) +
                ggplot2::theme_bw() +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_fill_manual(values=fill.col) +
                ggplot2::ggtitle(title_label) +
                ggplot2::labs(x=x,y="") +
                ggplot2::theme(plot.title=element_text(color="#666666",face="bold",
                                                       size=18,hjust=0.5),
                               axis.text=element_text(color="#666666"),
                               axis.title=element_text(color="#666666"))
        } else {
            title_label <- paste(fill,": nach ",x," und ",facet,sep="")
            out <- data %>%
                dplyr::mutate_(fac=facet) %>%
                dplyr::mutate(fac=as.character(fac)) %>%
                dplyr::bind_rows(data %>% 
                                     dplyr::mutate_(fac=facet) %>%
                                     dplyr::mutate(fac=as.character(fac)) %>%
                                     dplyr::mutate(fac="gesamt")) %>%
                dplyr::group_by(fac) %>%
                dplyr::mutate(dmmy=as.numeric(levels(Survived))[Survived]) %>%
                dplyr::mutate(y=mean_se(dmmy)$y,
                              ymin=mean_se(dmmy)$ymin,
                              ymax=mean_se(dmmy)$ymax) %>%
                dplyr::mutate(sur_pct_ges=sum(dmmy)/nrow(data)) %>%
                dplyr::group_by_(fill,x,"fac") %>%
                dplyr::summarize(sur_pct_ges=unique(sur_pct_ges),
                                 y=unique(y),
                                 ymin=unique(ymin),
                                 ymax=unique(ymax),
                                 cnt=n()) %>%
                dplyr::group_by_(x,"fac") %>%
                dplyr::mutate(pct=cnt/sum(cnt)) %>%
                dplyr::ungroup() %>%
                ggplot2::ggplot(aes_string(x=x,y="pct",fill=fill)) +
                ggplot2::geom_bar(stat="identity") +
                ggplot2::geom_errorbar(aes(ymin=y,
                                           ymax=y),
                                       color=err.col) +
                ggplot2::facet_grid(~fac,scale="free",margins=F) +
                ggplot2::theme_bw() +
                ggplot2::scale_y_continuous(labels = scales::percent) +
                ggplot2::scale_fill_manual(values=fill.col) +
                ggplot2::ggtitle(title_label) +
                ggplot2::labs(x=x,y="") +
                ggplot2::theme(plot.title=element_text(color="#666666",face="bold",
                                                       size=18,hjust=0.5),
                               axis.text=element_text(color="#666666"),
                               axis.title=element_text(color="#666666"))
        }
    }
    
    return(out)
    
}

plot_density <- function(data,x,y=NA,fill="Survived",facet="Sex"){
    title_label <- paste("Verteilung:",x)
    data %>%
        dplyr::mutate_(x=x,y=y,fill=fill,facet=facet) %>%
        dplyr::mutate(facet=as.character(facet)) %>%
        dplyr::bind_rows(data %>%
                             dplyr::mutate_(x=x,y=y,fill=fill,facet=facet) %>%
                             dplyr::mutate(facet=as.character(facet)) %>%
                             dplyr::mutate(facet="gesamt")) %>%
        ggplot2::ggplot(aes(x=x,y=y,fill=fill)) +
        #ggplot2::geom_density(fill="#00CDCD",alpha=0.4) +
        ggridges::geom_density_ridges(alpha=0.4) +
        #ggplot2::geom_vline(aes(xintercept=mean(x,na.rm=T)),
        #                    linetype="dashed",
        #                    colour="#666666") +
        #ggplot2::geom_vline(aes(xintercept=median(Age,na.rm=T),colour="blue")) +
        #ggplot2::geom_vline(aes(xintercept=mean(x,na.rm=T)-sd(x,na.rm=T)),
        #                    linetype="dashed",
        #                    colour="#666666") +
        #ggplot2::geom_vline(aes(xintercept=mean(x,na.rm=T)+sd(x,na.rm=T)),
        #                    linetype="dashed",
        #                    colour="#666666") +
        ggplot2::facet_grid(~facet) +
        ggplot2::theme_bw() + 
        #ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::scale_fill_manual(values=c("#FFD700","#666666","#00CDCD",
                                            "#FFC0CB","#404040")) +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x,y="") +
        ggplot2::guides(fill=guide_legend(title=fill)) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",
                                               size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
}

plot_box <- function(data,x,y) {
    data %>% 
        ggplot2::ggplot(aes_string(x=x,y=y)) + 
        ggplot2::geom_boxplot()
}

plot_pyramid <- function(data,x,grp,grp.lvl,facet) {
    t <- data %>%
        dplyr::mutate_(x=x,grp=grp,facet=facet) %>%
        dplyr::mutate(facet=as.character(facet)) %>%
        dplyr::bind_rows(data %>% 
                             dplyr::mutate_(x=x,grp=grp,facet=facet) %>%
                             dplyr::mutate(facet=as.character(facet)) %>%
                             dplyr::mutate(facet="gesamt")) %>%
        dplyr::mutate(x=floor(x)) %>%
        dplyr::count(x,grp,facet) %>%
        dplyr::mutate(n=ifelse(grp==grp.lvl[2],n*-1,n))

    sc_x <- seq(min(t$x),max(t$x),abs(max(t$x)/20))
    sc_y <- seq(max(t$n)*-1,max(t$n),abs(max(t$n)/5))
    title_label <- paste0("Verteilung nach ",x,", ",grp," und ","Survived")
    
    out <- ggplot2::ggplot(data=t,aes(x=x,y=n,fill=as.factor(grp))) +
        ggplot2::geom_bar(data=subset(t,grp==grp.lvl[1]),stat="identity") +
        ggplot2::geom_bar(data=subset(t,grp==grp.lvl[2]),stat="identity") + 
        ggplot2::facet_grid(~facet) +
        ggplot2::scale_y_continuous(breaks = sc_y, 
                           labels = abs(sc_y)) + 
        ggplot2::scale_x_continuous(breaks = sc_x,
                           labels = sc_x) +
        ggplot2::scale_fill_manual(values=c("#C0CBFF","#FFC0CB")) +
        ggplot2::coord_flip() + 
        ggplot2::theme_bw() +
        ggplot2::ggtitle(title_label) +
        ggplot2::labs(x=x,y="") +
        ggplot2::guides(fill=guide_legend(title=grp)) +
        ggplot2::theme(plot.title=element_text(color="#666666",face="bold",
                                               size=18,hjust=0.5),
                       axis.text=element_text(color="#666666"),
                       axis.title=element_text(color="#666666"))
    
    return(out)
}

dat2lu <- function(data,cols,path=getwd(),info=F){
    lu.df <- lapply(cols,function(f){
        if (info) cat("## Generiere Lookup-Wert für",f,"...\n")
        dplyr::data_frame(col=f,val=sort(unique(data[[f]]))) %>%
            dplyr::mutate(id=paste0(substr(f,1,1),row_number()),
                          val=as.character(val))
        }) %>%
        do.call(rbind,.)
    save(lu.df,file=paste0(path,"/dat2lu.RData"))
}

lu2dat <- function(data,cols,path=getwd(),info=F){
    load(file=paste0(path,"/dat2lu.RData"))
    for (x in cols){
        if (info) cat("## Ordne Lookup-Wert zu:",x,"...\n")
        data <- data %>%
            dplyr::mutate_(join_id=x) %>%
            dplyr::mutate(join_id=as.character(join_id)) %>%
            dplyr::left_join(lu.df %>%
                                 dplyr::filter(col==x) %>%
                                 dplyr::select(-col) %>%
                                 magrittr::set_colnames(c("val",paste0(x,"_lu"))),
                             by=c("join_id"="val")) 
        data[[paste0(x,"_lu")]][is.na(data[[paste0(x,"_lu")]])] <- "OTHER"
        
    }
    return(data)
}

gen_feat <- function(data,train=T,pred.col=T){
    #md.pattern(data)
    data <- data %>%
        dplyr::mutate(cabin1_deck=substr(Cabin,1,1)) %>%
        dplyr::mutate(cabin1_deck=ifelse(cabin1_deck=="",NA,cabin1_deck),
                      Embarked=ifelse(Embarked=="",NA,Embarked)) %>%
        dplyr::mutate(cabin1_deck=as.factor(cabin1_deck),
                      Embarked=as.factor(Embarked)) %>%
        dplyr::select(-Name,-Ticket,-Cabin) %>%
        mice(m=1,maxit=50,method="pmm",seed=1312) %>%
        complete() %>%
        left_join(data %>% 
                      dplyr::select(PassengerId,Name,Ticket,Cabin),
                  by=c("PassengerId"="PassengerId"))
    # Neuversuch
    crew <- read.csv("crew.csv",sep=";",header=T,stringsAsFactors=F) %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(fam3=str_match(name,"^\\w*, \\w*\\.? \\w*")[,1]) %>%
        dplyr::filter(!is.na(fam3)) 
    
    data <- data %>%
        # neue Variablen berechnen
        dplyr::mutate(grp_size=SibSp+Parch+1,
                      fare_pp=Fare/(SibSp+Parch+1),
                      cabin_cnt=ifelse(Cabin=="",0,str_count(Cabin," ")+1),
                      name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2],
                      fam_name=str_match(Name,"^\\w*")[,1],
                      #fam3=str_match(Name,"^\\w*, \\w*\\.? \\w*")[,1],
                      age_grp=dplyr::case_when(Age<2 ~ "u2",
                                               Age<6 ~ "u6",
                                               Age<14 ~ "u14",
                                               Age<21 ~ "u21",
                                               Age<31 ~ "u31",
                                               Age<41 ~ "u41",
                                               Age<51 ~ "u51",
                                               TRUE ~ "g51"),
                      fam_status=dplyr::case_when(Age>=18 & Parch>0 & Sex==0 ~ "mother",
                                              Age>=18 & Parch>0 & Sex==1 ~ "father",
                                              Age<=14 & Parch>0 ~ "fam_child",
                                              Age<=14 & Parch==0 ~ "nanny_child",
                                              TRUE ~ "other")) %>%
        # Crew/Passangers
        # dplyr::mutate(position=dplyr::case_when(
        #     fam3 %in% crew$fam3[crew$position=="engineer"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="deck_crew"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Electrician"] ~ "v",
        #     fam3 %in% crew$fam3[crew$position=="fireman"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Greaser"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="guarantee group"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Mess Steward"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="officer"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="orchestra"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Plumber"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="postal clerk"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Restaurant staff"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Trimmer"] ~ "crew",
        #     fam3 %in% crew$fam3[crew$position=="Victaulling crew"] ~ "crew",
        #     TRUE ~ "pas")) %>%
        dplyr::group_by(fam_name) %>%
        dplyr::mutate(fam_size=n()) %>%
        dplyr::group_by(Ticket) %>%
        dplyr::mutate(ticket_cnt=n()) %>%
        dplyr::group_by(Fare) %>%
        dplyr::mutate(fare_cnt=n()) %>%
        dplyr::ungroup() %>%
        # Gruppierungen 
        dplyr::mutate(#fam_cnt_grp=dplyr::case_when(fam_size>1 ~"fg1",
                      #                              TRUE ~ "fg0"),
                      # ticket_cnt_grp=dplyr::case_when(ticket_cnt>1 ~ "t1",
                      #                                 TRUE ~ "t0"),
                      # fare_cnt_grp=dplyr::case_when(fare_cnt>1 ~"f1",
                      #                               TRUE ~"f0"),
                      fare_grp=case_when(fare_pp == 0 ~ "f0",
                                         fare_pp < 5 ~ "f5",
                                         fare_pp < 10 ~ "f10", 
                                         fare_pp > 15 ~ "f15",
                                         fare_pp<25 ~ "f25",
                                         fare_pp < 30 ~ "f30", 
                                         fare_pp< 40 ~ "f40",
                                         fare_pp>=40 ~ "f40+"),
                      cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                            cabin1_deck=="B" ~ "d1",
                                            cabin1_deck=="C" ~ "d2",
                                            cabin1_deck=="D" ~ "d3",
                                            cabin1_deck=="E" ~ "d3",
                                            cabin1_deck=="F" ~ "d3",
                                            cabin1_deck=="G" ~ "d4",
                                            cabin1_deck=="T" ~ "d4",
                                            TRUE ~ "d5"),
                      cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                          cabin_cnt==1 ~ "c1",
                                          TRUE ~ "c1"),
                      name_title=case_when(name_title=="Mr." ~ "t1",
                                           name_title=="Mrs." ~ "t2",
                                           name_title=="Miss." ~ "t3",
                                           #name_title=="Master." ~ "t4",
                                           name_title=="Mme." ~ "t2",
                                           name_title=="Mlle." ~ "t3",
                                           #name_title=="Countess." ~ "t5",
                                           #name_title=="Lady." ~ "t5",
                                           #name_title=="Capt." ~ "t6",
                                           #name_title=="Col." ~ "t6",
                                           #name_title=="Major." ~ "t6",
                                           #name_title=="Rev." ~ "t6",
                                           #name_title=="Dr." ~ "t7",
                                           #name_title=="Don." ~ "t4",
                                           TRUE ~ "t8"),
                      grp_size=case_when(grp_size==1 ~ "g1",
                                         grp_size==2 ~ "g2",
                                         grp_size==3 ~ "g3",
                                         grp_size==4 ~ "g3",
                                         grp_size>4 ~ "g5",
                                         TRUE ~ "g6"))
    
    data[is.na(data)] <- ""
    # Daten als Lookup-Tabelle verwenden
    if (train) dat2lu(data=data,cols=c("Survived","Pclass","Sex","Embarked",
                                       "grp_size","fare_grp","cabin_cnt","age_grp",#"position",
                                       "cabin1_deck","name_title",
                                       "fam_status"))#,"fam_cnt_grp","ticket_cnt_grp",
                                       #"fare_cnt_grp"))
    if (pred.col){
        data <- lu2dat(data=data,cols=c("Survived","Pclass","Sex",#"Embarked",
                                        "grp_size","fare_grp","cabin_cnt","age_grp",#"position",
                                        "cabin1_deck","name_title",
                                        "fam_status")) %>% #,"fam_cnt_grp","ticket_cnt_grp",
                                        #"fare_cnt_grp")) %>%
            dplyr::select(Survived_lu,Pclass_lu,Sex_lu,#Embarked_lu,
                          grp_size_lu,fare_grp_lu,cabin_cnt_lu,age_grp_lu,#position_lu,
                          cabin1_deck_lu,name_title_lu,Age,SibSp,Parch,fare_pp,fam_size,
                          fam_status_lu)#,fam_cnt_grp_lu,ticket_cnt_grp_lu,
                          #fare_cnt_grp_lu)
    } else {
        data <- lu2dat(data=data,cols=c("Pclass","Sex",#"Embarked",
                                        "grp_size","fare_grp","cabin_cnt","age_grp",#"position",
                                        "cabin1_deck","name_title",
                                        "fam_status")) %>% #,"fam_cnt_grp","ticket_cnt_grp",
                                        #"fare_cnt_grp")) %>%
            dplyr::select(Pclass_lu,Sex_lu,#Embarked_lu,
                          grp_size_lu,fare_grp_lu,cabin_cnt_lu,age_grp_lu,#position_lu,
                          cabin1_deck_lu,name_title_lu,Age,SibSp,Parch,fare_pp,fam_size,
                          fam_status_lu)#,fam_cnt_grp_lu,ticket_cnt_grp_lu,
                          #fare_cnt_grp_lu)
    }
    
}

gen_feat2 <- function(data,train=T,pred.col=T){
    # Missings imputieren
    data <- data %>%
        dplyr::mutate(cabin1_deck=substr(Cabin,1,1)) %>%
        dplyr::mutate(cabin1_deck=ifelse(cabin1_deck=="",NA,cabin1_deck),
                      Embarked=ifelse(Embarked=="",NA,Embarked)) %>%
        dplyr::mutate(cabin1_deck=as.factor(cabin1_deck),
                      Embarked=as.factor(Embarked)) %>%
        dplyr::select(-Name,-Ticket,-Cabin) %>%
        mice(m=1,maxit=50,method="pmm",seed=1312) %>%
        complete() %>%
        left_join(data %>% 
                      dplyr::select(PassengerId,Name,Ticket,Cabin),
                  by=c("PassengerId"="PassengerId"))
    
    
    ggplot(data=dmmy %>%
               filter(fare_pp<100),aes(x=fare_pp,fill=Survived,alpha=0.4)) +
        geom_density() + 
        scale_x_continuous(breaks=c(seq(0,100,5),seq(200,500,100)),
                           labels=c(seq(0,100,5),seq(200,500,100))) +
        geom_vline(xintercept=3.5) +
        geom_vline(xintercept=15) +
        geom_vline(xintercept=24) +
        geom_vline(xintercept=40)
    
    x <- table(dmmy$cabin1_deck,dmmy$Survived) 
    y <- data.frame(id=dmmy$cabin1_deck %>% unique() %>% sort)
    y$dead <- x[,1]
    y$sur <- x[,2]
    y$sum <- y$dead+y$sur
    y$pct_dead <- round(y$dead/y$sum*100,2)
    y

    dmmy <- data %>%
        # neue Variablen berechnen
        dplyr::mutate(grp_size=SibSp+Parch+1,
                      fare_pp=Fare/(SibSp+Parch+1),
                      cabin_cnt=ifelse(Cabin=="",0,str_count(Cabin," ")+1),
                      name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2],
                      fam_name=str_match(Name,"^\\w*")[,1],
                      #fam3=str_match(Name,"^\\w*, \\w*\\.? \\w*")[,1],
                      age_grp=dplyr::case_when(Age<15 ~ "u15",
                                               Age<30 ~ "u30",
                                               Age<42 ~ "u42",
                                               TRUE ~ "g42"),
                      fam_status=dplyr::case_when(Age>=18 & Parch>0 & Sex==0 ~ "mother",
                                                  Age>=18 & Parch>0 & Sex==1 ~ "father",
                                                  Age<=14 & Parch>0 ~ "fam_child",
                                                  Age<=14 & Parch==0 ~ "nanny_child",
                                                  TRUE ~ "other")) %>%
        dplyr::group_by(fam_name) %>%
        dplyr::mutate(fam_size=n()) %>%
        dplyr::group_by(Ticket) %>%
        dplyr::mutate(ticket_cnt=n()) %>%
        dplyr::group_by(Fare) %>%
        dplyr::mutate(fare_cnt=n()) %>%
        dplyr::ungroup() %>%
        # Gruppierungen 
        dplyr::mutate(fare_grp=case_when(fare_pp <=3.5 ~ "f3.5",
                               fare_pp <= 15 ~ "f15",
                               TRUE ~ "f15+"),
            cabin1_deck=case_when(cabin1_deck=="A" ~ "d0",
                                  cabin1_deck=="B" ~ "d1",
                                  cabin1_deck=="C" ~ "d1",
                                  cabin1_deck=="D" ~ "d1",
                                  cabin1_deck=="E" ~ "d0",
                                  cabin1_deck=="F" ~ "d0",
                                  cabin1_deck=="G" ~ "d0",
                                  cabin1_deck=="T" ~ "d0",
                                  TRUE ~ "d2"),
            cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                cabin_cnt==1 ~ "c1",
                                TRUE ~ "c1"),
            name_title=case_when(name_title=="Mr." ~ "t1",
                                 name_title=="Mrs." ~ "t2",
                                 name_title=="Miss." ~ "t3",
                                 #name_title=="Master." ~ "t4",
                                 name_title=="Mme." ~ "t2",
                                 name_title=="Mlle." ~ "t3",
                                 #name_title=="Countess." ~ "t5",
                                 #name_title=="Lady." ~ "t5",
                                 #name_title=="Capt." ~ "t6",
                                 #name_title=="Col." ~ "t6",
                                 #name_title=="Major." ~ "t6",
                                 #name_title=="Rev." ~ "t6",
                                 #name_title=="Dr." ~ "t7",
                                 #name_title=="Don." ~ "t4",
                                 TRUE ~ "t8"),
            grp_size=case_when(grp_size==1 ~ "g1",
                               grp_size==2 ~ "g2",
                               grp_size==3 ~ "g3",
                               grp_size==4 ~ "g3",
                               grp_size>4 ~ "g5",
                               TRUE ~ "g6"))
}

#### Temp

dat2lu(data=x,cols=c("Survived","Pclass","Sex","Embarked","grp_size","cabin_cnt",
                     "name_title","fare_grp","cl_age_flag"))
lu2dat(data=x,cols=c("Survived","Pclass","Sex","Embarked","grp_size","cabin_cnt",
                     "name_title","fare_grp","cl_age_flag"))

train.feat <- gen_feat(train)
ctrl <- trainControl(method="boot",number=100,savePredictions="final",
                     classProbs=T)
fit.xgbTree <- caret::train(Survived_lu~.,
                            data=train.feat,
                            method="xgbTree",
                            trControl=ctrl)

out <- lapply(colnames(train.feat), function(f) list(table(train.feat[[f]],train.feat$Survived_lu)))
names(out) <- colnames(train.feat)
out

confusionMatrix(train.feat$Survived_lu,predict(fit.xgbTree,train.feat))

val.feat <- gen_feat(val,train=F)
confusionMatrix(val.feat$Survived_lu,predict(fit.xgbTree,val.feat))

t.data.feat <- gen_feat(t.data)
fit.xgbTree2 <- caret::train(Survived_lu~.,
                            data=t.data.feat,
                            method="xgbTree",
                            trControl=ctrl)
fit.pcaNNet<- caret::train(Survived_lu~.,
                            data=t.data.feat,
                            method="pcaNNet",
                            trControl=ctrl)
fit.svmLinear3 <-  caret::train(Survived_lu~.,
                                data=t.data.feat,
                                method="svmLinear3",
                                trControl=ctrl)
fit.glmStepAIC <- caret::train(Survived_lu~.,
                               data=t.data.feat,
                               method="glmStepAIC",
                               trControl=ctrl)

confusionMatrix(t.data.feat$Survived_lu,predict(fit.xgbTree2,t.data.feat))
confusionMatrix(t.data.feat$Survived_lu,predict(fit.pcaNNet,t.data.feat))
confusionMatrix(t.data.feat$Survived_lu,predict(fit.svmLinear3,t.data.feat))
confusionMatrix(t.data.feat$Survived_lu,predict(fit.glmStepAIC,t.data.feat))

test.feat <- gen_feat(test,train=F,pred.col=F)
pred <- predict(fit.xgbTree2,test.feat)
out <- test %>%
    dplyr::select(PassengerId) %>%
    dplyr::mutate(Survived=pred) %>%
    dplyr::mutate(Survived=ifelse(Survived=="S1",0,1))
write.table(out,file="titanic_submission_xgbTree.csv",row.names=F,sep=",")

# Ensemble
library(caretEnsemble)
ctrl <- trainControl(method="boot",number=100,savePredictions="final",
                     classProbs=T,index=createResample(t.data.feat$Survived_lu,25))
mod.list <- caretList(Survived_lu~.,
                      data=t.data.feat,
                      trControl=ctrl,
                      #methodList=c("xgbTree","svmLinear"))
                      methodList=c("xgbTree","svmLinear"))#,"fda"))#,"pcaNNet"))
xyplot(resamples(mod.list))
modelCor(resamples(mod.list))
confusionMatrix(t.data.feat$Survived_lu,predict(mod.list$svmLinear,t.data.feat))

ens <- caretEnsemble(mod.list,
                     metric="ROC",
                     trControl=ctrl) #trainControl(method="repeatedcv",repeats=5,
#       savePredictions=T,classProbs=T))
summary(ens)

stack.list <- caretStack(mod.list,
                         method="xgbTree",
                         metric="Accurancy",
                         trControl=ctrl)#trainControl(method="repeatedcv",repeats=5,
#        savePredictions=T,classProbs=T))

confusionMatrix(t.data.feat$Survived_lu,predict(ens,t.data.feat))
pred <- predict(stack.list,test.feat)
out <- test %>%
    dplyr::select(PassengerId) %>%
    dplyr::mutate(Survived=pred) %>%
    dplyr::mutate(Survived=ifelse(Survived=="S1",1,0))
write.table(out,file="titanic_submission_stack_xgbTree_svmLin_rev.csv",row.names=F,sep=",")

################################################################################
#   C. DATEN EINLESEN                                                          #
################################################################################

# Variablendefinition
# Pclass: ticket class (1/2/3) - socio-economic status proxy (upper/middle/lower)
# Age: in years (fractional if < 1, xx.5 = estimated age)
# Sibsp: of siblings/spouses aboard the titanic
# Parch: of parents/children aboard the titanic (parent=mother/father, not nanny)
# Ticket: ticket number
# Fare: passenger fare
# Cabin: cabin number
# Embarked: Port of Embarkation (C=Cherbourg, Q=Queenstown, S=Southampton)

t.data <- read.csv("train.csv",sep=",",header=T,stringsAsFactors=F) %>%
    as_data_frame() %>%
    mutate(Survived=as.factor(Survived),
           Pclass=as.numeric(Pclass),
           Sex=as.numeric(ifelse(Sex=="male",1,0)))
test <- read.csv("test.csv",sep=",",header=T,stringsAsFactors=F) %>%
    as_data_frame() %>%
    mutate(Pclass=as.factor(Pclass),
           Sex=as.numeric(ifelse(Sex=="male",1,0)))

# Train/Val-SPlit
set.seed(1312)
train.idx <- createDataPartition(y=t.data$Survived,p=0.8,list=F)
train <- t.data[train.idx,]
val <- t.data[-train.idx,]

################################################################################
#   D. EXPLORATIVE ANALYSE                                                     #
################################################################################

str(train)
summary(train)

length(unique(train$PassengerId))
unique(train$Survived)
unique(train$Pclass)
unique(train$Sex)
unique(train$Cabin)

train %>% filter(grepl("^.* [A-Z]",Cabin))
train %>% filter(grepl("^.* [A-Z]",Cabin) & Pclass==3)
train %>% filter(grepl("^Fortune",Name))
train %>% filter(Cabin=="C23 C25 C27")
train %>% filter(Cabin=="F G73")

# Manipulationen
# missing value imputation
md.pattern(train)
train.imp <- train %>%
    dplyr::select(-Name,-Ticket,-Cabin,-Embarked) %>%
    mice(m=1,maxit=50,method="pmm",seed=1312) %>%
    complete() %>%
    left_join(train %>% 
                  dplyr::select(PassengerId,Name,Ticket,Cabin,Embarked),
              by=c("PassengerId"="PassengerId"))
train.imp <- train.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  cabin1_deck=substr(Cabin,1,1),
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(fare_top=ifelse(fare_pp>50,50,fare_pp)) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
                  cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                        cabin1_deck=="B" ~ "d1",
                                        cabin1_deck=="C" ~ "d1",
                                        cabin1_deck=="D" ~ "d1",
                                        cabin1_deck=="E" ~ "d1",
                                        cabin1_deck=="F" ~ "d3",
                                        cabin1_deck=="G" ~ "d3",
                                        cabin1_deck=="T" ~ "d3",
                                        TRUE ~ "d4"),
                  cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                      cabin_cnt==1 ~ "c1",
                                      TRUE ~ "c3"),
                  name_title=case_when(name_title=="Mr." ~ "t1",
                                       name_title=="Mrs." ~ "t2",
                                       name_title=="Miss." ~ "t3",
                                       name_title=="Master." ~ "t4",
                                       name_title=="Mme." ~ "t2",
                                       name_title=="Mlle." ~ "t3",
                                       TRUE ~ "t5"),
                  grp_size=case_when(grp_size==1 ~ "g1",
                                     grp_size<5 ~ "g2",
                                     TRUE ~ "g3"),
                  Pclass=case_when(Pclass=="1" ~ "cl1",
                                   Pclass=="2" ~ "cl2",
                                   TRUE ~ "cl3"),
                  Survived=case_when(Survived=="0" ~ "s0",
                                     Survived=="1" ~ "s1",
                                     TRUE ~ "s3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5")))
train.imp[is.na(train.imp)] <- ""

# Personen unique, 
# Gruppen über Ticket-Nummer identifizierbar
# Preis immer für gesamtes Ticket (mitunter für mehrere Personen)

# Verteilung ausgew. Variablen (Barplot)
train.p <- train.imp %>%
    dplyr::mutate(cnt=1,
                  Survived=factor(ifelse(Survived=="s0",
                                            "0",
                                            "1")),
                  Sex=as.factor(ifelse(Sex==1,"m","w")),
                  GroupSize=as.factor(grp_size),
                  CabinCount=as.factor(cabin_cnt),
                  Deck=as.factor(cabin1_deck),
                  Titel=as.factor(case_when(name_title=="t1" ~ "Mr",
                                            name_title=="t2" ~ "Mrs",
                                                name_title=="t3" ~ "Miss",
                                                name_title=="t4" ~ "Master",
                                                name_title=="t5" ~ "Andere")),
                  AgeGroup=as.factor(case_when(Age < 20 ~ "0-19",
                                               Age < 25 ~ "20-24",
                                               Age < 30 ~ "25-29",
                                               Age < 35 ~ "30-34",
                                               Age < 40 ~ "35-39",
                                               Age < 45 ~ "40-44",
                                               TRUE ~ "45+")))


# test-umgestaltung
md.pattern(train)
train.imp <- train %>%
    dplyr::mutate(cab=ifelse(substr(Cabin,1,1)=="",NA,substr(Cabin,1,1))) %>%
    dplyr::mutate(cab=as.factor(cab)) %>%
    dplyr::select(-Name,-Ticket,-Cabin,-Embarked) %>%
    mice(m=1,maxit=50,method="pmm",seed=1312) %>%
    complete() %>%
    left_join(train %>% 
                  dplyr::select(PassengerId,Name,Ticket,Cabin,Embarked),
              by=c("PassengerId"="PassengerId"))
train.imp <- train.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  #cabin1_deck=substr(Cabin,1,1),
                  cabin1_deck=cab,
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(fare_grp=case_when(fare_pp < 5 ~ "f5",
                                     fare_pp < 10 ~ "f10", 
                                     fare_pp > 15 ~ "f15",
                                     fare_pp<25 ~ "f25",
                                     fare_pp < 30 ~ "f30", 
                                     fare_pp< 40 ~ "f40",
                                     fare_pp>=40 ~ "f40+"),
                  age_grp=case_when(Age <= 15 ~ "a15",
                                    Age <= 20 ~ "a20",
                                    Age <= 25 ~ "a25",
                                    Age <= 30 ~ "a30",
                                    Age <= 35 ~ "a35",
                                    Age <= 40 ~ "a40",
                                    Age <= 45 ~ "a45",
                                    TRUE ~ "a45+"),
                  cl_age_flag=case_when(Sex==0 & Pclass==3 & Age <= 5 ~ "caf_1",
                                        Sex==0 & Pclass==3 & Age <= 10 ~ "caf_0",
                                        Sex==0 & Pclass==3 & Age <= 20 ~ "caf_1",
                                        Sex==0 & Pclass==3 & Age <= 25 ~ "caf_0",
                                        Sex==0 & Pclass==3 & Age <= 38 ~ "caf_1",
                                        Sex==0 & Pclass==3 & Age > 38 ~ "caf_0",
                                        TRUE ~ "caf_2")) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
                  cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                        cabin1_deck=="B" ~ "d1",
                                        cabin1_deck=="C" ~ "d2",
                                        cabin1_deck=="D" ~ "d3",
                                        cabin1_deck=="E" ~ "d3",
                                        cabin1_deck=="F" ~ "d3",
                                        cabin1_deck=="G" ~ "d4",
                                        cabin1_deck=="T" ~ "d4",
                                        TRUE ~ "d5"),
                  cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                      cabin_cnt==1 ~ "c1",
                                      TRUE ~ "c3"),
                  name_title=case_when(name_title=="Mr." ~ "t1",
                                       name_title=="Mrs." ~ "t2",
                                       name_title=="Miss." ~ "t3",
                                       name_title=="Master." ~ "t4",
                                       name_title=="Mme." ~ "t2",
                                       name_title=="Mlle." ~ "t3",
                                       name_title=="Countess." ~ "t5",
                                       name_title=="Lady." ~ "t5",
                                       name_title=="Capt." ~ "t6",
                                       name_title=="Col." ~ "t6",
                                       name_title=="Major." ~ "t6",
                                       name_title=="Rev." ~ "t6",
                                       name_title=="Dr." ~ "t7",
                                       name_title=="Don." ~ "t4",
                                       TRUE ~ "t8"),
                  grp_size=case_when(grp_size==1 ~ "g1",
                                     grp_size==2 ~ "g2",
                                     grp_size==3 ~ "g3",
                                     grp_size==4 ~ "g4",
                                     grp_size>4 ~ "g5",
                                     TRUE ~ "g6"),
                  Pclass=case_when(Pclass=="1" ~ "cl1",
                                   Pclass=="2" ~ "cl2",
                                   TRUE ~ "cl3"),
                  Survived=case_when(Survived=="0" ~ "s0",
                                     Survived=="1" ~ "s1",
                                     TRUE ~ "s3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4","d5")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5","t6","t7","t8")))
train.imp[is.na(train.imp)] <- ""
train.imp <- train.imp%>% dplyr::select(Survived,Pclass,Sex,Age,Embarked,grp_size,
                                        cabin_cnt,cabin1_deck,name_title,fare_grp,
                                        cl_age_flag)
train.imp <- train.imp %>%
    dplyr::mutate(Sex=ifelse(Sex==1,"sex1","sex0"))

ctrl <- trainControl(method="boot",number=10,savePredictions="final",
                     classProbs=T)
fit.xgbTree <- caret::train(Survived~.,
                            data=train.imp,
                            method="xgbTree",
                            trControl=ctrl)

confusionMatrix(train.imp$Survived,predict(fit.xgbTree,train.imp))
out <- train.imp %>%
    dplyr::mutate(pred=predict(fit.xgbTree,train.imp))
out %>%
    dplyr::filter(Survived!=pred) %>%
    dplyr::arrange(Survived,Pclass,Sex,Embarked,grp_size,cabin_cnt)

# Neuversuch
train.feat <- gen_feat(train)
ctrl <- trainControl(method="boot",number=10,savePredictions="final",
                     classProbs=T)
fit.xgbTree <- caret::train(Survived_lu~.,
                            data=train.feat,
                            method="xgbTree",
                            trControl=ctrl)

confusionMatrix(train.feat$Survived_lu,predict(fit.xgbTree,train.feat))

val.feat <- gen_feat(val,train=F)
confusionMatrix(val.feat$Survived_lu,predict(fit.xgbTree,val.feat))

# train.imp
bar.list <- c("Pclass","Embarked","grp_size","cabin_cnt","cabin1_deck","name_title")
lapply(bar.list,function(f) plot_bar3(data=train.imp,
                                     x=f,
                                     val="abs",
                                     fill="Survived",
                                     facet="Sex")) 
# Kommentar:
# titel: bei frauen nur tote bei miss/mrs, bei männern überlebende nur bei col, 
#        dr,major,master, mr (wenig)
#        rel. anz nur miss, mrs, dr, master, mr, rev
#        countess=lady, miss=mlle, mme=mrs, capt=col=major=rev,dr, don=master, mr
# cabin1_deck: ges. hohe s1 b,d,e, schlechte bei 'ohne', a, t
#              frauen: kleine s1 bei 'ohne' und g
#              männer: kleine s1 bei 'ohne', a b c und f, hohe bei d,e
#               anz: d=e=f, g=t, 'ohne', a=b,c
# cabin_cnt: frauen mit 3+ überleben, allg. mehr kab, mehr überleben, 1 wenig,
#            2-3 mittel, 4 hoch
#            anz: meisten 1, 1+
# grp_size: ges. max überleben mit size=4, 2,3 hoch, min 1,5,6,8,11
#           w. 1-4 hoch, 5+ gering
#           m 4 hoch, 2-3 mittel, 1 5+ gering
#           grp: 1,2,3,4,5+
# embarked: ges. s,q,c
#           c,q,s,else
# plcass: kl > sur <
#         1,2,3




dens.list <- c("Fare","fare_pp","Age","SibSp","Parch")

plot_pyramid(data=z,x="fare_z",grp="Sex",facet="Survived",grp.lvl=c(0,1))

# pyramid:
# Alter: 0-15,-20,-25,-30,-35,-40,-45,46+
# fare_top: 


varlist <- c("Sex","Pclass","Embarked","GroupSize","CabinCount",
             "Deck","Titel","AgeGroup")
varlist <- c("Sex","Pclass")

lapply(varlist,function(f) table(train.p[[f]])) %>%
    setNames(varlist)

lapply(varlist,function(f) plot_bar2(data=train.p,
                                     x=f,
                                     val="abs",
                                     fill="Survived",
                                     facet="Sex")) %>%
    do.call(plot_grid,ncol=2,.)


plot_pyramid(data=train.p,x="Age",grp="Sex",facet="Survived",grp.lvl=c("m","w"))

# Verteilung ausgew. Variablen (Density)
varlist <- c("Age","Fare","fare_pp")
lapply(varlist,function(f) plot_density2(train.p,x=f,y="cnt")) %>%
    do.call(plot_grid,.)

#plot_density(train.imp,x="Age","name_title")

# train.imp %>%
#     ggplot2::ggplot(aes(x=Age,y=Pclass)) +
#     ggridges::geom_density_ridges()

# train.imp %>%
#     ggplot(aes(x=Survived,y=Fare)) + 
#     geom_boxplot()


# chi2-test - feature selection
weights <- chi.squared(Survived~., train.imp) 
weights %>% 
    top_n(5,attr_importance)
col.filter <- cutoff.k(weights, 10)

weights <- symmetrical.uncertainty(Survived~., train.imp)
cutoff.biggest.diff(weights)

################################################################################
#   E. MODEL                                                                   #
################################################################################

train.imp <- train.imp %>%
    #dplyr::select(col.filter) %>%
    dplyr::select(-Name,-Ticket,-Cabin)
ctrl <- trainControl(method="repeatedcv",repeats=1,classProbs=F,savePredictions=F)
# Ergebnisse ausgeben: Train
l<- list(fit.svmlin,fit.glm,fit.rf,fit.nb,fit.nnet,fit.glmboost,fit.LogitBoost,
         fit.xgbTree,fit.fda,fit.glmStepAIC,fit.kknn,fit.svmLinear3,fit.pcaNNet,
         fit.svmrad)
label <- c("fit.svmlin","fit.glm","fit.rf","fit.nb","fit.nnet","fit.glmboost",
           "fit.LogitBoost","fit.xgbTree","fit.fda","fit.glmStepAIC","fit.kknn",
           "fit.svmLinear3","fit.pcaNNet","fit.svmrad")

out <- lapply(l,function(f) f$results$Accuracy)
names(out) <- label
lapply(out,max,na.rm=T) %>%
    unlist() %>%
    sort()
# Ergebnisse ausgeben: Val
out2 <- lapply(l,function(f) confusionMatrix(val$Survived,predict(f,val.imp))$overall)
names(out2) <- label
x <- out2 %>%
    unlist() %>%
    .[grepl("Accuracy$",names(.))] 
which(x==max(x))

confusionMatrix(val$Survived,predict(fit.glmStepAIC,val.imp))$table
confusionMatrix(val$Survived,predict(fit.svmLinear3,val.imp))$table        

# ROC
library(pROC)
pred.prob <- predict(ensemble, train.imp, type="prob") # Prediction

result.roc <- roc(train.imp$Survived, pred.prob$s0) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")



# Mail senden
send_gmail <- function(from="samboldsky@gmail.com",
                       to="samboldsky@gmail.com",
                       subject="",
                       body="test",
                       user="samboldsky@gmail.com",
                       pswd="P@o!QHa&J9g#",
                       ssl=T,
                       authenticate=T,
                       send=T){
    
    mailR::send.mail(from=from,
                     to=to,
                     subj=subject,
                     body=body,
                     smtp=list(host.name="smtp.gmail.com",
                               port=465,
                               user.name=user,
                               passwd=pswd,
                               ssl=T),
                     authenticate=T,
                     send=T)
}
x <- fit.svmlin$results


label <- c("fit.svmlin","fit.glm","fit.rf","fit.nb","fit.nnet","fit.glmboost",
           "fit.LogitBoost","fit.xgbTree","fit.fda","fit.glmStepAIC","fit.kknn",
           "fit.svmLinear3","fit.pcaNNet","fit.svmrad")

out <- lapply(label,function(f) f$results$Accuracy)
names(out) <- label
lapply(out,max,na.rm=T) %>%
    unlist() %>%
    sort()

fit.svmlin <- caret::train(Survived~.,
                       data=train.imp,
                       method="svmLinear",
                       trControl=ctrl)
fit.glm <- caret::train(Survived~.,
                        data=train.imp,
                        method="glm",
                        trControl=ctrl)
fit.rf <- caret::train(Survived~.,
                       data=train.imp,
                       method="rf",
                       trControl=ctrl)
fit.nb <- caret::train(Survived~.,
                       data=train.imp,
                       method="nb",
                       trControl=ctrl)
fit.nnet <- caret::train(Survived~.,
                         data=train.imp,
                         method="nnet",
                         trControl=ctrl)
fit.glmboost <- caret::train(Survived~.,
                         data=train.imp,
                         method="glmboost",
                         trControl=ctrl)
fit.LogitBoost <- caret::train(Survived~.,
                             data=train.imp,
                             method="LogitBoost",
                             trControl=ctrl)
fit.xgbTree <- caret::train(Survived~.,
                               data=train.imp,
                               method="xgbTree",
                               trControl=ctrl)
fit.fda <- caret::train(Survived~.,
                            data=train.imp,
                            method="fda",
                            trControl=ctrl)
fit.glmStepAIC <- caret::train(Survived~.,
                      data=train.imp,
                      method="glmStepAIC",
                      trControl=ctrl)
fit.kknn <- caret::train(Survived~.,
                               data=train.imp,
                               method="kknn",
                               trControl=ctrl)
fit.svmLinear3 <-  caret::train(Survived~.,
                                data=train.imp,
                                method="svmLinear3",
                                trControl=ctrl)
fit.pcaNNet <-  caret::train(Survived~.,
                           data=train.imp,
                           method="pcaNNet",
                           trControl=ctrl)
fit.svmrad <- caret::train(Survived~.,
                           data=train.imp,
                           method="svmRadial",
                           trControl=ctrl)
plot(varImp(fit.nnet))

prob_pred = predict(classifier, type = 'response', newdata = test)


# Ensamble
#install.packages("caretEnsemble")
library(caretEnsemble)
train.imp <- train.imp %>%
    dplyr::select(-PassengerId,-Name,-Ticket,-Cabin)
ctrl <- trainControl(method="boot",number=25,savePredictions="final",
                     classProbs=T,index=createResample(train.imp$Survived,25))
mod.list <- caretList(Survived~.,
                      data=train.imp,
                      trControl=ctrl,
                     #methodList=c("xgbTree","svmLinear"))
                     methodList=c("xgbTree","fda","pcaNNet"))
xyplot(resamples(mod.list))
modelCor(resamples(mod.list))

ens <- caretEnsemble(mod.list,
                          metric="ROC",
                          trControl=ctrl) #trainControl(method="repeatedcv",repeats=5,
                                          #       savePredictions=T,classProbs=T))
summary(ens)

confusionMatrix(train.imp$Survived,predict(stack.list,train.imp))

stack.list <- caretStack(mod.list,
                         method="xgbTree",
                         metric="Accuracy",
                         trControl=ctrl)#trainControl(method="repeatedcv",repeats=5,
                                        #        savePredictions=T,classProbs=T))

# train + val
md.pattern(t.data)
t.imp <- t.data %>%
    dplyr::select(-Name,-Ticket,-Cabin,-Embarked) %>%
    mice(m=1,maxit=50,method="pmm",seed=1312) %>%
    complete() %>%
    left_join(val %>% 
                  dplyr::select(PassengerId,Name,Ticket,Cabin,Embarked),
              by=c("PassengerId"="PassengerId"))
t.imp <- t.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  cabin1_deck=substr(Cabin,1,1),
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(fare_grp=case_when(fare_pp < 5 ~ "f5",
                                     fare_pp < 10 ~ "f10", 
                                     fare_pp > 15 ~ "f15",
                                     fare_pp<25 ~ "f25",
                                     fare_pp < 30 ~ "f30", 
                                     fare_pp< 40 ~ "f40",
                                     fare_pp>=40 ~ "f40+")) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
                  cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                        cabin1_deck=="B" ~ "d1",
                                        cabin1_deck=="C" ~ "d2",
                                        cabin1_deck=="D" ~ "d3",
                                        cabin1_deck=="E" ~ "d3",
                                        cabin1_deck=="F" ~ "d3",
                                        cabin1_deck=="G" ~ "d4",
                                        cabin1_deck=="T" ~ "d4",
                                        TRUE ~ "d5"),
                  cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                      cabin_cnt==1 ~ "c1",
                                      TRUE ~ "c3"),
                  name_title=case_when(name_title=="Mr." ~ "t1",
                                       name_title=="Mrs." ~ "t2",
                                       name_title=="Miss." ~ "t3",
                                       name_title=="Master." ~ "t4",
                                       name_title=="Mme." ~ "t2",
                                       name_title=="Mlle." ~ "t3",
                                       name_title=="Countess." ~ "t5",
                                       name_title=="Lady." ~ "t5",
                                       name_title=="Capt." ~ "t6",
                                       name_title=="Col." ~ "t6",
                                       name_title=="Major." ~ "t6",
                                       name_title=="Rev." ~ "t6",
                                       name_title=="Dr." ~ "t7",
                                       name_title=="Don." ~ "t4",
                                       TRUE ~ "t8"),
                  grp_size=case_when(grp_size==1 ~ "g1",
                                     grp_size==2 ~ "g2",
                                     grp_size==3 ~ "g3",
                                     grp_size==4 ~ "g4",
                                     grp_size>4 ~ "g5",
                                     TRUE ~ "g6"),
                  Pclass=case_when(Pclass=="1" ~ "cl1",
                                   Pclass=="2" ~ "cl2",
                                   TRUE ~ "cl3"),
                  Survived=case_when(Survived=="0" ~ "s0",
                                     Survived=="1" ~ "s1",
                                     TRUE ~ "s3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4","d5")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5","t6","t7","t8")))
t.imp[is.na(t.imp)] <- ""
t.imp <- t.imp%>% dplyr::select(Survived,Pclass,Sex,Age,Embarked,grp_size,cabin_cnt,cabin1_deck,name_title,fare_grp)
t.imp <- t.imp %>%
    dplyr::mutate(Sex=ifelse(Sex==1,"sex1","sex0"))


label <- c("svmLinear","glm","rf","nb","nnet","glmboost",
           "LogitBoost","xgbTree","fda","glmStepAIC","kknn",
           "svmLinear3","pcaNNet","svmRadial")

label <- c("xgbTree","fda","svmLinear3","pcaNNet","nb")
ctrl <- trainControl(method="boot",number=100,savePredictions="final",
                     classProbs=T)
mods <- lapply(label,function(f){
    caret::train(Survived~.,
             data=t.imp,
             method=f,
             trControl=ctrl)
})

names(mods) <- label
x <- mods %>%
    unlist() %>%
    .[grepl("Accuracy$",names(.))] 
which(x==max(x))

# ctrl <- trainControl(method="boot",number=50,savePredictions="final",
#                      classProbs=T,index=createResample(train.imp$Survived,50))
# mod.list <- caretList(Survived~.,
#                       data=train.imp,
#                       trControl=ctrl,
#                       methodList=c("xgbTree","fda","pcaNNet"))
# ens <- caretEnsemble(mod.list,
#                      metric="ROC",
#                      trControl=ctrl)
# stack.list <- caretStack(mod.list,
#                          method="xgbTree",
#                          metric="Accuracy",
#                          trControl=ctrl)

pred <- predict(mods$xgbTree,test.imp)
out <- test %>%
    dplyr::select(PassengerId) %>%
    dplyr::mutate(Survived=pred) %>%
    dplyr::mutate(Survived=ifelse(Survived=="s1",1,0))
write.table(out,file="titanic_submission_xgbTree.csv",row.names=F,sep=",")


#########

# fehlersuche
# confusionMatrix(train.imp$Survived,predict(stack.list,train.imp))
ctrl <- trainControl(method="boot",number=10,savePredictions="final",
                     classProbs=T)
fit.xgbTree <- caret::train(Survived~.,
                            data=train.imp,
                            method="xgbTree",
                            trControl=ctrl)

confusionMatrix(train.imp$Survived,predict(fit.xgbTree,train.imp))
out <- train.imp %>%
    dplyr::mutate(pred=predict(fit.xgbTree,train.imp))
out %>%
    dplyr::filter(Survived!=pred) %>%
    dplyr::arrange(Survived,Pclass,Sex,Embarked,grp_size,cabin_cnt)



####################


# validation
md.pattern(val)
val.imp <- val %>%
    dplyr::select(-Name,-Ticket,-Cabin,-Embarked) %>%
    mice(m=1,maxit=50,method="pmm",seed=1312) %>%
    complete() %>%
    left_join(val %>% 
                  dplyr::select(PassengerId,Name,Ticket,Cabin,Embarked),
              by=c("PassengerId"="PassengerId"))
val.imp <- val.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  cabin1_deck=substr(Cabin,1,1),
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
        cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                              cabin1_deck=="B" ~ "d1",
                              cabin1_deck=="C" ~ "d1",
                              cabin1_deck=="D" ~ "d1",
                              cabin1_deck=="E" ~ "d1",
                              cabin1_deck=="F" ~ "d3",
                              cabin1_deck=="G" ~ "d3",
                              cabin1_deck=="T" ~ "d3",
                              TRUE ~ "d4"),
        cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                            cabin_cnt==1 ~ "c1",
                            TRUE ~ "c3"),
        name_title=case_when(name_title=="Mr." ~ "t1",
                             name_title=="Mrs." ~ "t2",
                             name_title=="Miss." ~ "t3",
                             name_title=="Master." ~ "t4",
                             name_title=="Mme." ~ "t2",
                             name_title=="Mlle." ~ "t3",
                             TRUE ~ "t5"),
        grp_size=case_when(grp_size==1 ~ "g1",
                           grp_size<5 ~ "g2",
                           TRUE ~ "g3"),
        Pclass=case_when(Pclass=="1" ~ "cl1",
                         Pclass=="2" ~ "cl2",
                         TRUE ~ "cl3"),
        Survived=case_when(Survived=="0" ~ "s0",
                           Survived=="1" ~ "s1",
                           TRUE ~ "s3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5")))
val.imp[is.na(val.imp)] <- ""



# test val
# test-umgestaltung
val.imp <- val.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  cabin1_deck=substr(Cabin,1,1),
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(fare_grp=case_when(fare_pp < 5 ~ "f5",
                                     fare_pp < 10 ~ "f10", 
                                     fare_pp > 15 ~ "f15",
                                     fare_pp<25 ~ "f25",
                                     fare_pp < 30 ~ "f30", 
                                     fare_pp< 40 ~ "f40",
                                     fare_pp>=40 ~ "f40+")) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
                  cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                        cabin1_deck=="B" ~ "d1",
                                        cabin1_deck=="C" ~ "d2",
                                        cabin1_deck=="D" ~ "d3",
                                        cabin1_deck=="E" ~ "d3",
                                        cabin1_deck=="F" ~ "d3",
                                        cabin1_deck=="G" ~ "d4",
                                        cabin1_deck=="T" ~ "d4",
                                        TRUE ~ "d5"),
                  cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                      cabin_cnt==1 ~ "c1",
                                      TRUE ~ "c3"),
                  name_title=case_when(name_title=="Mr." ~ "t1",
                                       name_title=="Mrs." ~ "t2",
                                       name_title=="Miss." ~ "t3",
                                       name_title=="Master." ~ "t4",
                                       name_title=="Mme." ~ "t2",
                                       name_title=="Mlle." ~ "t3",
                                       name_title=="Countess." ~ "t5",
                                       name_title=="Lady." ~ "t5",
                                       name_title=="Capt." ~ "t6",
                                       name_title=="Col." ~ "t6",
                                       name_title=="Major." ~ "t6",
                                       name_title=="Rev." ~ "t6",
                                       name_title=="Dr." ~ "t7",
                                       name_title=="Don." ~ "t4",
                                       TRUE ~ "t8"),
                  grp_size=case_when(grp_size==1 ~ "g1",
                                     grp_size==2 ~ "g2",
                                     grp_size==3 ~ "g3",
                                     grp_size==4 ~ "g4",
                                     grp_size>4 ~ "g5",
                                     TRUE ~ "g6"),
                  Pclass=case_when(Pclass=="1" ~ "cl1",
                                   Pclass=="2" ~ "cl2",
                                   TRUE ~ "cl3"),
                  Survived=case_when(Survived=="0" ~ "s0",
                                     Survived=="1" ~ "s1",
                                     TRUE ~ "s3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4","d5")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5","t6","t7","t8")))
val.imp[is.na(val.imp)] <- ""
val.imp <- val.imp%>% dplyr::select(Survived,Pclass,Sex,Age,Embarked,grp_size,cabin_cnt,cabin1_deck,name_title,fare_grp)
val.imp <- val.imp %>%
    dplyr::mutate(Sex=ifelse(Sex==1,"sex1","sex0"))


confusionMatrix(val.imp$Survived,predict(stack.list,val.imp))
####

val.imp <- val.imp %>%
    dplyr::select(-PassengerId,-Survived,-Name,-Ticket,-Cabin)
x <- case_when(val$Survived == "0" ~ "s0",
               val$Survived =="1" ~ "s1")

confusionMatrix(x,predict(ens,val.imp))

# test-data
test.imp <- test %>%
    dplyr::select(-Name,-Ticket,-Cabin,-Embarked) %>%
    mice(m=1,maxit=50,method="pmm",seed=1312) %>%
    complete() %>%
    left_join(test %>% 
                  dplyr::select(PassengerId,Name,Ticket,Cabin,Embarked),
              by=c("PassengerId"="PassengerId"))
test.imp <- test.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  cabin1_deck=substr(Cabin,1,1),
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
                  cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                        cabin1_deck=="B" ~ "d1",
                                        cabin1_deck=="C" ~ "d1",
                                        cabin1_deck=="D" ~ "d1",
                                        cabin1_deck=="E" ~ "d1",
                                        cabin1_deck=="F" ~ "d3",
                                        cabin1_deck=="G" ~ "d3",
                                        cabin1_deck=="T" ~ "d3",
                                        TRUE ~ "d4"),
                  cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                      cabin_cnt==1 ~ "c1",
                                      TRUE ~ "c3"),
                  name_title=case_when(name_title=="Mr." ~ "t1",
                                       name_title=="Mrs." ~ "t2",
                                       name_title=="Miss." ~ "t3",
                                       name_title=="Master." ~ "t4",
                                       name_title=="Mme." ~ "t2",
                                       name_title=="Mlle." ~ "t3",
                                       TRUE ~ "t5"),
                  grp_size=case_when(grp_size==1 ~ "g1",
                                     grp_size<5 ~ "g2",
                                     TRUE ~ "g3"),
                  Pclass=case_when(Pclass=="1" ~ "cl1",
                                   Pclass=="2" ~ "cl2",
                                   TRUE ~ "cl3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5")))
test.imp[is.na(test.imp)] <- ""

# test-umgestaltung
test.imp <- test.imp %>%
    dplyr::mutate(grp_size=SibSp+Parch+1,
                  fare_pp=Fare/(SibSp+Parch+1),
                  cabin_cnt=str_count(Cabin," ")+1,
                  cabin1_deck=substr(Cabin,1,1),
                  name_title=str_match(Name,"(?:^.*, )?([:alpha:]*\\.)(?:.*$)")[,2]) %>%
    dplyr::mutate(fare_grp=case_when(fare_pp < 5 ~ "f5",
                                     fare_pp < 10 ~ "f10", 
                                     fare_pp > 15 ~ "f15",
                                     fare_pp<25 ~ "f25",
                                     fare_pp < 30 ~ "f30", 
                                     fare_pp< 40 ~ "f40",
                                     fare_pp>=40 ~ "f40+")) %>%
    dplyr::mutate(cabin_cnt=ifelse(Cabin=="",0,cabin_cnt),
                  cabin1_deck=case_when(cabin1_deck=="A" ~ "d1",
                                        cabin1_deck=="B" ~ "d1",
                                        cabin1_deck=="C" ~ "d2",
                                        cabin1_deck=="D" ~ "d3",
                                        cabin1_deck=="E" ~ "d3",
                                        cabin1_deck=="F" ~ "d3",
                                        cabin1_deck=="G" ~ "d4",
                                        cabin1_deck=="T" ~ "d4",
                                        TRUE ~ "d5"),
                  cabin_cnt=case_when(cabin_cnt==0 ~ "c0",
                                      cabin_cnt==1 ~ "c1",
                                      TRUE ~ "c3"),
                  name_title=case_when(name_title=="Mr." ~ "t1",
                                       name_title=="Mrs." ~ "t2",
                                       name_title=="Miss." ~ "t3",
                                       name_title=="Master." ~ "t4",
                                       name_title=="Mme." ~ "t2",
                                       name_title=="Mlle." ~ "t3",
                                       name_title=="Countess." ~ "t5",
                                       name_title=="Lady." ~ "t5",
                                       name_title=="Capt." ~ "t6",
                                       name_title=="Col." ~ "t6",
                                       name_title=="Major." ~ "t6",
                                       name_title=="Rev." ~ "t6",
                                       name_title=="Dr." ~ "t7",
                                       name_title=="Don." ~ "t4",
                                       TRUE ~ "t8"),
                  grp_size=case_when(grp_size==1 ~ "g1",
                                     grp_size==2 ~ "g2",
                                     grp_size==3 ~ "g3",
                                     grp_size==4 ~ "g4",
                                     grp_size>4 ~ "g5",
                                     TRUE ~ "g6"),
                  Pclass=case_when(Pclass=="1" ~ "cl1",
                                   Pclass=="2" ~ "cl2",
                                   TRUE ~ "cl3")) %>%
    dplyr::mutate(Pclass=factor(Pclass,levels=c("cl1","cl2","cl3")),
                  cabin1_deck=factor(cabin1_deck,levels=c("d1","d2","d3","d4","d5")),
                  name_title=factor(name_title,levels=c("t1","t2","t3","t4","t5","t6","t7","t8")))
test.imp[is.na(test.imp)] <- ""
test.imp <- test.imp%>% dplyr::select(Pclass,Sex,Age,Embarked,grp_size,cabin_cnt,cabin1_deck,name_title,fare_grp)
test.imp <- test.imp %>%
    dplyr::mutate(Sex=ifelse(Sex==1,"sex1","sex0"))
###

pred <- predict(ens,test.imp)
out <- test %>%
    dplyr::select(PassengerId) %>%
    dplyr::mutate(Survived=pred) %>%
    dplyr::mutate(Survived=ifelse(Survived=="s1",0,1))
write.table(out,file="titanic_submission_neu.csv",row.names=F,sep=",")
