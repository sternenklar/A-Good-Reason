rm(list=ls())
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(DT)
library(curl)
df <- data.table(fread("https://github.com/sternenklar/A-Good-Reason/blob/382d0d9e52bda03c1860c12f888fc1255651ea52/Data/complete_data_compared.csv"))

df <- df[stayhome_day=="no" & stayhome_night=="no" & outside_masks=="no", colour:="#008500"]
df <- df[stayhome_day=="no" & stayhome_night=="no" & !outside_masks %chin% c("yes", "no"), colour:="#CCFF00"]
df <- df[stayhome_day=="no" & stayhome_night=="no" & outside_masks=="yes", colour:="#FFFF00"]
df <- df[!stayhome_night %chin% c("yes", "no") & stayhome_day=="no", colour:="#FF9900"]
df <- df[stayhome_night=="yes" & stayhome_day=="no", colour:="#FF6600"]
df <- df[stayhome_day!="no" & walk_day=="yes", colour:="#FF3300"]
df <- df[stayhome_day!="no" & walk_day!="yes", colour:="#FF0000"]
df <- df[stayhome_day=="yes" & walk_day=="no", colour:="#CC0000"]

df$country <- factor(df$country,levels=rev(unique(df$country)))

df_compare <- df[!country %in% c("Montenegro", "North Macedonia", "Northern Cyprus")]
                          
ui <- fluidPage(

    # Application title
    titlePanel("Data"),
    theme=shinytheme("superhero"),
    verticalLayout(
            mainPanel(width="400px",
                tags$div("Here you can get an overview of the restrictions in force in different European territories (*) from 1 January 2020 until 31 December 2022. My research essentially deals with the question: Were you allowed to take a walk? Did you need a \"good reason\" to leave your house and if yes, what counted as a good reason?", tags$br(), tags$br(),
                         "You can download the dataset ",
                         tags$a(href="https://github.com/sternenklar/A-Good-Reason/blob/382d0d9e52bda03c1860c12f888fc1255651ea52/Data/complete_data_compared.csv", "in .csv format here"),
                         " or you can explore the data below. For a definition of variables and classification, please refer to ",
                         tags$a(href="https://github.com/sternenklar/A-Good-Reason/blob/main/Data/codebook.pdf", "this .pdf document"),
                         ".", tags$br(), tags$br(),               
                         "For many countries, there are discrepancies with the data provided by the largest Covid-19 policy tracker OxCGRT. In the second tab, you can see an overview of all countries and days where either their data says that mandatory stay-at-home restrictions were in place at least locally in a country and my data doesn’t, or vice versa. Note that OxCGRT doesn't collect data for Montenegro, North Macedonia, Northern Cyprus. The differences with the OxCGRT data usually mean that either of our data is wrong, even though some discrepancies might be explainable with different interpretations of what constitutes a stay-at-home order. If you find any information that is incorrect or incomplete, please do not hesitate to ",
                         tags$a(href="mailto:a-good-reason@posteo.eu", "write me an e-mail"),
                         "! I tried to rely as little as possible on automated translation so that most sources are in English or other languages I have at least a basic understanding of.", tags$br(), tags$br(),
                         "Note that while stay-at-home orders for elderly people or local stay-at-home orders are included, this project does not deal with stay-at-home orders for people living in care facilities. This project also does not cover quarantine measures that are targeted towards persons who have been exposed to an infected person or have been tested positive themselves. Quarantine measures for travellers are not covered either. A policy is treated as a (local) stay-at-home order if the assessment whether someone needed to stay home was not taken on an individual basis. There have been examples of “quarantines” of entire villages or apartment blocks for example due to a local spike in positive tests. As it is not realistic to assume that everyone within a village or a residential complex has been in close contact with everyone else, these so-called quarantine measures are covered as stay-at-home orders in my database."), tags$br(),
                "As this data shall provide an overview of stay-at-home restrictions in Europe, I decided to include the curfews in Ukraine since February 2022. These are not motivated by the pandemic, however, but by the ongoing war.", tags$br(), tags$br(),
                tabsetPanel(
                    tabPanel(title="Overview", plotOutput("overview")),
                    tabPanel(title="Differences with OxCGRT", plotOutput("difference"))), tags$br(), tags$br(),
            "Here you can explore the dataset. For an explanation of variables and categorizations refer to above-mentioned ",
            tags$a(href="https://github.com/sternenklar/A-Good-Reason/blob/main/Data/codebook.pdf", ".pdf document"),
            ". You can also download the complete dataset including sources for every policy change in .csv format ",
            tags$a(href="https://github.com/sternenklar/A-Good-Reason/blob/382d0d9e52bda03c1860c12f888fc1255651ea52/Data/complete_data_compared.csv", "here"),
            ".", tags$br(), tags$br(),
            
           selectInput("selection","select country/territory", choices=unique(df$country)),
           tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }
                    "
                    )),
           DTOutput("selection", width="2000px"),
           tags$br(),"(*) There are various geographical definitions of which countries belong to Europe. For my research, I included all countries that are entirely located in Europe according to the Wikipedia article on Europe. Furthermore, I included Russia and Turkey where a significant share of the population lives in Europe, and Cyprus which is a member of the European Union. I included all microstates except the Vatican. I separately analysed policies in all de-facto independent territories no matter their international recognition. Therefore, Kosovo and the Turkish Republic of Northern Cyprus are included as separate entities. This is by no means a political statement on whether these territories should be recognised as independent or not. It merely reflects the status quo that they are independently governed. Initially, I wanted to include Transnistria as well, but as sources were very scarce, it is treated as part of Moldova (which it de jure is)."
            )
        )
    )

server <- function(input, output) {
    output$overview <- renderPlot(
        ggplot(df, aes(x=date, y=country, color=colour))+
        geom_point()+
        scale_colour_manual(values=c("#008500"="#008500","#CCFF00"="#CCFF00", "#FFFF00"="#FFFF00","#FF9900"="#FF9900", "#FF6600"="#FF6600", "#FF3300"="#FF3300", "#FF0000"="#FF0000", "#CC0000"="#CC0000"),
                            labels=c("no stay-at-home restrictions", "outdoors mask mandate locally or for some groups", "outdoors mask mandate in entire country", "night curfew locally or for some groups", "night curfew in entire country", "stay at home order at least locally or for some groups, but walks allowed", "stay at home order at least locally or for some groups with no free walks allowed", "stay-at-home order not allowing for walks in entire country"))+
        labs(x=NULL, y=NULL)+
        theme(legend.text=element_text(size=8),
              legend.position="bottom")+
        guides(colour=guide_legend(label.hjust=0, nrow=4, title=NULL))
    )
    output$difference <- renderPlot(
        ggplot(df, aes(x=date, y=country, color=difference_with_oxcgrt))+
            geom_point()+
            scale_colour_manual(values=c("#ffffff", "#0072B2", "#D55E00"))+
            labs(x=NULL, y=NULL, caption="Blue: mandatory stay-at-home restrictions reported by AGR but not by OxCGRT \nOrange: mandatory stay-at-home restrictions reported by OxCGRT but not by AGR")+
            theme(legend.position="none",
            plot.caption=element_text(hjust=0))
    )
    output$selection <- renderDT(DT::datatable(
        df[country==input$selection&change!="", c("date", "country", "change", "strictest_night", "strictest_day", "least_strict_night", "least_strict_day", "stayhome_day", "stayhome_night", "walk_day", "walk_night", "leave_municip", "outside_masks", "difference_with_oxcgrt")],
                                 options=list(
                                     scrollX=TRUE,
                                     autoWidth=TRUE,
                                     columnDefs=list((list(targets=c(1, 2), visible=TRUE, width='5%')), (list(targets=c(3), visible=TRUE, width='35%')), (list(targets=c(4:14), visible=TRUE, width='5%'))),
                                     filter="none")) %>%
          DT::formatStyle(columns = c("date", "country", "change", "strictest_night", "strictest_day", "least_strict_night", "least_strict_day", "stayhome_day", "stayhome_night", "walk_day", "walk_night", "leave_municip", "outside_masks", "difference_with_oxcgrt"), color="white"))
}

shinyApp(ui = ui, server = server)
