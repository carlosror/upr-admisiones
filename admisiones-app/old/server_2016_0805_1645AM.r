library(shiny)
library(ggplot2)
require('magrittr')


shinyServer(function(input, output, session) {
    filter_data <- reactive({
        admisiones <- read.csv("admisiones.csv", encoding="UTF-8")
                
        relevant_data <- subset(admisiones, Calendario == input$year & Campus == input$campus)
        
        # Reorder factors of IGS_bucket column
        # http://www.r-bloggers.com/reorder-factor-levels-2/
        relevant_data$IGS_bucket <- factor(relevant_data$IGS_bucket, levels(as.factor(relevant_data$IGS_bucket))[c(1, 3:7, 2)])
        
        relevant_data
    })
    output$campus <- renderPlot({
        relevant_data <- filter_data()
        
        # Base plot
        base_plot <- ggplot(relevant_data)
        
        # Color scheme
        # Color blind palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # removed black
        color_blind_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        # color_scheme <- scale_colour_brewer(palette="Accent")
        # color_fill_scheme <- scale_fill_brewer(palette="Accent")
        color_scheme <- scale_colour_manual(values = color_blind_palette)
        color_fill_scheme <- scale_fill_manual(values = color_blind_palette)
        
        # legend position
        legend_position <- theme(legend.position = "top")

        gpa_igs_scatter <- base_plot + geom_point(aes(x = GPA, y = IGS, colour = Genero)) + color_scheme + labs(colour=NULL) + legend_position
        
        gpa_boxplot <- base_plot + geom_boxplot(aes(x = Genero, y = GPA, fill = Genero), outlier.colour = "red") + color_fill_scheme + xlab("Género") + labs(fill=NULL) + legend_position
        
        igs_boxplot <- base_plot + geom_boxplot(aes(x = Genero, y = IGS, fill = Genero), outlier.colour = "red") + color_fill_scheme + xlab("Género") + labs(fill=NULL) + legend_position
        
        igs_histo <- base_plot + geom_histogram(aes(x=IGS, fill=Genero)) + color_fill_scheme + labs(fill=NULL) + ylab("Estudiantes") + legend_position
        
        igs_barplot <- base_plot + geom_bar( aes(x= IGS_bucket, fill=Genero), position="dodge") + color_fill_scheme + xlab("IGS") + ylab("Estudiantes") + legend_position + labs(fill=NULL)
        
        
        # multiplot(gpa_igs_scatter, gpa_boxplot, igs_boxplot, igs_histo, cols = 2)
        multiplot(gpa_igs_scatter, gpa_boxplot, igs_boxplot, igs_histo, igs_barplot, layout = matrix(c(1,2,3,4,5,5), nrow=2, byrow=TRUE))
    })

    output$escuelas <- renderPlot({
    
        relevant_data <- filter_data()
        
        # Filter to select the top 10 most popular programs
        escuela_table_sorted <- table(relevant_data$Institucion) %>% sort(decreasing = TRUE)
        escuela_top <- names(escuela_table_sorted)
        relevant_data <- subset(relevant_data, Institucion %in% escuela_top[1:10])
        
        relevant_data$Institucion <- factor(relevant_data$Institucion, levels(as.factor(escuela_top[1:10])))
        
        avg_igs_table <- tapply(X=relevant_data$IGS, INDEX=relevant_data$Institucion, FUN=mean) %>% round()
        escuela_counts_table <- table(relevant_data$Institucion)
        
        # print(avg_igs_table)
        
        institucion_df <- cbind(as.vector(names(avg_igs_table)), as.vector((avg_igs_table)), as.vector(escuela_counts_table)) %>% as.data.frame(stringsAsFactors = FALSE)
        colnames(institucion_df) <- c("Institucion", "Avg_IGS", "count")
        institucion_df$count <- as.integer(institucion_df$count)
        institucion_df$Avg_IGS <- as.integer(institucion_df$Avg_IGS)
        # Re-use earlier manipulation
        # programa_df$Institucion <- factor(programa_df$Institucion, levels = names(programa_char_lengths_top10))
        
        print(institucion_df)
        
        # Color scheme
        # Color blind palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # removed black
        color_blind_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        # color_scheme <- scale_colour_brewer(palette="Accent")
        # color_fill_scheme <- scale_fill_brewer(palette="Accent")
        color_scheme <- scale_colour_manual(values = color_blind_palette)
        color_fill_scheme <- scale_fill_manual(values = color_blind_palette)
        
        # legend position
        legend_position <- theme(legend.position = "top")
        
        # Second answer: http://stackoverflow.com/questions/10327267/annotation-above-bars
        # Also this one for ymax http://stackoverflow.com/questions/16821339/how-to-solve-the-ymax-not-defined
        
        base_plot <- ggplot(institucion_df, aes(x = Institucion,y = count, ymax = 1.1*max(count)))
        
        escuelas_plot <- base_plot + geom_bar(stat = "identity", fill=color_blind_palette[3],position = "dodge", width = 0.8) + geom_text(aes(label = paste("IGS =",Avg_IGS), x = Institucion, y = count), position = position_dodge(width = 0.75), vjust = -0.6, colour = "red") 
        escuelas_plot <- escuelas_plot + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab("Estudiantes") + xlab(NULL)
        escuelas_plot <- escuelas_plot + ggtitle(paste("Instituciones con mayor número de estudiantes admitidos a UPR -", input$campus, "en", input$year)) + theme(plot.title = element_text(size = 16, face = "bold"))
        escuelas_plot
        
        
    })
    
    output$populares_f <- renderPlot({
        relevant_data <- filter_data()
        relevant_data <- subset(relevant_data, Genero == "Femenino")
        relevant_data <- subset(relevant_data, Programa != "") # out of 69,300 records, about 1098 have Programa = ""
        
        # Filter to select the top 10 most popular programs
        programa_table_sorted <- table(relevant_data$Programa) %>% sort(decreasing = TRUE)
        programa_top <- names(programa_table_sorted)
        relevant_data <- subset(relevant_data, Programa %in% programa_top[1:10])
        
        # The names of each program (Programa) is LONG, so we want to plot
        # in increasing order of length of the "Bachillerato en..." strings
        # because they will be the values displayed on the x-axis and we want to save 
        # screen real estate. Originally I was going to plot "relevant_data".
        programa_char_lengths_top10 <- nchar(names(programa_table_sorted)[1:10])
        names(programa_char_lengths_top10) <- names(programa_table_sorted)[1:10]
        # programa_char_lengths_top10 will have the x-axis labels ordered by length of the strings "Bachillerato en ..."
        programa_char_lengths_top10 <- sort(programa_char_lengths_top10)
        
        relevant_data$Programa <- factor(relevant_data$Programa, names(programa_char_lengths_top10))
        
        avg_igs_table <- tapply(X=relevant_data$IGS, INDEX=relevant_data$Programa, FUN=mean) %>% round()
        program_counts_table <- table(relevant_data$Programa)
        
        programa_df <- cbind(as.vector(names(avg_igs_table)), as.vector((avg_igs_table)), as.vector(program_counts_table)) %>% as.data.frame(stringsAsFactors = FALSE)
        colnames(programa_df) <- c("Programa", "Avg_IGS", "count")
        programa_df$count <- as.integer(programa_df$count)
        programa_df$Avg_IGS <- as.integer(programa_df$Avg_IGS)
        # Re-use earlier manipulation
        programa_df$Programa <- factor(programa_df$Programa, levels = names(programa_char_lengths_top10))
        
        
        
        # Color scheme
        # Color blind palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # removed black
        color_blind_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        # color_scheme <- scale_colour_brewer(palette="Accent")
        # color_fill_scheme <- scale_fill_brewer(palette="Accent")
        color_scheme <- scale_colour_manual(values = color_blind_palette)
        color_fill_scheme <- scale_fill_manual(values = color_blind_palette)
        
        # legend position
        legend_position <- theme(legend.position = "top")
        
        # Second answer: http://stackoverflow.com/questions/10327267/annotation-above-bars
        # Also this one for ymax http://stackoverflow.com/questions/16821339/how-to-solve-the-ymax-not-defined
        
        base_plot <- ggplot(programa_df, aes(x = Programa,y = count, ymax = 1.1*max(count)))
        
        popular_plot <- base_plot + geom_bar(stat = "identity", fill=color_blind_palette[1],position = "dodge", width = 0.8) + geom_text(aes(label = paste("IGS =",Avg_IGS), x = Programa, y = count), position = position_dodge(width = 0.75), vjust = -0.6, colour = "red") 
        popular_plot <- popular_plot + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab("Estudiantes") + xlab(NULL)
        popular_plot <- popular_plot + ggtitle(paste("Programas más populares entre estudiantes femeninas admitidas a UPR -", input$campus, "en", input$year)) + theme(plot.title = element_text(size = 16, face = "bold"))
        popular_plot
        
    })
    
    output$populares_m <- renderPlot({
        relevant_data <- filter_data()
        relevant_data <- subset(relevant_data, Genero == "Masculino")
        relevant_data <- subset(relevant_data, Programa != "") # out of 69,300 records, about 1098 have Programa = ""
        
        # Filter to select the top 10 most popular programs
        programa_table_sorted <- table(relevant_data$Programa) %>% sort(decreasing = TRUE)
        programa_top <- names(programa_table_sorted)
        relevant_data <- subset(relevant_data, Programa %in% programa_top[1:10])
        
        # The names of each program (Programa) is LONG, so we want to plot
        # in increasing order of length of the "Bachillerato en..." strings
        # because they will be the values displayed on the x-axis and we want to save 
        # screen real estate. Originally I was going to plot "relevant_data".
        programa_char_lengths_top10 <- nchar(names(programa_table_sorted)[1:10])
        names(programa_char_lengths_top10) <- names(programa_table_sorted)[1:10]
        # programa_char_lengths_top10 will have the x-axis labels ordered by length of the strings "Bachillerato en ..."
        programa_char_lengths_top10 <- sort(programa_char_lengths_top10)
        
        relevant_data$Programa <- factor(relevant_data$Programa, names(programa_char_lengths_top10))
        
        avg_igs_table <- tapply(X=relevant_data$IGS, INDEX=relevant_data$Programa, FUN=mean) %>% round()
        program_counts_table <- table(relevant_data$Programa)
        
        programa_df <- cbind(as.vector(names(avg_igs_table)), as.vector((avg_igs_table)), as.vector(program_counts_table)) %>% as.data.frame(stringsAsFactors = FALSE)
        colnames(programa_df) <- c("Programa", "Avg_IGS", "count")
        programa_df$count <- as.integer(programa_df$count)
        programa_df$Avg_IGS <- as.integer(programa_df$Avg_IGS)
        # Re-use earlier manipulation
        programa_df$Programa <- factor(programa_df$Programa, levels = names(programa_char_lengths_top10))
        
        
        
        # Color scheme
        # Color blind palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # removed black
        color_blind_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        # color_scheme <- scale_colour_brewer(palette="Accent")
        # color_fill_scheme <- scale_fill_brewer(palette="Accent")
        color_scheme <- scale_colour_manual(values = color_blind_palette)
        color_fill_scheme <- scale_fill_manual(values = color_blind_palette)
        
        # legend position
        legend_position <- theme(legend.position = "top")
        
        # Second answer: http://stackoverflow.com/questions/10327267/annotation-above-bars
        # Also this one for ymax http://stackoverflow.com/questions/16821339/how-to-solve-the-ymax-not-defined
        
        base_plot <- ggplot(programa_df, aes(x = Programa,y = count, ymax = 1.1*max(count)))
        
        popular_plot <- base_plot + geom_bar(stat = "identity", fill=color_blind_palette[2],position = "dodge", width = 0.8) + geom_text(aes(label = paste("IGS =",Avg_IGS), x = Programa, y = count), position = position_dodge(width = 0.75), vjust = -0.6, colour = "red") 
        popular_plot <- popular_plot + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab("Estudiantes") + xlab(NULL)
        popular_plot <- popular_plot + ggtitle(paste("Programas más populares entre estudiantes masculinos admitidos a UPR -", input$campus, "en", input$year)) + theme(plot.title = element_text(size = 16, face = "bold"))
        popular_plot
        
    })
    
    output$selectivos <- renderPlot({
        relevant_data <- filter_data()
        relevant_data <- subset(relevant_data, Programa != "") # out of 69,300 records, about 1098 have Programa = ""
        
        # Sort by IGS
        avg_igs_table_sorted <- tapply(X=relevant_data$IGS, INDEX=relevant_data$Programa, FUN=mean) %>% round() %>% sort(decreasing=TRUE)
        
        programa_top_igs <- names(avg_igs_table_sorted)
        relevant_data <- subset(relevant_data, Programa %in% programa_top_igs[1:10])
        
        # The names of each program (Programa) is LONG, so we want to plot
        # in increasing order of length of the "Bachillerato en..." strings
        # because they will be the values displayed on the x-axis and we want to save 
        # screen real estate. Originally I was going to plot "relevant_data".
        programa_char_lengths_top10 <- nchar(names(avg_igs_table_sorted)[1:10])
        names(programa_char_lengths_top10) <- names(avg_igs_table_sorted)[1:10]
        # programa_char_lengths_top10 will have the x-axis labels ordered by length of the strings "Bachillerato en ..."
        programa_char_lengths_top10 <- sort(programa_char_lengths_top10)
        
        relevant_data$Programa <- factor(relevant_data$Programa, names(programa_char_lengths_top10))
        
        avg_igs_table <- tapply(X=relevant_data$IGS, INDEX=relevant_data$Programa, FUN=mean) %>% round()
        program_counts_table <- table(relevant_data$Programa)
        
        programa_df <- cbind(as.vector(names(avg_igs_table)), as.vector((avg_igs_table)), as.vector(program_counts_table)) %>% as.data.frame(stringsAsFactors = FALSE)
        colnames(programa_df) <- c("Programa", "Avg_IGS", "count")
        programa_df$count <- as.integer(programa_df$count)
        programa_df$Avg_IGS <- as.integer(programa_df$Avg_IGS)
        # Re-use earlier manipulation
        programa_df$Programa <- factor(programa_df$Programa, levels = names(programa_char_lengths_top10))
        
        
        
        # Color scheme
        # Color blind palette: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
        # removed black
        color_blind_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        # color_scheme <- scale_colour_brewer(palette="Accent")
        # color_fill_scheme <- scale_fill_brewer(palette="Accent")
        color_scheme <- scale_colour_manual(values = color_blind_palette)
        color_fill_scheme <- scale_fill_manual(values = color_blind_palette)
        
        # legend position
        legend_position <- theme(legend.position = "top")
        
        # Second answer: http://stackoverflow.com/questions/10327267/annotation-above-bars
        # Also this one for ymax http://stackoverflow.com/questions/16821339/how-to-solve-the-ymax-not-defined
        
        base_plot <- ggplot(programa_df, aes(x = Programa,y = count, ymax = 1.1*max(count)))
        
        selective_plot <- base_plot + geom_bar(stat = "identity", fill=color_blind_palette[3], position = "dodge", width = 0.8) + geom_text(aes(label = paste("IGS =",Avg_IGS), x = Programa, y = count), position = position_dodge(width = 0.75), vjust = -0.6, colour = "red") 
        selective_plot <- selective_plot + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab("Estudiantes") + xlab(NULL)
        selective_plot <- selective_plot + ggtitle(paste("Programas más selectivos entre estudiantes admitidos a UPR -", input$campus, "en", input$year)) + theme(plot.title = element_text(size = 16, face = "bold"))
        selective_plot
        
    })
    
    
    
    # Multiple plot function
    # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    #
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)

      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)

      numPlots = length(plots)

      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                        ncol = cols, nrow = ceiling(numPlots/cols))
      }

     if (numPlots==1) {
        print(plots[[1]])

      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
        }
      }
    }
})