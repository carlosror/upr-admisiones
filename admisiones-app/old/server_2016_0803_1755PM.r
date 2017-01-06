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
        p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
            geom_line() +
            ggtitle("Growth curve for individual chicks")

        # Second plot
        p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
            geom_point(alpha=.3) +
            geom_smooth(alpha=.2, size=1) +
            ggtitle("Fitted growth curve per diet")

        # Third plot
        p3 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) +
            geom_density() +
            ggtitle("Final weight, by diet")

        # Fourth plot
        p4 <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet)) +
            geom_histogram(colour="black", binwidth=50) +
            facet_grid(Diet ~ .) +
            ggtitle("Final weight, by diet") +
            theme(legend.position="none")        # No legend (redundant in this graph)    
        multiplot(p1, p2, p3, p4, cols=2)
    })
    
    output$populares <- renderPlot({
        relevant_data <- filter_data()
        relevant_data <- subset(relevant_data, Genero == "Femenino")
        relevant_data <- subset(relevant_data, Programa != "")
        
        programa_table_sorted <- table(relevant_data$Programa) %>% sort(decreasing = TRUE)
        programa_top <- names(programa_table_sorted)
        relevant_data <- subset(relevant_data, Programa %in% programa_top[1:10])
        
        programa_char_lengths_top10 <- nchar(names(programa_table_sorted)[1:10])
        names(programa_char_lengths_top10) <- names(programa_table_sorted)[1:10]
        
        # print(programa_top[1:10])
        # print(names(programa_char_lengths_top10))
        
        
        programa_char_lengths_top10 <- sort(programa_char_lengths_top10)
        
        relevant_data$Programa <- factor(relevant_data$Programa, names(programa_char_lengths_top10))
        # levels(relevant_data$Programa) <- names(programa_char_lengths_top10)
        
        avg_igs_table <- tapply(X=relevant_data$IGS, INDEX=relevant_data$Programa, FUN=mean) %>% round()
        # print(avg_igs_table)
        # avg_igs <- as.data.frame.matrix(avg_igs_table)
        # print(cbind(avg_igs, names(avg_igs_table)))
        # print(dimnames(avg_igs))
        # colnames(avg_igs) <- c("Programa", "Avg")
        program_counts_table <- table(relevant_data$Programa)
        # program_counts <- as.data.frame(program_counts)
        # colnames(program_counts) <- c("Programa", "Sums")
        # print(merge(program_counts, avg_igs))
        # print(program_counts)
        # print(avg_igs)
        
        programa_df <- cbind(as.vector(names(avg_igs_table)), as.vector(round(avg_igs)), as.vector(program_counts_table)) %>% as.data.frame(stringsAsFactors = FALSE)
        colnames(programa_df) <- c("Programa", "Avg_IGS", "count")
        programa_df$count <- as.integer(programa_df$count)
        programa_df$Avg_IGS <- as.integer(programa_df$Avg_IGS)
        print(str(programa_df))
        
        # print(cbind(names(avg_igs), program_counts, avg_igs))
        # print(tapply(X=relevant_data$IGS, INDEX=relevant_data$Programa, FUN=mean))
        
        
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
        
        # populares_barplot_count <- base_plot + geom_bar( aes(x= Programa), fill=color_blind_palette[1]) + color_fill_scheme + ylab("Estudiantes") + labs(fill=NULL) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
        populares_barplot_igs <- base_plot + geom_bar( aes(x= Programa, y= IGS), stat = "identity", fill=color_blind_palette[1]) + color_fill_scheme + ylab("Estudiantes") + labs(fill=NULL) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + legend_position
        dodgewidth <- position_dodge(width=0.9)  
        # print(str(relevant_data))
        # populares_barplot_count <- ggplot(programa_df,aes(x = Programa, y = count)) + geom_bar(colour = "black", position = dodgewidth ,stat = "identity") + stat_bin(geom="text", aes(label=count, vjust=-1))
        ggplot(programa_df, aes(x = Programa,y = count)) + geom_bar(stat = "identity", colour = "black",position = "dodge", width = 0.8) + geom_text(aes(label = Avg_IGS, x = Programa, y = count), position = position_dodge(width = 0.8), vjust = -0.6)
        # populares_barplot_count <- base_plot + geom_bar( aes(x= Programa), fill=color_blind_palette[1]) + color_fill_scheme + ylab("Estudiantes") + annotate("text", x = "Bachillerato en Sistemas de Oficina", label="hello")
        # populares_barplot_count
        # multiplot(populares_barplot_count, populares_barplot_igs, cols=1)
        
    })
    
    output$populares2 <- renderPlot({
        relevant_data <- filter_data()
        relevant_data <- subset(relevant_data, Genero == "Masculino")
        relevant_data <- subset(relevant_data, Programa != "")
        
        programa_table_sorted <- table(relevant_data$Programa) %>% sort(decreasing = TRUE)
        programa_top <- names(programa_table_sorted)
        relevant_data <- subset(relevant_data, Programa %in% programa_top[1:10])
        
        programa_char_lengths_top10 <- nchar(names(programa_table_sorted)[1:10])
        names(programa_char_lengths_top10) <- names(programa_table_sorted)[1:10]
        
        print(programa_top[1:10])
        # print(names(programa_char_lengths_top10))
        
        
        programa_char_lengths_top10 <- sort(programa_char_lengths_top10)
        
        relevant_data$Programa <- factor(relevant_data$Programa, names(programa_char_lengths_top10))
        # levels(relevant_data$Programa) <- names(programa_char_lengths_top10)
        
        
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
        
        populares_barplot <- base_plot + geom_bar( aes(x= Programa), fill=color_blind_palette[2]) + color_fill_scheme + xlab("IGS") + ylab("Estudiantes") + labs(fill=NULL) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + legend_position
        populares_barplot
        
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