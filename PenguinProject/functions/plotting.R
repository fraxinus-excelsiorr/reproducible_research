## ---------------------------
##
## Script name: plotting.R
##
## Purpose of script: 
##      Plotting penguin data as a scatterplot:
##      Specifically bill length against depth, and organised by species
##
## Author: Candidate number 1062369
##
## Date Created: 2023-12-03
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------



# creating a function which plots bill length against depth
plot_bill_figure <- function(penguins_data){
  penguins_data %>% 
    
    #creating the figure
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, colour=species, fill=species)) +
    
    #setting a title and axes labels
    labs(title='Penguin Bill Length vs Bill Depth', x='Bill length (mm)', y='Bill depth (mm)',color = "Species") +
    
    #setting the figure as a scatterplot
    geom_point(alpha = 0.7) +
    
    #adding regression lines and setting their colour according to species
    geom_smooth(method = "lm", alpha = 0.2, show.legend = FALSE) +
    
    #setting the colours for each species and saying we want only one figure legend
    scale_color_manual(values = c("Adelie" = "#1b9e77", "Chinstrap" = "#d95f02", "Gentoo" = "#7570b3"),
                       guide = guide_legend(title = "Species")) + 
    scale_fill_manual(values = c("Adelie" = "#1b9e77", "Chinstrap" = "#d95f02", "Gentoo" = "#7570b3"),
                      guide = guide_legend(title = "Species")) +
    
    #setting a theme for the figure
    theme_bw()
}



#####################

# creating a function which plots bill length against depth for adelie penguins

plot_figure_one <- function(adelie){
  adelie %>% 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point(alpha = 0.8, colour="#1b9e77") +
    geom_smooth(method="lm", colour="#1b9e77", fill="#1b9e77", alpha=0.2) +
    labs(title='Adelie Bill Length vs Bill Depth', x='Bill length (mm)', y='Bill depth (mm)') +
    theme_bw()
}


# creating a function which plots bill length against depth for chinstrap penguins

plot_figure_two <- function(chinstrap){
  chinstrap %>% 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point(alpha = 0.7, colour="#d95f02") +
    geom_smooth(method="lm", colour="#d95f02", fill="#d95f02", alpha=0.2) +
    labs(title='Chinstrap Bill Length vs Bill Depth', x='Bill length (mm)', y='Bill depth (mm)') +
    theme_bw()
}


# creating a function which plots bill length against depth for gentoo penguins
plot_figure_three <- function(gentoo){
  gentoo %>% 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point(alpha = 0.8, colour="#7570b3") +
    geom_smooth(method="lm", colour="#7570b3", fill="#7570b3", alpha=0.2) +
    labs(title='Gentoo Bill Length vs Bill Depth', x='Bill length (mm)', y='Bill depth (mm)') +
    theme_bw()
}


####################

#creating a function to save figure 1 as a png
save_fig1_png <- function(penguins_data, 
                                 filename, size, res, scaling){
  agg_png("figures/fig01_20x20_scaled.png", width = 20, height = 20, units = "cm", 
          res = 600, scaling = 1.8)
  figure1 <- plot_figure_one(adelie)
  print(figure1)
  dev.off()
}

#creating a function to save figure 1 as a svg
save_fig1_svg <- function(penguins_data, 
                                 filename, size, scaling){
  size_inches = size/2.54
  svglite("figures/fig01_20x20_scaled.svg", width = 20, height = 20, scaling = 1.8)
  figure1 <- plot_figure_one(adelie)
  print(figure1)
  dev.off()
}



#creating a function to save figure 2 as a png
save_fig2_png <- function(penguins_data, 
                          filename, size, res, scaling){
  agg_png("figures/fig02_20x20_scaled.png", width = 20, height = 20, units = "cm", 
          res = 600, scaling = 1.8)
  figure2 <- plot_figure_two(chinstrap)
  print(figure2)
  dev.off()
}

#creating a function to save figure 2 as a svg
save_fig2_svg <- function(penguins_data, 
                          filename, size, scaling){
  size_inches = size/2.54
  svglite("figures/fig02_20x20_scaled.svg", width = 20, height = 20, scaling = 1.8)
  figure2 <- plot_figure_two(chinstrap)
  print(figure2)
  dev.off()
}


#creating a function to save figure 3 as a png
save_fig3_png <- function(penguins_data, 
                          filename, size, res, scaling){
  agg_png("figures/fig03_20x20_scaled.png", width = 20, height = 20, units = "cm", 
          res = 600, scaling = 1.8)
  figure3 <- plot_figure_three(gentoo)
  print(figure3)
  dev.off()
}

#creating a function to save figure 3 as a svg
save_fig3_svg <- function(penguins_data, 
                          filename, size, scaling){
  size_inches = size/2.54
  svglite("figures/fig03_20x20_scaled.svg", width = 20, height = 20, scaling = 1.8)
  figure3 <- plot_figure_three(gentoo)
  print(figure3)
  dev.off()
}



# creating a function to save figure 4 as a png
save_bill_figure_png <- function(penguins_data, 
                                  filename, size, res, scaling){
  agg_png("figures/fig04_30x20_scaled.png", width = 30, height = 20, units = "cm", 
          res = 600, scaling = 1.8)
  bill_scatterplot <- plot_bill_figure(penguins_data)
  print(bill_scatterplot)
  dev.off()
}


# creating a function to save the figure 4 as a svg
save_bill_figure_svg <- function(penguins_data, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite("figures/fig04_30x20_scaled.svg", width = 30, height = 20, scaling = 1.8)
  bill_scatterplot <- plot_bill_figure(penguins_data)
  print(bill_scatterplot)
  dev.off()
}




