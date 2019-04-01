#sets data path
data_path <- '~/Desktop/Final Year Project/rdata-6COSC006W/Source/uowdata.csv'

#function that saves plots to local directory
save_plot <- function (plot_grid, width, height, file_name) {
  grid::grid.draw(plot_grid)
  #save the plot
  ggsave(filename=file_name, path = "~/Desktop/Final Year Project/rdata-6COSC006W/Charts/",
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}
  
  
  
