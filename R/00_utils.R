#sets data path
project_path <- "~/Desktop/Final Year Project/rdata-6COSC006W/"

#function that saves plots to local directory
save_plot <- function (plot_grid, width, height, file_name, save_path) {
  grid::grid.draw(plot_grid)
  #save the plot
  ggsave(filename=file_name, path = paste0(project_path, save_path),
                  plot=plot_grid, width=(width/72), height=(height/72),  bg="white")
}
