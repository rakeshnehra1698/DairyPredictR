#' Plot Model Performance
#'
#' Create publication-quality performance plots for
#' machine learning models.
#'
#' @param results Output from compare_models().
#' @param metric Character. Metric to plot.
#' One of "all", "R2", "RMSE", "MAE", "MAPE", or "Bias".
#' @param save Logical. Save figures?
#' @param path Folder to save figures.
#' @param width Figure width (inches).
#' @param height Figure height (inches).
#' @param dpi Figure resolution.
#'
#' @return A ggplot object or a list of ggplot objects.
#' @importFrom rlang .data
#' @export

plot_performance <- function(
    results,
    metric = "all",
    save = FALSE,
    path = getwd(),
    width = 7,
    height = 5,
    dpi = 600
) {
  
  # ----------------------------------------------------------
  # CHECK INPUT
  # ----------------------------------------------------------
  
  if (!is.data.frame(results)) {
    stop(
      "results must be the output of compare_models()."
    )
  }
  
  
  # ----------------------------------------------------------
  # AVAILABLE METRICS
  # ----------------------------------------------------------
  
  available_metrics <- c(
    "R2",
    "RMSE",
    "MAE",
    "MAPE",
    "Bias"
  )
  
  
  # ----------------------------------------------------------
  # CHECK METRIC
  # ----------------------------------------------------------
  
  if (!(metric %in% c("all", available_metrics))) {
    
    stop(
      paste(
        "metric must be one of:",
        paste(
          c("all", available_metrics),
          collapse = ", "
        )
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # CHECK REQUIRED COLUMNS
  # ----------------------------------------------------------
  
  required_columns <- c(
    "Model",
    available_metrics
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(results)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste(
        "Missing required column(s):",
        paste(
          missing_columns,
          collapse = ", "
        )
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # MODEL ORDER
  # ----------------------------------------------------------
  
  model_order <- c(
    "Linear Regression",
    "Random Forest",
    "Gradient Boosting Machine",
    "Support Vector Machine",
    "Artificial Neural Network"
  )
  
  
  # ----------------------------------------------------------
  # CREATE ONE PLOT
  # ----------------------------------------------------------
  
  create_plot <- function(metric_name) {
    
    # Keep only valid values
    
    plot_data <- results[
      !is.na(results[[metric_name]]),
      ,
      drop = FALSE
    ]
    
    
    if (nrow(plot_data) == 0) {
      
      stop(
        paste(
          "No valid values available for",
          metric_name
        )
      )
      
    }
    
    
    # --------------------------------------------------------
    # Preserve predefined model order
    # --------------------------------------------------------
    
    plot_data$Model <- factor(
      plot_data$Model,
      levels = model_order
    )
    
    
    # Remove models not present in predefined order
    
    plot_data <- plot_data[
      !is.na(plot_data$Model),
      ,
      drop = FALSE
    ]
    
    
    # --------------------------------------------------------
    # Plot
    # --------------------------------------------------------
    
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$Model,
        y = .data[[metric_name]]
      )
    ) +
      
      ggplot2::geom_col(
        width = 0.7
      ) +
      
      ggplot2::geom_text(
        ggplot2::aes(
          label = round(
            .data[[metric_name]],
            3
          )
        ),
        vjust = -0.3,
        size = 4
      ) +
      
      ggplot2::labs(
        title = paste(
          metric_name,
          "Comparison"
        ),
        x = "Machine Learning Algorithm",
        y = metric_name
      ) +
      
      ggplot2::theme_classic(
        base_size = 14
      ) +
      
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 30,
          hjust = 1
        )
      )
    
    
    # --------------------------------------------------------
    # SAVE FIGURES
    # --------------------------------------------------------
    
    if (save) {
      
      if (!dir.exists(path)) {
        
        dir.create(
          path,
          recursive = TRUE
        )
        
      }
      
      
      ggplot2::ggsave(
        filename = file.path(
          path,
          paste0(metric_name, ".png")
        ),
        plot = p,
        width = width,
        height = height,
        dpi = dpi
      )
      
      
      ggplot2::ggsave(
        filename = file.path(
          path,
          paste0(metric_name, ".pdf")
        ),
        plot = p,
        width = width,
        height = height
      )
      
      
      ggplot2::ggsave(
        filename = file.path(
          path,
          paste0(metric_name, ".tiff")
        ),
        plot = p,
        width = width,
        height = height,
        dpi = dpi,
        compression = "lzw"
      )
      
    }
    
    
    return(p)
    
  }
  
  
  # ----------------------------------------------------------
  # PLOT ALL OR SINGLE METRIC
  # ----------------------------------------------------------
  
  if (metric == "all") {
    
    plots <- lapply(
      available_metrics,
      create_plot
    )
    
    names(plots) <- available_metrics
    
    return(plots)
    
  }
  
  
  return(
    create_plot(metric)
  )
  
}