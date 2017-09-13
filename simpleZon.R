local(
  {
    build_raster <-
      function(n, n_layers, shape1 = 1, shape2 = 1) {
        dist <- array(NA_real_, dim = c(n + 2, n + 2, n_layers))
        dist[-c(1, n + 2), -c(1, n + 2), ] <-
          array(
            replicate(n_layers, rbeta(n ^ 2, shape1, shape2)),
            dim = c(n, n, n_layers)
          )
        dist
      }
    
    apr <-
      function(loss, pln, fs) {
        mean(
          raster::cellStats(fs * (pln$rasters$rank >= loss), sum) /
            raster::cellStats(fs, sum)
        )
      }
    
    n <- 5
    n_layers <- 2L
    n_species <- 2L
    loss <- ((n ^ 2L) - 1L) / (n ^ 2L)
    
    set.seed(0)
    
    features <- replicate(n_species, build_raster(n, n_layers))
    
    dist_features <- apply(features, 3, raster::brick)
    
    dist_plans <-
      lapply(
        dist_features,
        function(x) {
          rzonation::zonation(
            x,
            settings = list(
              "removal rule" = 2, "edge removal" = 0, "warp factor" = 1
            )
          )
        }
      )
    
    plot_matrix <-
      function(x, y, labels) {
        nrow = 5
        ncol = 5
        cell_size <- rep(.1, length.out =  2)
        
        text(
          rep(
            seq(x, by = cell_size[1], length.out = ncol) + cell_size[1] / 2,
            ncol
          ),
          rep(
            seq(y, by = -cell_size[2], length.out = nrow) - cell_size[2] / 2,
            each = nrow
          ),
          labels = labels
        )
        segments(
          c(rep(x, ncol + 1), seq(x, by = cell_size[1], length.out = ncol + 1)),
          c(seq(y, by = -cell_size[2], length.out = nrow + 1), rep(y, nrow + 1)),
          c(
            rep(x + cell_size[1] * ncol, ncol + 1),
            seq(x, by = cell_size[1], length.out = ncol + 1)
          ),
          c(
            seq(y, by = -cell_size[2], length.out = nrow + 1),
            rep(y - cell_size[2] * nrow, nrow + 1)
          )
        )
      }
    
    make_rect = function(x, y, cell, col) {
      cell_size = rep(.1, length.out =  2)
      grey_cell_ind <- arrayInd(cell, .dim = c(5, 5))
      rect(
        x + grey_cell_ind[1] * cell_size[1] - cell_size[1],
        y - grey_cell_ind[2] * cell_size[2],
        x + grey_cell_ind[1] * cell_size[1],
        y - grey_cell_ind[2] * cell_size[2] + cell_size[2],
        col = do.call(rgb, as.list(c(col2rgb(col)/ 255, .4))),
        border = NA
      )
    }
  
    tbl <- mapply(
      function(x, y) na.omit(as.integer(t(features[, , x, y]) * 100)),
      rep(1:2, 2),
      rep(1:2, each = 2)
    )
    
    ranks <- sapply(
      seq_len(n_layers),
      function(x) {
        rank(
          -na.omit(
            as.numeric(t(raster::as.matrix(dist_plans[[x]]$rasters$rank)))
          )
        )
      }
    )
    
    par(mar = rep(0, 4), ps = 14)
    plot.new()
    plot.window(xlim = c(0, 3.5), ylim = c(0, 1.5))

    text(.35, 1.4, "Species 1", pos = 3, cex = 1.3)
    plot_matrix(.1, 1.3, tbl[, 1])
    dev.print(svg, width = 10, height = 7, family = "Droid Serif", 
              filename = "voi-seminar_files/figure-html/simpleZon1.svg")
    
    text(.95, 1.4, "Species 2", pos = 3, cex = 1.3)
    plot_matrix(.7, 1.3, tbl[, 3])
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon2.svg")
    
    text(1.65, 1.4, "Rank", pos = 3, cex = 1.3)
    plot_matrix(1.4, 1.3, ranks[, 1])
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon3.svg")
    
    make_rect(.1, 1.3, which.min(ranks[, 1]), "dark olive green")
    make_rect(.7, 1.3, which.min(ranks[, 1]), "dark olive green")
    make_rect(1.4, 1.3, which.min(ranks[, 1]), "dark olive green")
    
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon4.svg")
    
    plot_matrix(.1, .6, tbl[, 2])
    plot_matrix(.7, .6, tbl[, 4])
    plot_matrix(1.4, .6, ranks[, 2])
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon5.svg")
    
    make_rect(.1, .6, which.min(ranks[, 2]), "dark olive green")
    make_rect(.7, .6, which.min(ranks[, 2]), "dark olive green")
    make_rect(1.4,.6, which.min(ranks[, 2]), "dark olive green")
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon6.svg")
    
    
    par(mar = rep(0, 4), ps = 14)
    plot.new()
    plot.window(xlim = c(0, 3.5), ylim = c(0, 1.5))
    
    text(.35, 1.4, "Species 1", pos = 3, cex = 1.3)
    plot_matrix(.1, 1.3, tbl[, 1])

    text(.95, 1.4, "Species 2", pos = 3, cex = 1.3)
    plot_matrix(.7, 1.3, tbl[, 3])

    text(1.65, 1.4, "Rank", pos = 3, cex = 1.3)
    plot_matrix(1.4, 1.3, ranks[, 1])

    make_rect(.1, 1.3, which.min(ranks[, 1]), "salmon")
    make_rect(.7, 1.3, which.min(ranks[, 1]), "salmon")
    make_rect(1.4, 1.3, which.min(ranks[, 1]), "salmon")
    
    plot_matrix(.1, .6, tbl[, 2])
    plot_matrix(.7, .6, tbl[, 4])
    plot_matrix(1.4, .6, ranks[, 2])

    make_rect(.1, .6, which.min(ranks[, 1]), "salmon")
    make_rect(.7, .6, which.min(ranks[, 1]), "salmon")

    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon7.svg")
    
    par(mar = rep(0, 4), ps = 14)
    plot.new()
    plot.window(xlim = c(0, 3.5), ylim = c(0, 1.5))
    
    text(.35, 1.4, "Species 1", pos = 3, cex = 1.3)
    plot_matrix(.1, 1.3, tbl[, 1])
    
    text(.95, 1.4, "Species 2", pos = 3, cex = 1.3)
    plot_matrix(.7, 1.3, tbl[, 3])
    
    text(1.65, 1.4, "Rank", pos = 3, cex = 1.3)
    plot_matrix(1.4, 1.3, ranks[, 1])
    
    make_rect(.1, 1.3, which.min(ranks[, 2]), "salmon")
    make_rect(.7, 1.3, which.min(ranks[, 2]), "salmon")

    plot_matrix(.1, .6, tbl[, 2])
    plot_matrix(.7, .6, tbl[, 4])
    plot_matrix(1.4, .6, ranks[, 2])
    
    make_rect(.1, .6, which.min(ranks[, 2]), "salmon")
    make_rect(.7, .6, which.min(ranks[, 2]), "salmon")
    make_rect(1.4, .6, which.min(ranks[, 2]), "salmon")
    
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon8.svg")
    
    par(mar = rep(0, 4), ps = 14)
    plot.new()
    plot.window(xlim = c(0, 3.5), ylim = c(0, 1.5))
    
    text(.35, 1.4, "Species 1", pos = 3, cex = 1.3)
    plot_matrix(.1, 1.3, tbl[, 1])
    
    text(.95, 1.4, "Species 2", pos = 3, cex = 1.3)
    plot_matrix(.7, 1.3, tbl[, 3])
    
    text(1.65, 1.4, "Rank", pos = 3, cex = 1.3)
    plot_matrix(1.4, 1.3, ranks[, 1])
    
    make_rect(.1, 1.3, which.min(ranks[, 1]), "dodger blue")
    make_rect(.7, 1.3, which.min(ranks[, 1]), "dodger blue")
    make_rect(1.4, 1.3, which.min(ranks[, 1]), "dodger blue")
    
    plot_matrix(.1, .6, tbl[, 2])
    plot_matrix(.7, .6, tbl[, 4])
    plot_matrix(1.4, .6, ranks[, 2])
    
    make_rect(.1, .6, which.min(ranks[, 2]), "dodger blue")
    make_rect(.7, .6, which.min(ranks[, 2]), "dodger blue")
    make_rect(1.4, .6, which.min(ranks[, 2]), "dodger blue")
    dev.print(svg, width = 10, height = 7, family = "Droid Serif",
              filename = "voi-seminar_files/figure-html/simpleZon9.svg")
    
  }
)
