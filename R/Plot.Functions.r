#' Plot Environmental Matrix
#'
#' This function creates a heatmap visualisation of an environmental matrix as produced by Sim.Space(). To keep rendering fast, only every ninth row and column of the matrix is plotted.
#'
#' @param Env_mat Environmental matrix as produced by Sim.Space(). Cells contain environmental values whereas column and row names contain spatial coordinates.
#'
#' @return A ggplot object visualising the (subsampled) environmental matrix as a heatmap.
#'
#' @import ggplot2
#'
#' @author Erik Kusch
#'
#' @examples
#' data("Env_mat")
#'
#' Plot.Environment(Env_mat)
#'
#' @export
Plot.Environment <- function(Env_mat) {
    Env_long <- as.data.frame(
        as.table(
            Env_mat[
                seq(from = 1, to = nrow(Env_mat), by = 9),
                seq(from = 1, to = ncol(Env_mat), by = 9)
            ]
        )
    )
    colnames(Env_long) <- c("Y", "X", "VALUES")
    Env_long <- apply(Env_long, 2, as.numeric)

    Env_gg <- ggplot(Env_long, aes(x = X, y = Y, fill = VALUES)) +
        geom_tile() +
        coord_fixed() +
        scale_fill_viridis_c(
            option = "C",
            name = "Environmental Value /\n Optimal Phenotype",
            guide = guide_colourbar(title.vjust = 0.75)
        ) +
        theme_bw() +
        theme(
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(1, "cm"),
            panel.background = element_rect(fill = "#2c2c2c", color = "#2c2c2c")
        )
    return(Env_gg)
}

#' Plot Association/Interaction Network Matrix
#'
#' This function creates a heatmap visualisation of an association/interaction network matrix as produced by Sim.Network().
#'
#' @param Network_mat A matrix object with association/interaction strength stored as cell values. Output of Sim.Network().
#'
#' @return A ggplot object visualising the network matrix as a heatmap of pairwise association/interaction strengths.
#'
#' @import ggplot2
#'
#' @author Erik Kusch
#'
#' @examples
#' data("Network_mat")
#'
#' Plot.NetMat(Network_mat)
#'
#' @export
Plot.NetMat <- function(Network_mat) {
    edg_df <- as.data.frame(as.table(as.matrix(Network_mat)))
    colnames(edg_df) <- c("Partner 1", "Partner 2", "Strength")
    NetMat_gg <- ggplot(edg_df, aes(x = `Partner 1`, y = `Partner 2`, fill = Strength)) +
        geom_tile(color = "black", lwd = 0.5, linetype = 1) +
        coord_fixed() +
        scale_fill_gradient2(
            low = "#5ab4ac",
            high = "#d8b365",
            name = "Association Strength",
            guide = guide_colourbar(title.vjust = 0.75)
        ) +
        theme_bw() +
        theme(
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(1, "cm"),
            panel.background = element_rect(fill = "#2c2c2c", color = "#2c2c2c"),
            axis.text.x = element_text(angle = -20, hjust = 0)
        )
    return(NetMat_gg)
}

#' Plot Individuals in Geographic Space
#'
#' This function visualises the spatial distribution of individuals of each species at a single simulation timestep. Data frames with zero rows (e.g. following a full extinction event) are handled gracefully and produce an empty (but valid) panel, which is useful when this function is called repeatedly across simulation timesteps (e.g. to build an animation).
#'
#' @param last_df Data frame of individuals with columns ID, Trait, X, Y, and Species, corresponding to a single simulation timestep. Typically an element of the list produced by Sim.Compute(). May have zero rows.
#' @param xlims Numeric vector of length 2 or NULL. Fixed X-axis limits. If NULL, limits are derived from last_df (or default to c(0, 1) if last_df has zero rows). Useful for keeping axes consistent when plotting a sequence of timesteps (e.g. for an animation).
#' @param ylims Numeric vector of length 2 or NULL. Fixed Y-axis limits. If NULL, limits are derived from last_df (or default to c(0, 1) if last_df has zero rows). Useful for keeping axes consistent when plotting a sequence of timesteps (e.g. for an animation).
#' @param species_levels Character or numeric vector or NULL. The full set of species (as they appear in the Species column, e.g. "Sp_01") observed across all timesteps of a simulation. If supplied, the Species factor (and hence colour/shape mapping and legend) is fixed to this set so that colours and shapes stay consistent across separately generated plots (e.g. successive frames of an animation) even as species go extinct. If NULL, levels are derived from last_df alone.
#'
#' @return A ggplot object visualising individual locations in geographic space, coloured and shaped by species.
#'
#' @import ggplot2
#'
#' @author Erik Kusch
#'
#' @examples
#' data("SimulationOutput")
#' last_df <- SimulationOutput[[length(SimulationOutput)]]
#'
#' Plot.IndivsInSpace(last_df)
#'
#' @export
Plot.IndivsInSpace <- function(last_df, xlims = NULL, ylims = NULL, species_levels = NULL) {
    Species_num <- as.numeric(gsub(last_df$Species, pattern = "Sp_", replacement = ""))
    ## when species_levels is supplied, the factor keeps a fixed set of levels so that
    ## colour/shape/legend mappings stay identical across separately produced plots,
    ## even for timesteps at which one or more species have gone extinct
    if (is.null(species_levels)) {
        last_df$Species <- factor(Species_num)
    } else {
        species_levels_num <- as.numeric(gsub(species_levels, pattern = "Sp_", replacement = ""))
        last_df$Species <- factor(Species_num, levels = sort(species_levels_num))
    }
    if (is.null(xlims)) {
        xlims <- if (nrow(last_df) > 0) c(min(last_df$X), max(last_df$X)) else c(0, 1)
    }
    if (is.null(ylims)) {
        ylims <- if (nrow(last_df) > 0) c(min(last_df$Y), max(last_df$Y)) else c(0, 1)
    }
    n_species <- nlevels(last_df$Species)
    Final_gg <- ggplot(
        last_df,
        aes(x = X, y = Y, col = Species, shape = Species)
    ) +
        geom_point() +
        scale_shape_manual(values = if (n_species > 0) seq_len(n_species) else integer(0), drop = FALSE) +
        scale_color_viridis_d(drop = FALSE) +
        xlim(
            xlims[1],
            xlims[2]
        ) +
        ylim(
            ylims[1],
            ylims[2]
        ) +
        theme_bw() +
        theme(legend.position = "top")
    return(Final_gg)
}

#' Plot Individuals Overlaid on Environmental Matrix
#'
#' This function visualises the spatial distribution of individuals of each species overlaid on top of the environmental matrix in which the simulation took place. The environmental matrix is rendered as a heatmap using a colour/fill gradient (as in Plot.Environment()), while individuals are drawn as white points with symbol shape (not colour) distinguishing species. Data frames with zero rows (e.g. following a full extinction event) are handled gracefully and produce a panel showing the environment alone.
#'
#' @param Env_mat Environmental matrix as produced by Sim.Space(). Cells contain environmental values whereas column and row names contain spatial coordinates.
#' @param last_df Data frame of individuals with columns ID, Trait, X, Y, and Species, corresponding to a single simulation timestep. Typically an element of the list produced by Sim.Compute(). May have zero rows.
#' @param xlims Numeric vector of length 2 or NULL. Fixed X-axis limits. If NULL, limits are derived from the column names of Env_mat. Useful for keeping axes consistent across successive calls (e.g. for an animation).
#' @param ylims Numeric vector of length 2 or NULL. Fixed Y-axis limits. If NULL, limits are derived from the row names of Env_mat. Useful for keeping axes consistent across successive calls (e.g. for an animation).
#' @param species_levels Character or numeric vector or NULL. The full set of species (as they appear in the Species column, e.g. "Sp_01") observed across all timesteps of a simulation. If supplied, the Species factor (and hence shape mapping and legend) is fixed to this set so that shapes stay consistent across separately generated plots (e.g. successive frames of an animation) even as species go extinct. If NULL, levels are derived from last_df alone.
#' @param fill_limits Numeric vector of length 2 or NULL. Fixed lower and upper bounds for the environmental value colourbar (fill scale). Values in Env_mat outside this range are clipped (squished) to the nearest bound rather than shown as missing data. Useful for keeping the colour scale consistent across successive calls (e.g. for an animation) or when perturbations shift the environmental range. If NULL, the scale defaults to the range of the (subsampled) environmental matrix.
#'
#' @return A ggplot object visualising the environmental matrix as a heatmap with individual locations overlaid as white points, shaped by species.
#'
#' @import ggplot2
#' @importFrom scales squish
#'
#' @author Erik Kusch
#'
#' @examples
#' data("Env_mat")
#' data("SimulationOutput")
#' last_df <- SimulationOutput[[length(SimulationOutput)]]
#'
#' Plot.IndivsOnEnv(Env_mat, last_df)
#'
#' # fixing the colourbar to a known range, e.g. across a set of perturbed simulations
#' Plot.IndivsOnEnv(Env_mat, last_df, fill_limits = c(0, 20))
#'
#' @export
Plot.IndivsOnEnv <- function(Env_mat, last_df, xlims = NULL, ylims = NULL, species_levels = NULL, fill_limits = NULL) {
    ## subsample the environmental matrix for fast rendering, as in Plot.Environment()
    Env_long <- as.data.frame(
        as.table(
            Env_mat[
                seq(from = 1, to = nrow(Env_mat), by = 9),
                seq(from = 1, to = ncol(Env_mat), by = 9)
            ]
        )
    )
    colnames(Env_long) <- c("Y", "X", "VALUES")
    Env_long <- as.data.frame(apply(Env_long, 2, as.numeric))

    if (is.null(xlims)) {
        xlims <- range(as.numeric(colnames(Env_mat)))
    }
    if (is.null(ylims)) {
        ylims <- range(as.numeric(rownames(Env_mat)))
    }

    ## fixing species factor levels (as in Plot.IndivsInSpace()) keeps shapes/legend
    ## consistent across separately produced plots, even as species go extinct
    Species_num <- as.numeric(gsub(last_df$Species, pattern = "Sp_", replacement = ""))
    if (is.null(species_levels)) {
        last_df$Species <- factor(Species_num)
    } else {
        species_levels_num <- as.numeric(gsub(species_levels, pattern = "Sp_", replacement = ""))
        last_df$Species <- factor(Species_num, levels = sort(species_levels_num))
    }
    n_species <- nlevels(last_df$Species)

    EnvIndivs_gg <- ggplot() +
        geom_tile(data = Env_long, aes(x = X, y = Y, fill = VALUES)) +
        scale_fill_viridis_c(
            option = "C",
            name = "Environmental Value /\n Optimal Phenotype",
            guide = guide_colourbar(title.vjust = 0.75),
            limits = fill_limits,
            oob = scales::squish
        ) +
        geom_point(
            data = last_df,
            aes(x = X, y = Y, shape = Species),
            color = "white"
        ) +
        scale_shape_manual(values = if (n_species > 0) seq_len(n_species) else integer(0), drop = FALSE) +
        ## coord_fixed(xlim=, ylim=) zooms the viewport without dropping any underlying
        ## data (unlike xlim()/ylim(), which would clip edge tiles whose rendered width
        ## straddles the boundary and silently remove them with a ggplot2 warning)
        coord_fixed(xlim = xlims, ylim = ylims) +
        theme_bw() +
        theme(
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.width = unit(2, "cm"),
            legend.key.height = unit(1, "cm"),
            panel.background = element_rect(fill = "#2c2c2c", color = "#2c2c2c")
        )
    return(EnvIndivs_gg)
}

#' Plot Species Abundances Through Time
#'
#' This function visualises the abundance trajectory of each species across simulation time.
#'
#' @param SimResult A named list of data frames, one per recorded simulation timestep, each containing individuals alive at that timestep with a Species column. List names must be coercible to numeric simulation timestamps. Output of Sim.Compute().
#' @param xlims Numeric vector of length 2 or NULL. Fixed X-axis limits. If NULL, limits are derived from the time series itself. Useful for keeping axes consistent across successive calls (e.g. for an animation).
#' @param ylims Numeric vector of length 2 or NULL. Fixed Y-axis limits. If NULL, limits are derived from the time series itself. Useful for keeping axes consistent across successive calls (e.g. for an animation).
#'
#' @return A ggplot object visualising the abundance of each species across simulation time as line plots.
#'
#' @import ggplot2
#'
#' @author Erik Kusch
#'
#' @examples
#' data("SimulationOutput")
#'
#' Plot.AbundTime(SimulationOutput)
#'
#' @export
Plot.AbundTime <- function(SimResult, xlims = NULL, ylims = NULL) {
    ## identify the full set of species observed at any point in the simulation so that timesteps at which a species has already gone extinct (including a possibly empty final data frame) are represented with an explicit abundance of 0 rather than being silently omitted
    All_Species <- sort(unique(unlist(lapply(SimResult, FUN = function(ID_iter) {
        unique(ID_iter$Species)
    }))))
    Abund_time <- lapply(names(SimResult),
        FUN = function(t) {
            ID_iter <- SimResult[[t]]
            Species_fac <- factor(ID_iter$Species, levels = All_Species)
            cbind(data.frame(table(Species_fac)), t)
        }
    )
    Abund_time <- do.call(rbind, Abund_time)
    Abund_time$t <- as.numeric(Abund_time$t)
    colnames(Abund_time)[1:2] <- c("Species", "Abundance")
    Abund_time$Species <- factor(as.numeric(gsub(Abund_time$Species,
        pattern = "Sp_", replacement = ""
    )))

    if (is.null(xlims)) {
        xlims <- range(Abund_time$t)
    }
    if (is.null(ylims)) {
        ylims <- c(0, max(Abund_time$Abundance))
    }

    AbundTime_gg <- ggplot(Abund_time, aes(x = t, y = Abundance, col = Species)) +
        geom_line(linewidth = 1.5) +
        scale_color_viridis_d() +
        theme_bw() +
        theme(legend.position = "bottom") +
        xlim(xlims) +
        ylim(ylims)
    return(AbundTime_gg)
}
