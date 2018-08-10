#------------------------------------------------------------------------------
# Program Name:    user_data_diagnostics.R
# Author:          Caleb Braun
# Last Updated:    August 2018
# Program Purpose: Provide support functions for comparing user-defined datasets
#                  in the historical energy extension to the default activity
#                  data.
#
# Output Files:
#   diagnostic-output/user-data/[comparison-description].png
#   diagnostic-output/user-data/[aggregate-comparison].csv
# -----------------------------------------------------------------------------


# Compare new CEDS data to older or original CEDS data
#
# Generate diagnostic charts and files showing the difference between the
# original data and an altered dataset. The types of charts are based on the
# aggregation level of the data.
#
# Args:
#   new_CEDS: A data.frame of CEDS data
#   old_CEDS: A data.frame of CEDS data, at an equal or more disaggregate level
#   src_new: String describing the source of new_CEDS
#   src_old: String describing the source of old_CEDS
#
# Returns:
#   NULL - saves charts to the diagnostic output directory
compareToDefault <- function( new_CEDS, old_CEDS, src_new, src_old = 'default' ) {

    # Get aggregation level of new data
    agg_cols <- aggLevelToCols( identifyLevel( new_CEDS ) )
    stopifnot( all( agg_cols %in% names( old_CEDS ) ) )

    # We only want to compare values from one country at a time
    if ( length( unique( new_CEDS$iso ) ) > 1 ) {
        lapply( split( new_CEDS, new_CEDS$iso ), compareToDefault, old_CEDS,
                src_new, src_old )
        return( invisible( NULL ) )
    }

    # Format iso for plot titles
    ISO <- toupper( new_CEDS$iso[1] )

    # TODO: Plot type or facet row/col number adjustments based on data agg lvl
    FACET_ROW_MAX <- 4
    FACET_COL_MAX <- 7

    # For use with tidyeval
    src_oldQ <- captureForNSE( src_old )
    src_newQ <- captureForNSE( src_new )

    # Convert both data sets to long form with same aggregation level
    new_long <- tidyr::gather( new_CEDS, 'year', !!src_new, matches( 'X\\d{4}' ) )
    old_long <- old_CEDS %>%
        dplyr::semi_join( new_CEDS, by = agg_cols ) %>%
        dplyr::select( names( new_CEDS ) ) %>%
        tidyr::gather( 'year', 'default', matches( 'X\\d{4}' ) ) %>%
        dplyr::group_by_at( c( agg_cols, 'year') ) %>%
        dplyr::summarise( !!src_old := sum( default ) ) %>%
        dplyr::ungroup()

    combined <- old_long %>%
        dplyr::left_join( new_long, by = c( agg_cols, 'year' ) ) %>%
        dplyr::mutate( year = as.integer( substr( year, 2, 5 ) ) )

    f_disagg <- if ( 'CEDS_fuel' %in% agg_cols ) 'CEDS_fuel' else 'agg_fuel'

    plot_factors <- list(
        list( fuel = 'agg_fuel', sector = 'CEDS_sector', type = 'heatmap' ),
        list( fuel = f_disagg,   sector = 'agg_sector',  type = 'linegraph' ),
        list( fuel = f_disagg,   sector = NULL,          type = 'linegraph' )
    )

    lapply( plot_factors, function(pf) {
        if ( all( c( pf$sector, pf$fuel ) %in% agg_cols ) ) {
            plot_title <- paste( ISO, pf$sector, 'by', pf$fuel )
            fname <- gsub( ' +', '-', paste( src_new, 'vs', src_old, plot_title ) )
            plotComp( combined, pf$fuel, pf$sector, plot_title, fname,
                      src_oldQ, src_newQ, pf$type )
        }
    })

    out_csv <- gsub( ' +', '-', paste( src_new, 'vs', src_old, ISO ) )
    writeData( combined, 'DIAG_OUT', out_csv, domain_extension = 'user-data/', meta = F )

    return( invisible( NULL ) )
}


# Calculate the percent difference between two columns
#
# Add an additional column `pct_diff` to the input data.frame containing the
# percentage difference between `c1` and `c2`.
#
# Args:
#   df: The data to work with
#   c1: Column to calculate difference from
#   c2: Column to calculate difference with
#
# Returns:
#   The input data.frame with an additional `pct_diff` column
calcPercentDiff <- function(df, c1, c2) {
    c1 <- captureForNSE( c1 )
    c2 <- captureForNSE( c2 )

    dplyr::mutate( df,
                   pct_diff = ( ( !!c2 - !!c1 ) / !!c1 ) * 100,
                   pct_diff = if_else( !!c1 == 0 & !!c2 == 0, 0, pct_diff ) )
}


# Fetch font for CEDS themes
#
# Add custom font to available system fonts.
#
# Returns:
#   The name of the font to call in the theme
themeCEDS.font <- function() {
    if (.Platform$OS.type == "windows") {
        windowsFonts(fnt = windowsFont("Century Gothic"))
        fnt <- "fnt"
    } else {
        fnt <- "sans"
    }
    # Linux uses X11Fonts() ?

    fnt
}


# Create custom theme for line graphs
#
# For use in functions that create line graphs with ggplot.
#
# Returns:
#   A ggplot2 theme object
themeCEDS.linegraph <- function() {
    BACKGROUND <- "white"
    FACET_FILL <- "#f2f2f2"
    FACET_FONT <- "serif"
    BORDER <- "#9a9a9a"

    fnt <- themeCEDS.font()
    theme_bw(base_size=10, base_family=fnt) %+replace%
    theme(
        panel.background = element_rect(fill=BACKGROUND, color = "grey88"),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(fill = NA, color = BORDER),

        strip.background = element_rect(fill=FACET_FILL, color=BORDER),
        strip.text       = element_text(family = FACET_FONT, size = 9,
                                        margin = margin(t=1, r=0, b=4, l=0)),

        plot.background = element_rect(fill="transparent", color=BACKGROUND),
        plot.title      = element_text(hjust = 0.5, size = 14,
                                       margin = margin(t=8, r=0, b=15, l=0)),

        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA)
    )
}


# Create custom theme for heatmaps
#
# For use in functions that create heatmaps with ggplot.
#
# Returns:
#   A ggplot2 theme object
themeCEDS.heatmap <- function() {
    fnt <- themeCEDS.font()
    theme_bw(base_size=11, base_family=fnt) %+replace%
        theme(
            plot.background = element_rect(fill="transparent", colour=NA),
            plot.title      = element_text(hjust = 0.5, size = 14,
                                           margin = margin(t=8, r=0, b=15, l=0)),
            panel.background  = element_blank(),
            panel.grid        = element_blank(),
            legend.background = element_rect(fill="transparent", colour=NA),
            legend.key        = element_rect(fill="transparent", colour=NA)
        )
}


# Build a line graph
#
# Create a line graph or multiple facetted line graphs using the CEDS custom
# theme.
#
# Args:
#   df: Data to plot
#   x: Column to plot on x-axis
#   y: Column to plot on y-axis
#   group: Grouping variable
#   title: Plot title
#   facet.x: Column to use for creating facet rows
#   facet.y: Column to use for creating facet columns
#
# Returns:
#   A ggplot object
cedsLinegraph <- function(df, x, y, group, title, facet.x = NULL, facet.y = NULL) {
    x_var <- captureForNSE(x)
    y_var <- captureForNSE(y)
    group_var <- captureForNSE(group)
    facet.x <- captureForNSE(facet.x)
    facet.y <- captureForNSE(facet.y)

    caption <- "Lighter shade represents the original data."

    if (is.null(facet.x)) {
        fwrap <- NULL
    } else if (is.null(facet.y)) {
        fwrap <- facet_wrap(vars(!!facet.x), scales = 'free_y', ncol=7)
    } else {
        fwrap <- facet_wrap(vars(!!facet.x, !!facet.y), scales = 'free_y', ncol=7)
    }

    ggplot(data = df, mapping = aes(x = !!x_var, y = !!y_var)) +
        geom_line(aes(group = !!group_var, color = !!group_var)) +
        scale_color_brewer(palette = 'Paired') +
        scale_y_continuous(limits = c(0, NA)) +
        fwrap +
        labs(y = "kt", title = title, caption = caption ) +
        themeCEDS.linegraph()
}


# Build a heatmap for showing percent differences
#
# Create a single heatmap or facetted heatmaps using the CEDS custom theme.
#
# Args:
#   df: Data to plot
#   x: Column to plot on x-axis
#   y: Column to plot on y-axis
#   value: Column containing heatmap values
#   title: Plot title
#   facet: Column to use for creating facet columns
#   labs: Add labels showing the value of each box? One of ['on', 'off', 'auto']
#
# Returns:
#   A ggplot object
cedsHeatmap <- function(df, x, y, value, title, facet = NULL, labs = 'auto') {
    MAX_LABEL_ROW <- 30
    MAX_LABEL_COL <- 100

    NA_COLOR <- 'grey45'
    BOX_COLOR <- '#aaaaaa'
    FILL_COLOR <- c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
    FILL_COLOR <- c('#0571b0', '#92c5de', '#f7f7f7', '#f4a582', '#ca0020')

    x_var <- captureForNSE(x)
    y_var <- captureForNSE(y)
    val_var <- captureForNSE(value)
    facet <- captureForNSE(facet)

    x_len <- length(unique(dplyr::pull(df, !!x_var)))
    y_len <- length(unique(dplyr::pull(df, !!y_var)))

    if (is.null(facet)) {
        n_fac <- 1
        fgrid <- NULL
    } else {
        n_fac <- length(unique(dplyr::pull(df, !!facet)))
        fgrid <- facet_grid(cols = vars(!!facet))
    }

    # Round values for labels
    df <- dplyr::mutate(df, label = round(!!val_var), value = !!val_var)

    # Only label boxes if the plot is under the max number of rows and cols
    if (labs == 'on' || (labs == 'auto' &&
                         x_len * n_fac <= MAX_LABEL_COL &&
                         y_len * n_fac <= MAX_LABEL_ROW)) {
        gtext <- geom_text(aes(label = label), size = 3, angle = 90, na.rm = T)
    } else {
        gtext <- NULL
    }

    # Get absolute min/max so that scale can be centered on zero, since nothing
    # can decrease less than 100% without being negative, let's limit the scale
    # to a maximum range of [-100%, 100%].# and replace infinite values with maximum value
    lim <- min(100, max(abs(df$value[is.finite(df$value)])))
    df$value[!is.na(df$value) & abs(df$value) > lim] <- lim

    # Get number of breaks for the x-axis for prettier output
    max_breaks <- min((10 / n_fac), x_len)

    ggplot(data = df, mapping = aes(x = !!x_var, y = !!y_var)) +
        geom_tile(aes(fill = value), color = BOX_COLOR) +
        gtext +
        scale_x_continuous(breaks = scales::pretty_breaks(n = max_breaks),
                           expand = c(0,0)) +
        scale_y_discrete(expand = c(0,0)) +
        scale_fill_gradientn(colors = FILL_COLOR, name = '% change',
                             na.value = NA_COLOR, expand = c(0,0),
                             limits = c(-lim, lim)) +
        fgrid +
        xlab("") +
        themeCEDS.heatmap() +
        ggtitle(title)
}


# Save a comparison plot
#
# Format the comparison data correctly for the desired plot type, then save as a
# .png file to the diagnostic-output directory.
#
# Args:
#   combined: CEDS dataset in long form, except with two value columns
#   fuel: Name of the fuel column (typically agg_fuel or CEDS_fuel)
#   sector: Name of the sector column, optionally NULL (typically agg_sector or
#     CEDS_sector)
#   plot_title: Title of the plot
#   fname: Output filename excluding the .png extension
#   src_oldQ: Quoted name of the original data
#   src_newQ: Quoted name of the new data
#   plot_type: Type of plot to create, either 'linegraph' or 'heatmap'
#
# Returns:
#   NULL
plotComp <- function( combined, fuel, sector, plot_title, fname, src_oldQ, src_newQ, plot_type ) {

    agg_cols <- c( 'iso', fuel, sector, 'year' )

    if ( plot_type == 'linegraph' ) {
        agg_cols <- c( agg_cols, 'source_fuel' )
        plt <- combined %>%
            tidyr::gather( 'source', 'value', !!src_oldQ, !!src_newQ ) %>%
            dplyr::mutate(
                source_fuel = is.null( sector ),
                source_fuel = if_else( source_fuel, source,
                                       paste0( !!as.name( fuel ), " (", source, ")" ) ) ) %>%
            dplyr::group_by_at( vars( agg_cols ) ) %>%
            dplyr::summarise( value = sum( value, na.rm = T ) ) %>%
            dplyr::ungroup() %>%
            cedsLinegraph( year, value, source_fuel, plot_title, fuel, sector )

    } else if ( plot_type == 'heatmap' ) {
        plt <- combined %>%
            dplyr::group_by_at( vars( agg_cols ) ) %>%
            dplyr::summarise_at( vars( !!src_oldQ, !!src_newQ ), sum ) %>%
            dplyr::ungroup() %>%
            calcPercentDiff( quo_name( src_oldQ ), quo_name( src_newQ ) ) %>%
            cedsHeatmap( year, sector, pct_diff, plot_title, facet = fuel, labs = 'on' )

    } else {
        stop( 'plot type "', plot_type, '" not supported' )
    }

    savePlot( 'DIAG_OUT', 'user-data/', fname, 16, 8, '.png', plt )
}



# Capture an argument for non-standard evaluation
#
# Prepare an argument for use in a function that uses non-standard evaluation.
# This helper function allows other functions to assume identical behavior when
# passed a string or an expression to be passed on to a further function. For
# example, say we wanted to write a function that plots highway miles per gallon
# from the mpg dataset against some other variable of choice:
#
#   plotmpg <- function(x_values) {
#       ggplot(mpg, aes(x_values, hwy)) + geom_point()
#   }
#
# Because the aes function quotes its arguments, this function will not work.
# Instead, we would have to use tidy evaluation to quote the argument to our
# function, then tell ggplot that we have already quoted it using !!:
#
#   plotmpg <- function(x_values) {
#       x_values <- enquo(x_values)
#       ggplot(mpg, aes(!!x_values, hwy)) + geom_point()
#   }
#
# Now we have a function that works well, provided the argument is the name of
# a column, such as `plotmpg(displ)`. This function goes further and allows the
# argument to be the name of a column or an expression that evaluates to the
# name of a column. Used in this way, `plotmpg(displ)`, `plotmpg('displ')`, and
# `x <- 'displ'; plotmpg(x)` all give the same plot:
#
#   plotmpg <- function(x_values) {
#       x_values <- captureForNSE(x_values)
#       ggplot(mpg, aes(!!x_values, hwy)) + geom_point()
#   }
#
# For more information on quoting and non-standard evaluation see:
#   - https://dplyr.tidyverse.org/articles/programming.html
#   - http://adv-r.had.co.nz/Computing-on-the-language.html
#
# The function works by executing the following steps:
#   1. Determine the argument name in the parent function
#   2. Look up the unevaluated parameter to the parent function
#   3. Try to evaluate this parameter and coerce its value to a symbol
#   4. Quote that value for use in non-standard evaluation
# Note that if step 3 fails, the parameter is assumed to be already a symbol
# naming the column.
#
# Args:
#   arg: The argument to capture
#
# Returns:
#   A quosure containing an expression that evaluates to a column nam
captureForNSE <- function(arg) {
    sub_arg <- substitute(substitute(arg))    # Ex. substitute(x_values)
    arg_val <- eval(sub_arg, parent.frame())  # Ex. displ, "displ", or x
    arg_val <- tryCatch(if (is.null(arg)) return(NULL) else as.name(arg),
                        error = function(e) arg_val)  # Ex. displ
    enquo(arg_val)  # Ex. <quosure> ...
}
