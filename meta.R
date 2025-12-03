library(meta)
library(tidyr)
library(dplyr)
library(metafor)
library(ggplot2)
library(ragg)
library(patchwork)
library(magick)
library(showtext)
library(grid)
library(magick)

####data####
or_data<- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), times = 4),
  Frailty_status = rep(c("pre_frailty", "pre_frailty", "frailty", "frailty"), each = 6),
  Edu_level = rep(c("Middle", "High"), each = 6, times = 2),
  OR = c(
    # pre_frailty Middle
    0.90, 0.54, 0.71, 0.95, 0.76, 0.57,
    # pre_frailty High
    0.73, 0.32, 0.35, 1.20, 0.55, 0.42,
    # frailty Middle
    0.61, 0.45, 0.55, 0.83, 0.55, 0.38,
    # frailty High
    0.38, 0.15, 0.17, 0.95, 0.35, 0.21
  ),
  Lower = c(
    0.79, 0.45, 0.66, 0.79, 0.69, 0.51,
    0.61, 0.27, 0.33, 0.89, 0.49, 0.36,
    0.48, 0.33, 0.50, 0.65, 0.46, 0.32,
    0.25, 0.10, 0.15, 0.65, 0.26, 0.15
  ),
  Upper = c(
    1.02, 0.63, 0.76, 1.15, 0.83, 0.64,
    0.88, 0.38, 0.38, 1.60, 0.61, 0.49,
    0.78, 0.61, 0.60, 1.06, 0.67, 0.45,
    0.57, 0.22, 0.19, 1.40, 0.48, 0.30
  )
)
or_data$logOR <- log(or_data$OR)
or_data$SE <- (log(or_data$Upper) - log(or_data$Lower)) / (2 * 1.96)

df <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), each = 3*3),
  Education = rep(rep(c("Low", "Middle", "High"), each = 3), times = 6),
  Frailty = rep(c("non_frailty", "pre_frailty", "frailty"), times = 6*3),
  Count = c(
    # HRS
    894, 1477, 249,
    808, 1155, 141,
    297, 339, 30,
    # ELSA
    371, 1922, 198,
    363, 979, 76,
    449, 722, 29,
    # SHARE
    1566, 7235, 2078,
    5122, 14181, 2508,
    3359, 4497, 481,
    # KLoSA
    357, 1773, 374,
    426, 2517, 363,
    86, 722, 88,
    # MHAS
    1294, 2785, 341,
    2682, 3962, 282,
    1107, 1149, 66,
    # CHARLS
    820, 3322, 702,
    1767, 3533, 380,
    615, 780, 50
  )
)

###cut-off:0/1/2-5
or_data2 <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), times = 4),
  Frailty_status = rep(c("pre_frailty", "pre_frailty", "frailty", "frailty"), each = 6),
  Edu_level = rep(c("Middle", "High"), each = 6, times = 2),
  OR = c(
    0.96, 0.54, 0.75, 0.88, 0.80, 0.61,
    0.79, 0.35, 0.43, 1.01, 0.60, 0.48,
    0.73, 0.50, 0.62, 0.99, 0.64, 0.47,
    0.55, 0.22, 0.22, 1.30, 0.41, 0.27
  ),
  Lower = c(
    0.83, 0.46, 0.70, 0.72, 0.73, 0.54,
    0.65, 0.29, 0.40, 0.74, 0.53, 0.41,
    0.62, 0.41, 0.58, 0.81, 0.57, 0.42,
    0.43, 0.18, 0.20, 0.96, 0.35, 0.23
  ),
  Upper = c(
    1.10, 0.64, 0.80, 1.07, 0.88, 0.68,
    0.96, 0.41, 0.46, 1.37, 0.68, 0.57,
    0.85, 0.61, 0.66, 1.19, 0.71, 0.53,
    0.69, 0.28, 0.24, 1.76, 0.49, 0.34
  )
)
or_data2$logOR <- log(or_data2$OR)
or_data2$SE <- (log(or_data2$Upper) - log(or_data2$Lower)) / (2 * 1.96)

df2 <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), each = 3*3),
  Education = rep(rep(c("Low", "Middle", "High"), each = 3), times = 6),
  Frailty = rep(c("non_frailty", "pre_frailty", "frailty"), times = 6*3),
  Count = c(
    # HRS
    894, 945, 781,  # Low
    808, 795, 501,  # Middle
    297, 237, 132,  # High
    # ELSA
    371, 1393, 727,  # Low
    363, 738, 317,  # Middle
    449, 586, 165,  # High
    # SHARE
    1566, 3949, 5364,  # Low
    5122, 8624, 8065,  # Middle
    3359, 3157, 1821,  # High
    # KLoSA
    357, 993, 1154,  # Low
    426, 1304, 1576,  # Middle
    86, 341, 469,  # High
    # MHAS
    1294, 1841, 1285,  # Low
    2682, 2898, 1346,  # Middle
    1107, 890, 325,  # High
    # CHARLS
    820, 1970, 2054,  # Low
    1767, 2368, 1545,  # Middle
    615, 594, 236   # High

  )
)

##H_edu
or_data3<- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), times = 4),
  Frailty_status = rep(c("pre_frailty", "pre_frailty", "frailty", "frailty"), each = 6),
  Edu_level = rep(c("Middle", "High"), each = 6, times = 2),
  OR = c(
    0.81, 0.40, 0.67, 1.08, 0.75, 0.69,
    0.65, 0.20, 0.33, 1.44, 0.66, 0.44,
    0.53, 0.32, 0.54, 0.91, 0.79, 0.37,
    0.25, 0.08, 0.16, 1.21, 0.48, 0.24
  ),
  Lower = c(
    0.68, 0.31, 0.63, 0.90, 0.61, 0.60,
    0.53, 0.15, 0.31, 1.08, 0.59, 0.31,
    0.40, 0.23, 0.50, 0.71, 0.47, 0.26,
    0.17, 0.05, 0.14, 0.82, 0.35, 0.09
  ),
  Upper = c(
    0.97, 0.50, 0.71, 1.30, 0.93, 0.79,
    0.80, 0.25, 0.36, 1.93, 0.73, 0.62,
    0.71, 0.44, 0.59, 1.17, 1.31, 0.53,
    0.37, 0.13, 0.18, 1.78, 0.66, 0.67
  )
)

or_data3$logOR <- log(or_data3$OR)
or_data3$SE <- (log(or_data3$Upper) - log(or_data3$Lower)) / (2 * 1.96)

df3 <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), each = 3*3),
  Education = rep(rep(c("Low", "Middle", "High"), each = 3), times = 6),
  Frailty = rep(c("non_frailty", "pre_frailty", "frailty"), times = 6*3),
  Count = c(
    # HRS
    237, 466, 94,  # Low
    1175, 1807, 266,  # Middle
    587, 698, 60,  # High
    # ELSA
    97, 811, 103,  # Low
    637, 2090, 171,  # Middle
    449, 722, 29,  # High
    # SHARE
    2018, 9482, 2560,  # Low
    4190, 11227, 1949,  # Middle
    3839, 5204, 558,  # High
    # KLoSA
    519, 2626, 526,  # Low
    278, 1769, 228,  # Middle
    72, 617, 71,  # High
    # MHAS
    3978, 6748, 623,  # Low
    172, 209, 17,  # Middle
    933, 939, 49,  # High
    # CHARLS
    2585, 6827, 1087,  # Low
    511, 723, 41,  # Middle
    106, 85, 4  # High
  )
)


##Weight
or_data4<- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), times = 4),
  Frailty_status = rep(c("pre_frailty", "pre_frailty", "frailty", "frailty"), each = 6),
  Edu_level = rep(c("Middle", "High"), each = 6, times = 2),
  OR = c(
    0.81, 0.54, 0.58, 0.90, 0.73, 0.55,
    0.65, 0.30, 0.29, 1.12, 0.60, 0.41,
    0.49, 0.44, 0.45, 0.77, 0.64, 0.35,
    0.37, 0.13, 0.16, 1.05, 0.28, 0.17
  ),
  Lower = c(
    0.71, 0.46, 0.54, 0.74, 0.67, 0.50,
    0.55, 0.25, 0.27, 0.85, 0.53, 0.35,
    0.38, 0.32, 0.41, 0.59, 0.53, 0.30,
    0.26, 0.08, 0.14, 0.72, 0.21, 0.12
  ),
  Upper = c(
    0.92, 0.64, 0.62, 1.09, 0.80, 0.62,
    0.77, 0.35, 0.32, 1.48, 0.68, 0.47,
    0.62, 0.59, 0.49, 1.00, 0.77, 0.42,
    0.54, 0.20, 0.18, 1.51, 0.39, 0.25
  )
)

or_data4$logOR <- log(or_data4$OR)
or_data4$SE <- (log(or_data4$Upper) - log(or_data4$Lower)) / (2 * 1.96)

df4 <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), each = 3*3),
  Education = rep(rep(c("Low", "Middle", "High"), each = 3), times = 6),
  Frailty = rep(c("non_frailty", "pre_frailty", "frailty"), times = 6*3),
  Count = c(
    # HRS
    894, 1477, 249,
    808, 1155, 141,
    297, 339, 30,
    # ELSA
    371, 1922, 198,
    363, 979, 76,
    449, 722, 29,
    # SHARE
    1566, 7235, 2078,
    5122, 14181, 2508,
    3359, 4497, 481,
    # KLoSA
    357, 1773, 374,
    426, 2517, 363,
    86, 722, 88,
    # MHAS
    1294, 2785, 341,
    2682, 3962, 282,
    1107, 1149, 66,
    # CHARLS
    820, 3322, 702,
    1767, 3533, 380,
    615, 780, 50
  )
)   


##subgroup
or_data_women<- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), times = 4),
  Frailty_status = rep(c("pre_frailty", "pre_frailty", "frailty", "frailty"), each = 6),
  Edu_level = rep(c("Middle", "High"), each = 6, times = 2),
  OR = c(
    0.93, 0.50, 0.64, 0.94, 0.77, 0.60,
    0.72, 0.32, 0.31, 1.09, 0.58, 0.55,
    0.66, 0.45, 0.47, 0.86, 0.56, 0.40,
    0.34, 0.16, 0.15, 0.85, 0.37, 0.25
  ),
  Lower = c(
    0.79, 0.40, 0.59, 0.74, 0.69, 0.52,
    0.56, 0.26, 0.28, 0.71, 0.49, 0.43,
    0.49, 0.30, 0.42, 0.63, 0.44, 0.32,
    0.19, 0.09, 0.13, 0.46, 0.24, 0.14
  ),
  Upper = c(
    1.10, 0.63, 0.70, 1.18, 0.86, 0.69,
    0.92, 0.41, 0.34, 1.68, 0.68, 0.70,
    0.89, 0.66, 0.53, 1.17, 0.71, 0.51,
    0.60, 0.28, 0.17, 1.57, 0.58, 0.44
  )
)


or_data_women$logOR <- log(or_data_women$OR)
or_data_women$SE <- (log(or_data_women$Upper) - log(or_data_women$Lower)) / (2 * 1.96)
df_women <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), each = 3*3),
  Education = rep(rep(c("Low", "Middle", "High"), each = 3), times = 6),
  Frailty = rep(c("non_frailty", "pre_frailty", "frailty"), times = 6*3),
  Count = c(
    # HRS
    524, 927, 157,  # Low
    459, 718, 90,  # Middle
    153, 183, 15,  # High
    # ELSA
    204, 1202, 136,  # Low
    178, 515, 46,  # Middle
    184, 341, 16,  # High
    # SHARE
    792, 4436, 1607,  # Low
    2680, 8205, 1701,  # Middle
    1654, 2354, 314,  # High
    # KLoSA
    274, 1321, 290,  # Low
    266, 1302, 185,  # Middle
    33, 190, 21,  # High
    # MHAS
    827, 1690, 210,  # Low
    1644, 2270, 158,  # Middle
    524, 508, 27,  # High
    # CHARLS
    586, 2219, 509,  # Low
    755, 1391, 166,  # Middle
    209, 279, 17  # High
    
  )
)

or_data_men <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), times = 4),
  Frailty_status = rep(c("pre_frailty", "pre_frailty", "frailty", "frailty"), each = 6 * 1),
  Edu_level = rep(c("Middle", "High"), each = 6, times = 2),
  OR = c(
    0.85, 0.58, 0.79, 1.12, 0.74, 0.51,
    0.77, 0.33, 0.42, 1.37, 0.51, 0.34,
    0.57, 0.45, 0.71, 0.99, 0.54, 0.34,
    0.47, 0.14, 0.21, 1.08, 0.32, 0.17
  ),
  Lower = c(
    0.70, 0.46, 0.72, 0.82, 0.64, 0.43,
    0.58, 0.26, 0.38, 0.92, 0.43, 0.27,
    0.38, 0.28, 0.61, 0.66, 0.41, 0.26,
    0.26, 0.07, 0.17, 0.64, 0.21, 0.11
  ),
  Upper = c(
    1.04, 0.74, 0.87, 1.53, 0.86, 0.61,
    1.00, 0.41, 0.47, 2.06, 0.61, 0.42,
    0.84, 0.73, 0.82, 1.48, 0.73, 0.44,
    0.85, 0.26, 0.26, 1.83, 0.49, 0.27
  )
)

or_data_men$logOR <- log(or_data_men$OR)
or_data_men$SE <- (log(or_data_men$Upper) - log(or_data_men$Lower)) / (2 * 1.96)
#样本量
df_men <- data.frame(
  Study = rep(c("United States", "England", "European countries", "South Korea", "Mexico", "China"), each = 3*3),
  Education = rep(rep(c("Low", "Middle", "High"), each = 3), times = 6),
  Frailty = rep(c("non_frailty", "pre_frailty", "frailty"), times = 6*3),
  Count = c(
    # United States
    370, 550, 92,  # Low
    349, 437, 51,  # Middle
    144, 156, 15,  # High
    # England
    167, 720, 62,
    185, 464, 30,
    265, 381, 13,
    # European countries
    774, 2799, 471,
    2442, 5976, 807,
    1705, 2143, 167,
    # South Korea
    83, 452, 84,
    160, 1215, 178,
    53, 532, 67,
    # Mexico
    467, 1095, 131, 
    1038, 1692, 124,
    583, 641, 67, 
    # China
    234, 1103, 193,
    1012, 2142, 214,
    406, 501, 33
  )
)    

make_complete_data <- function(or_data, df) {
  sample_summary_base <- aggregate(
    Count ~ Study + Education + Frailty,
    data = df,
    FUN = sum
  )
  
  total_sample <- aggregate(
    Count ~ Study + Education,
    data = df,
    FUN = sum
  )
  names(total_sample)[names(total_sample) == "Count"] <- "Total"
  
  sample_summary_base <- merge(
    sample_summary_base,
    total_sample,
    by = c("Study", "Education")
  )
  
  sample_wide <- tidyr::pivot_wider(
    sample_summary_base,
    id_cols = Study,
    names_from = c(Education, Frailty),
    values_from = Count,
    names_sep = "_"
  )
  
  total_wide <- tidyr::pivot_wider(
    total_sample,
    id_cols = Study,
    names_from = Education,
    values_from = Total,
    names_prefix = "Total_"
  )
  
  sample_wide <- merge(sample_wide, total_wide, by = "Study")

  complete_data <- merge(or_data, sample_wide, by.x = "Study", by.y = "Study")
  
  names(complete_data) <- gsub("_(.)", "\\U\\1", names(complete_data), perl = TRUE)
  
  complete_data$FrailtyStatus <- gsub("_", " ", complete_data$FrailtyStatus)          
  complete_data$FrailtyStatus <- tolower(complete_data$FrailtyStatus)                 
  complete_data$FrailtyStatus <- tools::toTitleCase(complete_data$FrailtyStatus)      
  is_integerish_col <- function(x) {
    is.numeric(x) && all(is.na(x) | abs(x - round(x)) < sqrt(.Machine$double.eps))
  }
  int_cols <- vapply(complete_data, is_integerish_col, logical(1))
  
  complete_data[int_cols] <- lapply(complete_data[int_cols], function(x) {
    formatC(x, format = "f", digits = 0, big.mark = ",")
  })
  return(complete_data)
}
run_meta <- function(data, frailty_status, edu_level, save_path = NULL) {
    # 数据子集
    df_sub <- subset(data, FrailtyStatus == frailty_status & EduLevel == edu_level)
    study_order <- c("United States", "England", "European countries", "South Korea", "Mexico", "China")
    df_sub$Study <- factor(df_sub$Study, levels = study_order)
    df_sub <- df_sub[order(df_sub$Study), ]

    m <- metagen(
      TE = logOR,
      seTE = SE,
      studlab = Study,
      sm = "OR",
      data = df_sub,
      common = TRUE,
      random = TRUE,
      method.tau = "REML"
    )
    m$lower <- log(df_sub$Lower)
    m$upper <- log(df_sub$Upper)
    
    cat("\n\nMeta analysis for", frailty_status, "_", edu_level, "\n")
    print(summary(m))
    
    # 左列标签设置
    if (frailty_status == "Pre Frailty") {
      frail_col_name_data  <- "PreFrailty"
      frail_col_name_label <- "Pre-frailty"
    } else if (frailty_status == "Frailty") {
      frail_col_name_data  <- "Frailty"
      frail_col_name_label <- "Frailty"
    } else {
      frail_col_name_data  <- frailty_status
      frail_col_name_label <- frailty_status
    }
    
    if (edu_level == "Middle") {
      leftcols <- c("studlab",
                    paste0("Middle", frail_col_name_data),
                    "TotalMiddle",
                    paste0("Low", frail_col_name_data),
                    "TotalLow")
      leftlabs <- c("Study",
                    paste0("      ", frail_col_name_label),
                    "\nTotal",
                    paste0("      ", frail_col_name_label),
                    "\nTotal")
    } else if (edu_level == "High") {
      leftcols <- c("studlab",
                    paste0("High", frail_col_name_data),
                    "TotalHigh",
                    paste0("Low", frail_col_name_data),
                    "TotalLow")
      leftlabs <- c("Study",
                    paste0("      ", frail_col_name_label),
                    "\n Total",
                    paste0("      ", frail_col_name_label),
                    "\n Total")
    } else {
      leftcols <- "studlab"
      leftlabs <- "Study"
    }
    
    study_colors <- setNames(rep("#1f78b4", length(study_order)), study_order)
    col_vector <- study_colors[as.character(df_sub$Study)]
    weights <- m$w.random / sum(m$w.random)
    cex_square <- 1.5 * sqrt(weights)
  
    
    # save
    if (!is.null(save_path)) {
      png(filename = save_path, width = 3500, height = 1100, res = 300)
    } else {
      dev.new()
    }
    
    forest(
      m,
      comb.fixed = FALSE,
      xlim = c(0.1, 2),
      at = c(0.1, 0.2, 0.5, 1, 1.5, 2),
      digits = 2,
      colgap.left = "0.02cm",
      leftcols = leftcols,
      leftlabs = leftlabs,
      leftwidth = 4,
      rightwidth = 3,
      xlab = "Odds Ratio",
      col.square = col_vector,
      col.lines = "black",
      col.study = col_vector,
      cex.square = cex_square,
      cex.lab = 1.2,
      cex.axis = 1.1,
      cex.study = 1.1
    )
    # title
    grid::grid.text(
      "a. Association of middle educational attainment with pre-frailty (reference:low educational attainment)",
      x = unit(0.02, "npc"),     
      y = unit(0.92, "npc"),     
      just = c("left", "top"), 
      gp = grid::gpar(
        fontsize = 12,          
        fontface = "bold" 
      )
    )
    
    grid::grid.text(
      "Middle",
      x = unit(0.30, "npc"),     
      y = unit(0.845, "npc"),  
      just = c("center", "top"),
      gp = grid::gpar(
        fontsize = 12,          
        fontface = "bold" 
      )
    )
    grid::grid.text(
      "Low",
      x = unit(0.43, "npc"),     
      y = unit(0.845, "npc"),  
      just = c("center", "top"),
      gp = grid::gpar(
        fontsize = 12,          
        fontface = "bold" 
      )
    )

  if (!is.null(save_path)) dev.off()
  
  showtext_auto(FALSE)
  
  return(list(meta = m))
}
add_margin <- function(img, margin = 70, side = 70, color = "white") {
  img %>%
    image_trim() %>%
    image_border(color, geometry_area(side, margin))
}

#main result cut-off 0/1-2/3-5 edu_quan
complete_data <- make_complete_data(or_data, df)
m1 <- run_meta(
  complete_data,
  "Pre Frailty",
  "Middle",
  save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_Middle.png"
)
m2 <- run_meta2(complete_data, "Pre Frailty", "High",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_high.png")
m3 <- run_meta3(complete_data, "Frailty", "Middle",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_frailty_middle.png")
m4 <- run_meta4(complete_data, "Frailty", "High",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_frailty_high.png")

# 
img_m1 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_Middle.png"))
img_m2 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_high.png"))
img_m3 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_frailty_middle.png"))
img_m4 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_frailty_high.png"))


prefrailty_col <- image_append(c(img_m1, img_m2), stack = TRUE)
frailty_col   <- image_append(c(img_m3, img_m4), stack = TRUE)

# save
image_write(prefrailty_col, "/Users/xiangxiang/Desktop/social frailty data/final_plot_prefrailty.png")
image_write(frailty_col, "/Users/xiangxiang/Desktop/social frailty data/final_plot_frailty.png")

#sen result cut-off 0/1/2-5 edu_quan
complete_data2 <- make_complete_data(or_data2, df2)
m1_2 <- run_meta(complete_data2, "Pre Frailty", "Middle",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest2_pre_frailty_middle.png")
m2_2 <- run_meta2(complete_data2, "Pre Frailty", "High",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest2_pre_frailty_high.png")
m3_2 <- run_meta3(complete_data2, "Frailty", "Middle",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest2_frailty_middle.png")
m4_2 <- run_meta4(complete_data2, "Frailty", "High",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest2_frailty_high.png")

img_m1 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest2_pre_frailty_Middle.png"))
img_m2 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest2_pre_frailty_high.png"))
img_m3 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest2_frailty_middle.png"))
img_m4 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest2_frailty_high.png"))

# 
prefrailty_col <- image_append(c(img_m1, img_m2), stack = TRUE)
frailty_col <- image_append(c(img_m3, img_m4), stack = TRUE)

image_write(prefrailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(cutoff)_plot_prefrailty.png")
image_write(frailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(cutoff)_plot_frailty.png")

#sen result H_edu
complete_data3 <- make_complete_data(or_data3, df3)
m1_3 <- run_meta(complete_data3, "Pre Frailty", "Middle",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest3_pre_frailty_middle.png")
m2_3 <- run_meta2(complete_data3, "Pre Frailty", "High",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest3_pre_frailty_high.png")
m3_3 <- run_meta3(complete_data3, "Frailty", "Middle",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest3_frailty_middle.png")
m4_3 <- run_meta4(complete_data3, "Frailty", "High",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest3_frailty_high.png")

img_m1 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest3_pre_frailty_Middle.png"))
img_m2 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest3_pre_frailty_high.png"))
img_m3 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest3_frailty_middle.png"))
img_m4 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest3_frailty_high.png"))

prefrailty_col <- image_append(c(img_m1, img_m2), stack = TRUE)
frailty_col <- image_append(c(img_m3, img_m4), stack = TRUE)

image_write(prefrailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(H_edu)_plot_prefrailty.png")
image_write(frailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(H_edu)_plot_frailty.png")

#weight
complete_data4 <- make_complete_data(or_data4, df4)
m1_4 <- run_meta(complete_data4, "Pre Frailty", "Middle",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest4_pre_frailty_middle.png")
m2_4 <- run_meta2(complete_data4, "Pre Frailty", "High",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest4_pre_frailty_high.png")
m3_4 <- run_meta3(complete_data4, "Frailty", "Middle",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest4_frailty_middle.png")
m4_4 <- run_meta4(complete_data4, "Frailty", "High",
                 save_path = "/Users/xiangxiang/Desktop/social frailty data/forest4_frailty_high.png")

img_m1 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest4_pre_frailty_Middle.png"))
img_m2 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest4_pre_frailty_high.png"))
img_m3 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest4_frailty_middle.png"))
img_m4 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest4_frailty_high.png"))

prefrailty_col <- image_append(c(img_m1, img_m2), stack = TRUE)
frailty_col <- image_append(c(img_m3, img_m4), stack = TRUE)

image_write(prefrailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(weight)_plot_prefrailty.png")
image_write(frailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(weight)_plot_frailty.png")


##gender
complete_data_women <- make_complete_data(or_data_women, df_women)
m1 <- run_meta(
  complete_data_women,
  "Pre Frailty",
  "Middle",
  save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_Middle_women.png"
)
m2 <- run_meta2(complete_data_women, "Pre Frailty", "High",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_high_women.png")
m3 <- run_meta3(complete_data_women, "Frailty", "Middle",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_frailty_middle_women.png")
m4 <- run_meta4(complete_data_women, "Frailty", "High",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_frailty_high_women.png")

img_m1 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_Middle_women.png"))
img_m2 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_high_women.png"))
img_m3 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_frailty_middle_women.png"))
img_m4 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_frailty_high_women.png"))

prefrailty_col <- image_append(c(img_m1, img_m2), stack = TRUE)
frailty_col <- image_append(c(img_m3, img_m4), stack = TRUE)

image_write(prefrailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(women)_plot_prefrailty.png")
image_write(frailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(women)_plot_frailty.png")


complete_data_men <- make_complete_data(or_data_men, df_men)
m1 <- run_meta(
  complete_data_men,
  "Pre Frailty",
  "Middle",
  save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_Middle_men.png"
)
m2 <- run_meta2(complete_data_men, "Pre Frailty", "High",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_high_men.png")
m3 <- run_meta3(complete_data_men, "Frailty", "Middle",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_frailty_middle_men.png")
m4 <- run_meta4(complete_data_men, "Frailty", "High",
               save_path = "/Users/xiangxiang/Desktop/social frailty data/forest_frailty_high_men.png")
img_m1 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_Middle_men.png"))
img_m2 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_pre_frailty_high_men.png"))
img_m3 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_frailty_middle_men.png"))
img_m4 <- add_margin(image_read("/Users/xiangxiang/Desktop/social frailty data/forest_frailty_high_men.png"))

prefrailty_col <- image_append(c(img_m1, img_m2), stack = TRUE)
frailty_col <- image_append(c(img_m3, img_m4), stack = TRUE)

image_write(prefrailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(men)_plot_prefrailty.png")
image_write(frailty_col, "/Users/xiangxiang/Desktop/social frailty data/final(men)_plot_frailty.png")

run_meta2 <- function(data, frailty_status, edu_level, save_path = NULL) {
  df_sub <- subset(data, FrailtyStatus == frailty_status & EduLevel == edu_level)
  study_order <- c("United States", "England", "European countries", "South Korea", "Mexico", "China")
  df_sub$Study <- factor(df_sub$Study, levels = study_order)
  df_sub <- df_sub[order(df_sub$Study), ]
  
  # 元分析
  m <- metagen(
    TE = logOR,
    seTE = SE,
    studlab = Study,
    sm = "OR",
    data = df_sub,
    common = TRUE,
    random = TRUE,
    method.tau = "REML"
  )
  m$lower <- log(df_sub$Lower)
  m$upper <- log(df_sub$Upper)
  
  cat("\n\nMeta analysis for", frailty_status, "_", edu_level, "\n")
  print(summary(m))
  
  # 左列标签设置
  if (frailty_status == "Pre Frailty") {
    frail_col_name_data  <- "PreFrailty"
    frail_col_name_label <- "Pre-frailty"
  } else if (frailty_status == "Frailty") {
    frail_col_name_data  <- "Frailty"
    frail_col_name_label <- "Frailty"
  } else {
    frail_col_name_data  <- frailty_status
    frail_col_name_label <- frailty_status
  }
  
  if (edu_level == "Middle") {
    leftcols <- c("studlab",
                  paste0("Middle", frail_col_name_data),
                  "TotalMiddle",
                  paste0("Low", frail_col_name_data),
                  "TotalLow")
    leftlabs <- c("Study",
                  paste0("      ", frail_col_name_label),
                  "\nTotal",
                  paste0("      ", frail_col_name_label),
                  "\nTotal")
  } else if (edu_level == "High") {
    leftcols <- c("studlab",
                  paste0("High", frail_col_name_data),
                  "TotalHigh",
                  paste0("Low", frail_col_name_data),
                  "TotalLow")
    leftlabs <- c("Study",
                  paste0("      ", frail_col_name_label),
                  "\n Total",
                  paste0("      ", frail_col_name_label),
                  "\n Total")
  } else {
    leftcols <- "studlab"
    leftlabs <- "Study"
  }
  
  # 颜色和权重
  study_colors <- setNames(rep("#1f78b4", length(study_order)), study_order)
  col_vector <- study_colors[as.character(df_sub$Study)]
  weights <- m$w.random / sum(m$w.random)
  cex_square <- 1.5 * sqrt(weights)
  
  
  # 保存图像或新设备
  if (!is.null(save_path)) {
    png(filename = save_path, width = 3500, height = 1100, res = 300)
  } else {
    dev.new()
  }
  
  forest(
    m,
    comb.fixed = FALSE,
    xlim = c(0.1, 2),
    at = c(0.1, 0.2, 0.5, 1, 1.5, 2),
    digits = 2,
    colgap.left = "0.02cm",
    leftcols = leftcols,
    leftlabs = leftlabs,
    leftwidth = 4,
    rightwidth = 3,
    xlab = "Odds Ratio",
    col.square = col_vector,
    col.lines = "black",
    col.study = col_vector,
    cex.square = cex_square,
    cex.lab = 1.2,
    cex.axis = 1.1,
    cex.study = 1.1
  )
  # 主标题
  grid::grid.text(
    "b. Association of high educational attainment with pre-frailty (reference:low educational attainment)",
    x = unit(0.02, "npc"),     
    y = unit(0.92, "npc"),     
    just = c("left", "top"), 
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  
  grid::grid.text(
    "High",
    x = unit(0.30, "npc"),     
    y = unit(0.845, "npc"),  
    just = c("center", "top"),
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  grid::grid.text(
    "Low",
    x = unit(0.43, "npc"),     
    y = unit(0.845, "npc"),  
    just = c("center", "top"),
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  
  if (!is.null(save_path)) dev.off()
  
  showtext_auto(FALSE)
  
  return(list(meta = m))
}
run_meta3 <- function(data, frailty_status, edu_level, save_path = NULL) {
  # 数据子集
  df_sub <- subset(data, FrailtyStatus == frailty_status & EduLevel == edu_level)
  study_order <- c("United States", "England", "European countries", "South Korea", "Mexico", "China")
  df_sub$Study <- factor(df_sub$Study, levels = study_order)
  df_sub <- df_sub[order(df_sub$Study), ]
  
  # 元分析
  m <- metagen(
    TE = logOR,
    seTE = SE,
    studlab = Study,
    sm = "OR",
    data = df_sub,
    common = TRUE,
    random = TRUE,
    method.tau = "REML"
  )
  m$lower <- log(df_sub$Lower)
  m$upper <- log(df_sub$Upper)
  
  cat("\n\nMeta analysis for", frailty_status, "_", edu_level, "\n")
  print(summary(m))
  
  # 左列标签设置
  if (frailty_status == "Pre Frailty") {
    frail_col_name_data  <- "PreFrailty"
    frail_col_name_label <- "Pre-frailty"
  } else if (frailty_status == "Frailty") {
    frail_col_name_data  <- "Frailty"
    frail_col_name_label <- "Frailty"
  } else {
    frail_col_name_data  <- frailty_status
    frail_col_name_label <- frailty_status
  }
  
  if (edu_level == "Middle") {
    leftcols <- c("studlab",
                  paste0("Middle", frail_col_name_data),
                  "TotalMiddle",
                  paste0("Low", frail_col_name_data),
                  "TotalLow")
    leftlabs <- c("Study",
                  paste0("      ", frail_col_name_label),
                  "\nTotal",
                  paste0("      ", frail_col_name_label),
                  "\nTotal")
  } else if (edu_level == "High") {
    leftcols <- c("studlab",
                  paste0("High", frail_col_name_data),
                  "TotalHigh",
                  paste0("Low", frail_col_name_data),
                  "TotalLow")
    leftlabs <- c("Study",
                  paste0("      ", frail_col_name_label),
                  "\n Total",
                  paste0("      ", frail_col_name_label),
                  "\n Total")
  } else {
    leftcols <- "studlab"
    leftlabs <- "Study"
  }
  
  # 颜色和权重
  study_colors <- setNames(rep("#1f78b4", length(study_order)), study_order)
  col_vector <- study_colors[as.character(df_sub$Study)]
  weights <- m$w.random / sum(m$w.random)
  cex_square <- 1.5 * sqrt(weights)
  
  
  # 保存图像或新设备
  if (!is.null(save_path)) {
    png(filename = save_path, width = 3500, height = 1100, res = 300)
  } else {
    dev.new()
  }
  
  forest(
    m,
    comb.fixed = FALSE,
    xlim = c(0.1, 2),
    at = c(0.1, 0.2, 0.5, 1, 1.5, 2),
    digits = 2,
    colgap.left = "0.02cm",
    leftcols = leftcols,
    leftlabs = leftlabs,
    leftwidth = 4,
    rightwidth = 3,
    xlab = "Odds Ratio",
    col.square = col_vector,
    col.lines = "black",
    col.study = col_vector,
    cex.square = cex_square,
    cex.lab = 1.2,
    cex.axis = 1.1,
    cex.study = 1.1
  )
  # 主标题
  grid::grid.text(
    "a. Association of middle educational attainment with frailty (reference:low educational attainment)",
    x = unit(0.038, "npc"),     
    y = unit(0.92, "npc"),     
    just = c("left", "top"), 
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  
  grid::grid.text(
    "Middle",
    x = unit(0.30, "npc"),     
    y = unit(0.845, "npc"),  
    just = c("center", "top"),
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  grid::grid.text(
    "Low",
    x = unit(0.41, "npc"),     
    y = unit(0.845, "npc"),  
    just = c("center", "top"),
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  
  if (!is.null(save_path)) dev.off()
  
  showtext_auto(FALSE)
  
  return(list(meta = m))
}
run_meta4 <- function(data, frailty_status, edu_level, save_path = NULL) {
  # 数据子集
  df_sub <- subset(data, FrailtyStatus == frailty_status & EduLevel == edu_level)
  study_order <- c("United States", "England", "European countries", "South Korea", "Mexico", "China")
  df_sub$Study <- factor(df_sub$Study, levels = study_order)
  df_sub <- df_sub[order(df_sub$Study), ]
  
  # 元分析
  m <- metagen(
    TE = logOR,
    seTE = SE,
    studlab = Study,
    sm = "OR",
    data = df_sub,
    common = TRUE,
    random = TRUE,
    method.tau = "REML"
  )
  m$lower <- log(df_sub$Lower)
  m$upper <- log(df_sub$Upper)
  
  cat("\n\nMeta analysis for", frailty_status, "_", edu_level, "\n")
  print(summary(m))
  
  # 左列标签设置
  if (frailty_status == "Pre Frailty") {
    frail_col_name_data  <- "PreFrailty"
    frail_col_name_label <- "Pre-frailty"
  } else if (frailty_status == "Frailty") {
    frail_col_name_data  <- "Frailty"
    frail_col_name_label <- "Frailty"
  } else {
    frail_col_name_data  <- frailty_status
    frail_col_name_label <- frailty_status
  }
  
  if (edu_level == "Middle") {
    leftcols <- c("studlab",
                  paste0("Middle", frail_col_name_data),
                  "TotalMiddle",
                  paste0("Low", frail_col_name_data),
                  "TotalLow")
    leftlabs <- c("Study",
                  paste0("      ", frail_col_name_label),
                  "\nTotal",
                  paste0("      ", frail_col_name_label),
                  "\nTotal")
  } else if (edu_level == "High") {
    leftcols <- c("studlab",
                  paste0("High", frail_col_name_data),
                  "TotalHigh",
                  paste0("Low", frail_col_name_data),
                  "TotalLow")
    leftlabs <- c("Study",
                  paste0("      ", frail_col_name_label),
                  "\n Total",
                  paste0("      ", frail_col_name_label),
                  "\n Total")
  } else {
    leftcols <- "studlab"
    leftlabs <- "Study"
  }
  
  # 颜色和权重
  study_colors <- setNames(rep("#1f78b4", length(study_order)), study_order)
  col_vector <- study_colors[as.character(df_sub$Study)]
  weights <- m$w.random / sum(m$w.random)
  cex_square <- 1.5 * sqrt(weights)
  
  
  # 保存图像或新设备
  if (!is.null(save_path)) {
    png(filename = save_path, width = 3500, height = 1100, res = 300)
  } else {
    dev.new()
  }
  
  forest(
    m,
    comb.fixed = FALSE,
    xlim = c(0.1, 2),
    at = c(0.1, 0.2, 0.5, 1, 1.5, 2),
    digits = 2,
    colgap.left = "0.02cm",
    leftcols = leftcols,
    leftlabs = leftlabs,
    leftwidth = 4,
    rightwidth = 3,
    xlab = "Odds Ratio",
    col.square = col_vector,
    col.lines = "black",
    col.study = col_vector,
    cex.square = cex_square,
    cex.lab = 1.2,
    cex.axis = 1.1,
    cex.study = 1.1
  )
  # 主标题
  grid::grid.text(
    "b. Association of high educational attainment with frailty (reference:low educational attainment)",
    x = unit(0.038, "npc"),     
    y = unit(0.92, "npc"),     
    just = c("left", "top"), 
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  
  grid::grid.text(
    "High",
    x = unit(0.30, "npc"),     
    y = unit(0.845, "npc"),  
    just = c("center", "top"),
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  grid::grid.text(
    "Low",
    x = unit(0.41, "npc"),     
    y = unit(0.845, "npc"),  
    just = c("center", "top"),
    gp = grid::gpar(
      fontsize = 12,          
      fontface = "bold" 
    )
  )
  
  if (!is.null(save_path)) dev.off()
  
  showtext_auto(FALSE)
  
  return(list(meta = m))
}