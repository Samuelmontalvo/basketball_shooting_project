# ==============================================================================
# GRF Shooting Data - Exploratory Data Analysis (EDA)
# ==============================================================================
# This script performs thorough EDA on the force-plate shooting dataset
# BEFORE any statistical modeling. No mixed models or hypothesis tests.
# ==============================================================================

# ==============================================================================
# 1. Load Packages
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
})

# Optional packages (load if available)
has_skimr <- require(skimr, quietly = TRUE)
has_naniar <- require(naniar, quietly = TRUE)
has_GGally <- require(GGally, quietly = TRUE)

# Set theme for all plots
theme_set(theme_minimal(base_size = 11))

# Initialize plot storage
plots <- list()

# ==============================================================================
# 2. Read Data
# ==============================================================================

cat("==============================================================================\n")
cat("LOADING DATA\n")
cat("==============================================================================\n\n")

# Try primary file first, then fallback
primary_file <- "data/grf_shooting_clean.csv"
fallback_file <- "data/GRF Shooting SM[40].csv"

if (file.exists(primary_file)) {
  df <- read_csv(primary_file, show_col_types = FALSE)
  cat("Loaded primary file:", primary_file, "\n")
} else if (file.exists(fallback_file)) {

  df <- read_csv(fallback_file, show_col_types = FALSE) %>%
    janitor::clean_names()
  cat("Primary file not found. Loaded fallback:", fallback_file, "\n")
  cat("Applied janitor::clean_names()\n")
} else {
  stop("Neither primary nor fallback data files found!\n",
       "Expected: ", primary_file, " or ", fallback_file)
}

cat("\nDimensions:", nrow(df), "rows x", ncol(df), "columns\n")
cat("\nColumn names:\n")
cat(paste(names(df), collapse = "\n"), "\n")

# ==============================================================================
# 2b. Identify Key Columns
# ==============================================================================

cat("\n==============================================================================\n")
cat("COLUMN IDENTIFICATION\n")
cat("==============================================================================\n\n")

# Helper function to find columns
find_col <- function(cols, patterns) {
  for (p in patterns) {
    matches <- cols[str_detect(tolower(cols), tolower(p))]
    if (length(matches) > 0) return(matches[1])
  }
  return(NA_character_)
}

# Find player_id
player_id_col <- find_col(names(df), c("^player_id$", "^player$", "subject", "athlete", "^pid$", "^id$"))

if (is.na(player_id_col)) {
  stop("ERROR: Could not identify player_id column. ",
       "Expected column containing: player, pid, athlete, subject, or id")
}

# Rename to player_id if needed
if (player_id_col != "player_id") {
  df <- df %>% rename(player_id = !!sym(player_id_col))
  cat("Renamed '", player_id_col, "' to 'player_id'\n", sep = "")
}
df <- df %>% mutate(player_id = as.character(player_id))
cat("player_id column identified: ", player_id_col, "\n", sep = "")

# Find shot_type
shot_type_col <- find_col(names(df), c("^shot_type$", "shot.*type", "type.*2.*3"))
has_shot_type <- !is.na(shot_type_col)

if (has_shot_type && shot_type_col != "shot_type") {
  df <- df %>% rename(shot_type = !!sym(shot_type_col))
  cat("Renamed '", shot_type_col, "' to 'shot_type'\n", sep = "")
}
if (has_shot_type) {
  # Standardize shot_type
  df <- df %>%
    mutate(shot_type = case_when(
      shot_type %in% c(2, "2", "2PT", "2pt") ~ "2PT",
      shot_type %in% c(3, "3", "3PT", "3pt") ~ "3PT",
      TRUE ~ as.character(shot_type)
    ))
  cat("shot_type column identified and standardized\n")
} else {
  cat("shot_type column: NOT FOUND (will skip stratified analyses)\n")
}

# Find made/result
made_col <- find_col(names(df), c("^made$", "miss.*made", "result", "outcome", "make"))
has_made <- !is.na(made_col)

if (has_made && made_col != "made") {
  df <- df %>% rename(made = !!sym(made_col))
  cat("Renamed '", made_col, "' to 'made'\n", sep = "")
}
if (has_made) {
  df <- df %>% mutate(made = as.integer(made))
  cat("made column identified\n")
} else {
  cat("made/result column: NOT FOUND (will skip make rate analyses)\n")
}

cat("\nFinal column count:", ncol(df), "\n")

# ==============================================================================
# 3. Data Integrity Checks
# ==============================================================================

cat("\n==============================================================================\n")
cat("DATA INTEGRITY CHECKS\n")
cat("==============================================================================\n\n")

# --- 3a. Missingness Summary by Column ---
cat("--- Missingness by Column ---\n")

missingness_by_col <- df %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
  mutate(
    n_total = nrow(df),
    pct_missing = round(100 * n_missing / n_total, 2)
  ) %>%
  arrange(desc(pct_missing))

print(missingness_by_col, n = Inf)

# --- 3b. Missingness by Player ---
cat("\n--- Missingness by Player (total NAs per player) ---\n")

missingness_by_player <- df %>%
  group_by(player_id) %>%
  summarise(
    n_trials = n(),
    total_na = sum(is.na(pick(everything()))),
    .groups = "drop"
  ) %>%
  arrange(desc(total_na))

print(missingness_by_player)

# --- 3c. Duplicate Row Check ---
cat("\n--- Duplicate Rows Check ---\n")

n_duplicates <- df %>%
  duplicated() %>%
  sum()

cat("Number of fully duplicated rows:", n_duplicates, "\n")

# Check for duplicate trial identifiers if shot_number exists
if ("shot_number" %in% names(df)) {
  dup_trials <- df %>%
    group_by(player_id, shot_number) %>%
    filter(n() > 1) %>%
    nrow()

  if (has_shot_type) {
    dup_trials_by_type <- df %>%
      group_by(player_id, shot_type, shot_number) %>%
      filter(n() > 1) %>%
      nrow()
    cat("Duplicate (player_id, shot_type, shot_number) combinations:", dup_trials_by_type, "\n")
  } else {
    cat("Duplicate (player_id, shot_number) combinations:", dup_trials, "\n")
  }
}

# --- 3d. Trial Counts per Player ---
cat("\n--- Trial Counts per Player ---\n")

trials_per_player <- df %>%
  count(player_id, name = "n_trials") %>%
  arrange(player_id)

print(trials_per_player)
cat("\nExpected: 20 trials per player (10 2PT + 10 3PT)\n")

# By shot type if available
if (has_shot_type) {
  cat("\n--- Trial Counts by Shot Type ---\n")

  trials_by_type <- df %>%
    count(player_id, shot_type) %>%
    pivot_wider(names_from = shot_type, values_from = n, values_fill = 0)

  print(trials_by_type)

  # Check completeness
  complete_players <- trials_by_type %>%
    filter(`2PT` == 10 & `3PT` == 10) %>%
    nrow()

  cat("\nPlayers with complete data (10 2PT + 10 3PT):",
      complete_players, "of", n_distinct(df$player_id), "\n")
}

# ==============================================================================
# 4. Numeric Variable Overview
# ==============================================================================

cat("\n==============================================================================\n")
cat("NUMERIC VARIABLE OVERVIEW\n")
cat("==============================================================================\n\n")

# Identify numeric columns (exclude identifiers)
exclude_cols <- c("player_id", "shot_type", "made", "position")
numeric_cols <- df %>%
  select(-any_of(exclude_cols)) %>%
  select(where(is.numeric)) %>%
  names()

cat("Identified", length(numeric_cols), "numeric columns:\n")
cat(paste(numeric_cols, collapse = ", "), "\n\n")

# Create summary table
numeric_summary <- df %>%
  select(all_of(numeric_cols)) %>%
  summarise(across(everything(), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    iqr = ~ IQR(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE),
    p1 = ~ quantile(.x, 0.01, na.rm = TRUE),
    p99 = ~ quantile(.x, 0.99, na.rm = TRUE),
    pct_missing = ~ round(100 * sum(is.na(.x)) / length(.x), 2)
  ))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_pattern = "(.+)_(.+)$"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    p99_p1_ratio = ifelse(p1 != 0, round(p99 / p1, 2), NA),
    range = max - min
  ) %>%
  arrange(variable)

cat("--- Numeric Summary Table ---\n")
print(numeric_summary, n = Inf, width = Inf)

# Flag potential artifact columns
cat("\n--- Potential Artifact Columns (extreme p99/p1 ratio > 100 or suspicious ranges) ---\n")

artifact_flags <- numeric_summary %>%
  filter(p99_p1_ratio > 100 | is.na(p99_p1_ratio) | min < 0)

if (nrow(artifact_flags) > 0) {
  print(artifact_flags)
} else {
  cat("No obvious artifact columns detected.\n")
}

# ==============================================================================
# 5. Outlier Screening (EDA only - no deletion)
# ==============================================================================

cat("\n==============================================================================\n")
cat("OUTLIER SCREENING\n")
cat("==============================================================================\n\n")

# --- 5a. Global Outliers (IQR rule) ---
cat("--- Global Outliers (IQR x 1.5 rule) ---\n")

global_outliers <- tibble(
  column = character(),
  n_outliers = integer(),
  pct_outliers = numeric()
)

for (col in numeric_cols) {
  x <- df[[col]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr

  n_out <- sum(x < lower | x > upper, na.rm = TRUE)
  pct_out <- round(100 * n_out / sum(!is.na(x)), 2)

  global_outliers <- global_outliers %>%
    add_row(column = col, n_outliers = n_out, pct_outliers = pct_out)
}

global_outliers <- global_outliers %>% arrange(desc(n_outliers))
print(global_outliers, n = Inf)

# --- 5b. Within-Player Outliers (z-score) ---
cat("\n--- Within-Player Outliers (|z| > 3, players with >= 5 trials) ---
\n")

within_player_outliers <- tibble(
  column = character(),
  n_outliers = integer()
)

# Players with >= 5 trials
valid_players <- df %>%
  count(player_id) %>%
  filter(n >= 5) %>%
  pull(player_id)

df_valid <- df %>% filter(player_id %in% valid_players)

for (col in numeric_cols) {
  # Calculate z-scores within player
  z_scores <- df_valid %>%
    group_by(player_id) %>%
    mutate(
      col_mean = mean(.data[[col]], na.rm = TRUE),
      col_sd = sd(.data[[col]], na.rm = TRUE),
      z = ifelse(col_sd > 0, (.data[[col]] - col_mean) / col_sd, 0)
    ) %>%
    ungroup() %>%
    pull(z)

  n_out <- sum(abs(z_scores) > 3, na.rm = TRUE)

  within_player_outliers <- within_player_outliers %>%
    add_row(column = col, n_outliers = n_out)
}

within_player_outliers <- within_player_outliers %>%
  arrange(desc(n_outliers))

print(within_player_outliers, n = Inf)

# --- Combined Outlier Summary ---
cat("\n--- Combined Outlier Summary ---\n")

outlier_summary <- global_outliers %>%
  rename(global_outliers = n_outliers, global_pct = pct_outliers) %>%
  left_join(
    within_player_outliers %>% rename(within_player_outliers = n_outliers),
    by = "column"
  ) %>%
  arrange(desc(global_outliers + within_player_outliers))

print(outlier_summary, n = Inf)

# ==============================================================================
# 6. Visualizations
# ==============================================================================

cat("\n==============================================================================\n")
cat("GENERATING VISUALIZATIONS\n")
cat("==============================================================================\n\n")

# Check if figures directory exists
save_figs <- dir.exists("figures/eda")
if (save_figs) {
  cat("Figures will be saved to figures/eda/\n\n")
} else {
  cat("figures/eda/ not found - plots will display only (not saved)\n\n")
}

# --- 6A. Missingness Plot ---
cat("Creating missingness plot...\n")

plots$missingness <- missingness_by_col %>%
  filter(pct_missing > 0) %>%
  ggplot(aes(x = reorder(column, pct_missing), y = pct_missing)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(pct_missing, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Missing Data by Column",
    x = "Column",
    y = "% Missing"
  ) +
  theme(axis.text.y = element_text(size = 8)) +
  ylim(0, max(missingness_by_col$pct_missing) * 1.15)

print(plots$missingness)

if (save_figs) {
  ggsave("figures/eda/01_missingness.png", plots$missingness,
         width = 8, height = 6, dpi = 150)
}

# --- 6B. Distributions ---
cat("Creating distribution plots...\n")

# Select key variables (by variance + keywords)
key_keywords <- c("force", "power", "jump", "brak", "propul", "duration", "depth", "height")

# Get top 10 by variance
var_ranking <- df %>%
  select(all_of(numeric_cols)) %>%
  summarise(across(everything(), ~ var(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "variance") %>%
  arrange(desc(variance))

top_by_variance <- var_ranking %>% slice_head(n = 10) %>% pull(column)

# Get keyword matches
keyword_matches <- numeric_cols[str_detect(tolower(numeric_cols),
                                           paste(key_keywords, collapse = "|"))]

# Combine (unique)
key_vars <- unique(c(keyword_matches, top_by_variance))[1:min(12, length(unique(c(keyword_matches, top_by_variance))))]

cat("Key variables for distribution plots:", paste(key_vars, collapse = ", "), "\n\n")

# Overall histograms
plots$histograms <- df %>%
  select(all_of(key_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  labs(title = "Distributions of Key Numeric Variables", x = "Value", y = "Count") +
  theme(strip.text = element_text(size = 7))

print(plots$histograms)

if (save_figs) {
  ggsave("figures/eda/02_histograms.png", plots$histograms,
         width = 12, height = 8, dpi = 150)
}

# Stratified by shot_type if available
if (has_shot_type) {
  cat("Creating stratified boxplots by shot type...\n")

  plots$boxplots_by_shot <- df %>%
    select(shot_type, all_of(key_vars)) %>%
    pivot_longer(-shot_type, names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = shot_type, y = value, fill = shot_type)) +
    geom_boxplot(alpha = 0.7, outlier.size = 1) +
    facet_wrap(~ variable, scales = "free_y", ncol = 4) +
    scale_fill_manual(values = c("2PT" = "#1f77b4", "3PT" = "#ff7f0e")) +
    labs(title = "Distributions by Shot Type", x = "Shot Type", y = "Value") +
    theme(
      strip.text = element_text(size = 7),
      legend.position = "none"
    )

  print(plots$boxplots_by_shot)

  if (save_figs) {
    ggsave("figures/eda/03_boxplots_by_shot.png", plots$boxplots_by_shot,
           width = 12, height = 8, dpi = 150)
  }
}

# --- 6C. Player-Level Variability ---
cat("Creating player-level variability plots...\n")

# Find jump height column
jump_col <- find_col(numeric_cols, c("jump_height", "jump", "height"))

if (!is.na(jump_col) && has_shot_type) {
  cat("Using", jump_col, "for player-level plots\n")

  # Player means by shot type
  player_means <- df %>%
    group_by(player_id, shot_type) %>%
    summarise(mean_value = mean(.data[[jump_col]], na.rm = TRUE), .groups = "drop")

  # Paired dot plot
  plots$player_paired <- player_means %>%
    pivot_wider(names_from = shot_type, values_from = mean_value) %>%
    ggplot(aes(x = `2PT`, y = `3PT`)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_text(aes(label = player_id), hjust = -0.2, vjust = -0.2, size = 3) +
    labs(
      title = paste("Player Mean", jump_col, ": 2PT vs 3PT"),
      x = "Mean 2PT",
      y = "Mean 3PT"
    ) +
    coord_equal()

  print(plots$player_paired)

  if (save_figs) {
    ggsave("figures/eda/04_player_paired_jump.png", plots$player_paired,
           width = 7, height = 7, dpi = 150)
  }

  # Spaghetti plot
  plots$spaghetti <- player_means %>%
    ggplot(aes(x = shot_type, y = mean_value, group = player_id, color = player_id)) +
    geom_line(alpha = 0.7, linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Player-Level Mean", jump_col, "by Shot Type"),
      x = "Shot Type",
      y = paste("Mean", jump_col),
      color = "Player"
    ) +
    theme(legend.position = "right")

  print(plots$spaghetti)

  if (save_figs) {
    ggsave("figures/eda/05_spaghetti_jump.png", plots$spaghetti,
           width = 8, height = 6, dpi = 150)
  }
}

# --- 6D. Correlation Structure ---
cat("Creating correlation heatmap...\n")

# Compute correlation matrix
cor_matrix <- df %>%
  select(all_of(numeric_cols)) %>%
  cor(use = "complete.obs")

# Convert to long format for ggplot
cor_long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

# Heatmap
plots$cor_heatmap <- cor_long %>%
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1)
  ) +
  labs(title = "Correlation Matrix of Numeric Variables", x = "", y = "") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6)
  )

print(plots$cor_heatmap)

if (save_figs) {
  ggsave("figures/eda/06_correlation_heatmap.png", plots$cor_heatmap,
         width = 12, height = 10, dpi = 150)
}

# Highlight strong correlations (|r| > 0.7)
cat("\n--- Strong Correlations (|r| > 0.7) ---\n")

strong_cors <- cor_long %>%
  filter(var1 < var2) %>%  # Upper triangle only
  filter(abs(correlation) > 0.7) %>%
  arrange(desc(abs(correlation)))

print(strong_cors, n = 30)

# ==============================================================================
# 7. Make Rate Analysis (if made exists)
# ==============================================================================

if (has_made) {
  cat("\n==============================================================================\n")
  cat("MAKE RATE ANALYSIS\n")
  cat("==============================================================================\n\n")

  # Overall make rate
  overall_make_rate <- mean(df$made, na.rm = TRUE)
  cat("Overall make rate:", round(overall_make_rate * 100, 1), "%\n")

  # By shot type
  if (has_shot_type) {
    cat("\nMake rate by shot type:\n")
    make_by_type <- df %>%
      group_by(shot_type) %>%
      summarise(
        n = n(),
        makes = sum(made, na.rm = TRUE),
        make_rate = round(mean(made, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      )
    print(make_by_type)
  }

  # By player (ranked)
  cat("\nMake rate by player (ranked):\n")
  make_by_player <- df %>%
    group_by(player_id) %>%
    summarise(
      n = n(),
      makes = sum(made, na.rm = TRUE),
      make_rate = round(mean(made, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(make_rate))

  print(make_by_player)

  # Make rate bar chart by player
  plots$make_rate_player <- make_by_player %>%
    ggplot(aes(x = reorder(player_id, make_rate), y = make_rate)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = overall_make_rate * 100, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = "Make Rate by Player",
      subtitle = paste("Red line = overall mean (", round(overall_make_rate * 100, 1), "%)"),
      x = "Player ID",
      y = "Make Rate (%)"
    )

  print(plots$make_rate_player)

  if (save_figs) {
    ggsave("figures/eda/07_make_rate_player.png", plots$make_rate_player,
           width = 8, height = 6, dpi = 150)
  }

  # Make rate vs jump height (if jump column exists)
  if (!is.na(jump_col)) {
    cat("\nCreating make rate vs jump height plot...\n")

    # Bin jump height
    df_binned <- df %>%
      mutate(jump_bin = cut(.data[[jump_col]],
                            breaks = quantile(.data[[jump_col]], probs = seq(0, 1, 0.1), na.rm = TRUE),
                            include.lowest = TRUE))

    make_by_jump <- df_binned %>%
      filter(!is.na(jump_bin)) %>%
      group_by(jump_bin) %>%
      summarise(
        n = n(),
        make_rate = mean(made, na.rm = TRUE),
        mean_jump = mean(.data[[jump_col]], na.rm = TRUE),
        .groups = "drop"
      )

    plots$make_vs_jump <- df %>%
      ggplot(aes(x = .data[[jump_col]], y = made)) +
      geom_jitter(alpha = 0.3, height = 0.1, width = 0) +
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      labs(
        title = paste("Make Rate vs", jump_col, "(EDA only, no inference)"),
        x = jump_col,
        y = "Made (0/1)"
      )

    print(plots$make_vs_jump)

    if (save_figs) {
      ggsave("figures/eda/08_make_vs_jump.png", plots$make_vs_jump,
             width = 8, height = 5, dpi = 150)
    }
  }
}

# ==============================================================================
# 8. EDA Takeaways
# ==============================================================================

cat("\n==============================================================================\n")
cat("EDA TAKEAWAYS\n")
cat("==============================================================================\n\n")

cat("COMPLETENESS ISSUES:\n")
high_missing <- missingness_by_col %>% filter(pct_missing > 5)
if (nrow(high_missing) > 0) {
  cat("- Columns with >5% missing:", paste(high_missing$column, collapse = ", "), "\n")
} else {
  cat("- No columns with >5% missing after covariate propagation\n")
}

incomplete <- trials_per_player %>% filter(n_trials != 20)
if (nrow(incomplete) > 0) {
  cat("- Players with incomplete trials:", paste(incomplete$player_id, collapse = ", "), "\n")
} else {
  cat("- All players have expected 20 trials\n")
}

cat("\nPOTENTIAL ARTIFACT VARIABLES:\n")
if (nrow(artifact_flags) > 0) {
  cat("- Flagged:", paste(artifact_flags$variable, collapse = ", "), "\n")
  cat("  (Extreme p99/p1 ratios or negative minimums)\n")
} else {
  cat("- No obvious artifact variables detected\n")
}

cat("\nVARIABLE STABILITY:\n")
# Variables with low coefficient of variation (CV)
cv_summary <- numeric_summary %>%
  mutate(cv = sd / abs(mean)) %>%
  arrange(cv)

stable_vars <- cv_summary %>% filter(cv < 0.3) %>% pull(variable)
noisy_vars <- cv_summary %>% filter(cv > 0.5) %>% pull(variable)

cat("- Most stable (CV < 0.3):", paste(head(stable_vars, 5), collapse = ", "), "\n")
cat("- Most variable (CV > 0.5):", paste(head(noisy_vars, 5), collapse = ", "), "\n")

cat("\nCOLLINEARITY PATTERNS:\n")
if (nrow(strong_cors) > 0) {
  cat("- Found", nrow(strong_cors), "pairs with |r| > 0.7\n")
  cat("- Strongest correlations:\n")
  top_cors <- strong_cors %>% slice_head(n = 5)
  for (i in 1:nrow(top_cors)) {
    cat("  ", top_cors$var1[i], " <-> ", top_cors$var2[i],
        " (r = ", round(top_cors$correlation[i], 2), ")\n", sep = "")
  }
} else {
  cat("- No pairs with |r| > 0.7\n")
}

if (has_shot_type) {
  cat("\nSHOT TYPE DIFFERENCES (visual inspection):\n")
  cat("- Check boxplots for systematic 2PT vs 3PT differences\n")
  cat("- Jump height typically higher for 3PT (greater distance)\n")
}

if (has_made) {
  cat("\nMAKE RATE OBSERVATIONS:\n")
  cat("- Overall:", round(overall_make_rate * 100, 1), "%\n")
  if (has_shot_type) {
    cat("- By shot type: see table above\n")
  }
  cat("- Player variation: ", round(min(make_by_player$make_rate), 1), "% - ",
      round(max(make_by_player$make_rate), 1), "%\n", sep = "")
}

cat("\n==============================================================================\n")
cat("EDA COMPLETE\n")
cat("==============================================================================\n")
cat("\nPlots stored in 'plots' list with elements:\n")
cat(paste(names(plots), collapse = ", "), "\n")
