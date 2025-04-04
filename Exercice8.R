#to check out later 
# understand the code and try to repeat it by my self 

library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

backtest_ma_strategy <- function(df, execution_delay = 0, verbose = TRUE) {
  # Étape 1 : filtrer les clôtures
  df_close <- df %>%
    filter(metric == "close") %>%
    mutate(date = as.Date(date)) %>%
    arrange(date)
  
  # Étape 2 : extraire le dernier prix de chaque mois
  df_monthly <- df_close %>%
    group_by(month = floor_date(date, "month")) %>%
    summarise(close = last(value), .groups = "drop")
  
  # Étape 3 : MA(7)
  df_monthly <- df_monthly %>%
    mutate(ma7 = rollmean(close, k = 7, fill = NA, align = "right"))
  
  # Étape 4 : signaux
  df_monthly <- df_monthly %>%
    mutate(signal = ifelse(close > ma7, 1, -1))
  
  # Étape 5 : appliquer un délai d’exécution
  df_monthly <- df_monthly %>%
    mutate(signal_exec = lag(signal, execution_delay))
  
  # Étape 6 : rendement mensuel
  df_monthly <- df_monthly %>%
    mutate(
      return = close / lag(close) - 1,
      strategy_return = lag(signal_exec) * return,
      equity_curve = cumprod(1 + coalesce(strategy_return, 0))
    )
  
  # Étape 7 : stats
  if (verbose) {
    num_trades <- sum(diff(df_monthly$signal_exec) != 0, na.rm = TRUE)
    final_return <- last(df_monthly$equity_curve) - 1
    cat("📈 Nombre de trades :", num_trades, "\n")
    cat("💰 Rendement final :", round(final_return * 100, 2), "%\n")
  }
  
  # Étape 8 : courbe d'équité
  ggplot(df_monthly, aes(x = month, y = equity_curve)) +
    geom_line(color = "steelblue") +
    labs(
      title = "📊 Courbe d’Équité — Stratégie MA(7)",
      x = "Date",
      y = "Valeur du portefeuille"
    ) +
    theme_minimal()
  
  return(df_monthly)
}
