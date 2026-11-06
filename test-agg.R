invisible(source("load-model-explorer.R", echo = FALSE))
suppressMessages(library(dplyr))

model_means <- joined %>%
  group_by(model_id) %>%
  summarize(
    across(
      c(intel, code, agent, py, term, ifb, lctx, econ, hon,
        price_in = input_usd_per_mtok, price_out = output_usd_per_mtok,
        context = context_tokens),
      ~ mean(.x, na.rm = TRUE)
    ),
    n_variants = n(),
    .groups = "drop"
  )

cat("rows:", nrow(model_means), "\n")
sel <- model_means[model_means$model_id %in%
  c("z-ai/glm-5.3", "openai/gpt-6.1-sol", "moonshotai/kimi-k3.1-pro",
    "deepseek/deepseek-v4.1-flash-1017"), ]
print(as.data.frame(sel), row.names = FALSE, digits = 3)

cat("\nNaN per column (grupa bez zadnego pomiaru):\n")
print(colSums(is.nan(as.matrix(model_means[-1]))))
