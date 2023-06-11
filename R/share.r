library(tidyverse)
library(scales)
library(parallel)
library(glue)

source('R/stock.r')
source('R/plot.r')


percent2 <- function(number, accuracy=0.01){
  percent(number, accuracy)
}

gen_monkey_return <- function(ret.space, rescale=TRUE){
  count <- length(ret.space)
  # Use algorithm in the lecture notes
  U <- sort(runif(count))
  # if no rescale, the total weight will be equal to the max of U, which is always less than 1
  if (rescale){
    U <- U/U[count]
  }
  # bug if "lag" is used
  W <- U - dplyr::lag(U, 1, default = 0)
  return(sum(W * ret.space))
}

simulate_monkey <- function(x, path.count, ret.space){
  replicate(
    path.count,
    gen_monkey_return(rescale = !ALLOW.CASH, ret.space = ret.space)
  )
}

simulate_monkey_many <- function(ret.space, sample.count, path.count){
  # cat('sample.count', sample.count, 'path.count', path.count, '\n')
  CORES = detectCores() - 1
  cl <- makeCluster(CORES)
  clusterExport(cl, "gen_monkey_return")
  clusterExport(cl, "ALLOW.CASH")
  monkey.many <- parLapply(
    cl,
    1:sample.count,
    simulate_monkey,
    path.count=path.count,
    ret.space=ret.space
  )
  stopCluster(cl)
  monkey.many
}

# example.p.many <- simulate_p_many(
#   ret.space = table$Return,
#   candidate.ret = candidate_ret,
#   benchmark.ret = benchmark_ret,
#   sample.count = 10,
#   path.count = 10000
# )
# t.test(example.p.many)
# ggplot(tibble(p=example.p.many), aes(x=p)) + geom_density()

compute_q_value <- function(monkey.ret, candidate.ret, benchmark.ret){
  monkey_cdf <- ecdf(monkey.ret)
  candidate.p <- 1- monkey_cdf(candidate.ret)
  benchmark.p <- 1- monkey_cdf(benchmark.ret)
  # cat(candidate.p, benchmark.p, '\n')
  if (candidate.p == 0){
    return (1)
  }
  return (1 - candidate.p / benchmark.p)
}

compute_q_many <- function(monkey.ret.many, candidate.ret, benchmark.ret){
  sapply(
    monkey.ret.many,
    compute_q_value,
    candidate.ret=candidate.ret,
    benchmark.ret=benchmark.ret
  )
}

# simulate_q_value <- function(x, path.count, ret.space, candidate.ret, benchmark.ret){
#   monkey <- replicate(
#     path.count,
#     gen_monkey_return(rescale = !ALLOW_CASH, ret.space = ret.space)
#   )
#   compute_q_value(monkey, candidate.ret, benchmark.ret)
# }


generate_q_plot <- function(q.many, name='', conf.level=0.95){
  q.pdf <- density(q.many)

  # t.test gives error if all data are the same
  if (var(q.many) > 0){
    q.t.test <- t.test(q.many, conf.level = conf.level)
    q.lq <- q.t.test$conf.int[1]
    q.hq <- q.t.test$conf.int[2]
  } else {
    q.lq <- q.many[1]
    q.hq <- q.many[1]
  }

  pdf.segments <- gen_pdf_segments(q.pdf, table=bind_rows(
    list(value=q.lq, col=paste('95% LQ =', formatC(q.lq, 4))),
    list(value=q.hq, col=paste('95% HQ =', formatC(q.hq, 4))),
  ))

  tibble(q=q.many) %>% ggplot(aes(x=q)) +
    ggtitle(paste('Distribution of Q Measure', name), paste('samples =', length(q.many))) +
    geom_density() +
    pdf.segments
}

save_plot <- function(plot, name){
  ggsave(glue('output/{name}.png'), plot, bg='transparent')
}

generate_report <- function(
    stock.space,
    candidate.ret,
    benchmark.ret,
    start.date,
    end.date,
    sample.count = 30,
    path.count = 5000,
    candidate.name = 'Candidate',
    benchmark.name = 'Benchmark'
){
  name <- gsub(" ", ".", candidate.name)

  stock.space <- sort(stock.space)
  stock.data <- get_stock_data_cached(stock.space, start.date, end.date) %>%
    mutate(ret=end.price/start.price-1)

  monkey.many <- simulate_monkey_many(stock.data$ret, sample.count=sample.count, path.count=path.count)

  monkey.one <- monkey.many[[1]]
  monkey.df <- tibble(ret=monkey.one)
  monkey.pdf <- density(monkey.one)
  monkey.cdf <- ecdf(monkey.one)
  monkey.ret <- median(monkey.one)

  monkey.pdf.segments <- gen_pdf_segments(monkey.pdf, table=bind_rows(
    list(value=candidate.ret, col=paste(candidate.name, percent2(candidate.ret))),
    list(value=benchmark.ret, col=paste(benchmark.name, percent2(benchmark.ret))),
    list(value=monkey.ret, col=paste('Monkey', percent(monkey.ret, 0.01))),
  ))

  plot.pdf <- monkey.df %>%
    ggplot(aes(x=ret)) +
    ggtitle('Monkey Portfolio PDF') +
    geom_density() +
    monkey.pdf.segments
  plot.pdf %>% save_plot(glue('{name}.pdf'))

  monkey.cdf.segments <- gen_cdf_segments(monkey.cdf, bind_rows(
    list(value=candidate.ret, col=paste(candidate.name, percent2(candidate.ret))),
    list(value=benchmark.ret, col=paste(benchmark.name, percent2(benchmark.ret))),
    list(value=monkey.ret, col=paste('Monkey', percent2(monkey.ret))),
  ))

  plot.cdf <- monkey.df %>%
    ggplot(aes(x=ret)) +
    ggtitle('Monkey Portfolio CDF') +
    stat_ecdf(geom = "step") +
    monkey.cdf.segments
  plot.cdf %>% save_plot(glue('{name}.cdf'))

  q.many <- compute_q_many(monkey.many, candidate.ret, benchmark.ret)

  result <- bind_rows(
    tibble(Name='Candidate Name', Value=candidate.name),
    tibble(Name='Candidate Ret', Value=percent2(candidate.ret)),
    tibble(Name='Benchmark Name', Value=benchmark.name),
    tibble(Name='Benchmark Ret', Value=percent2(benchmark.ret)),
    tibble(Name='Evaluation Period', Value=paste(start.date, end.date)),
    tibble(Name='Outperformance', Value=percent2(candidate.ret - benchmark.ret)),
    tibble(Name='Q Measure', Value=formatC(mean(q.many), 4))
  )

  plot.q <- generate_q_plot(q.many)
  plot.q %>% save_plot(glue('{name}.q'))

  return (list(
    result=result,
    plot.pdf=plot.pdf,
    plot.cdf=plot.cdf,
    plot.q=plot.q,
    q.many=q.many,
    monkey.many=monkey.many
  ))
}

# For computation test

simple_run <- function(n, sample.count, path.count){
  ret.space <- rnorm(n)
  monkey.many <- simulate_monkey_many(ret.space, sample.count, path.count)
  mean.ret <- mean(monkey.many[[1]])
  error <- sd(monkey.many[[1]])
  compute_q_many(monkey.many, mean.ret + error, mean.ret - error)
}

generate_computation_data <- function(sample.count, n.choices, path.choices) {
  COMP.CACHE.NAME <- 'rds/comp.rds'
  if (file.exists(COMP.CACHE.NAME)){
    return (readRDS(COMP.CACHE.NAME))
  }
  # Reduce the sample count and multiply the result time by this factor
  # as the time is almost proportional to the sample count
  cheat.factor.s <- 1
  samples.count <- sample.count / cheat.factor.s
  # Reduce the sample count and multiply the result time by this factor
  # as the time is almost proportional to the path count
  cheat.factor.p <- 1
  path.choices <- path.choices / cheat.factor.p

  result <- crossing(path.choices, n.choices) %>% mutate(time=0)
  time.col <- numeric(nrow(result))

  index = 1
  for (path.count in path.choices){
    for (n in n.choices){
      timed <- system.time(simple_run(n, sample.count, path.count))
      result[index, 'time'] <- timed['elapsed'] * cheat.factor.s * cheat.factor.p
      index <- index + 1
    }
  }
  result <- result %>% mutate(path.choices=path.choices * cheat.factor.p)
  result %>% saveRDS(COMP.CACHE.NAME)
  return (result)
}
