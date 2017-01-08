require(blockmatrix)
require(magrittr)
require(Matrix)
require(plyr)
require(foreach)
require(quadprog)

nseats <- 1000
ntrains <- 3

M <- 10^7
eps <- 10^-3

S_mats <- list(matrix(1, 3, 3), matrix(2, 5, 3)) %>% 
  do.call(what = 'rbind')

S_mats <- sparseMatrix(1:3, 1:3, x = 1)


data <- generate_data(beta, price, time_bp, ntrains, nstations, 1000, keep_all = T)  

data2 <- data %>% 
  left_join(as.data.frame(routes), by = c('from', 'to')) %>% 
  mutate(i = row_number(2*train + time)) %>% 
  as_data_frame()

S_mats <- data2 %$%
  sparseMatrix(i, route, x = 1)

R_mats_list <- 
  data2 %>% 
  select_if(grepl('^occup_', colnames(.)) | colnames(.) == 'train') %>% 
  dlply('train') %>% 
  lapply(select, -train) %>% 
  lapply(as.matrix) %>%
  lapply(unname)

constraints <- c('capacity', 'utility', 'availability', 'first_come')
Amat_list <- vector('list', 3 * length(constraints)) %>%
  matrix(ncol = 3, dimnames = list(constraints, c('x', 'y', 'z')))
bvec_list <- vector('list', length(constraints)) %>% set_names(constraints)
dim <- list(
  nrow = set_names(vector('list', length(constraints)), constraints),
  ncol = list(x = nrow(routes), y = nrow(data), z = nrow(data) * length(unique(data$from)))
)

# capacity constraints
Amat_list[['capacity', 'y']] <- t(bdiag(R_mats_list))
bvec_list$capacity <- rep(nseats, nrow(Amat_list[['capacity', 'y']]))
dim$nrow$capacity <- length(bvec_list$capacity)

# utility constraints
Amat_list[['utility', 'x']] <- S_mats
Amat_list[['utility', 'y']] <- Diagonal(nrow(S_mats), M)
bvec_list$utility <- data2$max_price + M
dim$nrow$utility <- nrow(data2)

# availability (z)
stairs <- function(mat) {
  n <- ncol(mat)
  foreach(i = seq_len(n), .combine = rbind) %do% {
    cbind(mat[,seq_len(i-1)], matrix(0, nrow = nrow(mat), ncol = n + 1 - i))
  }
}
Amat_list[['availability', 'z']] <- Diagonal(dim$ncol$z, nseats)
Amat_list[['availability', 'y']] <- R_mats_list %>% lapply(t) %>% lapply(stairs) %>% bdiag()
bvec_list$availability <- rep(nseats, dim$ncol$z)
dim$nrow$availability <- dim$ncol$z

# first comers
n_stat_from <- length(unique(data$from))
idx <- split(data2$i, data2$train) %>% 
  lapply(combn, m = 2) %>% lapply(as.vector) %>% unlist() %>% unname()
idx_i <- matrix(idx, 2)[1,]
Amat_list[['first_come', 'x']] <- S_mats[idx_i,]
Amat_list[['first_come', 'y']] <- data_frame(idx = idx, row = rep(seq(length(idx) / 2), each = 2), coef = rep(c(1, -1), length(idx) / 2)) %$%
  sparseMatrix(i = row, j = idx, x = coef * M)
Amat_list[['first_come', 'z']] <- 
  sparseMatrix(
    rep(seq(length(idx) / 2), each = n_stat_from),
    outer(seq(n_stat_from), (idx_i - 1) * n_stat_from, '+'),
    x = -M, dims = c(length(idx_i), dim$ncol$z)
  )
bvec_list$first_come <- -M * (n_stat_from + 1) + data2$max_price[matrix(idx, 2)[1,]] + eps
dim$nrow$first_come <- length(bvec_list$first_come)

Amat_l <- vector('list', nrow(Amat_list))
for (i in 1:nrow(Amat_list)) {
  for (j in 1:ncol(Amat_list)) {
    if(is.null(Amat_list[[i,j]])) {
      print(c(i,j))
      Amat_list[[i,j]] <- sparseMatrix(i = NULL, j = NULL, dims = c(dim$nrow[[i]], dim$ncol[[j]]))
    }
  }
  Amat_l[[i]] <- do.call('cbind', Amat_list[i,])
}
str(Amat_l)
Amat <- do.call('rbind', Amat_l)
bvec <- do.call('c', bvec_list) %>% unname()




Dmat <- 
  cbind(
    rbind(
      sparseMatrix(i = NULL, j = NULL, x = 1, dims = rep(ncol(S_mats), 2)),
      S_mats
    ),
    rbind(
      t(S_mats),
      sparseMatrix(i = NULL, j = NULL, x = 1, dims = rep(nrow(S_mats), 2))
    )
  ) %>% 
  cbind(matrix(0, nrow(Dmat), ncol(Amat) - ncol(Dmat))) %>% 
  rbind(matrix(0, ncol(Amat) - ncol(Dmat), ncol(Amat)))
dvec <- rep(0, ncol(Dmat))


solve.QP(-Dmat - Diagonal(nrow(Dmat), eps), dvec, t(Amat), bvec)
list(Dmat, dvec, Amat, bvec) %>% lapply(dim)
list(Dmat, dvec, Amat, bvec) %>% lapply(length)
Amat_list[1,] %>% str()

dim(Amat)

x[[1]] %>% attributes()


dim(R_mats)

bdiag()
