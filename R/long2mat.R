# data_long: 长格式的空间权重矩阵
# country_id: 国家或地区的 ID 的列名
# counter_id: 目标国家或地区的 ID 的列名
# contiguity: 权重列的列名
# id_is_int: 国家或地区的 ID 是否为整数型变量
# W_style: 对空间权重做什么样的标准化处理，默认 `NULL` 为不做处理，参考包 `spdep`

# m_df: 数据框格式的矩阵型空间权重矩阵，第一列代表 ID
# check_list: 原来字符型的 ID 与新生成的整数型 ID 的对照表

long2mat <- function(data_long, country_id, counter_id, contiguity, 
                     ..., id_is_int = FALSE, W_style = NULL) {
  if (id_is_int) {
    # 如果国家或地区的 ID 是整数型变量，那么就可以直接选取
    da <- data_long %>%
      select(c_id = !!country_id, cc_id = !!counter_id, Contiguity = !!contiguity)

    check_list <- NULL

  } else {
    # 如果国家或地区的 ID 不是整数型变量，就生成一组整数型变量与之对应
    da <- data_long %>%
      select(sc_id = !!country_id, scc_id = !!counter_id, Contiguity = !!contiguity)

    scid_cid <- da %>%
      select(sc_id) %>%
      unique.data.frame(.) %>%
      mutate(c_id = c(1:n()))

    sccid_ccid <- scid_cid %>%
      rename(scc_id = sc_id, cc_id = c_id)

    da <- da %>% left_join(scid_cid) %>% left_join(sccid_ccid) %>%
      select(c_id, cc_id, Contiguity)
    
    check_list <- scid_cid %>%
      `colnames<-`(c(country_id, "ID"))

  }
  # 长格式转变成矩阵型
  da_m <- da %>%
    spread(key = cc_id, value = Contiguity, drop = FALSE)

  # 是否做标准化处理
  if (is.null(W_style)) {

    m_df <- da_m

  } else {

    l <- mat2listw(as.matrix(da_m[, -1]), row.names = da_m$c_id, style = W_style)

    m <- listw2mat(l)

    m_df <- as_tibble(cbind(rownames(m), m))

  }

  # 返回一个列表，包括 [`m_df`, `check_list`, `id_is_int`]
  return(list(m_df = m_df, check_list = check_list, id_is_int = id_is_int))

}




