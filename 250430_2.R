df.match <- bruceR::import(here::here('data', 'match', 'match_raw.csv')) %>%
  tidyr::extract(
    Shape,
    into = c('Valence', 'Identity'),
    regex = '(moral|immoral)(Self|Other)',
    remove = FALSE) %>% # 将Shape列分为两列
  dplyr::mutate(
    Valence = factor(Valence,
                     levels = c('moral', 'immoral'),
                     labels = c('moral', 'immoral')),
    Identity = factor(Identity,
                      levels = c('Self', 'Other'),
                      labels = c('Self', 'Other'))) %>%
  dplyr::filter(ACC == 0 | ACC == 1, RT >= 0.2 & RT <= 1.5, Match == 'match', 
                (!Sub %in% c(7302, 7303, 7338))) # 过滤无效被试的数据