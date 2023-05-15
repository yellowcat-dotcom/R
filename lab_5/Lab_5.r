library("rvest")

url_21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

nodes_21<-html_nodes(url_21, 'table')
nodes_20<-html_nodes(url_20, 'table')
nodes_19<-html_nodes(url_19, 'table')
nodes_18<-html_nodes(url_18, 'table')
nodes_17<-html_nodes(url_17, 'table')
nodes_16<-html_nodes(url_16, 'table')
nodes_15<-html_nodes(url_15, 'table')
nodes_14<-html_nodes(url_14, 'table')

df_21<-html_table(nodes_21[[2]])%>%as.data.frame();df_21
df_20<-html_table(nodes_20[[2]])%>%as.data.frame()
df_19<-html_table(nodes_19[[2]])%>%as.data.frame()
df_18<-html_table(nodes_18[[2]])%>%as.data.frame()
df_17<-html_table(nodes_17[[2]])%>%as.data.frame()
df_16<-html_table(nodes_16[[2]])%>%as.data.frame()
df_15<-html_table(nodes_15[[2]])%>%as.data.frame()
df_14<-html_table(nodes_14[[2]])%>%as.data.frame()

rownames(df_21)<-df_21[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_14)<-df_14[, 2]

df_21<-df_21[, 3:11];df_21
df_20<-df_20[, 3:11]
df_19<-df_19[, 3:11]
df_18<-df_18[, 3:11]
df_17<-df_17[, 3:11]
df_16<-df_16[, 3:11]
df_15<-df_15[, 3:11]
df_14<-df_14[, 3:11]

v<-c('China', 'India', 'Japan', 'Austria', 'Australia')

s<-'Quality of Life Index'
QLI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
);QLI
colnames(QLI)<-v

years<-2014:2021
mn<-min(QLI, na.rm=TRUE)
mx<-max(QLI, na.rm=TRUE)
plot(
  years,
  QLI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс качества жизни',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, QLI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, QLI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, QLI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, QLI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)


s<-'Purchasing Power Index'
PPI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPI)<-v

years<-2014:2021
mn<-min(PPI, na.rm=TRUE)
mx<-max(PPI, na.rm=TRUE)
plot(
  years,
  PPI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс покупательной способности',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, PPI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, PPI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, PPI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, PPI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'topright',  
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)


s<-'Safety Index'
SI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(SI)<-v

years<-2014:2021
mn<-min(SI, na.rm=TRUE)
mx<-max(SI, na.rm=TRUE)
plot(
  years,
  SI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс безопасности',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, SI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, SI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, SI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, SI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

s<-'Health Care Index'
HCI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(HCI)<-v

years<-2014:2021
mn<-min(HCI, na.rm=TRUE)
mx<-max(HCI, na.rm=TRUE)
plot(
  years,
  HCI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс здравоохранения',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, HCI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, HCI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, HCI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, HCI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright',  
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

s<-'Cost of Living Index'
CLI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(CLI)<-v

years<-2014:2021
mn<-min(CLI, na.rm=TRUE)
mx<-max(CLI, na.rm=TRUE)
plot(
  years,
  CLI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс стоимости жизни',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, CLI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, CLI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, CLI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, CLI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'topright',  
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

s<-'Property Price to Income Ratio'
PPIR<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PPIR)<-v

years<-2014:2021
mn<-min(PPIR, na.rm=TRUE)
mx<-max(PPIR, na.rm=TRUE)
plot(
  years,
  PPIR$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс соотношения цены недвижемости к доходу',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, PPIR$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, PPIR$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, PPIR$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, PPIR$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright',  
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

s<-'Traffic Commute Time Index'
TCTI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(TCTI)<-v

years<-2014:2021
mn<-min(TCTI, na.rm=TRUE)
mx<-max(TCTI, na.rm=TRUE)
plot(
  years,
  TCTI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс времени в пути на транспорте',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, TCTI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, TCTI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, TCTI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, TCTI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright',  
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

s<-'Pollution Index'
PI<-as.data.frame(
  rbind(
    df_14[v, s],
    df_15[v, s],
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2014:2021
)
colnames(PI)<-v

years<-2014:2021
mn<-min(PI, na.rm=TRUE)
mx<-max(PI, na.rm=TRUE)
plot(
  years,
  PI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Индекс загрязнения',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, PI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, PI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, PI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, PI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright', 
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

s<-'Climate Index'
CI<-as.data.frame(
  rbind(
    #df_14[v, s], нет данный
    #df_15[v, s], нет данных
    df_16[v, s],
    df_17[v, s],
    df_18[v, s],
    df_19[v, s],
    df_20[v, s],
    df_21[v, s]
  ),
  row.names<-2016:2021
)
colnames(CI)<-v

years<-2016:2021
mn<-min(CI, na.rm=TRUE)
mx<-max(CI, na.rm=TRUE)
plot(
  years,
  CI$'China',
  xlab='Год',
  ylab='Индекс',
  ylim=c(mn-13,mx+13),
  main='Климатический индекс',
  col='blue',
  type='o',
  lty=1,
  pch=1, 
  lwd=2
)
lines(years, CI$'India', type='o', col='green', lty=1, pch=1, lwd=2)
lines(years, CI$'Japan', type='o', col='red', lty=1, pch=1, lwd=2)
lines(years, CI$'Austria', type='o', col='purple', lty=1, pch=1, lwd=2)
lines(years, CI$'Australia', type='o', col='gold', lty=1, pch=1, lwd=2)
legend(y.intersp = 0.7, 
  'bottomright',  
  cex=0.7, 
  v,
  fill=c('blue', 'green', 'red', 'purple', 'gold')
)

url<-read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/');url

selector_name<-'a.post-list-item-title-link'
fnames<-html_nodes(url, selector_name)%>%html_text()%>%as.vector();fnames

selector_name<-'address.post-list-item-info'
fnames2<-html_nodes(url, selector_name)%>%html_text()%>%as.vector();fnames2

selector_name<-'.post-list-item-title-link'
fnames_addr<-html_nodes(url, selector_name)%>%html_attr('href')
fnames_addr2<-fnames_addr

d<-data.frame(fnames, fnames2, fnames_addr2);d
colnames(d)<-c('Название музея', 'Местоположение', 'Ссылка')
View(d)
