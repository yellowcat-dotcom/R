print('MEN')
dm <- read.csv("C:/Users/valen/Documents/olimp_men.csv", sep = ";" , header = T, fileEncoding="cp1251"); dm
print('WOMEN')
dw <- read.csv("C:/Users/valen/Documents/olimp_women.csv", sep = ";" , header = T, fileEncoding="cp1251"); dw
print('MIX')
dx <- read.csv("C:/Users/valen/Documents/olimp_mix.csv", sep = ";" , header = T, fileEncoding="cp1251"); dx



stat_places_m <- sapply(dm[,-1], sum); stat_places_m
stat_places_w <- sapply(dw[,-1], sum); stat_places_w
stat_places_x <- sapply(dx[,-1], sum); stat_places_x

print("Cтолбчатая диаграмма по количеству мест 1-8")

par(mfrow=c(1,3))

barp <- barplot(stat_places_m, names=c(1:8), ylim=c(0,10), xlab="Место", ylab="Количество", main="Мужчины (за последние 30 лет)")
text(barp, stat_places_m + 0.5, labels = stat_places_m)

barp <- barplot(stat_places_w, names=c(1:8), ylim=c(0,10), xlab="Место", ylab="Количество", main="Женщины (за последние 30 лет)")
text(barp, stat_places_w + 0.5, labels = stat_places_w)

barp <- barplot(stat_places_x, names=c(1:8), ylim=c(0,10),  xlab="Место", ylab="Количество", main="Микст (за последние 30 лет)")
text(barp, stat_places_x + 0.5, labels = stat_places_x)


print("Круговая диаграмма по количеству первых мест в каждой из олимпиад")

gold_m <- dm[,c(1:2)][dm$X1 > 0, ];gold_m
gold_w <- dw[,c(1:2)][dw$X1 > 0, ];gold_w
gold_x <- dx[,c(1:2)][dx$X1 > 0, ];gold_x


pie(gold_m$X1, labels=gold_m$X1, col=rainbow(length(gold_m$X1)), main = "Золото. Мужчины\nза последние 30 лет")
legend(-1, -1, gold_m$Год, cex = 0.7, fill=rainbow(length(gold_m$Год)))

pie(gold_w$X1, labels=gold_w$X1, col=rainbow(length(gold_w$X1)), main = "Золото. Женщины\nза последние 30 лет")
legend(-1, -1, gold_w$Год, cex = 0.7, fill=rainbow(length(gold_w$Год)))

pie(gold_x$X1, labels=gold_x$X1, col=rainbow(length(gold_x$X1)), main = "Золото. Микст\nза последние 30 лет")
legend(-1, -1, gold_x$Год, cex = 0.7, fill=rainbow(length(gold_x$Год)))


print("Функциональные графики - тенденции изменения количества призовых мест")

prize_m <- data.frame(Год=dm$Год, Призовых=rowSums(dm[, 2:4]));prize_m
prize_w <- data.frame(Год=dw$Год, Призовых=rowSums(dw[, 2:4]))
prize_x <- data.frame(Год=dx$Год, Призовых=rowSums(dx[, 2:4]))


par(mfrow=c(1,1))
plot(prize_m, type="o", pch=19, col="darkblue", xaxt="n", ylim=c(0,7), panel.first=grid(), main="Призовые места Индонезии по бадминтону за 30 лет")
lines(prize_w, type="o", pch=19, col="red")
lines(prize_x, type="o", pch=19, col="black")
legend(2012, 7, c("Мужчины", "Женщины","Микст"), cex=0.6, fill=c("darkblue", "red","black"))
axis(side=1, at=prize_m$Год)



print('GOLD')
gold <- read.csv("C:/Users/valen/Documents/gold.csv", sep = ";" , header = T, fileEncoding="cp1251"); gold

plot(gold$Год, gold$США, type="o", pch=19, col="#CC0000", xaxt="n", ylim=c(0,50), xlab="Год", panel.first=grid(), ylab="Кол-во медалей", main="Золотые медали за 6 последних олимпиад (лето)")
lines(gold$Год, gold$Китай, type="o", pch=19, col="#FF8000")
lines(gold$Год, gold$Япония, type="o", pch=19, col="#999900")
lines(gold$Год, gold$Великобритания, type="o", pch=19, col="#00FF00")
lines(gold$Год, gold$Россия, type="o", pch=19, col="#000099")
lines(gold$Год, gold$Австралия, type="o", pch=19, col="#FF33ff")
lines(gold$Год, gold$Нидерланды, type="o", pch=19, col="#1aafd0")
axis(side=1, at=gold$Год)
legend(max(gold$Год)-1, 55, cex=0.5, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#CC0000", "#FF8000", "#999900", "#00FF00", "#000099", "#FF33ff", "#1aafd0"))



print('PRIZ')
priz <- read.csv("C:/Users/valen/Documents/priz.csv", sep = ";" , header = T, fileEncoding="cp1251"); priz

plot(priz$Год, priz$США, type="o", pch=19, col="#CC0000", xaxt="n", ylim=c(0,125), xlab="Год", ylab="Кол-во медалей", panel.first=grid(), main="Призовые медали за 6 последних олимпиад(лето)")
lines(priz$Год, priz$Китай, type="o", pch=19, col="#FF8000")
lines(priz$Год, priz$Япония, type="o", pch=19, col="#999900")
lines(priz$Год, priz$Великобритания, type="o", pch=19, col="#00FF00")
lines(priz$Год, priz$Россия, type="o", pch=19, col="#000099")
lines(priz$Год, priz$Австралия, type="o", pch=19, col="#FF33ff")
lines(priz$Год, priz$Нидерланды, type="o", pch=19, col="#1aafd0")
axis(side=1, at=priz$Год)
legend(max(priz$Год)-3.5, 135, cex=0.5, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#CC0000", "#FF8000", "#999900", "#00FF00", "#000099", "#FF33ff", "#1aafd0"))


print('task_4')

sum_m <- data.frame(Год=dm$Год, Всего=rowSums(dm[, 2:9]));sum_m
sum_w <- data.frame(Год=dw$Год, Всего=rowSums(dw[, 2:9]));sum_w


par(mfrow=c(1,3))
plot(sum_m, type="b", pch=19, col="darkblue", xaxt="n", ylim=c(0,7), panel.first=grid(), main="Всего меделей\nМужчины.Женщины")
lines(sum_w, type="o", pch=11, col="red")
legend(min(sum_w$Год), 7.2, cex=0.7 ,c("Мужчины", "Женщины"), fill=c("darkblue", "red"))
axis(side=1, at=sum_m$Год)


posled <- read.csv("C:/Users/valen/Documents/posled.csv", sep = ";" , header = T, fileEncoding="cp1251"); posled
prize_grouped = data.frame(М=posled$men, Ж=posled$women);prize_grouped
barplot(height=t(as.matrix(prize_grouped)), beside=TRUE, xlab="Год", ylab="Количество", names.arg=posled$X, col=c("darkblue", "red"), panel.first=grid(), main="Всего меделей\nМужчины.Женщины")

igig <- sapply(prize_grouped, sum);igig
pie(igig, labels=c(igig["М"], igig["Ж"]), col=c("darkblue", "red"), panel.first=grid(), main="Всего меделей\nМужчины.Женщины")


