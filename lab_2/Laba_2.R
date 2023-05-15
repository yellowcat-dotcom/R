#читаем данные из файла
df <- read.csv("C:/Users/valen/Documents/Model.csv", sep = ";" , header = T, fileEncoding="cp1251"); df

# 1 вычислить max, min, mean по каждому столбцу, 
# функции,вычисляющие требуемые значения
max_znach<-sapply(df[2:11], max, na.rm = TRUE);max_znach
min_znach<-sapply(df[2:11], min, na.rm = TRUE);min_znach
mean_znach<-sapply(df[2:11], mean, na.rm = TRUE);mean_znach

# 2 подсчитать количество людей, отдавших предпочтение >0.7 и <0.3 (составить вектор), 

#создаем нулевые вектора (счетчики)
count_07 <- integer(10)
count_03 <- integer(10)

#после чего суммируем значения, удовлетворяющие условию, по столбцам
print("Количество людей с предпочтениями >7:")
count_03 <-colSums(df[2:11]>7);count_03
print("Количество людей с предпочтениями <3:")
count_07 <-colSums(df[2:11]<3);count_07


# 3 вывести рейтинг компьютерных брендов в списке по убыванию, 
# создание вектора для рейтинга
rating <- integer(10)
# сортируем по убыванию средние значения оценок
rating<-sort(sapply(df[2:11], mean, na.rm = TRUE),decreasing = TRUE);rating

# 4 построить столбчатую диаграмму оценок (можно сделать разными способами),
barplot(count_07,main = " Оценки больше 7",xlab="Бренды",ylab="Количество",names = names(df[2:11]),cex.names = 0.8, col=rainbow(1))
barplot(count_03,main = " Оценки меньше 3",xlab="Бренды",ylab="Количество",names = names(df[2:11]),cex.names = 0.8, col=rainbow(1))
barplot(rating,main = "Рейтинг брендов",xlab="Бренды",ylab="Количество",names = names(df[2:11]),cex.names = 0.8, col=rainbow(1))



