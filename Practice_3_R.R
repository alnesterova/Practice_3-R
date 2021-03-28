# Практика 3

# Аналитический пакет R
# Создание веб-приложения в пакете "shiny"

# Создайте приложение shiny, которое подтягивает статистику из любой базы данных (или скрэппит её с сайта) и строит по ней график.
# Пользователь должен иметь возможность менять как минимум три настройки, которые изменяют фильтрацию данных и/или отображение 
# графика. Интерфейс приложения должен содержать подсказки, облегчающие работу с программой.

# Как вариант можно создать приложение «shiny», которое строит ваш график из упражнения № 2 (можно выбрать любую графическую 
# систему). Данные для графика загружать с UN COMTRADE, используя API сайта, работа с которым описана в первой практике. 
# Дать пользователю возможность фильтровать статистику импорта в РФ по трём показателям:
# • код товара, причём из списка в приложении должно быть понятно, что это за товары. В список кодов включить от 5 до 10 позиций.
# • период времени.
# • товарный поток: импорт, экспорт, реэкспорт.

# Учитывая ограничения на количество обращений к базе UN COMTRADE через API, можно сохранить файл с нужной статистикой по выбранным 
# кодам в репозитории на github.com и в приложении ссылаться на него.

# Коды товаров для парсинга: 
# `0701` - картофель, свежий или охлажденный;
# `0702` - помидоры, свежие или охлажденные;
# `0703` - лук, лук-шалот, чеснок, лук-порей и т.д. в свежем или охлажденном виде;
# `0704` - кабачки, цветная капуста, кольраби и капуста, свежие и охлажденные;
# `0705` - латук и цикорий, свежий или охлажденный;
# `0706` - морковь, турнепс, свекла, сельдерей, редис и т.д. в свежем или охлажденном виде;
# `0707` - огурцы и корнишоны, свежие или охлажденные.

# График (в ggplot2): разброс массы поставки в зависимости от её стоимости. Добавить горизонтальные прямые на уровне медианы 
# массы поставок. Пропуски заменить на средние.

#------------------------------------------------------------------------------------------------------------------------------
# загрузка пакетов
library('shiny')               # создание интерактивных приложений
library('data.table')          # работаем с объектами "таблица данных"
library('ggplot2')             # графики ggplot2
library('dplyr')               # трансформации данных
library('RCurl')

# функция, реализующая API (источник: UN COMTRADE)
source("https://raw.githubusercontent.com/aksyuk/R-data/master/API/comtrade_API.R")

# парсинг данных с UN COMTRADE
code = c('0701', '0702', '0703', '0704', '0705', '0706', '0707')
DT = data.frame()
for (i in code){
  for (j in 2010:2020){
    Sys.sleep(5)
    s1 <- get.Comtrade(r = 'all', p = 643,
                       ps = as.character(j), freq = "M",
                       cc = i, fmt = 'csv')
    DT <- rbind(DT, s1$data)
    print(paste('Данные для товара с кодом', i, 'за', j, 'загружены'))
  }
}

# запись данных в файл
file.name <- paste('./data/Comtrade_DT.csv', sep = '')
write.csv(DT, file.name, row.names = FALSE)
write(paste('Файл', paste('Comtrade_DT.csv', sep = ''),'загружен', Sys.time()), file = './data/download.log', append=TRUE)

# # чтение файла с github
# # адрес файла
# fileURL <- 'https://raw.githubusercontent.com/alnesterova/Practice_3-R/main/comtrade/Comtrade_DT.csv'
# dest.file <- './data/Comtrade_DT.csv'
# # загружаем файл, если он ещё не существует, и делаем запись о загрузке в лог:
# if (!file.exists(dest.file)) {
#  download.file(fileURL, dest.file) # загрузить файл
#  # сделать запись в лог
#  write(paste('Файл', dest.file, 'загружен', Sys.time()),
#  file = log.filename, append = T)
# }

# чтение файла
DT <- read.csv('./data/Comtrade_DT.csv', header = T, sep = ',')
# переводим в формат data.table
DT <- data.table(DT) 
names(DT)

# копируем имена в символьный вектор, чтобы ничего не испортить
nms <- names(DT)
# заменить серии из двух и более точек на одну
nms <- gsub('[.]+', '.', nms)
# убрать все хвостовые точки
nms <- gsub('[.]+$', '', nms)
# заменить US на USD
nms <- gsub('US', 'USD', nms)
# проверяем, что всё получилось, и заменяем имена столбцов
names(DT) <- nms
# результат обработки имён столбцов
names(DT)

# отбираем необходимые столбцы
DT <- select(DT, Year, Period, Trade.Flow, Commodity.Code, Netweight.kg, Trade.Value.USD)

# код продукции
filter1 <- as.character(unique(DT$Commodity.Code))
names(filter1) <- filter1
filter1 <- as.list(filter1)
filter1

# выбор вида внешнеторговой операции
filter2 <- as.character(unique(DT$Trade.Flow))
names(filter2) <- filter2
filter2 <- as.list(filter2)
filter2

# заполним пропуски для массы поставок
# считаем средние и округляем до целого, как исходные данные
DT[, round(mean(.SD$Netweight.kg, na.rm = T), 0), by = Year]

# заменяем пропуски на средние
DT[, Netweight.kg.mean := round(mean(.SD$Netweight.kg, na.rm = T), 0),by = Year]
DT[!is.na(Netweight.kg), Netweight.kg.mean := Netweight.kg]

# смотрим результат
DT[, Netweight.kg, Netweight.kg.mean]

# запись данных в файл
file.name <- paste('./data/Comtrade_DT_updated.csv', sep = '')
write.csv(DT, file.name, row.names = FALSE)

# запуск приложения
# загрузить архив с приложением
file.URL <- 'https://github.com/alnesterova/Practice_3-R/raw/main/comtrade_app.zip'
download.file(file.URL, destfile = 'comtrade_app.zip',
              mode = 'wb', cacheOK = FALSE)
# распаковать
unzip('comtrade_app.zip', overwrite = T)
# запуск приложения
runApp('./comtrade_app', launch.browser = TRUE,
       display.mode = 'showcase')