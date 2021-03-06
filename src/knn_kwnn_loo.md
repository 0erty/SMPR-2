#KNN / KWNN / LEAVE-ONE-OUT

###Теория

Метод ближайших соседей — простейший метрический классификатор, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки.


w(i,u) = [i\leq k] - метод k ближайших соседей, в данном случае для
определения оптимального k используется LOO(leave-one-out)

### Программная реализация
[1nn.R](//1nn.R)

[knn.R](//knn_loo.R)

[kwnn.R](//kwnn.R)

Суть knn в том чтобы получив список расстояний от классифицируемой точки до точек выборки и k, отсортировать дистанции и из k первых дистанций выбрать преобладащий
класс, что и будет классом классифицируемой точки.

Функция ```knn(sort_distances, k)```, где ```sort_distances``` - отсортированый список дистанций, а ```k``` количество учитываемых соседей.

LOO классифицирует каждую точку используемой выборки с разными ```k``` и проверяет количество ошибок, определяя таким образом оптимальный ```k```.

Функция ```loo(points, class_of_points)```, где ```points``` - точки выборки,
a ```class_of_points``` имена классов этих точек.

####Карта классификации для ирисов Фишера

![knn](//loo_knn_R.png)

KWNN отличается тем, что в качестве оценки близости объекта u к классу y выступает функция w(i,z) = [i <= k]w(i), где

i — порядок ближайшего соседа к классифицируемой точке u;
w(i) = (k + 1 - i) / k — вес относительно точки z, в данном случае для
определения оптимального k используется LOO(leave-one-out).

Отличия не кардинальные, просто каждой из k дистанций присваивается вес который
зависит от их ранга, по формуле (k + 1 - i) / k.

![kwnn](//kwnn.png)
