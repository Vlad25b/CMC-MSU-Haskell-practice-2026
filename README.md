# Сборка
stack build

# Запуск в режиме командной строки
stack run -- products.json reviews.json dictionary.json rules.json defects.json results.json

# Запуск в интерактивном режиме
stack run

# Входные файлы
products.json	    Список товаров (productId, productName, productDescription, productCategory)
reviews.json	    Список отзывов (reviewId, productId, author, date, text, sentiment)
dictionary.json	  Словарь тональности (word, sentiment, weight)
rules.json	      Правила оценки (repetitionPolicy, datePolicy, ratingFormula)

# Выходные файлы
defects.json	    Словарь бракованных товаров (формируется автоматически)
results.json	    Результаты анализа (оценка, ключевые слова, полезный отзыв)

# Основные типы и функции
Product	          Информация о товаре
Review	          Отзыв пользователя
Sentiment	        Тональность (Positive, Neutral, Negative)
WordWeight	      Слово с тональностью и весом
EvaluationRules	  Правила оценки (повторы, дата, формула)
ProductRating	    Результат анализа товара
loadProducts	    Загрузка товаров из JSON
loadReviews	      Загрузка отзывов из JSON
loadDictionary	  Загрузка словаря тональности
loadRules	        Загрузка правил оценки
evaluateReview	  Оценка одного отзыва
analyzeProduct	  Анализ товара по всем отзывам
runAnalysis	      Главная функция анализа
saveResults	      Сохранение результатов в JSON
