(ns myfitnesspall.data)

(def filename "C:\\Users\\andrijama\\Downloads\\archive (1)\\food.csv")
(slurp filename)

(defn parse
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r"))
  )

(defn parse2
  [string]
  (#(clojure.string/replace % #"\n" "") string))

(def text (parse (parse2 (slurp filename))))

(prn text)

(def comp-keys [:fdc_id :data_type :description :food_category_id :publication_date])

(defn strToInt
  [str]
  (Integer. str))

(defn strToFloat
  [str]
  (Float. str))

(def conversions {:fdc_id strToInt
                  :data_type identity
                  :description identity
                  :food_category_id strToInt
                  :publication_date identity})

(defn convert
  [comp-key value]
  ((get conversions comp-key) value))


(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [comp-key value]]
                   (assoc row-map comp-key (convert comp-key value)))
                 {}
                 (map vector comp-keys unmapped-row)))
       rows))

(def mapped-text (mapify text))
(prn mapped-text)