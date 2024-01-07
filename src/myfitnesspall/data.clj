(ns myfitnesspall.data
  (:require [codax.core :as c]
            [clj-time.core :as time]))

(def filename "C:\\Users\\andrijama\\Desktop\\myfitnesspall\\resources\\food.csv")
(def exercise-file "C:\\Users\\andrijama\\Desktop\\myfitnesspall\\resources\\exercise.csv")
(slurp filename)
(slurp exercise-file)

(defn parse
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r"))
  )

(defn parse2
  [string]
  (#(clojure.string/replace % #"\n" "") string))

(def text (parse (parse2 (slurp filename))))
(def text-exercise (parse (parse2 (slurp exercise-file))))

(def comp-keys [:name :calories :proteins :carbs :fats])
(def exercise-keys [:exercise :calories-per-kg])

(defn strToInt
  [str]
  (Integer. str))

(defn strToFloat
  [str]
  (Float. str))

(def conversions {:name     identity
                  :calories strToInt
                  :proteins strToFloat
                  :carbs    strToFloat
                  :fats     strToFloat})
(def conversions-ex {:exercise identity
                     :calories-per-kg strToFloat})

(defn convert
  [comp-key value]
  ((get conversions comp-key) value))

(defn convert-ex
  [exercise-keys value]
  ((get conversions-ex exercise-keys) value))


(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [comp-key value]]
                   (assoc row-map comp-key (convert comp-key value)))
                 {}
                 (map vector comp-keys unmapped-row)))
       rows))

(defn mapify-ex
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [exercise-keys value]]
                   (assoc row-map exercise-keys (convert-ex exercise-keys value)))
                 {}
                 (map vector exercise-keys unmapped-row)))
       rows))

(def mapped-text (mapify text))
(def exercise-text (mapify-ex text-exercise))

(def db (c/open-database! "data/demo-database"))

(c/assoc-at! db [:foods] mapped-text)
(c/assoc-at! db [:exercises] exercise-text)

(c/get-at! db)

#_(c/with-write-transaction [db tx]
                          (c/assoc-at tx [:counters] {:id 0 :users 0}))
(defn add-user
  "create a user and assign them an id"
  [user]
  (c/with-write-transaction [db tx]
                            (let [user-id (c/get-at tx [:counters :id])]
                              (-> tx
                                  (assoc user :id user-id)
                                  (c/assoc-at [:users user-id] user)
                                  (c/assoc-at [:usernames (:username user)] user-id)
                                  (c/update-at [:counters :id] inc)
                                  (c/update-at [:counters :users] inc))))
  user)

#_(c/with-write-transaction [db tx]
                          (c/assoc-at tx [:food-counters] {:id 0 :foods 0}))

(defn add-user-food
  "add food that user took"
  [food]
  (c/with-write-transaction [db tx]
                            (let [user-food-id (c/get-at tx [:food-counters :id])]
                              (-> tx
                                  (c/assoc-at [:users-food user-food-id] food)
                                  (c/assoc-at [:food-ids user-food-id] (:user-id food))
                                  (c/update-at [:food-counters :id] inc)
                                  (c/update-at [:food-counters :foods] inc)))
  )food)

#_(c/with-write-transaction [db tx]
                            (c/assoc-at tx [:exercise-counters] {:id 0 :exercises 0}))

(defn add-user-exercise
  "add exercise that user performed"
  [exercise]
  (c/with-write-transaction [db tx]
                            (let [user-exercise-id (c/get-at tx [:exercise-counters :id])]
                              (-> tx
                                  (c/assoc-at [:users-exercise user-exercise-id] exercise)
                                  (c/assoc-at [:exercise-ids user-exercise-id] (:user-id exercise))
                                  (c/update-at [:exercise-counters :id] inc)
                                  (c/update-at [:exercise-counters :exercises] inc)))
  )exercise)

(defn check-username
  "check username"
  [username]
  (if (not (nil? (c/get-at! db [:usernames username])))
    (throw (Exception. "username already exists"))
    username
    )
  )

(defn login
  "login user"
  [username password]
  (if (not (nil? (c/get-at! db [:usernames username])))
    (let [user-id (c/get-at! db [:usernames username])]
      (if (= password (c/get-at! db [:users user-id :password]))
        (c/get-at! db [:users user-id])
        (throw (Exception. "invalid password"))
        )
      )
    (throw (Exception. "username doesn't exists"))
    )
  )


(defn get-user
  "fetch a user by their username"
  [username]
  (c/with-read-transaction [db tx]
                           (when-let [user-id (c/get-at tx [:usernames username])]
                             (c/get-at tx [:users user-id]))))

(defn get-users-food
  "Fetch a user's meals by user id"
  [user-id]
  (c/with-read-transaction [db tx]
                           (let [food-user-ids (map key (filter #(= user-id (val %)) (c/get-at! db [:food-ids])))]
                             (when (seq food-user-ids)
                               (map #(c/get-at tx [:users-food %]) food-user-ids)))))

(defn get-users-exercise
  "Fetch a user's exercises by user id"
  [user-id]
  (c/with-read-transaction [db tx]
                           (let [exercise-user-ids (map key (filter #(= user-id (val %)) (c/get-at! db [:exercise-ids])))]
                             (when (seq exercise-user-ids)
                               (map #(c/get-at tx [:users-exercise %]) exercise-user-ids)))))


(defn get-food
  "fetch a food by its name"
  [name]
  (c/with-read-transaction [db tx]
                           (let [food-map (c/get-at tx [:foods])]
                             (first (filter #(= (:name %) name) food-map)))))

(defn get-exercise
  "fetch a exercise by its name"
  [name]
  (c/with-read-transaction [db tx]
                           (let [exercise-map (c/get-at tx [:exercises])]
                             (first (filter #(= (:exercise %) name) exercise-map)))))

(defn rename-user
  "change a username"
  [username new-username]
  (c/with-write-transaction [db tx]
                            (when (c/get-at tx [:usernames new-username])
                              (throw (Exception. "username already exists")))
                            (when-let [user-id (c/get-at tx [:usernames username])]
                              (-> tx
                                  (c/dissoc-at [:usernames username])
                                  (c/assoc-at [:usernames new-username] user-id)
                                  (c/assoc-at [:users user-id :username] new-username)))))

(defn update-user
  "update user"
  [user]
  (c/with-write-transaction [db tx]
                            (when-let [user-id (c/get-at tx [:usernames (:username user)])]
                              (-> tx
                                  (c/assoc-at [:users user-id] user))))
  user)

(defn remove-user
  "remove a user"
  [username]
  (c/with-write-transaction [db tx]
                            (when-let [user-id (c/get-at tx [:usernames username])]
                              (-> tx
                                  (c/dissoc-at [:usernames username])
                                  (c/dissoc-at [:users user-id])
                                  (c/update-at [:counters :users] dec)))))
