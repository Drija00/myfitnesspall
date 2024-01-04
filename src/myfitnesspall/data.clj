(ns myfitnesspall.data
  (:require [codax.core :as c]))

(def filename "C:\\Users\\andrijama\\Desktop\\myfitnesspall\\resources\\food.csv")
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

(def comp-keys [:name :calories :proteins :carbs :fats])

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

(def db (c/open-database! "data/demo-database"))

(c/assoc-at! db [:foods] mapped-text)

(c/get-at! db)


#_(c/with-write-transaction [db tx]
                          (c/assoc-at tx [:counters] {:id 0 :users 0}))
(defn add-user
  "create a user and assign them an id"
  [user]
  (c/with-write-transaction [db tx]
                            (let [user-id (c/get-at tx [:counters :id])]
                              (-> tx
                                  (c/assoc-at [:users user-id] user)
                                  (c/assoc-at [:usernames (:username user)] user-id)
                                  (c/update-at [:counters :id] inc)
                                  (c/update-at [:counters :users] inc))))
  user)

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
(defn get-food
  "fetch a food by its name"
  [name]
  (c/with-read-transaction [db tx]
                           (let [food-map (c/get-at tx [:foods])]
                             (first (filter #(= (:name %) name) food-map)))))



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
