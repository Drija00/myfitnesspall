(ns myfitnesspall.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def predefined-keys [:name :calories])

(defn create-empty-map []
  (zipmap predefined-keys (repeat nil)))

(defn add-element [my-map key value]
  (assoc my-map key value))

(def Foods (create-empty-map))
(println Foods)

(defn food-map [name calories]
  {:name name :calories calories})

(defn exercise-map [name calories]
  {:name name :calories calories})

(def Exercises (create-empty-map))
(println Exercises)

(defn print-food [food]
  (println (str/join " " ["Food:" (:name food) "Calories:" (:calories food)])))

(defn create-user [username]
  {:username username
   :daily-calorie-goal 2000
   :consumed-calories 0
   :foods []
   :exercises []})

(defn add-food [user food-name calories]
  ;;(print (str user "/n" food-name "/n" calories))
  (let [food (food-map food-name calories)
        updated-user (assoc user
                       :consumed-calories (+ (:consumed-calories user) calories)
                       :foods (conj (:foods user) food))]
    ;;(print updated-user)
    updated-user
    ))

(defn add-exercise [user exercise-name calories]
  ;;(print (str user "/n" food-name "/n" calories))
  (let [exercise (exercise-map exercise-name calories)
        updated-user (assoc user
                       :consumed-calories (- (:consumed-calories user) calories)
                       :exercises (conj (:exercises user) exercise))]
    ;;(print updated-user)
    updated-user
    ))

(defn print-user-summary [user]
  (println (str/join " "
                     ["User:" (:username user)
                      "Consumed Calories:" (:consumed-calories user)
                      "Daily Goal:" (:daily-calorie-goal user)]))
  (println "Foods:")
  (doseq [food (:foods user)]
    (println (str/join " " [" - " (:name food) "Calories:" (:calories food)])))
  (println "Exercises:")
  (doseq [exercise (:exercises user)]
    (println (str/join " " [" - " (:name exercise) "Burned Calories:" (:calories exercise)]))))

(defn -main []
  (let [user (create-user "John")]
    ;;(println user)
    (println "Welcome to MyFitnessPal!")
    (println "===================================")
    (loop [u user]
      (println "1. Add Food\n2. Add Exercise\n3. View Summary\n4. Exit")
      (print "Select an option: ")
      (flush)
      (let [choice (read-line)]
        (cond
          (= choice "1")
          (do (print "Enter food name: ")
              (flush)
              (let [food-name (read-line)
                    calories (Integer/parseInt (read-line))]
                (recur (add-food u food-name calories))
                ;;(println (add-food u food-name calories))
                ))

          (= choice "2")
          (do (print "Enter exercise name: ")
              (flush)
              (let [exercise-name (read-line)
                    calories (Integer/parseInt (read-line))]
                (recur (add-exercise u exercise-name calories))))

          (= choice "3")
          (do
            (print-user-summary u)
            (recur u))

          (= choice "4")
          (println "Goodbye!")
          :else
          (do (println "Invalid choice. Try again.")
              (recur u)))))))