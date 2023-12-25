(ns myfitnesspall.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [org.httpkit.client :as client]))

;;(def filename (json/read-str (slurp "https://api.edamam.com/api/food-database/v2/parser?app_id=f29b4b5d&app_key=b846e04d671945be9286c6c5f33bc08a&ingr=chicken")))

(def predefined-keys [:name :calories])

(defn create-empty-map []
  (zipmap predefined-keys (repeat nil)))

(defn add-element [my-map key value]
  (assoc my-map key value))

(def Foods (create-empty-map))
(println Foods)

(defn food-map [name calories proteins fats carbs]
  {:name name :calories calories :proteins proteins :fats fats :carbs carbs})

(defn exercise-map [name calories]
  {:name name :calories calories})

(def Exercises (create-empty-map))
(println Exercises)

(defn print-food [food]
  (println (str/join " " ["Food:" (:name food) "Calories:" (:calories food)])))

(defn create-user [username password gender age height weight calorie-goal proteins-goal fats-goal carbs-goal]
  {:username            username
   :password            password
   :gender              gender
   :age                 age
   :height              height
   :weight              weight
   :daily-calorie-goal  calorie-goal
   :consumed-calories   0
   :daily-proteins-goal proteins-goal
   :consumed-proteins   0
   :daily-fats-goal     fats-goal
   :consumed-fats       0
   :daily-carbs-goal    carbs-goal
   :consumed-carbs      0
   :foods               []
   :exercises           []})

(defn add-food [user food-name calories proteins fats carbs]
  ;;(print (str user "/n" food-name "/n" calories))
  (let [food (food-map food-name calories proteins fats carbs)
        updated-user (assoc user
                       :consumed-calories (+ (:consumed-calories user) calories)
                       :consumed-proteins (+ (:consumed-proteins user) proteins)
                       :consumed-fats (+ (:consumed-fats user) fats)
                       :consumed-carbs (+ (:consumed-carbs user) carbs)
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

(defn prompt [message]
  (print message)
  (flush)
  (read-line))

(defn round [num]
  (let [factor (Math/pow 10 2)]
    (/ (Math/round (* num factor)) factor)))

(defn BMR [gender weight height age]
  (if (= gender "Male")
    (- (+ (* 10 weight) (* 6.25 height)) (- (* 5 age) 5))
    (- (+ (* 10 weight) (* 6.25 height)) (+ 161 (* 5 age)))
    )
  )

(defn gain-loss []
  (println "\n===================================\n1. Sedentary\n2. Lightly active\n3. Mandatory active\n4. Very active\n5. Extra active\n===================================\n")
  (let [choice (-> (prompt "Enter your daily activity:") Integer/parseInt)]
    (cond
      (= choice 1)
      1.2
      (= choice 2)
      1.375
      (= choice 3)
      1.55
      (= choice 4)
      1.725
      (= choice 5)
      1.9
      :else
      (do (println "Invalid choice. Try again.")
          (recur))
      ))
  )

(defn kg-goal []
  (let [kg (-> (prompt "\n===================================\nSelect your weight goal?\nExample:\nIf the goal is to lose 5kg type:\n-5\nIf the goal is to gain 5kg type:\n5\n===================================\n") Integer/parseInt)]
    kg
    )
  )

(defn days-goal []
  (let [days (-> (prompt "\n===================================\nSelect the number of days in which\nyou want to achieve weight goal?\n===================================\n") Integer/parseInt)]
    days
    )
  )
(defn calorie-adjustment [kg-goal days-goal]
  (float (/ (* 7700 kg-goal) days-goal))
  )

(defn calculate-macronutrients [protein-percentage fat-percentage carb-percentage daily-calories]
  (let [protein-calories (* protein-percentage daily-calories)
        fat-calories (* fat-percentage daily-calories)
        carb-calories (* carb-percentage daily-calories)

        protein-grams (/ protein-calories 4)
        fat-grams (/ fat-calories 9)
        carb-grams (/ carb-calories 4)]

    {:protein-goal protein-grams
     :fat-goal     fat-grams
     :carb-goal    carb-grams}
    )
  )

(defn def-macronutrients-percentage [gain-loss daily-calories]
  (if (<= gain-loss 0)
    (calculate-macronutrients 0.15 0.3 0.55 daily-calories)
    (calculate-macronutrients 0.25 0.2 0.55 daily-calories)
    )
  )

(defn final-goal [gender weight height age]
  (let [activity (gain-loss)
        kg-goal (kg-goal)
        days-goal (days-goal)
        daily-goal (+ (* (BMR gender weight height age) activity) (calorie-adjustment kg-goal days-goal))
        macros (def-macronutrients-percentage kg-goal daily-goal)]
    (assoc macros :daily-goal daily-goal)
    )
  )

(defn print-user-summary [user]
  (println (str/join "\n"
                     ["User:" (str " - " (:username user))
                      "Gender:" (str " - " (:gender user))
                      "Age:" (str " - " (:age user) "y")
                      "Height:" (str " - " (:height user) "cm")
                      "Weight:" (str " - " (:weight user) "kg")
                      "Daily Calorie Goal:" (str " - " (round (:daily-calorie-goal user)) " kcal")
                      "Consumed Calories:" (str " - " (round (:consumed-calories user)) " kcal")
                      "Daily Proteins Goal:" (str " - " (round (:daily-proteins-goal user)) "g")
                      "Consumed Proteins:" (str " - " (round (:consumed-proteins user)) "g")
                      "Daily Fats Goal:" (str " - " (round (:daily-fats-goal user)) "g")
                      "Consumed Fats:" (str " - " (round (:consumed-fats user)) "g")
                      "Daily Carbs Goal:" (str " - " (round (:daily-carbs-goal user)) "g")
                      "Consumed Carbs:" (str " - " (round (:consumed-carbs user)) "g")
                      ]))
  (println "Foods:")
  (doseq [food (:foods user)]
    (println (str/join " " [" - " (:name food) "\n    - Calories:" (str (:calories food) "kcal")
                            "\n    -" "Proteins:" (str (:proteins food) "g")
                            "\n    -" "Fats:" (str (:fats food) "g")
                            "\n    -" "Carbs:" (str (:carbs food) "g")
                            ])))
  (println "Exercises:")
  (doseq [exercise (:exercises user)]
    (println (str/join " " [" - " (:name exercise) "\n    - Burned Calories:" (:calories exercise) "kcal"]))))

(defn choose-gender []
  (let [choice (-> (prompt "\n===================================\n1. Male\n2. Female\nSelect an option:\n===================================\n") Integer/parseInt)]
    (cond
      (= choice 1)
      "Male"
      (= choice 2)
      "Female"
      :else
      (do (println "Invalid choice. Try again.")
          (recur))
      ))
  )

(defn create-account []
  (let [username (prompt "Enter username:\n")
        password (prompt "Enter password:\n")
        gender (choose-gender)
        age (-> (prompt "Enter age:\n") Integer/parseInt)
        height (-> (prompt "Enter height in cm:\n") Integer/parseInt)
        weight (-> (prompt "Enter weight in kg:\n") Integer/parseInt)
        goal (final-goal gender weight height age)
        calorie-goal (:daily-goal goal)
        proteins-goal (:protein-goal goal)
        fats-goal (:fat-goal goal)
        carbs-goal (:carb-goal goal)]
    (create-user username password gender age height weight calorie-goal proteins-goal fats-goal carbs-goal)))

(defn user-menu [user]
  ;; Add user menu functionality here
  (println "User Menu Placeholder"))


(defn -main [u1]
    (print (str "\n===================================\nWelcome to MyFitnessPal\t" (:username u1) "!\n===================================\n"))
    (loop [u u1]
      (let [choice (-> (prompt "\n1. Add Food\n2. Add Exercise\n3. View Summary\n4. Exit\n\nSelect an option: ") Integer/parseInt)]
        (cond
          (= choice 1)
          (let [food-name (-> (prompt "Enter food name:\n"))
                calories (-> (prompt "Enter calories:\n") Integer/parseInt)
                proteins (-> (prompt "Enter proteins in grams:\n") Integer/parseInt)
                fats (-> (prompt "Enter fats in g:\n") Integer/parseInt)
                carbs (-> (prompt "Enter carbs in g:\n") Integer/parseInt)]
                  (recur (add-food u food-name calories proteins fats carbs)))

          (= choice 2)
                (let [exercise-name (-> (prompt "Enter exercise name:\n"))
                      calories (-> (prompt "Enter calories:\n") Integer/parseInt)]
                  (recur (add-exercise u exercise-name calories)))

          (= choice 3)
          (do
            (print-user-summary u)
            (recur u))

          (= choice 4)
          (do (println "Goodbye!")
              u)
          :else
          (do (println "Invalid choice. Try again.")
              (recur u))))))




(defn login []
    (println "\n===================================\nWelcome to MyFitnessPal login page!\n===================================\n")
    (loop [u nil]
      (let [choice (-> (prompt "\n1. Create account\n2. Login\n3. Exit\n\nSelect an option: \n") Integer/parseInt)]
        (cond

          (= choice 1)
          (recur (create-account))

          (= choice 2)
                (let [username (-> (prompt "Enter username:\n"))
                      password (-> (prompt "Enter password:\n"))]
                  (if (and (= username (:username u))
                           (= password (:password u)))
                    (recur (-main u))
                    (do (println "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n!!!Invalid username or password!!!\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                        (recur u))))
          (= choice 3)
          (println "Goodbye!")
          :else
          (do (println "Invalid choice. Try again.")
              (recur u))))))
