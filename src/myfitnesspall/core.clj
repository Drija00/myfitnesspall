(ns myfitnesspall.core
  (:gen-class)
  (:require [clojure.string :as str]
            [myfitnesspall.data :as data]
            [clj-time.core :as time]))

(def space-line "\n===================================\n")

(defn food-map [user-id name calories proteins fats carbs]
  {:user-id user-id :date (time/now) :name name :calories calories :proteins proteins :fats fats :carbs carbs})

(defn exercise-map [name calories]
  {:name name :calories calories})

(defn print-food [food]
  (println (str/join " " ["Food:" (:name food) "Calories:" (:calories food)])))

(defn create-user [username password gender age height weight calorie-goal proteins-goal fats-goal carbs-goal]
  (let [user {:username            username
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
              :consumed-carbs      0}]
    (data/add-user user)))

(defn add-food [user food-name calories proteins fats carbs]
  ;;(print (str user "/n" food-name "/n" calories))
  (let [food (food-map (:id user) food-name calories proteins fats carbs)
        updated-user (assoc user
                       :consumed-calories (+ (:consumed-calories user) calories)
                       :consumed-proteins (+ (:consumed-proteins user) proteins)
                       :consumed-fats (+ (:consumed-fats user) fats)
                       :consumed-carbs (+ (:consumed-carbs user) carbs))]
    (data/add-user-food food)
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
  (println space-line "1. Sedentary\n2. Lightly active\n3. Mandatory active\n4. Very active\n5. Extra active" space-line)
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
  (let [kg (-> (prompt (str space-line "Select your weight goal?\nExample:\nIf the goal is to lose 5kg type:\n-5\nIf the goal is to gain 5kg type:\n5" space-line)) Integer/parseInt)]
    kg
    )
  )

(defn days-goal []
  (let [days (-> (prompt (str space-line "Select the number of days in which\nyou want to achieve weight goal?" space-line)) Integer/parseInt)]
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

        protein-grams (round (/ protein-calories 4))
        fat-grams (round (/ fat-calories 9))
        carb-grams (round (/ carb-calories 4))]

    {:protein-goal protein-grams
     :fat-goal     fat-grams
     :carb-goal    carb-grams}
    )
  )

(defn intake-calculator [default asked]
    (/ (* default asked) 100)
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
  (println space-line)
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
  (doseq [food (data/get-users-food (:id user))]
    (println (str/join " " [" - " (:name food) "\n    - Calories:" (str (round (:calories food)) "kcal")
                            "\n    -" "Proteins:" (str (round (:proteins food)) "g")
                            "\n    -" "Fats:" (str (round (:fats food)) "g")
                            "\n    -" "Carbs:" (str (round (:carbs food)) "g")
                            ])))
  (println "Exercises:")
  (doseq [exercise (:exercises user)]
    (println (str/join " " [" - " (:name exercise) "\n    - Burned Calories:" (:calories exercise) "kcal"])))
  (println space-line))

(defn choose-gender []
  (let [choice (-> (prompt (str space-line "1. Male\n2. Female\nSelect an option:" space-line)) Integer/parseInt)]
    (cond
      (= choice 1)
      "Male"
      (= choice 2)
      "Female"
      :else
      (do (println space-line "Invalid choice. Try again.")
          (recur))
      ))
  )
(defn set-username []
  (let [result (try
                 (-> (prompt (str space-line "Enter username:\n")) data/check-username)
                 (catch Exception e
                   e))]
    (if (instance? Exception result)
      (do
        (println space-line "Caught an exception:" (.getMessage result))
        (recur))
      result)))



(defn create-account []
  (let [username (set-username)
        password (prompt (str space-line "Enter password:\n"))
        gender (choose-gender)
        age (-> (prompt (str space-line "Enter age:\n")) Integer/parseInt)
        height (-> (prompt (str space-line "Enter height in cm:\n")) Integer/parseInt)
        weight (-> (prompt (str space-line "Enter weight in kg:\n")) Integer/parseInt)
        goal (final-goal gender weight height age)
        calorie-goal (:daily-goal goal)
        proteins-goal (:protein-goal goal)
        fats-goal (:fat-goal goal)
        carbs-goal (:carb-goal goal)]
    (create-user username password gender age height weight calorie-goal proteins-goal fats-goal carbs-goal)))

(defn get-food []
  (let [food-name (clojure.string/upper-case (prompt (str space-line "Enter food name:\n")))
        result (data/get-food food-name)]
    (if (nil? result)
      (do
        (println (str space-line "Food doesn't exist in our system."))
        (recur))
      result))
  )

(defn -main [u1]
    (print (str space-line "Welcome to MyFitnessPal\t" (:username u1) "!" space-line))
    (loop [u u1]
      (let [choice (-> (prompt (str space-line "1. Add Food\n2. Add Exercise\n3. View Summary\n4. Exit\n\nSelect an option: " space-line)) Integer/parseInt)]
        (cond
          (= choice 1)
          (let [food (get-food)
                grams (-> (prompt (str space-line "Enter grams:\n")) Integer/parseInt)
                calories (intake-calculator (:calories food) grams)
                proteins (intake-calculator (:proteins food) grams)
                fats (intake-calculator (:fats food) grams)
                carbs (intake-calculator (:carbs food) grams)]
                  (recur (add-food u (:name food) calories proteins fats carbs)))

          (= choice 2)
                (let [exercise-name (-> (prompt (str space-line "Enter exercise name:\n")))
                      calories (-> (prompt (str space-line "Enter calories:\n")) Integer/parseInt)]
                  (recur (add-exercise u exercise-name calories)))

          (= choice 3)
          (do
            (print-user-summary u)
            (data/update-user u)
            (recur u))

          (= choice 4)
          (do (println space-line "Goodbye!\n" space-line)
              u)
          :else
          (do (println space-line "Invalid choice. Try again.")
              (recur u))))))




(defn login []
    (println space-line "Welcome to MyFitnessPal login page!" space-line)
    (loop [u nil]
      (let [choice (-> (prompt (str space-line "1. Create account\n2. Login\n3. Exit\n\nSelect an option: " space-line)) Integer/parseInt)]
        (cond

          (= choice 1)
          (recur (create-account))

          (= choice 2)
                (let [username (-> (prompt (str space-line "Enter username:\n")))
                      password (-> (prompt (str space-line "Enter password:\n")))
                      result (try
                               (data/login username password)
                               (catch Exception e
                                 e))]
                  (if (not (instance? Exception result))
                    (recur (-main result))
                    (do (println space-line "Caught an exception:" (.getMessage result))
                      (recur u)))
                  )

          (= choice 3)
          (println space-line "Goodbye!\n" space-line)
          :else
          (do (println "\nInvalid choice. Try again.")
              (recur u))))))
