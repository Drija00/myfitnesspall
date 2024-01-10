(ns myfitnesspall.core
  (:gen-class)
  (:require [clojure.string :as str]
            [myfitnesspall.data :as data]
            [clj-time.core :as time]
            [clj-time.format :as fmt]))

(def space-line "\n===================================\n")
(def kg2lb 0.45359237)

(def my-format (fmt/formatter "yyyy-MM-dd"))

(defn food-map [user-id food-id name description calories proteins fats carbs]
  {:user-id user-id :food-id food-id :date (time/now) :name name :description description :calories calories :proteins proteins :fats fats :carbs carbs})

(defn exercise-map [user-id ex-id name duration calories]
  {:user-id user-id :date (time/now) :id ex-id :exercise name :duration duration :calories calories})

(defn create-user [username password gender age height weight weight-goal day-goal calorie-goal proteins-goal fats-goal carbs-goal]
  (let [user {:id                  nil
              :username            username
              :password            password
              :gender              gender
              :age                 age
              :height              height
              :weight              weight
              :weight-goal         (+ weight weight-goal)
              :day-goal            day-goal
              :daily-calorie-goal  calorie-goal
              :consumed-calories   0
              :daily-proteins-goal proteins-goal
              :consumed-proteins   0
              :daily-fats-goal     fats-goal
              :consumed-fats       0
              :daily-carbs-goal    carbs-goal
              :consumed-carbs      0}]
    (data/add-user user)))

(defn add-food [user food-id food-name food-desc calories proteins fats carbs]
  (let [food (food-map (:id user) food-id food-name food-desc calories proteins fats carbs)
        updated-user (assoc user
                       :consumed-calories (+ (:consumed-calories user) calories)
                       :consumed-proteins (+ (:consumed-proteins user) proteins)
                       :consumed-fats (+ (:consumed-fats user) fats)
                       :consumed-carbs (+ (:consumed-carbs user) carbs))]
    (data/add-user-food food)
    updated-user
    ))

(defn add-exercise [user id exercise-name duration calories]
  (let [exercise (exercise-map (:id user) id exercise-name duration calories)
        updated-user (assoc user
                       :consumed-calories (- (:consumed-calories user) calories))]
    (data/add-user-exercise exercise)
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

(defn BMR-2 [gender weight height age]
  (if (= gender "Male")
    (+ 88.362 (* 13.397 weight) (* 4.799 height) (- (* 5.677 age)))
    (+ 447.593 (* 9.247 weight) (* 3.098 height) (- (* 4.33 age)))
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

(defn exercise-calorie-calculator [exercise-calories duration user-weight]
  (/ (* (* (/ user-weight kg2lb) (/ exercise-calories kg2lb)) duration) 60)
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
        daily-goal (+ (* (BMR-2 gender weight height age) activity) (calorie-adjustment kg-goal days-goal))
        macros (def-macronutrients-percentage kg-goal daily-goal)]
    (assoc macros :daily-goal daily-goal :kg-goal kg-goal :days-goal days-goal)
    )
  )

(defn set-consumed-zero [user]
  (let [u (assoc user
    :consumed-calories 0
    :consumed-proteins 0
    :consumed-fats 0
    :consumed-carbs 0)]
    u
    ))

(defn consumed-cals [user]
  (let [food (data/get-users-food (:id user))
        exercises (data/get-users-exercise (:id user))
        user (set-consumed-zero user)]
    (reduce (fn [u f]
              (assoc u
                :consumed-calories (+ (:consumed-calories u) (:calories f))
                :consumed-proteins (+ (:consumed-proteins u) (:proteins f))
                :consumed-fats (+ (:consumed-fats u) (:fats f))
                :consumed-carbs (+ (:consumed-carbs u) (:carbs f))))
            (reduce (fn [u e]
                      (assoc u
                        :consumed-calories (- (:consumed-calories u) (:calories e))))
                    user
                    exercises)
            food)))

(defn print-date [date]
    (fmt/unparse my-format date)
  )

(defn print-food-list [list]
  (doseq [l list]
    (println (str/join " " [" - ID:" (:id l) "\t - " (:description l)])))
  )

(defn print-exercise-list [list]
  (doseq [l list]
    (println (str/join " " [" - ID:" (:id l) "\t - " (:exercise l)])))
  )

(defn print-all-user-food [user]
  (doseq [food (data/get-all-users-food (:id user))]
    (println (str/join " " [" - " (:description food) "\n    - Calories:" (str (round (:calories food)) "kcal")
                            "\n    -" "Proteins:" (str (round (:proteins food)) "g")
                            "\n    -" "Fats:" (str (round (:fats food)) "g")
                            "\n    -" "Carbs:" (str (round (:carbs food)) "g")
                            "\n    -" "Date:" (print-date (:date food))
                            ])))
  )

(defn print-daily-food [user]
  (doseq [food (data/get-users-food(:id user))]
    (println (str/join " " [" - " (:description food) "\n    - Calories:" (str (round (:calories food)) "kcal")
                            "\n    -" "Proteins:" (str (round (:proteins food)) "g")
                            "\n    -" "Fats:" (str (round (:fats food)) "g")
                            "\n    -" "Carbs:" (str (round (:carbs food)) "g")
                            "\n    -" "Date:" (print-date (:date food))
                            ])))
  )

(defn print-all-user-exercise [user]
  (doseq [exercise (data/get-all-users-exercise (:id user))]
    (println (str/join " " [" - " (:exercise exercise) "\n    - Duration:" (:duration exercise) "min"
                            "\n    - Burned Calories:" (round (:calories exercise)) "kcal"
                            "\n    -" "Date:" (print-date (:date exercise))
                            ])))
  )

(defn print-daily-exercise [user]
  (doseq [exercise (data/get-users-exercise (:id user))]
    (println (str/join " " [" - " (:exercise exercise) "\n    - Duration:" (:duration exercise) "min"
                            "\n    - Burned Calories:" (round (:calories exercise)) "kcal"
                            "\n    -" "Date:" (print-date (:date exercise))
                            ])))
  )

(defn print-user-all-intakes [user]
   (println space-line)
   (println "Foods:")
   (print-all-user-food user)
   (println "Exercises:")
   (print-all-user-exercise user)
   (println space-line))

(defn print-user-daily-summary [user]
  (println space-line)
  (println (str/join "\n"
                     ["User:" (str " - " (:username user))
                      "Gender:" (str " - " (:gender user))
                      "Age:" (str " - " (:age user) "y")
                      "Height:" (str " - " (:height user) "cm")
                      "Weight:" (str " - " (:weight user) "kg")
                      "Weight goal:" (str " - " (:weight-goal user) "kg")
                      "Day goal:" (str " - " (:day-goal user))
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
  (print-daily-food user)
  (println "Exercises:")
  (print-daily-exercise user)
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

(defn change-goal [user]
  (let [gender (choose-gender)
        age (-> (prompt (str space-line "Enter age:\n")) Integer/parseInt)
        height (-> (prompt (str space-line "Enter height in cm:\n")) Integer/parseInt)
        weight (-> (prompt (str space-line "Enter weight in kg:\n")) Integer/parseInt)
        goal (final-goal gender weight height age)
        calorie-goal (:daily-goal goal)
        proteins-goal (:protein-goal goal)
        fats-goal (:fat-goal goal)
        carbs-goal (:carb-goal goal)
        day-goal (:days-goal goal)
        weight-goal (:kg-goal goal)
        u (assoc user :gender gender :age age :weight weight :height height :daily-calorie-goal calorie-goal :daily-proteins-goal proteins-goal :daily-fats-goal fats-goal :daily-carbs-goal carbs-goal :weight-goal (+ weight weight-goal) :day-goal day-goal)]
    (data/update-user u)
    )
  )

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
        carbs-goal (:carb-goal goal)
        day-goal (:days-goal goal)
        weight-goal (:kg-goal goal)]
    (create-user username password gender age height weight weight-goal day-goal calorie-goal proteins-goal fats-goal carbs-goal)
    (println (str space-line "Account created successfuly"))))

(defn choose-food [foods]
  (let [id (-> (prompt (str space-line "Enter foods id that you want to add:\n")) Integer/parseInt)
        food (data/get-food-by-id id)
        ]
    (if (nil? food)
      (do
        (println (str space-line "Food doesn't exist in our system."))
        (recur foods))food))
  )

(defn get-food []
  (let [food-name (clojure.string/upper-case (prompt (str space-line "Enter food name:\n")))
        result (data/get-food food-name)]
    (if (nil? result)
      (do
        (println (str space-line "Food doesn't exist in our system."))
        (recur))
      result))
  )

(defn get-food1 []
  (let [food-name (clojure.string/upper-case (prompt (str space-line "Enter food name:\n")))
        result (data/get-all-food-by-name food-name)]
    (if (empty? result)
      (do
        (println (str space-line "Food doesn't exist in our system."))
        (recur))
      (do (print-food-list result)
          (choose-food result))
      ))
  )

(defn choose-exercise [exercises]
  (let [id (-> (prompt (str space-line "Enter exercises id that you want to add:\n")) Integer/parseInt)
        exercise (data/get-exercise-by-id id)
        ]
    (if (nil? exercise)
      (do
        (println (str space-line "Exercise doesn't exist in our system."))
        (recur exercises))exercise))
  )

(defn get-exercise1 []
  (let [exercise-name (clojure.string/upper-case (prompt (str space-line "Enter exercise name:\n")))
        result (data/get-all-exercise-by-name exercise-name)]
    (if (empty? result)
      (do
        (println (str space-line "Exercise doesn't exist in our system."))
        (recur))
      (do (print-exercise-list result)
          (choose-exercise result))
      ))
  )


(defn get-exercise []
  (let [exercise-name (clojure.string/upper-case (prompt (str space-line "Enter exercise name:\n")))
        result (data/get-exercise exercise-name)]
    (if (nil? result)
      (do
        (println (str space-line "Exercise doesn't exist in our system."))
        (recur))
      result))
  )

(defn -main [u1]
    (print (str space-line "Welcome to MyFitnessPal\t" (:username u1) "!" space-line))
    (loop [u u1]
      (let [choice (try
                     (-> (prompt (str space-line "1. Add Food\n2. Add Exercise\n3. View Daily Summary\n4. View User All Summary\n5. Change plan\n0. Exit\n\nSelect an option: " space-line)) Integer/parseInt)
                     (catch Exception e
                       e))]
        (cond

          (instance? Exception choice)
          (do
            (println space-line "Caught an exception:" (.getMessage choice))
            (recur u))

          (= choice 1)
          (let [food (first (get-food1))
                grams (-> (prompt (str space-line "Enter grams:\n")) Integer/parseInt)
                calories (intake-calculator (:calories food) grams)
                proteins (intake-calculator (:proteins food) grams)
                fats (intake-calculator (:fats food) grams)
                carbs (intake-calculator (:carbs food) grams)]
                  (recur (add-food u (:id food) (:name food) (:description food) calories proteins fats carbs)))


          (= choice 2)
          (let [exercise (first (get-exercise1))
                duration (-> (prompt (str space-line "Enter exercise duration in minutes:\n")) Integer/parseInt)
                calories (exercise-calorie-calculator (:calories-per-kg exercise) duration (:weight u))]
            (recur (add-exercise u (:id exercise) (:exercise exercise) duration calories)))

          (= choice 3)
          (do
            (print-user-daily-summary (consumed-cals u))
            (data/update-user u)
            (recur u))

          (= choice 4)
          (do
            (print-user-all-intakes u)
            (recur u))

          (= choice 5)
          (recur (change-goal u))

          (= choice 0)
          (do (println space-line "Goodbye!\n" space-line)
              u)
          :else
          (do (println space-line "Invalid choice. Try again.")
              (recur u))))))


(defn login []
    (println space-line "Welcome to MyFitnessPal login page!" space-line)
    (loop [u nil]
      (let [choice (try
                     (-> (prompt (str space-line "1. Create account\n2. Login\n0. Exit\n\nSelect an option: " space-line)) Integer/parseInt)
                     (catch Exception e
                       e))]
        (cond

          (instance? Exception choice)
          (do
            (println space-line "Caught an exception:" (.getMessage choice))
            (recur u))

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

          (= choice 0)
          (println space-line "Goodbye!\n" space-line)
          :else
          (do (println "\nInvalid choice. Try again.")
              (recur u))))))
