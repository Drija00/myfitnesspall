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

(defn create-user [username password gender age height weight calorie-goal]
  {:username username
   :password password
   :gender gender
   :age age
   :height height
   :weight weight
   :daily-calorie-goal calorie-goal
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

(defn BMR [gender weight height age]
  (if (= gender "Male")
    (- (+ (* 10 weight) (* 6.25 height)) (- (* 5 age) 5))
    (- (+ (* 10 weight) (* 6.25 height)) (+ 161 (* 5 age)))
    )
  )

(defn gain-loss []
  (println "\n===================================")
  (println "1. Sedentary\n2. Lightly active\n3. Mandatory active\n4. Very active\n5. Extra active\n")
  (println "===================================")
  (print "Select an option: ")
  (flush)
  (let [choice (read-line)]
    (cond
      (= choice "1")
      1.2
      (= choice "2")
      1.375
      (= choice "3")
      1.55
      (= choice "4")
      1.725
      (= choice "5")
      1.9
      :else
      (do (println "Invalid choice. Try again.")
          (recur))
      ))
  )

(defn kg-goal []
  (println "\n===================================")
  (println "Select your weight goal?")
  (println "\nExample:\nIf the goal is to lose 5kg type:\n-5\nIf the goal is to gain 5kg type:\n5 ")
  (println "===================================")
  (flush)
  (let [kg (Integer/parseInt (read-line))]
        kg
      )
  )
(defn days-goal []
  (println "\n===================================")
  (println "Select the number of days in which you want to achieve weight goal?")
  (println "===================================")
  (flush)
  (let [days (Integer/parseInt (read-line))]
    days
    )
  )
(defn calorie-adjustment []
  (float (/ (* 7700 (kg-goal)) (days-goal)))
  )

(defn final-goal [gender weight height age]
  (+ (* (BMR gender weight height age) (gain-loss)) (calorie-adjustment))
  )

(defn print-user-summary [user]
  (println (str/join "\n"
                     ["User:" (str " - " (:username user))
                      "Gender:" (str " - " (:gender user))
                      "Age:" (str " - " (:age user) "y")
                      "Height:" (str " - " (:height user) "cm")
                      "Weight:" (str " - " (:weight user) "kg")
                      "Consumed Calories:" (str " - " (:consumed-calories user))
                      "Daily Goal:" (str " - " (:daily-calorie-goal user))]))
  (println "Foods:")
  (doseq [food (:foods user)]
    (println (str/join " " [" - " (:name food) "Calories:" (:calories food)])))
  (println "Exercises:")
  (doseq [exercise (:exercises user)]
    (println (str/join " " [" - " (:name exercise) "Burned Calories:" (:calories exercise)]))))

(defn choose-gender []
  (println "\n===================================")
  (println "1. Male\n2. Female\n")
  (println "===================================")
  (print "Select an option: ")
  (flush)
  (let [choice (read-line)]
    (cond
      (= choice "1")
      "Male"
      (= choice "2")
      "Female"
      :else
      (do (println "Invalid choice. Try again.")
          (recur))
    ))
  )

(defn -main [u1]
  (let [user u1]
    (println "\n===================================")
    (print (str "Welcome to MyFitnessPal\t" (:username user) "!"))
    (loop [u user]
      (println "\n===================================")
      (println "1. Add Food\n2. Add Exercise\n3. View Summary\n4. Exit")
      (println "===================================")
      (print "Select an option: ")
      (flush)
      (let [choice (read-line)]
        (cond
          (= choice "1")
          (do (println "Enter food name: ")
              (flush)
              (let [food-name (read-line)]
                (println "Enter calories: ")
                (flush)
                (let [calories (Integer/parseInt (read-line))]
                (recur (add-food u food-name calories)))
                ;;(println (add-food u food-name calories))
                ))

          (= choice "2")
          (do (println "Enter exercise name: ")
              (flush)
              (let [exercise-name (read-line)]
                (println "Enter calories: ")
                (flush)
                (let [calories (Integer/parseInt (read-line))]
                (recur (add-exercise u exercise-name calories)))))

          (= choice "3")
          (do
            (println "")
            (print-user-summary u)
            (recur u))

          (= choice "4")
          (do (println "Goodbye!")
              u)
          :else
          (do (println "Invalid choice. Try again.")
              (recur u)))))))

(defn login []
  (let [user nil]
    (println "\n===================================")
    (print "Welcome to MyFitnessPal login page!")
    (loop [u user]
      (println "\n===================================")
      (println "1. Create account\n2. Login\n3. Exit")
      (println "===================================")
      (print "Select an option: ")
      (flush)
      (let [choice (read-line)]
        (cond
          (= choice "1")
          (do (println "Enter username:")
              (flush)
              (let [username (read-line)]
                (println "Enter password:")
                (flush)
                (let [password (read-line)]
                  (println "Enter gender:")
                  (flush)
                  (let [gender (choose-gender)]
                    (println "Enter age:")
                    (flush)
                    (let [age (Integer/parseInt (read-line))]
                      (println "Enter height in cm:")
                      (flush)
                      (let [height (Integer/parseInt (read-line))]
                        (println "Enter weight in kg:")
                        (flush)
                        (let [weight (Integer/parseInt (read-line))]
                          ;;(println "Enter daily calorie goal:")
                          ;;(flush)
                          (let [calorie-goal (final-goal gender weight height age)]
                (recur (create-user username password gender age height weight calorie-goal))
                ;;(println (add-food u food-name calories))
                ))))))))

          (= choice "2")
          (do (println "Enter username:")
              (flush)
              (let [username (read-line)]
                (println "Enter password:")
                (flush)
                (let [password (read-line)]
                (if (and (= username (:username u))
                         (= password (:password u)))
                  (recur (-main u))
                  (do (println "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                      (println "!!!Invalid username or password!!!")
                      (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
                      (recur u))))
                ;;(println (add-food u food-name calories))
                ))


          (= choice "3")
          (println "Goodbye!")
          :else
          (do (println "Invalid choice. Try again.")
              (recur u))))))

  )
