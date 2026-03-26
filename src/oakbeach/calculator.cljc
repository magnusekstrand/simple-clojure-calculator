(ns oakbeach.calculator
  (:require [clojure.string :as str]))

(defn error [msg]
  (throw #?(:clj  (Exception. msg)
            :cljs (js/Error. msg))))

(defn parse-number [s]
  #?(:clj  (Double/parseDouble s)
     :cljs (js/parseFloat s)))

(def operations
  {"+" +
   "-" -
   "*" *
   "/" (fn [a b]
         (when (zero? b)
           (error "Divide by zero"))
         (/ a b))
   "^" #?(:clj  #(Math/pow %1 %2)
          :cljs #(js/Math.pow %1 %2))})

(defn tokenize [input]
  (when (re-find #"[^\d\s+\-*/^().]" input)
    (error "Invalid character in expression"))
  (->> (re-seq #"-?\d+\.?\d*|[()+\-*/^]" input)
       (map #(if (re-matches #"-?\d+\.?\d*" %)
               (parse-number %)
               %))))

(declare parse-expr)

(defn parse-primary [tokens]
  (let [t (first tokens)]
    (cond
      (number? t)
      [t (rest tokens)]

      (= t "(")
      (let [[value remaining] (parse-expr (rest tokens))]
        (when-not (= (first remaining) ")")
          (error "Missing closing parenthesis"))
        [value (rest remaining)])

      (= t "-") ;; unary minus
      (let [[value remaining] (parse-primary (rest tokens))]
        [(- value) remaining])

      :else
      (error (str "Unexpected token: " t)))))

(defn parse-power [tokens]
  (let [[left remaining] (parse-primary tokens)]
    (if (= (first remaining) "^")
      (let [[right remaining2] (parse-power (rest remaining))]
        [((operations "^") left right) remaining2])
      [left remaining])))

(defn parse-term [tokens]
  (loop [[left remaining] (parse-power tokens)]
    (let [op (first remaining)]
      (if (contains? #{"*" "/"} op)
        (let [[right remaining2] (parse-power (rest remaining))]
          (recur [((operations op) left right) remaining2]))
        [left remaining]))))

(defn parse-expr [tokens]
  (loop [[left remaining] (parse-term tokens)]
    (let [op (first remaining)]
      (if (contains? #{"+" "-"} op)
        (let [[right remaining2] (parse-term (rest remaining))]
          (recur [((operations op) left right) remaining2]))
        [left remaining]))))

(defn evaluate [input]
  (let [[result remaining] (parse-expr (tokenize input))]
    (when (seq remaining)
      (error (str "Unexpected input: " remaining)))
    result))

(defn- show-help []
  (str "Available operations: +, -, *, /, ^\n"
       "Usage:\n"
       "  Simple: 1 + 2\n"
       "  Nested: 1 + (2 * 3)\n"
       "  Commands: help, exit"))

(defn handle-input [input]
  (try
    (let [trimmed (str/trim input)]
      (cond
        (str/blank? trimmed) ""
        (= "help" (str/lower-case trimmed)) (show-help)
        :else (str (evaluate trimmed))))
    (catch #?(:clj Exception :cljs js/Error) e
      #?(:clj (.getMessage e)
         :cljs (.-message e)))))

#?(:clj
   (defn -main [& _]
     (println "--- CLI Calculator ---")
     (println "Enter expressions like: 1 + (2 * 3)")
     (println "Type 'help' for help, 'exit' to quit.")
     (loop []
       (print "> ")
       (flush)
       (let [input (read-line)]
         (if (or (nil? input) (= "exit" (str/lower-case (str/trim input))))
           (println "Goodbye!")
           (do
             (println (handle-input input))
             (recur)))))))