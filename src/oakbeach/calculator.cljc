(ns oakbeach.calculator
  (:require [clojure.string :as str]))

;; 1. The Operations Map
(def operations
  {"+" +
   "-" -
   "*" *
   "/" (fn [a b]
         (if (zero? b)
           (throw #?(:clj (ArithmeticException. "Divide by zero")
                     :cljs (js/Error. "Divide by zero")))
           (/ a b)))
   "^" #?(:clj  #(Math/pow %1 %2)
          :cljs #(js/Math.pow %1 %2))})

(def precedence
  {"+" 1 "-" 1
   "*" 2 "/" 2
   "^" 3})

(defn tokenize [input]
  (->> (re-seq #"\d+\.?\d*|[+\-*\^/()]" input)
       (map (fn [token]
              (if (re-matches #"\d+\.?\d*" token)
                ;; Här väljer vi rätt parser beroende på plattform
                #?(:cljs (js/parseFloat token)
                   :clj  (Double/parseDouble token))
                token)))))

;; 3. The Nesting Logic: Turns flat tokens into nested vectors
(defn nest-tokens [tokens]
  (letfn [(parse-subtree [ts]
            (loop [remaining ts
                   acc []]
              (let [t (first remaining)]
                (cond
                  (nil? t) [acc nil]
                  (= t "(") (let [[inner r] (parse-subtree (rest remaining))]
                              (recur r (conj acc inner)))
                  (= t ")") [acc (rest remaining)]
                  :else (recur (rest remaining) (conj acc t))))))]
    (first (parse-subtree tokens))))

;; 4. The Recursive Solver
(declare solve)

(defn- solve-flat-list [elems]
  (let [clean-elems (remove #(or (nil? %) (= "" %)) elems)]
    (cond
      (empty? clean-elems) 0.0
      (= (count clean-elems) 1) (solve (first clean-elems))

      ;; Handle Unary Minus: [- 5] -> [0 - 5]
      (= (first clean-elems) "-")
      (solve-flat-list (cons "0" clean-elems))

      :else
      (let [ops-in-expr (filter #(contains? precedence %) clean-elems)
            ;; We use lastIndexOf to ensure left-to-right evaluation for same precedence
            pivot-op (apply min-key #(get precedence % 100) (reverse ops-in-expr))
            idx (.lastIndexOf (vec clean-elems) pivot-op)
            lhs (take idx clean-elems)
            rhs (drop (inc idx) clean-elems)
            f (get operations pivot-op)]
        (f (solve-flat-list lhs) (solve-flat-list rhs))))))

(defn solve [expr]
  (cond
    (vector? expr) (solve-flat-list expr)

    ;; Handle the string-to-number conversion for both platforms
    (string? expr)
    #?(:cljs (js/parseFloat expr)    ;; Only seen by ClojureScript
       :clj  (Double/parseDouble expr)) ;; Only seen by Java/Leiningen

    (number? expr) (double expr)

    :else
    (throw
      #?(:cljs (js/Error. (str "Cannot solve: " expr)) ;; JavaScript Error
         :clj  (Exception. (str "Cannot solve: " expr)))))) ;; Java Exception

(defn- show-help []
  (let [op-list (str/join ", " (sort (keys operations)))]
    (str "Available operations: " op-list "\n"
         "Usage: \n"
         "  Simple: 1 + 2\n"
         "  Nested: 1 + (2 * 3)\n"
         "  Commands: help, exit")))

;; 5. Entry point
(defn handle-input [input]
  (try
    (let [tokens (tokenize input)
          tree   (nest-tokens tokens)]
      (str (solve tree)))
    ;; Här väljer vi Exception för Java och Error för JavaScript
    (catch #?(:clj Exception :cljs js/Error) e
      (str "Error: " #?(:clj (.getMessage e) :cljs (.-message e))))))

#?(:clj
  (defn -main [& args]
    (println "--- CLI Calculator ---")
    (println "Enter expressions like: 1 + (2 * 3)")
    (println "Type 'exit' to quit.")
    (loop []
      (print "> ")
      (flush) ;; Ensures the prompt appears before input
      (let [input (read-line)]
        (if (or (nil? input) (= (str/lower-case input) "exit"))
          (println "Goodbye!")
          (do
            (println (handle-input input))
            (recur)))))))