(defproject simple-clojure-calculator "0.1.0-SNAPSHOT"
  :description "A functional calculator with Web UI"
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [thheller/shadow-cljs "2.28.2"]
                 [reagent "1.2.0"]] 
  :main oakbeach.calculator
  :test-paths ["test"]
  :repl-options {:init-ns oakbeach.calculator})
