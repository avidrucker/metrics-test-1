(ns clojure-metrics-test-1.core
  (:gen-class) 
  ;;(:require [clojure.repl :as repl])
  )

(defmacro defsource
  "Similar to clojure.core/defn, but saves the function's definition in the var's
   :source meta-data."
  {:arglists (:arglists (meta (var defn)))}
  [fn-name & defn-stuff]
  `(do (defn ~fn-name ~@defn-stuff)
       (alter-meta! (var ~fn-name) assoc :source (quote ~&form))
       (var ~fn-name)))

(defsource square [x]
  (* x x))

(defmacro source-meta [func]
  `(:source (meta (var ~func))))

(defn -main
  [& args]
  (println
   (source-meta square)))

(-main)