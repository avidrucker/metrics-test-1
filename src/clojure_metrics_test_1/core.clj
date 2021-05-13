(ns clojure-metrics-test-1.core
  (:gen-class))

;; goal: clojure dynamically parse function syntax tree tokens
;; research: https://stackoverflow.com/questions/21216991/how-to-transform-instaparse-output-into-a-function-that-can-be-evaluated

(defmacro defsource
  "Similar to clojure.core/defn, but saves the function's definition in the var's
   :source meta-data."
  {:arglists (:arglists (meta (var defn)))}
  [fn-name & defn-stuff]
  `(do (defn ~fn-name ~@defn-stuff)
       (alter-meta! (var ~fn-name) assoc :source (quote ~&form))
       (var ~fn-name)))

;; example function
;; question: Is there a way to save a function as-is in
;;    context (bind with "defn") while still being able
;;    to analyze syntax tree dynamically (?)?

(defn square2
  "takes in number x and returns x^2"
  [x]
  (* x x))

(defsource square [x]
  (* x x))

(defsource rect-area [x y]
  (* x y))

(defsource slope [m x b]
  (+ (* m x) b))

(defmacro source-meta
  "takes in function and returns its meta data"
  [function]
  `(:source (meta (var ~function))))

;; TODO: marked for deletion, assess utility to project
;; (defn tree-to-list
;;   "takes in tree 't' and returns it as a list"
;;   [t]
;;   (->> t
;;        (tree-seq seq? rest)
;;        (map identity)))

;; question: can this function be rewritten
;;    more simply with complement or partial?
;;    If yes, what would its syntax be?
(defn non-lists [s]
  (filter #(not (list? %)) s))


(defn get-tokens
  "takes in a function meta data. returns its syntax tree as a list of tokens"
  [x]
  (rest (tree-seq seq? non-lists
                  (tree-seq seq? seq x))))

;; TODO: Implement stub
(defn get-sexp-count [x]
  x)

;; TODO: Implement stub
(defn get-unique-token-count [x]
  x)

;; TODO: Implement stub
(defn get-full-docs-page [x]
  x)

(defn get-args [s]
  (vec (filter vector? s)))

(defn -main
  [& args]
  (doseq []
    ;;(println (count (source-meta square)))
    ;;(println (source-meta square))
    ;;(println (tree-seq seq? seq
    ;;    (source-meta square)))
    ;;(println (tree-to-list (source-meta square)))
    ;;(println (non-lists (source-meta square)))
    (let [squa-meta (source-meta square)
          rect-meta (source-meta rect-area)
          squa (get-tokens (source-meta square))
          rect (get-tokens (source-meta rect-area))
          flat-squa (flatten (get-tokens (source-meta square)))
          flat-rect (flatten (get-tokens (source-meta rect-area)))]
      (println (str "square meta: " (seq squa-meta)))
      (println (str "square tokens: " (seq squa)))
      (println (str "square tokens denatured: " (seq flat-squa)))
      (println (str "square token count: " (count flat-squa)))
      (println (str "square args: " (first (get-args (seq squa)))))

      (println (str "rect meta: " (seq rect-meta)))
      (println (str "rect tokens: " (seq rect)))
      (println (str "rect tokens denatured: " (seq flat-rect)))
      (println (str "rect token count: " (count flat-rect)))
      (println (str "rect args: " (first (get-args (seq rect))))))))

(-main)