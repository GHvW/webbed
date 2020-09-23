(ns webbed.pure-graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(defn complete-graph?
  "Determines whether a given graph is complete, i.e. an edge exists from each vertex to every other vertex"
  [graph]
  (let [vertex-count (count (graph :v))]
    (/ (* vertex-count (- vertex-count 1)) 2))