(ns webbed.pure-graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(defn empty
  "not really a graph but will"
  [] 
  {})

(defn vertex
  "A graph of a single vertex"
  [v]
  {:v #{v} :e #{}})