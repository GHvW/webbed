(ns webbed.pure-graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(defn empty
  []
  {:g #{} :v #{}})