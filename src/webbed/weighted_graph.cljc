
(ns webbed.weighted-graph
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]
            [webbed.graph :refer [update-edges]])
  (:import (clojure.lang PersistentQueue)))


; ----------------- Weighted Tests --------------------

;(defn weighted-directed-graph->adj-list [{:keys [vertices edges]}]
;  (->> edges
;       (reduce
;         (fn [adj edge]
;           (update-edges adj (vertices (edge :from)) edge))
;         (->> vertices
;              (reduce
;                (fn [adj vertex]
;                  (conj adj {vertex #{}}))
;                {})))))

(defn weighted-directed-graph->adj-list [{:keys [vertices edges]}]
  (reduce
    (fn [adj edge]
      (update-edges adj (vertices (edge :from)) edge))
    (->> vertices
         (map (fn [vertex] {vertex #{}}))
         (into (hash-map)))
    edges))

(defn switch-direction [{:keys [from to weight]}]
  {:from to :to from :weight weight})

(defn weighted-undirected-graph->adj-list [{:keys [vertices edges]}]
  (reduce
    (fn [adj edge]
      (update-edges
        (update-edges adj (vertices (edge :to)) (switch-direction edge))
        (vertices (edge :from)) edge))
    (->> vertices
         (map (fn [vertex] {vertex #{}}))
         (into (hash-map)))
    edges))

(defn relax [[edge-to dist-to pq] edge]
  (let [{:keys [from to weight]} edge]
    ;(println dist-to)
    ;(println edge-to)
    ;(println (str "from " from))
    ;(println (str "to " to))
    ;(println (str "weight " weight))
    (if (or
          (nil? (dist-to to))
          (> (dist-to to) (+ (dist-to from) weight)))
      (let [dist-to' (assoc dist-to to (+ (dist-to from) weight))
            edge-to' (assoc edge-to to edge)]
        [edge-to' dist-to' (assoc pq to (dist-to' to))])
      [edge-to dist-to pq])))

(defn dijkstra [edge-to dist-to pq adj-list]
  (if-let [[next-vertex _] (peek pq)]
    (let [[e d p] (reduce
                    relax
                    [edge-to dist-to (pop pq)]
                    (adj-list next-vertex))]
      (recur e d p adj-list))
    {:edge-to edge-to :dist-to dist-to}))

(defn directed-shortest-path [adj-list start]
  (dijkstra {start nil} {start 0} (priority-map start 0) adj-list))

(defn build-shortest-path [to paths path]
  (if-let [next-edge (paths to)]
    (recur (next-edge :from) paths (cons to path))
    path))




; -------------------- TESTING --------------------

(def weighted-graph
  {:vertices #{:a :b :c :d :h :i :j}
   :edges #{{:from :a
             :to :b
             :weight 4}
            {:from :d
             :to :c
             :weight 3}
            {:from :c
             :to :b
             :weight 1}
            {:from :b
             :to :i
             :weight 7}
            {:from :j
             :to :d
             :weight 2}
            {:from :h
             :to :c
             :weight 3}
            {:from :i
             :to :d
             :weight 19}
            {:from :b
             :to :a
             :weight 3}
            {:from :d
             :to :i
             :weight 15}}})

(def dij-test
  {:s #{{:from :s :to :e :weight 2}
        {:from :s :to :a :weight 4}}
   :a #{{:from :a :to :d :weight 3}
        {:from :a :to :b :weight 5}
        {:from :a :to :c :weight 6}}
   :b #{{:from :b :to :a :weight 5}}
   :c #{{:from :c :to :b :weight 1}}
   :d #{{:from :d :to :a :weight 1}
        {:from :d :to :c :weight 3}}
   :e #{{:from :e :to :d :weight 1}}})


(def dshort (build-shortest-path :b ((directed-shortest-path dij-test :s) :edge-to) ()))
