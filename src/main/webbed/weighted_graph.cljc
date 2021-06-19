
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

;; (defn relax [[edge-to dist-to pq] edge]
;;   (let [{:keys [from to weight]} edge]
;;     (if (or
;;           (nil? (dist-to to))
;;           (> (dist-to to) (+ (dist-to from) weight)))
;;       (let [dist-to' (assoc dist-to to (+ (dist-to from) weight))
;;             edge-to' (assoc edge-to to edge)]
;;         [edge-to' dist-to' (assoc pq to (dist-to' to))])
;;       [edge-to dist-to pq])))

;; STILL NEEDS TO BE TESTED
(defn relax-reducer
  [[distances priority-queue] edge]
  (let [{:keys [from to weight]} edge
        current-distance-plus-weight (+ (distances from) weight)]
    (if (or
         (nil? (distances to))
         (> (distances to) current-distance-plus-weight))
      [(assoc distances to current-distance-plus-weight)
       (assoc priority-queue to (distances to))]
      [distances priority-queue])))

;; (defn dijkstra [edge-to dist-to pq adj-list]
;;   (if-let [[next-vertex _] (peek pq)]
;;     (let [[e d p] (reduce
;;                     relax
;;                     [edge-to dist-to (pop pq)]
;;                     (adj-list next-vertex))]
;;       (recur e d p adj-list))
;;     {:edge-to edge-to :dist-to dist-to}))


;; STILL NEEDS TO BE TESTED
(defn dijkstra [distances priority-queue adj-list]
  (lazy-seq
   (when-let [[next-vertex _] (peek priority-queue)]
     (let [[dists pq] (reduce
                       relax-reducer
                       [distances (pop priority-queue)]
                       (adj-list next-vertex))]
       (cons dists pq adj-list)))))


;; STILL NEEDS TO BE TESTED
(defn directed-shortest-path [start adj-list]
  (dijkstra {start 0} (priority-map start 0) adj-list))


;; (defn build-shortest-path [to paths path]
;;   (if-let [next-edge (paths to)]
;;     (recur (next-edge :from) paths (cons to path))
;;     path))




; -------------------- TESTING --------------------

(comment
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
   :e #{{:from :e :to :d :weight 1}}}))
