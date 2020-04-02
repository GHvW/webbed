(ns webbed.graph
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]])
  (:import (clojure.lang PersistentQueue)))

; graph is { :vertices #{vertices}, :edges #{edges} }
(defn update-edges [adjacency-list from to]
  "Adds edges to a vertex in an adjacency list"
  (update adjacency-list from conj to))

(defn undirected-graph->adj-list [vertices edges]
  "Takes a set of vertices and a set of edges and constructs an adjacency list from them"
  (reduce
    (fn [adj edge]
      (let [{from :from to :to} edge]
        (update-edges (update-edges adj from to) to from)))
    (reduce
      (fn [adj vertex]
        (conj adj { vertex #{}}))
      {}
      vertices)
    edges))

(defn directed-graph->adj-list [vertices edges]
  (reduce
    (fn [adj edge]
      (let [{from :from to :to} edge]
        (update-edges adj from to)))
    (reduce
      (fn [adj vertex]
        (conj adj { vertex #{}}))
      {}
      vertices)
   edges))

(defn degree [adjacency-list vertex]
  (count (adjacency-list vertex)))

(defn bf-paths [adjacency-list paths visited queue]
  "Breadth first search helper for shortest-paths"
  (if (empty? queue)
    paths
    (let [next-vertex (peek queue)
          connections (-> queue
                          (peek)
                          (adjacency-list)
                          (set/difference visited))]
      (recur adjacency-list
             (->> connections (reduce (fn [acc vertex] (conj acc {vertex next-vertex})) paths))
             (set/union connections visited)
             (apply conj (pop queue) connections)))))

(defn shortest-paths [start adjacency-list]
  "Uses breadth first search to find the shortest paths to all connected vertices in an unweighted graph"
  (bf-paths adjacency-list {start nil} #{start} (conj PersistentQueue/EMPTY start)))

(defn build-path [to paths path]
  "Builds the path as a sequence to the 'to' vertex"
  (if (nil? to)
    path
    (recur (paths to) paths (cons to path))))


(defn degrees-of-separation [paths to]
  "Gets the path to the 'to' vertex"
  (build-path to paths ()))

(defn traverse [adjacency-list path visited memory]
  (if (empty? memory)
    path
    (let [connections (-> memory
                          (peek)
                          (adjacency-list)
                          (set/difference visited))]
      (recur adjacency-list
             (conj path (peek memory))
             (set/union connections visited)
             (apply conj (pop memory) connections)))))

(defn lazy-traverse [adjacency-list visited memory]
  (lazy-seq
    (when-let [next (peek memory)]
      (let [connections (-> next
                            (adjacency-list)
                            (set/difference visited))]
        (cons next (lazy-traverse adjacency-list
                                 (set/union connections visited)
                                 (apply conj (pop memory) connections)))))))

(defn depth-first-order [adjacency-list start]
  (traverse adjacency-list [] #{start} (list start)))

(defn breadth-first-order [adjacency-list start]
  traverse adjacency-list [] #{start} (conj PersistentQueue/EMPTY start))

(defn df-seq [start adjacency-list]
  (lazy-traverse adjacency-list #{start} (list start)))

(defn bf-seq [start adjacency-list]
  (lazy-traverse adjacency-list #{start} (conj PersistentQueue/EMPTY start)))


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
      (let [_dist-to (assoc dist-to to (+ (dist-to from) weight))
            _edge-to (assoc edge-to to edge)]
        [_edge-to _dist-to (assoc pq to (_dist-to to))])
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

; --------------- Testing stuff ----------------

(def graph
  {:vertices #{:a :b :c :d}
   :edges #{{:from :a :to :c}
            {:from :d :to :c}
            {:from :c :to :b}}})

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

(def adjlist
  {:a #{:b :c :d}
   :b #{:a :e}
   :c #{:a :d}
   :d #{:a :c}
   :e #{:b}})

(def adjlist2
  {:1 #{:3 :4}
   :2 #{}
   :3 #{:1 :5 :7}
   :4 #{:1 :8}
   :5 #{:3 :8 :6}
   :6 #{:5}
   :7 #{:3}
   :8 #{:5 :4}})

(def testmake (undirected-graph->adj-list (graph :vertices) (graph :edges)))
(def testmake2 (directed-graph->adj-list (graph :vertices) (graph :edges)))
(def test2 (update-edges adjlist :e :c))

(build-shortest-path :b ((directed-shortest-path dij-test :s) :edge-to) ())

; (degrees-of-separation ((directed-shortest-path dij-test :s) :edge-to) :b)
((directed-shortest-path dij-test :s) :edge-to)
