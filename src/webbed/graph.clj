(ns webbed.graph
  (:require [clojure.set :as set])
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
        (conj adj { vertex #{} }))
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
        (conj adj { vertex #{} }))
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

(defn build-path [to shortest-paths path]
  "Builds the path as a sequence to the 'to' vertex"
  (if (nil? to)
    path
    (recur (shortest-paths to) shortest-paths (cons to path))))

(defn degrees-of-separation [shortest-paths to]
  "Gets the path to the 'to' vertex"
  (build-path to shortest-paths ()))

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

(defn seq-traverse [adjacency-list visited memory] ; we'll see ...
  (if ()))

(defn depth-first-traverse [adjacency-list start]
  (traverse adjacency-list [] #{start} (list start)))

(defn breadth-first-traverse [adjacency-list start]
  traverse adjacency-list [] #{start} (conj PersistentQueue/EMPTY start))
; --------------- Testing stuff ----------------

(def graph
  {:vertices #{:a :b :c :d}
   :edges #{{:from :a :to :c}
            {:from :d :to :c}
            {:from :c :to :b}}})

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

