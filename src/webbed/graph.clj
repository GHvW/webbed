(ns webbed.graph
  (:import (clojure.lang PersistentQueue)))

; graph is { :vertices #{vertices}, :edges #{edges} }
(defn update-edges [adjacency-list from to]
  (update adjacency-list from conj to))

(defn make->adj-list [vertices edges]
  "Takes a set of vertices and a set of edges and constructs an adjacency list from them"
  (reduce
    (fn [adj edge]
      (let [{from :from to :to} edge]
        (update-edges (update-edges adj from to) to from)))
    (->> vertices
         (reduce
           (fn [adj vertex]
             (conj adj { vertex #{} }))
           {}))
    edges))

(defn bfs [from to adjacency-list queue visited path]
  (let [next (peek queue)]
   (if (empty? queue))))

(defn shortest-path [from to adjacency-list]
  (bfs from to adjacency-list (conj PersistentQueue/EMPTY from) #{from} ()))

(def graph
  {:vertices #{:a :b :c :d}
   :edges #{{:from :a :to :c} {:from :d :to :c} {:from :c :to :b}}})

(def adjlist
  {:a #{:b :c :d}
   :b #{:a :e}
   :c #{:a :d}
   :d #{:a :c}
   :e #{:b}})

(def testmake (make->adj-list (graph :vertices) (graph :edges)))
(def test2 (update-edges adjlist :e :c))