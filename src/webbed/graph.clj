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

(defn bfs [adjacency-list queue visited paths]
  (if (empty? queue)
    paths
    (let [state (->> (adjacency-list (peek queue))
                     (filter (fn [connection] (nil? (visited connection))))
                     (reduce
                       (fn [state connection]
                         (let [{_paths :paths _visited :visited _queue :queue} state]
                           {:paths (conj _paths {connection (peek queue)})
                            :visited (conj _visited connection)
                            :queue (conj _queue connection)}))
                       {:paths paths :visited visited :queue queue}))]
      (recur adjacency-list (pop (state :queue)) (state :visited) (state :paths)))))

(defn shortest-paths [start adjacency-list]
  (bfs adjacency-list (conj PersistentQueue/EMPTY start) #{start} {start nil}))

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

(def test3 (->> (adjlist :a)
                (filter (fn [connection] (nil? (#{:a} connection))))
                (reduce
                  (fn [state connection]
                    (let [{_paths :paths _visited :visited _queue :queue} state]
                      {:paths (conj _paths {connection :a})
                       :visited (conj _visited connection)
                       :queue (conj _queue connection)}))
                  {:paths {} :visited #{:a} :queue (conj PersistentQueue/EMPTY :a)})))