(ns webbed.graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

; graph is { :vertices #{vertices}, :edges #{edges} }
(defn update-edges 
  "Adds edges to a vertex in an adjacency list"
  [adjacency-list from to]
  (update adjacency-list from conj to))

(defn undirected-graph->adj-list 
  "Takes a set of vertices and a set of edges and constructs an adjacency list from them"
  [vertices edges]
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

(defn degree 
  "The number of connections to the vertex"
  [adjacency-list vertex]
  (count (adjacency-list vertex)))

(defn average-degree 
  "Find the average degree of the nodes in the graph"
  [adjacency-list]
  (as-> adjacency-list x
    (reduce-kv (fn [total k v] (+ total (count v))) 0 x)
    (/ x (count adjacency-list))))

(defn max-degree 
  "Find max connections of all nodes"
  [adjacency-list]
  (reduce-kv
   (fn [max k _]
     (if (> (degree adjacency-list k) max)
       (degree adjacency-list k)
       max))
   0
   adjacency-list))

(defn max-degree-with-key 
  "Find the vertex with the most connections of all nodes"
  [adjacency-list]
  (reduce-kv
   (fn [max k _]
     (let [vertex-degree (degree adjacency-list k)]
       (if (> vertex-degree (max :degree))
         {:vertex k :degree vertex-degree}
         max)))
   {:vertex nil :degree 0}
   adjacency-list))

(defn bf-paths ; going trhough connections 3 times. change it?
  "Breadth first search helper for shortest-paths"
  [adjacency-list paths visited queue]
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

(defn bf-paths-edges ;change this ; goes through connections multiple times, change
  "Breadth first search helper for shortest-paths" [adjacency-list paths visited queue]
  (if (empty? queue)
    paths
    (let [next-vertex (peek queue)
          connections (set/difference 
                       (->> queue
                            (peek)
                            (adjacency-list)
                            (map (fn [edge] (edge :to)))
                            (set))
                       visited)]
      (recur adjacency-list
             (->> connections (reduce (fn [acc vertex] (conj acc {vertex next-vertex})) paths))
             (set/union connections visited)
             (apply conj (pop queue) connections)))))

(defn bf-paths-proto
  "Breadth first search helper for shortest-paths"
  [adjacency-list paths visited queue]
  (if (empty? queue)
    paths
    (let [vertex (peek queue)
          [paths' visited' queue'] (->> vertex
                                        (adjacency-list)
                                        (map (fn [edge] (edge :to))) ;abstract the "getting edges?"
                                        (reduce  ; move the reducer to its own fn?
                                         (fn [[p v q] connection]
                                           (if (contains? visited connection)
                                             [p v q]
                                             [(conj p {connection vertex}) 
                                              (conj v connection) 
                                              (conj q connection)]))
                                         [paths visited (pop queue)]))]
      (recur adjacency-list paths' visited' queue'))))



(defn shortest-paths 
  "Uses breadth first search to find the shortest paths to all connected vertices in an unweighted graph"
  [start adjacency-list]
  (bf-paths adjacency-list {start nil} #{start} (conj PersistentQueue/EMPTY start)))

(defn shortest-paths-edges ;change this
  "Uses breadth first search to find the shortest paths to all connected vertices in an unweighted graph"
  [start adjacency-list]
  ;(bf-paths-edges adjacency-list {start nil} #{start} (conj PersistentQueue/EMPTY start)))
  (bf-paths-proto adjacency-list {start nil} #{start} (conj PersistentQueue/EMPTY start)))

(defn build-path 
  "Builds the path as a sequence to the 'to' vertex"
  [to paths path]
  (if (nil? to)
    path
    (recur (paths to) paths (cons to path))))


(defn degrees-of-separation 
  "Gets the path to the 'to' vertex"
  [paths to]
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

(defn traverse-proto 
  [adjacency-list path visited memory]
  (if (empty? memory)
    path
    (let [next-vertex (peek memory)
          [visited' memory'] (->> next-vertex
                                        (adjacency-list)
                                        (map (fn [edge] (edge :to)))
                                        (reduce 
                                         (fn [[v m] connection]
                                           (if (contains? v connection)
                                             [v m]
                                             [(conj v connection)
                                              (conj m connection)]))
                                         [visited (pop memory)]))]
      (recur adjacency-list (conj path next-vertex) visited' memory'))))


(defn lazy-traverse [adjacency-list visited memory]
  (lazy-seq
    (when-let [next (peek memory)]
      (let [connections (-> next
                            (adjacency-list)
                            (set/difference visited))]
        (cons next (lazy-traverse adjacency-list
                                 (set/union connections visited)
                                 (apply conj (pop memory) connections)))))))

(defn lazy-traverse-proto 
  [adjacency-list visited memory]
  (lazy-seq
   (when-let [next-vertex (peek memory)]
     (let [[visited' memory'] (->> next-vertex
                                   (adjacency-list)
                                   (map (fn [edge] (edge :to)))
                                   (reduce 
                                    (fn [[v m] connection]
                                      (if (contains? v connection)
                                        [v m]
                                        [(conj v connection)
                                         (conj m connection)]))
                                    [visited (pop memory)]))]
       (cons next-vertex (lazy-traverse-proto adjacency-list
                                              visited'
                                              memory'))))))



(defn depth-first-order [adjacency-list start]
  (traverse adjacency-list [] #{start} (list start)))

(defn depth-first-order-proto [adjacency-list start]
  (traverse-proto adjacency-list [] #{start} (list start)))

(defn breadth-first-order [adjacency-list start]
  (traverse adjacency-list [] #{start} (conj PersistentQueue/EMPTY start)))

(defn breadth-first-order-proto [adjacency-list start]
  (traverse-proto adjacency-list [] #{start} (conj PersistentQueue/EMPTY start)))


(defn df-seq [start adjacency-list]
  (lazy-traverse adjacency-list #{start} (list start)))

(defn df-seq-proto [start adjacency-list]
  (lazy-traverse-proto adjacency-list #{start} (list start)))

(defn bf-seq [start adjacency-list]
  (lazy-traverse adjacency-list #{start} (conj PersistentQueue/EMPTY start)))

(defn bf-seq-proto [start adjacency-list]
  (lazy-traverse-proto adjacency-list #{start} (conj PersistentQueue/EMPTY start)))

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

(->> (peek (list :s))
     (dij-test)
     (map (fn [edge] (edge :to)))
     (reduce
      (fn [[v m] conn]
        (if (contains? v conn)
          [v m]
          [(conj v conn)
           (conj m conn)]))
      [#{:s} '(:s)]))

(def testmake (undirected-graph->adj-list (graph :vertices) (graph :edges)))
(def testmake2 (directed-graph->adj-list (graph :vertices) (graph :edges)))
(def test2 (update-edges adjlist :e :c))

(average-degree adjlist2)


(def maxdeg (max-degree adjlist2))
(def maxwkey (max-degree-with-key adjlist2))
(defn adder [x y] (+ x y))

(adder 10 20)
