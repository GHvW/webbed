(ns webbed.graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))


; graph is { :vertices #{vertices}, :edges #{edges} }
(defn update-edges 
  "Adds edges to a vertex in an adjacency list"
  [from to adjacency-list]
  (update adjacency-list from conj to))


(defn undirected-graph->adj-list 
  "Takes a set of vertices and a set of edges and constructs an adjacency list from them"
  [vertices edges]
  (reduce
   (fn [adj edge]
     (let [{from :from to :to} edge]
       (update-edges to from (update-edges from to adj))))
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
        (update-edges from to adj)))
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


(defn lazy-traverse 
  [visited memory adjacency-list]
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
       (cons next-vertex (lazy-traverse visited'
                                        memory'
                                        adjacency-list))))))


(defn df-seq [start adjacency-list]
  (lazy-traverse #{start} (list start) adjacency-list))


(defn bf-seq [start adjacency-list]
  (lazy-traverse #{start} (conj PersistentQueue/EMPTY start) adjacency-list))

; --------------- Testing stuff ----------------

(comment
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

  (->> {:a #{:b :c :d}
        :b #{:a :e}
        :c #{:a :d}
        :d #{:a :c}
        :e #{:b}}
       (bf-seq :a))

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

  (undirected-graph->adj-list ({:vertices #{:a :b :c :d}
                              :edges #{{:from :a :to :c}
                                       {:from :d :to :c}
                                       {:from :c :to :b}}} :vertices) ({:vertices #{:a :b :c :d}
                              :edges #{{:from :a :to :c}
                                       {:from :d :to :c}
                                       {:from :c :to :b}}} :edges))
  (directed-graph->adj-list ({:vertices #{:a :b :c :d}
                              :edges #{{:from :a :to :c}
                                       {:from :d :to :c}
                                       {:from :c :to :b}}} :vertices) (graph :edges))
  (update-edges :e :c {:a #{:b :c :d}
                       :b #{:a :e}
                       :c #{:a :d}
                       :d #{:a :c}
                       :e #{:b}})

  (average-degree {:1 #{:3 :4}
     :2 #{}
     :3 #{:1 :5 :7}
     :4 #{:1 :8}
     :5 #{:3 :8 :6}
     :6 #{:5}
     :7 #{:3}
     :8 #{:5 :4}})


  (max-degree {:1 #{:3 :4}
     :2 #{}
     :3 #{:1 :5 :7}
     :4 #{:1 :8}
     :5 #{:3 :8 :6}
     :6 #{:5}
     :7 #{:3}
     :8 #{:5 :4}})

  (max-degree-with-key {:1 #{:3 :4}
                        :2 #{}
                        :3 #{:1 :5 :7}
                        :4 #{:1 :8}
                        :5 #{:3 :8 :6}
                        :6 #{:5}
                        :7 #{:3}
                        :8 #{:5 :4}})

  (+ 10 20))