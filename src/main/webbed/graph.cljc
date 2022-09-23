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
      (conj adj {vertex #{}}))
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
      (conj adj {vertex #{}}))
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


(defn- walk-reducer
  [[visited memory] edge]
  (let [to (edge :to)]
    (if (contains? visited to)
      [visited memory]
      [(conj visited to)
       (conj memory edge)])))

(defn lazy-walk
  [visited memory adjacency-list]
  (lazy-seq
   (when-let [next-edge (peek memory)]
     (let [[visited' memory'] (->> (next-edge :to)
                                   (adjacency-list)
                                   (reduce walk-reducer [visited (pop memory)]))]
       (cons next-edge (lazy-walk visited'
                                  memory'
                                  adjacency-list))))))


(defn df-seq
  [start adjacency-list]
  (lazy-walk #{start}
             (list {:from start :to start})
             adjacency-list))


(defn bf-seq [start adjacency-list]
  (lazy-walk #{start}
             (conj PersistentQueue/EMPTY {:from start :to start})
             adjacency-list))


(defn- update-paths
  [paths {:keys [from to]}]
  (assoc paths to from))

;; TODO - combine these and then partial apply the xx-seq func?
;; (vertex adjacency-map) -> seq[map[vertex, vertex]]
(defn depth-first-paths
  [start graph]
  (->> graph
       (df-seq start)
       (reductions update-paths {})))


;; (vertex adjacency-map) -> seq[map[vertex, vertex]]
(defn breadth-first-paths
  [start graph]
  (->> graph
       (bf-seq start)
       (reductions update-paths {})))


;; (vertex, vertex-map) -> seq[vertex]
(defn path
  [from paths]
  (let [find (fn [results from_ paths]
               (let [next (paths from_)]
                 (if (nil? next)
                   results
                   (recur (conj results from_) next paths))))]
    (find [] from paths)))


; --------------- Testing stuff ----------------

(comment
  (def graph
    {:vertices #{:a :b :c :d}
     :edges #{{:from :a :to :c}
              {:from :d :to :c}
              {:from :c :to :b}}})

  ;; result not really deterministic since uses a set
  ;; should start with a, go to b c or d, 
  ;; from :b -> done
  ;; from :c -> :b (if not :b first) -> done
  ;; from :d -> :c (if not :c first) -> :b (if not :b first) -> done

  (->> {:s #{{:from :s :to :e :weight 2}
             {:from :s :to :a :weight 4}}
        :a #{{:from :a :to :d :weight 3}
             {:from :a :to :b :weight 5}
             {:from :a :to :c :weight 6}}
        :b #{{:from :b :to :a :weight 5}}
        :c #{{:from :c :to :b :weight 1}}
        :d #{{:from :d :to :a :weight 1}
             {:from :d :to :c :weight 3}}
        :e #{{:from :e :to :d :weight 1}}}
       (bf-seq :a))



  (->> {:s #{{:from :s :to :e :weight 2}
             {:from :s :to :a :weight 4}}
        :a #{{:from :a :to :d :weight 3}
             {:from :a :to :b :weight 5}
             {:from :a :to :c :weight 6}}
        :b #{{:from :b :to :a :weight 5}}
        :c #{{:from :c :to :b :weight 1}}
        :d #{{:from :d :to :a :weight 1}
             {:from :d :to :c :weight 3}}
        :e #{{:from :e :to :d :weight 1}}}
       (bf-seq :s))

  ;; should start with :s, go to :a or :e,
  ;; from :a ->
    ;; from :b -> done
    ;; from :c -> :b (if not :b first) -> done
    ;; from :d -> :c (if not :c first) -> :b (if not :b first) -> done
  ;; from :e ->
    ;; :d -> :c -> :b -> done
  (->> {:s #{{:from :s :to :e :weight 2}
             {:from :s :to :a :weight 4}}
        :a #{{:from :a :to :d :weight 3}
             {:from :a :to :b :weight 5}
             {:from :a :to :c :weight 6}}
        :b #{{:from :b :to :a :weight 5}}
        :c #{{:from :c :to :b :weight 1}
             {:from :c :to :f :weight 3}}
        :d #{{:from :d :to :a :weight 1}
             {:from :d :to :c :weight 3}}
        :e #{{:from :e :to :d :weight 1}}
        :f #{{:from :f :to :d :weight 2}}}
       (df-seq :s))


  (->> {:s #{{:from :s :to :e :weight 2}
             {:from :s :to :a :weight 4}}
        :a #{{:from :a :to :d :weight 3}
             {:from :a :to :b :weight 5}
             {:from :a :to :c :weight 6}}
        :b #{{:from :b :to :a :weight 5}}
        :c #{{:from :c :to :b :weight 1}
             {:from :c :to :f :weight 3}}
        :d #{{:from :d :to :a :weight 1}
             {:from :d :to :c :weight 3}}
        :e #{{:from :e :to :d :weight 1}}
        :f #{{:from :f :to :d :weight 2}}}
       (depth-first-paths :s))

  (->> {:s #{{:from :s :to :e :weight 2}
             {:from :s :to :a :weight 4}}
        :a #{{:from :a :to :d :weight 3}
             {:from :a :to :b :weight 5}
             {:from :a :to :c :weight 6}}
        :b #{{:from :b :to :a :weight 5}}
        :c #{{:from :c :to :b :weight 1}}
        :d #{{:from :d :to :a :weight 1}
             {:from :d :to :c :weight 3}}
        :e #{{:from :e :to :d :weight 1}}}
       (breadth-first-paths :s))

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