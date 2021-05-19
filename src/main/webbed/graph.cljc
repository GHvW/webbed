(ns webbed.graph
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))


; graph is { :vertices #{vertices}, :edges #{edges} }
(defn update-edges 
  "Adds edges to a vertex in an adjacency list"
  [from to adjacency-list]
  (update adjacency-list from conj to))

;; maybe new add edge
;; (defn add-edge
;;   [adjacency-list from to]
;;   (update adjacency-list from conj to))

;; maybe new add vertex
;; (defn add-vertex
;;   [adjacency-list vertex] 
;;   (assoc adjacency-list vertex #{}))


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


;; (defn lazy-traverse 
;;   [visited memory adjacency-list]
;;   (lazy-seq
;;    (when-let [next-vertex (peek memory)]
;;      (let [[visited' memory'] (->> next-vertex
;;                                    (adjacency-list)
;;                                    (map (fn [edge] (edge :to)))
;;                                    (reduce 
;;                                     (fn [[v m] connection]
;;                                       (if (contains? v connection)
;;                                         [v m]
;;                                         [(conj v connection)
;;                                          (conj m connection)]))
;;                                     [visited (pop memory)]))]
;;        (cons next-vertex (lazy-traverse visited'
;;                                         memory'
;;                                         adjacency-list))))))


(defn lazy-traverse 
  [visited memory adjacency-list]
  (lazy-seq
   (when-let [next-edge (peek memory)]
     (let [[visited' memory'] (->> (next-edge :to)
                                   (adjacency-list) ;; this still needs work
                                  ;;  (map (fn [edge] (edge :to)))
                                   (reduce 
                                    (fn [[v m] edge]
                                      (let [to (edge :to)]
                                        (if (contains? v to)
                                          [v m]
                                          [(conj v to)
                                           (conj m edge)])))
                                    [visited (pop memory)]))]
       (cons next-edge (lazy-traverse visited'
                                      memory'
                                      adjacency-list))))))


;; (defn df-seq [start adjacency-list]
;;   (lazy-traverse #{start} (list start) adjacency-list))

(defn df-seq [start adjacency-list]
  (lazy-traverse #{start} (list {:from start :to start}) adjacency-list)) ;; need to get the vertices into a set (adjacency-list start) start) (apply conj '() (adjacency-list start)) adjacency-list))

;; (defn bf-seq [start adjacency-list]
;;   (lazy-traverse #{start} (conj PersistentQueue/EMPTY start) adjacency-list))

(defn bf-seq [start adjacency-list]
  (lazy-traverse #{start} (conj PersistentQueue/EMPTY {:from start :to start}) adjacency-list))
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
        ; needs to convert each connection to a full edge
       (bf-seq :a)
       (take 4))

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