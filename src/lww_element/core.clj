(ns lww-element.core
  (:refer-clojure :exclude [get merge remove update])
  (:require [clojure.set :as set]))

;; LWW-element-Set - similar implementation as dict
;; Has add set and remove set with a timestamp for each element
;; Elements are added to an LWW-Element-Set by inserting the element into the add set, with a timestamp
;; Elements are "removed" from the set by being moved to the remove set
;; An element is a member of the set if it is in the add set, and either not in the remove set, or in the
;; remove set but with an earlier timestamp than the latest timestamp in the add set
;; Merging two replicas of the set consists of taking the union of the add sets and the union of the remove sets
;; When timestamps are equal, use "bias. It can be biased towards adds or removals.

;; LWW-element-dict
;; Similar to LWW-Element-Set, the dictionary variant you are going to implement
;; will store a timestamp for each key-value pair. In addition to the add and remove
;; operations, the dictionary variant will also allow updating the value of a key.
;; There should be a function to merge two dictionaries. Test cases should be clearly
;; written and document what aspect of CRDT they test. We recommend you to spend no more
;; than 4 hours on this challenge. The provided readings should be sufficient to
;; understand LWW-Element-Set and CRDT on a high level. You are welcome to dig deeper on
;; those but we expect you to come up with the implementation yourself without any help
;; from other open sourced implementations.

(defn- now []
  (inst-ms (java.util.Date.)))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(defrecord Dict [id added removed])

(defn- cmp-ts-reverse [x y]
  (compare (:ts y) (:ts x)))

(defn- cmp-val-reverse [x y]
  (compare (:val y) (:val x)))

(defn- cmp-ts-vl-reverse
  "Compare ts first and if there's a tie break
  use val to compare"
  [x y]
  (let [c (cmp-ts-reverse x y)]
    (if-not (zero? c)
      c
      (cmp-val-reverse x y))))

(defn- ts-desc-sorted-set
  "Descending sorted set orderd by ts.
  You can get the latest item by using the first function."
  ;; in case ts is exactly the same, just providing one compare
  ;; will not add element to set and get lost
  ;; providing a tie-breaking function to prevent that
  [& items]
  (apply sorted-set-by cmp-ts-vl-reverse items))

(defn- union-ts-desc-sort-set [x y]
  (apply ts-desc-sorted-set (set/union x y)))

(defn- make-item-val [v ts]
  {:val v
   :ts ts})

(defn- make-item [k v ts]
  {k (ts-desc-sorted-set (make-item-val v ts))})

(defn- make-dict-items
  "Use this to create items in either added or removed entries of Dict"
  [m ts]
  (reduce-kv
   (fn [res k v]
     (clojure.core/merge res (make-item k v ts)))
   {}
   m))

;; create
;; Add ID when creating so merge knows when merging two instances of the same dict
(defn- init-dict
  "Initialize dictionary data structure when
   instantiating a Dict. This can only be used
   when instantiating since it will automatically
   add the map entries passed in to the 'added' items."
  [m id ts]
  {:pre [(map? m)]}
  {:id id
   :added (make-dict-items m ts)
   :removed {}})

;; API
(defn make-dict
  ([]
   (make-dict {}))
  ([m]
   (make-dict m (uuid) (now)))
  ([m id]
   (make-dict m id (now)))
  ([m id ts]
   {:pre [(map? m)]}
   (map->Dict (init-dict m id ts))))

(defprotocol Add
  (add [d k v] [d k v ts]))

(defn -add
  ([d k v]
   (-add d k v (now)))
  ([d k v ts]
   (cond
     (contains? (:removed d) k)
       (let [existing-entry (clojure.core/get (:removed d) k)
             new-entry #{(make-item-val v ts)}
             merged-entries (union-ts-desc-sort-set existing-entry new-entry)]
         (-> d
             ;; remove the entry from removed
             (clojure.core/update :removed #(dissoc % k))
             ;; add the new merged entry to added
             (assoc-in [:added k] merged-entries)))
       ;; if there is no entry in added, just add a new entry
     (not (contains? (:added d) k))
       (assoc-in d [:added k] (k (make-item k v ts)))
     :else
     ;; otherwise, don't do anything
     d)))

(extend-protocol Add
  Dict
  (add
    ;; This takes care of both adding
    ;; and updating since
    ;; assoc used in add-item
    ;; updates values if the key already
    ;; exists, since it is a function
    ;; when the item is updated,
    ;; timestamp will be the latest one as well
    ;; This only adds items if the key doesn't exist

    ;; if it's in remove, then move it back to added
    ([d k v]
     (-add d k v))
    ([d k v ts]
     (-add d k v ts))))

(defn -update
  ([d k v]
   (-update d k v (now)))
  ([d k v ts]
   (if (contains? (:added d) k)
     (update-in d [:added k] #(union-ts-desc-sort-set % (k (make-item k v ts))))
     d)))

(defprotocol Update
  "Retrieve the value of the specified value."
  (update [d k v] [d k v ts]))

(extend-protocol Update
  Dict
  (update
    ([d k v]
     (-update d k v))
    ([d k v ts]
     (-update d k v ts))))

(defprotocol Get
  "Retrieve the value of the specified value"
  (get [d k]))

(extend-protocol Get
  Dict
  (get [d k]
    ;; Note that this implementation
    ;; only retrieves the latest value
    (-> d (get-in [:added k]) first :val)))

(defn -remove
  ([d k]
   (-remove d k (now)))
  ([d k ts]
   (if (contains? (:added d) k)
     (let [added (:added d)
           entries-to-remove (select-keys added [k])
           entries-to-remove-ts-updated (clojure.core/update entries-to-remove k
                                                             #(->> %
                                                                   (map (fn [x] (assoc x :ts ts)))
                                                                   (apply ts-desc-sorted-set)))
           new-removed (clojure.core/merge (:removed d)
                                           entries-to-remove-ts-updated)
           new-added (dissoc added k)]
       (assoc d :added new-added
                :removed new-removed))
     d)))

(defprotocol Remove
  (remove [d k] [d k ts]))

(extend-protocol Remove
  Dict
  (remove
    ([d k]
     (-remove d k))
    ([d k ts]
     (-remove d k ts))))

(defn- de-dupe [coll]
  (->> coll
   (group-by :val) ;; group by val to see if there are any dupe
   (reduce-kv
    (fn [res k v]
      (if (not= 1 (count v)) ;; if dupe, pick the one with the latest timestamp
        (conj res
              (->> v (sort-by :ts >) first))
        (concat res v)))
    [])))

;; merge
(defn merge-items [x y]
  (merge-with union-ts-desc-sort-set x y))

(defn -merge
  "Bias can be towards either :added or :removed.
   If not supplied, it defaults towards :added."
  ([d1 d2]
   (-merge d1 d2 :added))
  ([d1 d2 bias]
   {:pre [(contains? #{:added :removed} bias)]}
   (if (identical? (:id d1) (:id d2))
     (let [added (merge-items (:added d1) (:added d2))
           removed (merge-items (:removed d1) (:removed d2))
           id (:id d1)]
       (->> removed
            (reduce-kv
             (fn [res k v]
               ;; if added has the same key in removed
               (if (contains? added k)
                 (cond
                   (> (:ts (first v)) (:ts (first (clojure.core/get added k))))
                   ;; if the newest item's timestamp in removed is
                   ;; newer, then the items need to be moved to removed
                   ;; if the timestamp happens to be the same
                   ;; use the bias provided (add or remove) and move the items
                   ;; otherwise merge the items and do the opposite
                   ;; merge and add to removed
                   (do
                     (-> res
                         (update-in [:removed k]
                                    #(->> (set/union % v (clojure.core/get added k))
                                          de-dupe
                                          (apply ts-desc-sorted-set)))
                         ;; remove from added
                         (clojure.core/update :added #(dissoc % k))))
                   (= (:ts (first v)) (:ts (first (clojure.core/get added k))))
                   (if (= bias :added)
                     ;; refactor
                     (-> res
                         (update-in [:added k]
                                    #(->> (set/union % v (clojure.core/get added k))
                                          de-dupe
                                          (apply ts-desc-sorted-set)))
                         (clojure.core/update :removed #(dissoc % k)))
                     (-> res
                         (update-in [:removed k]
                                    #(->> (set/union % v (clojure.core/get removed k))
                                          de-dupe
                                          (apply ts-desc-sorted-set)))
                         (clojure.core/update :added #(dissoc % k))))
                   :else
                   (-> res
                       (update-in [:added k]
                                  #(union-ts-desc-sort-set % v))
                       (clojure.core/update :removed #(dissoc % k))))
                 res))
             (map->Dict {:id id
                         :added added
                         :removed removed}))))
     (throw (Exception. "Abort merge as you are probably not merging replicate.")))))

(defprotocol Merge
  (merge [d1 d2] [d1 d2 bias]))

(extend-protocol Merge
  Dict
  (merge
    ([d1 d2]
     (-merge d1 d2))
    ([d1 d2 bias]
     (-merge d1 d2 bias))))

;; Merge cases
;; key collision
;; 1. same keys in both added
;;   add the latest timestamp one to added
;;   move the lost one to removed
;; 2. same keys in both removed
;;
;; no key collision
;; 3. 

;; 1. same key in added with different ts
;; a. {:added {:title "1" :ts 1}} b. {:added {:title "2" :ts 2}} - pick b because last write wins
;; Added exactly at the same time; which one to pick? - edge case
;; {:added {:title "1" :ts 1}} {:added {:title "2" :ts 1}} - bias?
;; 
;; Same key in removed with different ts
;; a. {:removed {:title "1" :ts 1}} b. {:removed {:title "2" :ts 2}} - pick b
;; a. {:removed {:title "1" :ts 1}} b. {:removed {:title "2" :ts 1}} - which one?
;; 
;; a. {:add {:title "1" :ts 1}} b. {:removed {:title "2" :ts 2}} - pick b
;; use sorted sets to handle collision case
;;
;; 2. same key but one replica has it in added and other in removed
;;   2a. added one has later timestamp
;;   2b. removed one has later timestamp
;;   2c. one in added, removed but same timestamp -> use bias
;;   2d. both in added but same timestamp -> use bias
;;   2e. both in removed but same timestamp -> use bias
;; 3. key that only exists in one replicat (either in added or removed) -> just added 

;; tests cases
;; create
;; add
;; get
;; get items that don't exist
;; update
;; update items that don't exist -> add
;; remove
;; remove items that don't exist

;; merge
;; merge - timestamp same
;; merge - empty dicts

;; merge wrong types

;; This data structure can keep growing infinitely
;; TTL?
