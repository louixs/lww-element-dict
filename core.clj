(ns lww-element.core
  (:refer-clojure :exclude [get merge remove])
  (:import [clojure.lang IPersistentMap ILookup]))

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

;; utils
(defn now []
  (inst-ms (java.util.Date.)))

(defn uuid [] (str (java.util.UUID/randomUUID)))
;; need to convert to be able to compare

;; LWW-Element (Last Write Wins Elelent)
;; there can be LWW-Element-Set, Dict or Map, Seq etc.

;; (defprotocol LWW-Element
;;   (get [this data])
;;   (assoc [data])
;;   (dissoc [data]))

;; You can also do something this to make it more integrated with Clojure's primitive data types
;; in this case with map, by using Java interface
;; (deftype LWW-Element-Dict [data]
;;   ILookup
;;   (valAt [_ k]
;;     (.valAt data k)))

(defrecord Dict [id added removed])

;; This should be a spec
;; (def dict-default
;;   {:id nil
;;    :added {}
;;    :removed {}})

(defn ts-desc-sorted-set
  "Descending sorted set orderd by ts key in items."
  [& items]
  (apply sorted-set-by #(< (:ts %1) (:ts %2)) items))

(defn make-item-value [v]
  {:val v
   :ts (now)})

(defn make-item [k v]
  {k (make-item-value v)})

(defn make-dict-items
  "Use this to create items in either added or removed entries of Dict.
  e.g.
  (make-dict-items {:title 'Language Notes'
                    :note 'This is my notes.'})
  ;; => {:title {:val 'Language Notes', :ts 1600671280235},
         :note {:val 'This is my notes.', :ts 1600671280235}}"
  [m]
  (reduce-kv
   (fn [res k v]
     (clojure.core/merge res (make-item k v)))
   {}
   m))

;; create
;; Add ID when creating so merge knows when merging two instances of the same dict
(defn init-dict
  "Initialize dictionary data structure when
   instantiating a Dict. This can only be used
   when instantiating since it will automatically
   add the map entries passed in to the 'added' items."
  ([]
   (init-dict {}))
  ([m]
   {:pre [(map? m)]}
   {:id (uuid)
    :added (make-dict-items m)
    :removed {}}))

(defn make-dict
  ([]
   (make-dict {}))
  ([m]
   {:pre [(map? m)]}
   (map->Dict (init-dict m))))

;; add
;; update
;; (update d :a 2) => #lww-element{:a 2}
;; note variadic args are not supported in defprotocol
(defprotocol Add
 (add [d k v]))
;; (def d (make-dict {}))
;; (add d :a 1) => #lww-element{:a 1}

(extend-protocol Add
  Dict
  (add [d k v]
    ;; This takes care of both adding
    ;; and updating since
    ;; assoc used in add-item
    ;; updates values if the key already
    ;; exists, since it is a function
    ;; when the item is updated,
    ;; timestamp will be the latest one as well
    (assoc-in d [:added k] (k (make-item k v)))))

(defprotocol Get
  "Retrieve the value of the specified value"
  (get [d k]))

(extend-protocol Get
  Dict
  (get [d k]
    (get-in d [:added k :val])))

;; get
;; (get d :a) => 1

;; remove
;; (remove d :a) => #lww-element{}

(defn remove-from-dict [d k]
  (if (contains? (:added d) k)
    (let [added (:added d)
          entries-to-remove (select-keys added [k])
          new-removed (clojure.core/merge (:removed d)
                                          entries-to-remove)
          new-added (dissoc added k)]
      (assoc d :added new-added
               :removed new-removed))
    d))

(defprotocol Remove
  (remove [d k]))

(extend-protocol Remove
  Dict
  (remove [d k]
    (remove-from-dict d k)))

(defprotocol Merge
  (merge [d1 d2]))
;; merge
;; (def d1 (lww-elemt-dict {}))
;; (def d2 (lww-elemt-dict {}))
;; (add d :a 1)
;; (add d :a 2)
;; (merge d1 d2) => #lww-element{:a 2}

;; merge add

(defn pick-first [a b]
  a)

(defn pick-second [a b]
  b)


(defn ts-conflict-resolver [a b bias-fn]
  (let [ts-a (:ts a)
        ts-b (:ts b)]
    (cond
      (> ts-a ts-b) a
      (< ts-a ts-b) b
      (= ts-a ts-b) (bias-fn a b) ;; what to do when entries have exactly the same time?
      )))

(defn merge-dict [d1 d2]
  ;; Check ID to ensure
  ;; you are merging replicas
  ;; merge added first
  ;; then removed ones
  ;; this way you'll not remove entries without checking
  )

(defn merge-added [m1 m2]
  ;; move the winning ones to add
  ;; move the losing ones to remove
  ()
)

(defn mmm [d1 d2]
(let [added (merge-added d1 d2)
      removed (merge-removed d1 d2)]
;; still removed
(reduce-kv
 (fn [res k v]
    (if (contains? res k)

     (let [added-entry (clojure.core/get added k)
           removed-entry v
           chosen-entry (ts-conflict-resolver added-entry removed-entry pick-first)]
       (clojure.core/merge res chosen-entry))
     res))
)))

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
