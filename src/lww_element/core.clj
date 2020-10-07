(ns lww-element.core
  (:refer-clojure :exclude [get merge remove update])
  (:require [clojure.set :as set]))

(defn- now []
  (inst-ms (java.util.Date.)))

(defn- uuid [] (str (java.util.UUID/randomUUID)))

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

(defrecord Dict [id added removed])

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

(defn- -add
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

(defn- -remove
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

;; merge
(defn- merge-items [x y]
  (merge-with union-ts-desc-sort-set x y))

(defn- put-entry [m entry to]
  (update-in m to
             #(->> (set/union % entry)
                   (apply ts-desc-sorted-set))))

(defn- move-entry [m entry from to]
  (-> m
      (put-entry entry to)
      (update-in (butlast from) #(dissoc % (last from)))))

(defn- merge-dict
  "Bias can be towards either :added or :removed.
   If not supplied, it defaults towards :added."
  [{:keys [bias]
    :or {bias :added}}
   d1 d2]
  {:pre [(contains? #{:added :removed} bias)]}
  (if (identical? (:id d1) (:id d2))
    (let [added (merge-items (:added d1) (:added d2))
          removed (merge-items (:removed d1) (:removed d2))
          id (:id d1)]
      (->> removed
           (reduce-kv
            (fn [res k v]
              (let [entry-in-added (clojure.core/get added k)
                    entry-in-removed (clojure.core/get removed k)]
                ;; if added has the same key in removed
                (if (contains? added k)
                  (cond
                    (> (:ts (first v)) (:ts (first entry-in-added)))
                    ;; if the newest item's timestamp in removed is
                    ;; newer, then the items need to be moved to removed
                    ;; if the timestamp happens to be the same
                    ;; use the bias provided (add or remove) and move the items
                    ;; otherwise merge the items and do the opposite
                    ;; merge and add to removed
                    (move-entry res entry-in-added [:added k] [:removed k])
                    (= (:ts (first v)) (:ts (first entry-in-added)))
                    (if (= bias :added)
                      (move-entry res entry-in-added [:removed k] [:added k])
                      (move-entry res entry-in-removed [:added k] [:removed k]))
                    :else
                    (-> res
                        (update-in [:added k]
                                   #(union-ts-desc-sort-set % v))
                        (clojure.core/update :removed #(dissoc % k))))
                  res)))
            (map->Dict {:id id
                        :added added
                        :removed removed}))))
    (throw (Exception. "Abort merge as you are probably not merging replicate."))))

(defprotocol Merge*
  (merge* [opts] [opts d1] [opts d1 d2]))

(extend-protocol Merge*
  clojure.lang.PersistentArrayMap
  (merge*
    ([opts]
     nil)
    ([opts d1]
     d1)
    ([opts d1 d2]
     (merge-dict opts d1 d2))))

;; protocol doesn't support
;; variadic functions
;; for simplicity making a map of opts mandatory
(defn merge
  ([opts]
   (merge* opts))
  ([opts d1]
   (merge* opts d1))
  ([opts d1 d2]
   (merge* opts d1 d2))
  ([opts d1 d2 & more]
   (reduce (partial merge opts) (merge {} d1 d2) more)))
