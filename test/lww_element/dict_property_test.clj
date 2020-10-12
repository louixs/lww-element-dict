(ns lww-element.dict-property-test
  (:require [lww-element.dict :as lww-dict]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defonce gen-map
  (gen/map gen/keyword gen/string-alphanumeric))

(defonce gen-id
  (gen/elements ["#id"]))

(defonce gen-dict
  (gen/fmap (fn [[m id ts]]
              (lww-dict/make-dict m id ts))
            (gen/tuple
             gen-map
             gen-id
             gen/large-integer)))

(defonce merge-fn (partial lww-dict/merge {}))

;; Applying merge is same as applying it twice
;; given the same inputs
(defspec merge-is-idempotent 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict]
   (= (merge-fn d1 d2)
      (merge-fn d1 (merge-fn d1 d2)))))

;; Order of operands / args does not change the results
(defspec merge-is-commutative 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict]
    (= (merge-fn d1 d2)
       (merge-fn d2 d1))))

(defn- merge-dicts [d1 d2 d3]
  (merge-fn d1 (merge-fn d2 d3)))

;; Rearranging the parentheses in an expression will not change the result
(defspec merge-is-associative 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict
                 d3 gen-dict]
    (= (merge-dicts d1 d2 d3)
       (merge-dicts d2 d1 d3)
       (merge-dicts d3 d2 d1))))
