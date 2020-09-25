(ns lww-element.core-property-test
  (:require [lww-element.core :as lww]
            [clojure.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def gen-map
  (gen/map gen/keyword gen/string-alphanumeric))

(def gen-id
  (gen/elements ["#id"]))

(def gen-dict
  (gen/fmap (fn [[m id ts]]
              (lww/make-dict m id ts))
            (gen/tuple
             gen-map
             gen-id
             gen/large-integer)))

;; Applying merge is same as appling twice
;; given the same inputs
(defspec merge-is-idempotent 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict]
   (= (lww/merge d1 d2)
      (lww/merge d1 (lww/merge d1 d2)))))

;; Order of operands / args does not change the results
(defspec merge-is-commutative 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict]
    (= (lww/merge d1 d2)
       (lww/merge d2 d1))))

(defn- merge-dicts [d1 d2 d3]
  (lww/merge d1 (lww/merge d2 d3)))

;; Rearranging the parentheses in an expression will not change the result
(defspec merge-is-associative 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict
                 d3 gen-dict]
    (= (merge-dicts d1 d2 d3)
       (merge-dicts d2 d1 d3)
       (merge-dicts d3 d2 d1))))
