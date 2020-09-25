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

(defspec merge-is-commutative 100
  (prop/for-all [d1 gen-dict
                 d2 gen-dict]
    (= (lww/merge d1 d2)
       (lww/merge d2 d1))))
