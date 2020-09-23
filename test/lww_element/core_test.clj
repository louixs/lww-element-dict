(ns lww-element.core-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [lww-element.core :as lww]))


(def d (lww/make-dict {:title "My First Note"
                       :author "Ryuei Sasaki"}))

;; Init
(deftest make-dict-tests
  (testing "it creates a dictionary with expected keys and values"
    (is
     (contains? d :id))))
