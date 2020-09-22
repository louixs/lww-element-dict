(ns lww-element.core-test
  (:require [clojure.test :as t]
            [lww-element.core :as lww]))


(def d (lww/make-dict {:title "My First Note"
                       :author "Ryuei Sasaki"}))

;; Init
(t/deftest make-dict-tests
  (t/testing "it creates a dictionary with expected keys and values"
    (t/is
     (contains? d :id))))
