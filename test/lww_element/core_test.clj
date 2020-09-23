(ns lww-element.core-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [lww-element.core :as lww]))

;; Init
(deftest make-dict-tests
  (testing "it creates a lww_element.core.Dict with a supplied map, and add id and timestamp"
    (is (= #lww_element.core.Dict{:id "#id"
                                  :added {:title #{{:val "My First Note" :ts 1}}}
                                  :removed {}}
           (lww/make-dict {:title "My First Note"} "#id" 1))))
  (testing "passing an empty map will create a dict with empty values"
    (is (= #lww_element.core.Dict{:id "#id" :added {} :removed {}}
           (lww/make-dict {} "#id")))))

;; Add
(deftest add
  (testing "it inserts an entry if the key doesn't already exist in the added"
    (let [d (lww/make-dict {} "#id")]
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "Language History"
                                                      :ts 1}}}
                                    :removed {}}
             (lww/add d :title "Language History" 1))))
    ;; add multiple entries?
    (testing "if the key is in already, add the value of the entry on top and move it back to the added")
    (testing "if not in added or removed, do nothing and just return the dict")))

;; Update


;; Remove
;; Elements are "removed" from the set by being moved to the remove set

;; GET
;; An element is a member of the set if it is in the add set, and either not in the remove set, or in the
;; remove set but with an earlier timestamp than the latest timestamp in the add set

;; Merge
;; Merging two replicas of the set consists of taking the union of the add sets and the union of the remove sets
;; When timestamps are equal, use "bias. It can be biased towards adds or removals.
