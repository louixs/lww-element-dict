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
             (lww/add d :title "Language History" 1)))))
  ;; add multiple entries?
  (testing "if the key is in removed, add the value of the entry on top and move it back to
the added"
    (let [k :title
          d (-> (lww/make-dict {} "#id")
                (lww/add k "Language History" 1)
                (lww/remove k 2))]
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "New Title" :ts 3}
                                                     {:val "Language History" :ts 2}}}
                                    :removed {}}
             (lww/add d k "New Title" 3)))))
  (testing "if the same key is already in added, do nothing and return the dict"
    (let [k :title
          d (-> (lww/make-dict {} "#id")
                (lww/add k "Language History" 1))]
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/add d k "Now Title"))))))

;; Update
(deftest update
  (let [d (-> (lww/make-dict {:title "Language History"} "#id" 1))]
    (testing "it inserts an updated value with a timestamp if the entry already exists in the added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "New Title" :ts 2}
                                                     {:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/update d :title "New Title" 2))))
    (testing "it does nothing and just returns the dict if the insert key does not exist in the added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/update d :non-existing-entry "New Title" 2))))))

;; GET
(deftest get
  (let [d (-> (lww/make-dict {:title "Language History"} "#id" 1))]
    (testing "it retrieves the current/latest value of an entry from added"
      (is (= "Language History"
             (lww/get d :title))))))

;; Remove
;; Elements are "removed" from the set by being moved to the remove set
(deftest remove
  (let [k :title
        d (-> (lww/make-dict {:title "Language History"} "#id" 1))]
    (testing "it moves an entry from added to removed, and timestamp gets updated"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {}
                                    :removed {:title #{{:val "Language History" :ts 2}}}}
             (lww/remove d :title 2))))
    (testing "it does nothing and just returns the provided dict if the entry does not exist in added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/remove d :non-existing-entry 2))))))

;; Merge
;; An element is a member of the set if it is in the add set, and either not in the remove set, or in the
;; remove set but with an earlier timestamp than the latest timestamp in the add set
;; Merging two replicas of the set consists of taking the union of the add sets and the union of the remove sets
;; When timestamps are equal, use "bias. It can be biased towards adds or removals.
(deftest merge
  (let [k :title
        d (-> (lww/make-dict {:title "Language History"} "#id" 1))
        replica d] ;; clojure doesn't mutate value so this isn't strictly necessary; this is for clarify
    (testing "if an entry is in added and removed, the one with the latest timestamp wins. Note that if there are duplicate in values, the one with the latest timestamp is retained due to dedupe? being true by default."
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added
                                    {:title
                                     #{{:val "Updated Title" :ts 3}
                                       {:val "Language History" :ts 2}
                                       {:val "Language History" :ts 1}}}
                                    :removed {}}
           (lww/merge
            (lww/remove replica :title 2)
            (lww/update d :title "Updated Title" 3))))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {}
                                    :removed
                                    {:title
                                     #{{:val "Language History" :ts 4}
                                       {:val "New Title" :ts 3}}}}
             (lww/merge
              (lww/remove replica :title 4)
              (lww/update d :title "New Title" 3))))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {}
                                    :removed {:title #{{:val "Language History" :ts 2}}}}
             (lww/merge
              (lww/remove replica :title 2)
              d))))
    (testing "if an entry is in added and removed and the timestamp is equal, bias (either :add or :remove) is used to chooses where to place it in the merged output, default bias is :add"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "Language History"
                                                      :ts 1}}}
                                    :removed {}}
             (lww/merge
              (lww/remove replica :title 1)
              d)))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {}
                                    :removed {:title #{{:val "Language History"
                                                        :ts 1}}}}
             (lww/merge
              (lww/remove replica :title 1)
              d
              :removed))))
    (testing "if there is no conflict, entries are merged to their respective places (added or removed)"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added
                                    {:title #{{:val "Language History" :ts 1}}
                                     :note #{{:val "This is a note." :ts 1}}}
                                    :removed {}}
             (lww/merge
              (lww/add replica :note "This is a note." 1)
              d)))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {:note #{{:val "This is a note." :ts 3}}}}
             (lww/merge
              (-> replica
                  (lww/add :note "This is a note." 2)
                  (lww/remove :note 3))
              d))))
    (testing "if dedupe? is false it retains the values even if the value is the same; makes it possible to implement undo or state restoration feature based on time"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :added {}
                                    :removed
                                    {:title #{{:val "Language History" :ts 2}
                                              {:val "Language History" :ts 1}}}}
             (lww/merge
              (lww/remove replica :title 2)
              d
              :added
              false))))
    (testing "merging empty dicts returns empty dicts"
      (is (= #lww_element.core.Dict{:id "#id", :added {}, :removed {}}
             (lww/merge
              (-> (lww/make-dict {} "#id" 1))
              (-> (lww/make-dict {} "#id" 1))))))
    (testing "if trying to merge non-replica i.e. dicts' ids are different it throws an error"
      (is (thrown? Exception
           (lww/merge
            (lww/make-dict {:title "different dict"} "#different-id")
            d))))))
