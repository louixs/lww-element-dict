(ns lww-element.core-test
  (:refer-clojure :exclude [get merge remove update])
  (:require [clojure.test :as t :refer [deftest is testing]]
            [lww-element.core :as lww]))

(defonce ^:private default-max-item-count 10)

;; Init
(deftest make-dict-tests
  (testing "create dict"
    (is (= #lww_element.core.Dict{:id "#id"
                                  :max-item-count 10
                                  :added {:title #{{:val "My First Note" :ts 1}}}
                                  :removed {}}
           (lww/make-dict {:title "My First Note"} "#id" 1 default-max-item-count)))
    (is (= #lww_element.core.Dict{:id "#id" :max-item-count 10 :added {} :removed {}}
           (lww/make-dict {} "#id" 1 default-max-item-count)))))

;; Add
(deftest add
  (testing "insert entry if the key doesn't already exist in the added set"
    (let [d (lww/make-dict {} "#id" default-max-item-count)]
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "Language History"
                                                      :ts 1}}}
                                    :removed {}}
             (lww/add d :title "Language History" 1)))))
  (testing "if the key is in removed, add the value of the entry on top and move it back to
the added"
    (let [k :title
          d (-> (lww/make-dict {} "#id" default-max-item-count)
                (lww/add k "Language History" 1)
                (lww/remove k 2))]
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "New Title" :ts 3}
                                                     {:val "Language History" :ts 2}}}
                                    :removed {}}
             (lww/add d k "New Title" 3)))))
  (testing "if the same key is already in added, do nothing and return the dict. Updating is taken care by the update function"
    (let [k :title
          d (-> (lww/make-dict {} "#id" default-max-item-count)
                (lww/add k "Language History" 1))]
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/add d k "Now Title"))))))

;; Update
(deftest update
  (let [d (lww/make-dict {:title "Language History"} "#id" 1 default-max-item-count)]
    (testing "insert an updated value with a timestamp if the entry already exists in the added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "New Title" :ts 2}
                                                     {:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/update d :title "New Title" 2))))
    (testing "do nothing and just return the dict if the insert key does not exist in the added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/update d :non-existing-entry "New Title" 2))))))

(deftest max-item-count
  (testing "item counts of an entry in dict does not exceed the specified max count"
    (is (= #lww_element.core.Dict{:id "#id"
                                  :max-item-count 2
                                  :added {:title #{{:val "buz" :ts 4} {:val "bez" :ts 3}}}
                                  :removed {}}
           (-> (lww/make-dict {:title "foo"} "#id" 1 2)
               (lww/update :title "bar" 1)
               (lww/update :title "baz" 2)
               (lww/update :title "bez" 3)
               (lww/update :title "buz" 4))))))

;; GET
(deftest get
  (let [d (lww/make-dict {:title "Language History"} "#id" 1)]
    (testing "retriev the current/latest value of an entry from the added set"
      (is (= "Language History"
             (lww/get d :title))))
    (testing "return nil if the key does not exist"
      (is (nil? (lww/get d :non-existing-key))))))

;; Remove
;; Elements are "removed" from the set by being moved to the remove set
(deftest remove
  (let [k :title
        d (lww/make-dict {:title "Language History"} "#id" 1 default-max-item-count)]
    (testing "move an entry from added to removed, and timestamp gets updated"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {}
                                    :removed {:title #{{:val "Language History" :ts 2}}}}
             (lww/remove d :title 2))))
    (testing "do nothing and just return the provided dict if the entry does not exist in added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/remove d :non-existing-entry 2))))))

;; Merge
;; An element is a member of the set if it is in the add set, and either not in the remove set, or in the
;; remove set but with an earlier timestamp than the latest timestamp in the add set
;; Merging two replicas of the set consists of taking the union of the add sets and the union of the remove sets
;; When timestamps are equal, use "bias. It can be biased towards adds or removals.
(deftest merge
  (let [default-opts {}
        d (lww/make-dict {:title "Language History"} "#id" 1 default-max-item-count)
        replica d] ;; clojure doesn't mutate value so this isn't strictly necessary; this is for clarify
    (testing "if an entry is in added and removed, the one with the latest timestamp wins"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added
                                    {:title
                                     #{{:val "Updated Title" :ts 3}
                                       {:val "Language History" :ts 2}
                                       {:val "Language History" :ts 1}}}
                                    :removed {}}
             (lww/merge
              default-opts
              (lww/remove replica :title 2)
              (lww/update d :title "Updated Title" 3))))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {}
                                    :removed {:title #{{:val "Language History" :ts 2}
                                                       {:val "Language History" :ts 1}}}}
             (lww/merge
              default-opts
              (lww/remove replica :title 2)
              d))))
    (testing "When timestamp is equal for removing and adding an entry, 'bias' (either :added or :removed) is used to chooses where to place it in the merged output, default bias is :added"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "Language History"
                                                      :ts 1}}}
                                    :removed {}}
             (lww/merge
              default-opts
              (lww/remove replica :title 1)
              d)))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {}
                                    :removed {:title #{{:val "Language History"
                                                        :ts 1}}}}
             (lww/merge
              {:bias :removed}
              (lww/remove replica :title 1)
              d))))
    (testing "if there is no conflict, entries are merged to their respective places (added or removed)"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added
                                    {:title #{{:val "Language History" :ts 1}}
                                     :note #{{:val "This is a note." :ts 1}}}
                                    :removed {}}
             (lww/merge
              default-opts
              (lww/add replica :note "This is a note." 1)
              d)))
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added {:title #{{:val "Language History" :ts 1}}}
                                    :removed {:note #{{:val "This is a note." :ts 3}}}}
             (lww/merge
              default-opts
              (-> replica
                  (lww/add :note "This is a note." 2)
                  (lww/remove :note 3))
              d))))
    (testing "merge is variadic i.e. accept any number of Dicts to merge"
      (is (= #lww_element.core.Dict{:id "#id"
                                    :max-item-count 10
                                    :added
                                    {:title #{{:val "Language History" :ts 1}}
                                     :foo #{{:val "foo" :ts 1}}
                                     :bar #{{:val "bar" :ts 2}}
                                     :baz #{{:val "baz" :ts 3}}}
                                    :removed {}}
             (lww/merge
              default-opts
              (lww/add d :foo "foo" 1)
              (lww/add d :bar "bar" 2)
              (lww/add d :baz "baz" 3)
              d))))
    (testing "return dict when only one dict is supplied"
      (is (= d (lww/merge default-opts d))))
    (testing "return nil when only the options map is supplied"
      (is (nil? (lww/merge default-opts))))
    (testing "return empty dict when merging empty dicts"
      (is (= #lww_element.core.Dict{:id "#id" :max-item-count 10 :added {} :removed {}}
             (lww/merge
              default-opts
              (-> (lww/make-dict {} "#id" 1))
              (-> (lww/make-dict {} "#id" 1))
              (-> (lww/make-dict {} "#id" 1))))))
    (testing "if trying to merge non-replica i.e. dicts' ids are different it throws an error"
      (is (thrown? Exception
            (lww/merge
             default-opts
             (lww/make-dict {:title "different dict"} "#different-id")
             d))))))
