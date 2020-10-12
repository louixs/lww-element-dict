# LWW-Element-Dict

This is a LWW-Element-Dict implementation based on [LWW-Element-Set](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#LWW-Element-Set_(Last-Write-Wins-Element-Set)).

The main namespace is lww-element and not lww-element-dict. This is to allow room for potential expansion of other lww-element data types. The main dict is definied with defrecord for simplicity and for performance. Furthermore, protocols are used to allow polymorphism which makes it easy to add other data types. For this version of implementation, it is not strictly necessary to allow expansion but I have chosen protocols since in practice it's going to be useful e.g. add lww-element-set. Some of the functions diverge from the idiomatic Clojure functions. For example "add" could have been called "assoc". The reason for choosing such names is to align with the description of LWW-Element-Dictionary [here](https://github.com/GoodNotes/interviews/blob/master/software-engineering.md) for clarify.
I am also aware that each key-value pair is supposed have a timestamp but I wanted to approached it slightly differently in favour of a more realistic implementation where each key has a set of values and timestamps instead. This can be used to implement for example change historyr undo. This however can add to one of the downsides of state-based CRDT by increasing the size of the data to be passed around. To avoid infinite growth of added or removed sets, max-item-count parameter is used to specify the maximum number of items that can be held in each set. The default max-item-count is 10 but it can be overridden when making a new dict with make-dict. To make the behavior of the functions clear I have provided some simple documentation here. Complete set of behaviour is documented in the tests. 

# Usage

# Creating an instance of Dict
``` clojure
(require [lww-element.dict :as lww-dict])
;; require the lww namespace

(lww-dict/make-dict {:title "My Title"})
;; this returns a dict
;; =>
;; #lww_element.dict.Dict{:id "b6081449-139f-4b0f-aa82-b375fa595588",
;;                        :added {:title #{{:val "My Title", :ts 1601017478040}}},
;;                        :removed {}}

;; Optionally you can supply id, timestamp, and/or max-item-count
;; Note that timestamp needs to be a comparable values with the 'comp' function
(lww-dict/make-dict
 {:title "My Title"}
 "#id" ;; id
 1 ;; timestamp
 10 ;; max-item-count
 )
;; =>
;;#lww_element.dict.Dict{:id "#id",
;;                       :max-item-count 10
;;                       :added {:title #{{:val "My Title", :ts 1}}},
;;                       :removed {}}
 ```

# Add

Works almost like assoc in Clojure but it doesn't upate the value if the key already exists. It puts an element to the :added set. It also accepts optional timestamp param. If it's not provided it puts the current time in milliseconds when the function runs. 

``` clojure
(def d (lww-dict/make-dict {:title "My Title"}))

(lww-dict/add d :note "My note")
;; =>
;;#lww_element.dict.Dict{:id "ade4ed3e-4383-4c27-8298-c240028c70fd",
;;                       :max-item-count 10
;;                       :added
;;                       {:title #{{:val "My Title", :ts 1601018506011}},
;;                        :note #{{:val "My note.", :ts 1601018522194}}},
;;                       :removed {}}

```
# Update

``` clojure
(def d (lww-dict/make-dict {:title "My Title"}))
(lww-dict/update d :title "New Title")
;; =>
;;#lww_element.dict.Dict{:id "a51f3949-cfc4-4df9-8f1e-82e513866377",
;;                       :max-item-count 10
;;                       :added
;;                       {:title
;;                        #{{:val "New Title", :ts 1601018673017}
;;                          {:val "My Title", :ts 1601018632242}}},
;;                       :removed {}}
```

# Remove

If an element exists, it moves from the :added set to the :removed set. Timestamp also gets updated. It also allows an optional timestamp param to make running tests easier.

``` clojure
(def d (lww-dict/make-dict {:title "My Title"}))

(lww-dict/remove d :title)
;; =>
;;#lww_element.dict.Dict{:id "36cb2bca-035f-4a85-b062-48cdbdef8e0d",
;;                       :max-item-count 10
;;                       :added {},
;;                       :removed {:title #{{:val "My Title", :ts 1601018762901}}}}
```

# Get

Gets the latest value of the specified key from the added set.

``` clojure
(def d (lww-dict/make-dict {:title "My Title"}))
(lww-dict/get d :title)
;; => "My Title"
```

# Merge

``` clojure
(def d (lww-dict/make-dict {:title "My Title"}))
(def replica d)

(lww-dict/merge 
 d
 (lww-dict/add replica :note "My Note"))

;; =>
;;#lww_element.dict.Dict{:id "481a5ba0-dba2-4d38-b132-35415a7bf3c8",
;;                       :max-item-count 10
;;                       :added
;;                       {:title #{{:val "My Title", :ts 1601019191306}},
;;                        :note #{{:val "My Note", :ts 1601019328180}}},
;;                       :removed {}}

;; If the element is removed later
;; in any of the replicas, the element
;; gets moved to removed in the merged dict
(def replica-edited
  (lww-dict/remove replica :title))

(lww-dict/merge d replica-edited)
;; =>
;;#lww_element.dict.Dict{:id "0f44ec6e-8103-40f7-b523-a4500d9f521c",
;;                       :max-item-count 10
;;                       :added {},
;;                       :removed {:title #{{:val "My Title", :ts 1601019625069}}}}
```

# A note about ts - timestamp
Except for the `get` function, all other functions _optionally_ accepts a ts (timestamp) argument as timestamp. The reason the timestamp argument is optional is mostly to make this data structure and accompanying protocols (functions) testable. By default it just uses current time in millisecond and the users of the function does not have to care about time as it is an internal details of this data structure.

# Tests

Tests are in the `test/lww_element` directory.
Example based tests are defined in `dict_test.clj`. These are used to test and document API against normal inputs as well as some edge cases.
Property based tests are defined in `dict_property_test.clj`. These mainly test the property of merge function as it needs to have these three properties commutative, associative and idempotent. 

To run tests:
- Please [install](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools) Clojure 
- Clone this project
- Run `clj -A:test` in the root directory of this project

You can of course run the tests using your favourite editor if you have your editor configured. Emacs' Cider provides excellent tools to develop and test Clojure code.
  
