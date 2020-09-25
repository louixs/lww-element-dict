# LWW-Element-Dict

This is a LWW-Element-Dict implementation based on [LWW-Element-Set](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#LWW-Element-Set_(Last-Write-Wins-Element-Set)).

Note that the main namespace is lww-element and not lww-element-dict. This is to allow room for potential expansion of other lww-element data types. The main dict is definied with defrecord. It could have used deftype to signify that it is a new primitive type however, at this stage defrecord was used to prefer simplicity. Protocols are used to specify methods to allow polymorphism which makes it easy to add other data types.

# Usage

# Creating an instance of Dict
``` clojure
(require [lww-element.core :as lww])
;; require the lww namespace

(lww/make-dict {:title "My Title"})
;; this returns a dict
;; =>
;; #lww_element.core.Dict{:id "b6081449-139f-4b0f-aa82-b375fa595588",
;;                        :added {:title #{{:val "My Title", :ts 1601017478040}}},
;;                        :removed {}}

;; Optionally you can supply id and/or timestamp
;; Note that timestamp nees to be an integer as the compare function cannot handle
;; other type like a date instance 
(lww/make-dict {:title "My Title"} "#id" 1)
;; =>
;;#lww_element.core.Dict{:id "#id",
;;                       :added {:title #{{:val "My Title", :ts 1}}},
;;                       :removed {}}
 ```

# Add

Works almost like assoc in Clojure but it doesn't upate the value if the key already exists. It puts an element to the :added set. It also accepts optional timestamp param. If it's not provided it puts the current time in milliseconds when the function runs. 

``` clojure
(def d (lww/make-dict {:title "My Title"}))

(lww/add d :note "My note")
;; =>
;;#lww_element.core.Dict{:id "ade4ed3e-4383-4c27-8298-c240028c70fd",
;;                       :added
;;                       {:title #{{:val "My Title", :ts 1601018506011}},
;;                        :note #{{:val "My note.", :ts 1601018522194}}},
;;                       :removed {}}

```
# Update

``` clojure
(def d (lww/make-dict {:title "My Title"}))
(lww/update d :title "New Title")
;; =>
;;#lww_element.core.Dict{:id "a51f3949-cfc4-4df9-8f1e-82e513866377",
;;                       :added
;;                       {:title
;;                        #{{:val "New Title", :ts 1601018673017}
;;                          {:val "My Title", :ts 1601018632242}}},
;;                       :removed {}}
```

# Remove

If an element exists, it moves from the :added set to the :removed set. Timestamp also gets updated. It also allows an optional timestamp param to make running tests easier.

``` clojure
(def d (lww/make-dict {:title "My Title"}))

(lww/remove d :title)
;; =>
;;#lww_element.core.Dict{:id "36cb2bca-035f-4a85-b062-48cdbdef8e0d",
;;                       :added {},
;;                       :removed {:title #{{:val "My Title", :ts 1601018762901}}}}
```

# Get

Gets the latest value of the specified key from the added set.

``` clojure
(def d (lww/make-dict {:title "My Title"}))
(lww/get d :title)
;; => "My Title"
```

# Merge

``` clojure
(def d (lww/make-dict {:title "My Title"}))
(def replica d)

(lww/merge 
 d
 (lww/add replica :note "My Note"))

;; =>
;;#lww_element.core.Dict{:id "481a5ba0-dba2-4d38-b132-35415a7bf3c8",
;;                       :added
;;                       {:title #{{:val "My Title", :ts 1601019191306}},
;;                        :note #{{:val "My Note", :ts 1601019328180}}},
;;                       :removed {}}

;; If the element is removed later
;; in any of the replicas, the element
;; gets moved to removed in the merged dict
(def replica-edited
  (lww/remove replica :title))

(lww/merge
 d
 replica-edited)
;; =>
;;#lww_element.core.Dict{:id "0f44ec6e-8103-40f7-b523-a4500d9f521c",
;;                       :added {},
;;                       :removed {:title #{{:val "My Title", :ts 1601019625069}}}}
```

# Tests

Tests are in the `test/lww_element` directory.
Example based tests are defined in `core_test.clj`. These are used to test and document API agains normal inputs as well as some edge cases.
Property based tests are defined in `core_property_test.clj`. These mainly test the property of merge function as it needs to be commutative, associative and idempotent. 

- Please [install](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools) Clojure 
- Clone this project
- Run `clj -A:test` in the root directory of this project

You can of course run the tests using your favourite editor if you have your editor configured. Emacs' Cider provides excellent tools to develop and test Clojure code.
  
