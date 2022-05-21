(ns malli-forms.test-schemas
  (:require [malli-forms :as-alias mf]))

(def asn-pool-schema
  "Schema for https://medium.com/@kirill.ishanov/building-forms-with-re-frame-and-clojure-spec-6cf1df8a114d"
  [:map {:closed true
         :registry {::range-data [:map {:closed true
                                        :doc "range, pure data requirements only"}
                                  [:first [:int {:min 1, :max 65536}]]
                                  [:last  [:int {:min 1, :max 65536}]]]
                    ::range [:and {:doc "range-data with validation for first < last"
                                   ::mf/use-child 0}
                             ::range-data
                             [:fn {:error/message "first must be less than last"}
                              #(< (:first %) (:last %))]]}}
   [:label
    [:string {:min 1, :max 64}]]
   [:preallocated {:default false}
    boolean?]
   [:type         {:optional true}
    [:enum "multihomed" "stub" "transit" "ixp"]]
   ;; TODO: this is resulting in required tags - not good
   [:tags         {:optional true}
    [:set [:string {:max 32}]]]
   [:ranges
    [:and {::mf/use-child 0} ;; TODO: not good; somehow simplify this situation
     ;[:vector {:min 1} ::range]
     [:set {:min 1} ::range]
     [:fn {:error/message "Ranges should not overlap"}
      (fn [ranges]
        (every? (fn [[r1 r2]]
                  (or (nil? r2) ;; only one
                      (and (< (:first r1) (:first r2))
                           (< (:last r1) (:last r2)))))
                (partition-all 2 1 (sort-by :first ranges))))]]]])

(def ex-input
  "input valid for ex-schema"
  {:label "blah"
   :preallocated false
   :type "stub"
   :ranges #{{:first  1
              :last   5}
             {:first  10
              :last   20}}})

(def ex-source
  "input data valid for schema before decoding"
  {"label" "blah"
   "preallocated" "false"
   "tags" ["test"]
   "ranges" {"%7B%3Afirst%20nil%2C%20%3Alast%20nil%7D"
             {"first" "10","last" "20"}}
   "submit" "Submit"})


(def activity
  "Schema for an activity in a TODO app"
  [:map
   [:name string?]
   [:location {:optional true}
    [:orn
     [:coords [:tuple double? double?]]
     [:anywhere [:= "anywhere"]]
     [:named-location string?]]]
   [:cost {:optional true}
    [:orn
     [:dollar-signs [:enum :$ :$$ :$$$ :$$$$ :$$$$$]]
     [:unknown [:= "?"]]
     [:free [:= "free"]]]]
   [:type {:optional true} string?]
   [:notes {:optional true} string?]])
