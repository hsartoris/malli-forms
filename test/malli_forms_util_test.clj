(ns malli-forms-util-test
  (:require
    [clojure.test :as test :refer [deftest is testing]]
    [malli-forms :as mf]
    [malli.core :as m]
    [malli.util :as mu]))

(def ^:private registry
  "Malli registry with default schemas + util schemas"
  (merge (m/default-schemas) (mu/schemas)))

(deftest deref-subschemas-test
  (testing "Special cases"
    (is (= '[:map
             [:x string?]
             [:y int?]
             [:a :any]]
           (-> [:merge {:registry {::map-a [:map [:x string?] [:y int?]]
                                   ::map-b [:map [:a :any]]}}
                ::map-a ::map-b]
               (mf/deref-subschemas {:registry registry})
               m/form))
        "merge is replaced with derefed form")
    (is (= '[:map [:a string?]]
           (-> [:select-keys [:map [:a string?] [:b int?]] [:a]]
               (mf/deref-subschemas {:registry registry})
               m/form))
        "select-keys is derefed, with second child preserved")
    (is (= '[:maybe {::mf/spec {::m/name ::ping}}
             [:tuple
              [:= :ping]
              [:maybe {::mf/spec {::m/name ::pong}}
               [:tuple
                [:= :pong]
                [:maybe {::mf/spec {::m/name ::ping}}
                 [:tuple
                  [:= :ping]
                  [:ref ::pong]]]]]]]
           (-> [:schema {:registry {::ping [:maybe [:tuple
                                                    [:= :ping]
                                                    [:ref ::pong]]]
                                    ::pong [:maybe [:tuple
                                                    [:= :pong]
                                                    [:ref ::ping]]]}}
                ::ping]
               mf/deref-subschemas
               m/form))
        "ping derefs to a tuple with a maybe pong, which derefs to a tuple with
        a maybe ping, which has already been derefed and is replaced with a
        tuple with a maybe pong, which doesn't get derefed for reasons I don't
        fully understand.")))

;(deftest add-field-spec-test
(comment
  (mf/add-field-specs
    [:schema {:registry {::a string?
                         ::s [:set [:ref ::a]]}}
     [:map [:s [:ref ::s]]]]))
