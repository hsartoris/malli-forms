(ns malli-forms.collections-test
  (:require
    [clojure.test :as test :refer [deftest is]]
    [malli-forms :as mf]
    [malli.core :as m]))

(def s1
  "Test schema"
  [:map
   [:a [:set :keyword]]])

(def s2
  "Test schema"
  [:map
   [:a [:set [:maybe :keyword]]]])

(deftest add-specs-test
  (is (= [:map {::mf/spec {:type  ::mf/map
                           :root? true
                           :path  []}}
          [:a [:set {::mf/spec {:type ::mf/collection
                                :path [:a]}}
               [:keyword {::mf/spec {:type :text
                                     :render? true
                                     :required true
                                     :path [:a ::m/in]}}]]]]
         (-> s1 mf/add-field-specs m/form))))

(deftest collect-specs-test
  (is (= '{:type      ::mf/map
           :root?     true
           :path      []
           :children  ({:type ::mf/collection
                        :path [:a]
                        :children ({:type     :text
                                    :render?  true
                                    :path     [:a ::m/in]
                                    :required true
                                    :value    nil
                                    :idxs     (0)})})}
         (mf/collect-field-specs s1 nil {::mf/auto-placeholder true}))))

