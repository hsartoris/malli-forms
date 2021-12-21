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
                           :abs-path []
                           :path  []}}
          [:a [:set {::mf/spec {:type ::mf/collection
                                :abs-path [:a]
                                :path [:a]}}
               ;; not required, as could be 0
               [:keyword {::mf/spec {:type :text
                                     :render? true
                                     :abs-path [:a ::m/in]
                                     :path [:a ::m/in]}}]]]]
         (-> s1 mf/add-field-specs m/form))))

(deftest collect-specs-test
  (is (= '{:type      ::mf/form
           :path      []
           :abs-path  []
           :children  ({:type     ::mf/map
                        :path     []
                        :abs-path []
                        :children ({:type ::mf/collection
                                    :path [:a]
                                    :abs-path [:a]
                                    :children ({:type     :text
                                                :render?  true
                                                :path     [:a ::m/in]
                                                :abs-path [:a ::m/in]
                                                :value    nil
                                                :idxs     (0)})})}
                       {:type   :submit
                        :name   "submit"
                        :value  "Submit"})}
         (mf/collect-field-specs s1 nil {::mf/auto-placeholder true}))))

