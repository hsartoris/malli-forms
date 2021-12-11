(ns malli-forms-test
  (:require
    [clojure.test :as test :refer [deftest is testing]]
    [malli-forms :as mf]
    [malli.core :as m]))

(deftest munge-name-part-test
  (is (= "some_FSLASH_field"
         (mf/munge-name-part "some/field")
         (mf/munge-name-part :some/field)))
  (is (= "some_LT_field"
         (mf/munge-name-part "some<field")))
  (is (= "some_GT_field"
         (mf/munge-name-part "some>field")))
  (is (= "some_DOT_namespaced_FSLASH_keyword"
         (mf/munge-name-part :some.namespaced/keyword))))


(deftest path->name-test
  (is (= "my-map[some_FSLASH_field]"
         (mf/path->name [:my-map :some/field])))
  (is (= "something" (mf/path->name [:something]))))

(deftest field-spec-test
  (is (= [:and {::mf/spec {:path      []
                           :type      :number
                           :required  true
                           :render?   true
                           :name      "root"
                           :label     nil
                           :id        "mf-root"}}
          [:< {::mf/spec {:path     [0]
                          :required true
                          :type     :number}}
           4]
          [:> {::mf/spec {:path     [1]
                          :required true
                          :type     :number}}
           0]]
         (-> [:and [:< 4] [:> 0]]
             mf/add-field-specs
             m/form)))
  (is (= [:and {::mf/spec {:path      []
                           :type      :number
                           :required  true
                           :render?   true
                           :name      "root"
                           :label     nil
                           :id        "mf-root"}}
          [:< {::mf/spec {:path     [0]
                          :required true
                          :type     :number}}
           4]
          [:fn {::mf/spec {:no-spec true
                           :path    [1]}} nat-int?]] ;; ignore that this is a schema too - just a common reference function
         (-> [:and [:< 4] [:fn nat-int?]]
             mf/add-field-specs
             m/form))
      "Function schema under and should not contribute to resulting spec"))

(deftest single-input-test
  (is (= [:input {:id       "mf-root"
                  :name     "root"
                  :required true
                  :type     :email
                  :label    nil
                  :path     []
                  :render?  true}]
         (mf/encode-fields [string? {::mf/type :email}] nil)))
  (is (= '[:fieldset
           ([:label {:for "mf-password"} "Password"]
            [:input {:id        "mf-password"
                     :name      "password"
                     :type      :text
                     :required  true
                     ;; TODO: should these make it to this point?
                     :path      [:password]
                     :label     "Password"
                     :default   nil
                     :render?   true}])]
         (mf/encode-fields [:map [:password string?]] nil)))
  (testing "Properties on schema vs on val schema"
    (is (= '[:fieldset
             ([:label {:for "mf-email"} "Email"]
              [:input {:id        "mf-email"
                       :name      "email"
                       :type      :text
                       :required  true
                       :path      [:email]
                       :label     "Email"
                       :render?   true
                       :default   nil}])]
           (mf/encode-fields [:map [:email {::mf/type :email} string?]] nil))
        "Type property on val schema is ignored")
    (is (= '[:fieldset
             ([:label {:for "mf-email"} "Email"]
              [:input {:id        "mf-email"
                       :name      "email"
                       :type      :email
                       :required  true
                       :path      [:email]
                       :label     "Email"
                       :render?   true
                       :default   nil}])]
           (mf/encode-fields [:map [:email [string? {::mf/type :email}]]] nil))
        "Type property on schema itself is respected")))
           
;(deftest enum-test
;  (is (= [:select {:name    "root"
;                   :id      "mf-root"
;                   :required true}
;          '([:option {:value "one"} "One"]
;            [:option {:value "two"} "Two"]
;            [:option {:value "three"} "Three"])]
;         (mf/encode-fields [:enum :one :two :three] nil))))

(deftest map-test
  (is (= '[:fieldset
           ([:label {:for "mf-user_FSLASH_id"} "User ID"]
            [:input {:type :email
                     :required true
                     :id "mf-user_FSLASH_id"
                     :name "user_FSLASH_id"
                     :path    [:user/id]
                     :render? true
                     :label   "User ID"
                     :default nil}])
           ([:label {:for "mf-user_FSLASH_state"} "User state"]
            [:select {:id "mf-user_FSLASH_state"
                      :name "user_FSLASH_state"
                      :required true
                      :path     [:user/state]
                      :render?  true
                      :label    "User state"
                      :default  nil}
             ([:option {:value "active"} "Active"]
              [:option {:value "locked"} "Locked"]
              [:option {:value "suspended"} "Suspended"])])]
         (mf/encode-fields
            [:map
             [:user/id [string? {::mf/type :email}]]
             [:user/state [:enum :active :locked :suspended]]]
            nil)))
  (is (= '[:fieldset
           ([:label {:for "mf-user_FSLASH_id"} "User ID"]
            [:input {:type :email
                     :required true
                     :id "mf-user_FSLASH_id"
                     :name "user_FSLASH_id"
                     :value "user@example.com"
                     :path    [:user/id]
                     :render? true
                     :label   "User ID"
                     :default nil}])
           ([:label {:for "mf-user_FSLASH_state"} "User state"]
            [:select {:id "mf-user_FSLASH_state"
                      :name "user_FSLASH_state"
                      :required true
                      :path     [:user/state]
                      :render?  true
                      :label    "User state"
                      :default  nil}
             ([:option {:value "active"} "Active"]
              [:option {:value "locked"
                        :selected true} "Locked"]
              [:option {:value "suspended"} "Suspended"])])]
         (mf/encode-fields
            [:map
             [:user/id [string? {::mf/type :email}]]
             [:user/state [:enum :active :locked :suspended]]]
            ;; TODO: was broken when providing keyword as it expected string input
            ;; definitely the way to go is to expect correctly-typed input, and have
            ;; a decoding pass before any encoding, I think
            {:user/id     "user@example.com"
             :user/state  :locked}))))
