(ns malli-forms-test
  (:require
    [clojure.test :as test :refer [deftest is testing]]
    [malli-forms :as mf]
    [malli.core :as m]))

(deftest add-field-specs-test
  (is (= [:and {::mf/spec {:path      []
                           :type      :number
                           :required  true
                           :render?   true
                           :name      "root"
                           :label     "Root"
                           :id        "mf-root"}}
          [:< {::mf/spec {:path     [0]
                          :type     :number}}
           4]
          [:> {::mf/spec {:path     [1]
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
                           :label     "Root"
                           :id        "mf-root"}}
          [:< {::mf/spec {:path     [0]
                          :type     :number}}
           4]
          [:fn {::mf/spec {:no-spec true
                           :path    [1]}} nat-int?]] ;; ignore that this is a schema too - just a common reference function
         (-> [:and [:< 4] [:fn nat-int?]]
             mf/add-field-specs
             m/form))
      "Function schema under and should not contribute to resulting spec"))

(defn- collect->unwrap
  "Calls mf/collect-field-specs on schema, then unwraps expected form node to
  inspect first child"
  [schema]
  (-> schema mf/collect-field-specs :children first))

(deftest collect-field-specs-test
  (is (= {:id       "mf-root"
          :name     "root"
          :required true
          :type     :email
          :label    "Root"
          :path     []
          :render?  true
          :value    nil}
         (collect->unwrap [string? {::mf/type :email}])))
  (is (= '{:type   ::mf/map
           :path   []
           :children ({:id        "mf-password"
                       :name      "password"
                       :type      :text
                       :required  true
                       :path      [:password]
                       :label     "Password"
                       :value     nil
                       :render?   true})}
         (collect->unwrap [:map [:password string?]])))
  (testing "Properties on schema vs on val schema"
    (is (= '{:type   ::mf/map
             :path   []
             :children ({:id        "mf-email"
                         :name      "email"
                         :type      :text
                         :required  true
                         :path      [:email]
                         :label     "Email"
                         :value     nil
                         :render?   true})}
           (collect->unwrap [:map [:email {::mf/type :email} string?]]))
        "Type property on val schema is ignored")
    (is (= '{:type   ::mf/map
             :path   []
             :children ({:id        "mf-email"
                         :name      "email"
                         :type      :email
                         :required  true
                         :path      [:email]
                         :label     "Email"
                         :value     nil
                         :render?   true})}
           (collect->unwrap [:map [:email [string? {::mf/type :email}]]]))
        "Type property on schema itself is respected"))
  (is (= {:name   "custom-name"
          :id     "mf-custom-name"
          :label  "Custom name"
          :path   []}
         (-> [string? {::mf/name "custom-name"}]
             collect->unwrap
             (select-keys [:name :id :label :path]))))
  (is (= {:minlength  1
          :maxlength  10
          :type       :text}
         (-> [string? {:min 1, :max 10}]
             collect->unwrap
             (select-keys [:minlength :maxlength :type]))
         (-> [:string {:min 1, :max 10}]
             collect->unwrap
             (select-keys [:minlength :maxlength :type])))
      "String with min/max on malli gets translated to relevant HTML"))
;; TODO: boolean radio when required
           
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
         (mf/render-form
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
         (mf/render-form
            [:map
             [:user/id [string? {::mf/type :email}]]
             [:user/state [:enum :active :locked :suspended]]]
            ;; TODO: was broken when providing keyword as it expected string input
            ;; definitely the way to go is to expect correctly-typed input, and have
            ;; a decoding pass before any encoding, I think
            {:user/id     "user@example.com"
             :user/state  :locked}))))
