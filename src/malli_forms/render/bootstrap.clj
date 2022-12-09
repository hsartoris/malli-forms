(ns malli-forms.render.bootstrap
  (:require
    [clojure.string :as str]
    [malli-forms :as-alias mf]
    [malli-forms.util :as util]))

(defn- props->attrs
  "Wraps util/props->attrs to add common error detection"
  [{:keys [errors] :as spec}]
  ;; TODO: notes
  (let [attrs (util/props->attrs spec)]
    (cond-> attrs
      (seq errors) (-> (update :class str " is-invalid")
                       ;; TODO: very bad solution
                       (assoc :aria-describedby (str (or (:id spec)
                                                         (str "mf-" (util/path->name (:path spec))))
                                                     "-feedback"))))))

(defn- error-spans
  "Get a seq of error spans from a spec"
  [{:keys [errors] :as spec}]
  (when (seq errors)
    [:div.invalid-feedback {:id (str (or (:id spec)
                                         (str "mf-" (util/path->name (:path spec))))
                                     "-feedback")}
     (str/join ", " (map :message errors))]))

(defmulti render "Bootstrap 5 renderer" :type)

(defn- labeled-input
  [{:keys [id label prefix suffix] :as field-spec}]
  (let [group? (or prefix suffix)
        field [:input.form-control (util/props->attrs field-spec)]
        input (if-not group?
                field
                [:div.input-group
                 (when prefix [:span.input-group-text prefix])
                 field
                 (when suffix [:span.input-group-text suffix])
                 ;; TODO: errors
                 ])]
    [:div.mb-3
     (when label
       [:label.form-label {:for id} label])
     input
     (error-spans field-spec)
     ]))

(defmethod render :default [spec] (labeled-input spec))

(defmethod render :radio
  [{:keys [options label value path] :as spec}]
  [:fieldset
   (when label [:label.form-label {:for (:name spec)} label])
   (for [option options
         :let [label (util/value->label option)
               id (util/path->name (conj path option))
               sel? (= option value)]]
     [:div.form-check
      [:input.form-check-input
       (-> (props->attrs spec) (assoc :checked sel?, :id id, :value (str option)))]
      [:label.form-check-label {:for id} label]])])

(defmethod render :select
  [{:keys [options label id] :as spec}]
  [:div.mb-3
   (when label [:label.form-label {:for id} label])
   [:select.form-select
    (-> spec props->attrs (dissoc :label :options :value))
    (map #(vector :option % (:value %)) options)]
   (error-spans spec)])

(defmethod render :submit [spec]
  [:button.btn.btn-primary (props->attrs spec) (or (util/label spec) (:value spec))])

(defmethod render :button [spec]
  [:button.btn.btn-secondary (assoc (props->attrs spec) :type "submit") ;; consequences?
   (or (util/label spec) (:value spec))])

(defmethod render ::mf/group
  [{:keys [children] :as spec}]
  (list
    [:fieldset.row
     (props->attrs spec)
     (when-some [l (util/label spec)] [:legend l])
     (for [child children]
       [:div.col child])]
    (error-spans spec)))
     

(defmethod render ::mf/collection
  [{:keys [children] :as spec}]
  (list
    [:fieldset
     (props->attrs spec)
     (when-some [l (util/label spec)] [:legend l])
     (seq children)]
   (error-spans spec)))
  

(defmethod render ::mf/form
  [{:keys [children] :as spec}]
  [:form (util/props->attrs spec) (seq children)])
