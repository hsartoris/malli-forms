(ns malli-forms.middleware
  "Middleware support for different use cases of malli-forms"
  (:require
    [clojure.pprint :refer [pprint]]
    [malli-forms :as mf]))

(defn remove-empty-params
  "Middleware that removes entirely any kv pairs that come from a form with
  an empty string as value. This should occur before unnesting params, as it
  becomes very difficult to track down empty strings in potentially deeply-
  nested data structures."
  [handler]
  (fn [req]
    (handler (update req :params #(into {} (filter (fn [kv] (not-empty (val kv)))) %)))))

(defn- pprint-pre
  "Pretty-print and wrap in a :pre"
  [data]
  [:pre (with-out-str (pprint data))])

(defn stub-handler
  "Not middleware. Use with schema-endpoint middleware to generate a simple
  page for inspecting output of handle-submit."
  [{::mf/keys [result value] :as req}]
  (list
    [:div {:style {:width "50%", :float "left"}}
     @(:form result)]
    [:div {:style {:width "50%", :float "left"}}
     [:details
      [:summary "Params"]
      (pprint-pre (:params req))]
     [:details
      [:summary "Result of decoding"]
      (pprint-pre result)]
     [:br]
     [:span "Value"]
     [:br]
     (pprint-pre value)]))

(defn schema-endpoint
  "Middleware that takes a schema and an optional map of malli options,
  wrapping a handler to render a form for the schema on GET requests, and to
  validate the input on POST, re-rendering the form with any validation errors,
  or passing the validated input to the handler function under the key
  ::mf/value, and the full result of parsing under ::mf/result.

  Assumes you have upstream logic to handle wrapping a response with a hiccup
  body, and rendering appropriately.

  See the schema definition of :malli-forms/options for specific keys that can
  be provided to control the behavior of malli-forms. Of course other keys for
  malli can also be passed here."
  ([handler schema] (schema-endpoint handler schema))
  ([handler schema options] (schema-endpoint schema handler nil options))
  ([handler schema placeholder options]
   ;; warm up caches
   (mf/render-form schema placeholder options)
   (fn [{:keys [request-method] :as req}]
     (case request-method
       :get   {:status 200
               :body (mf/render-form schema placeholder options)}
       :post  (let [params (:params req)
                    decoded (mf/handle-submit schema params options)]
                (if (mf/parse-failed? decoded)
                  {:status  400
                   :body    @(:form decoded)}
                  (handler (assoc req
                                  ::mf/result decoded
                                  ::mf/value  (:value decoded)))))))))

(def schema-endpoint-reitit
  "Middleware in the data-driven style for reitit. Mounts schema-endpoint,
  detecting ::mf/schema and other matching namespaced keys from the route
  config."
  {:name ::schema-endpoint
   :compile (fn [{::mf/keys [schema placeholder] :as data} _opts]
              (let [options (into {} (filter #(= mf/form-ns (namespace (key %)))) data)]
                (when schema
                  #(schema-endpoint % schema placeholder options))))})
