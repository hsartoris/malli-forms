(ns malli-forms.reitit-test
  (:require
    [clojure.pprint :refer [pprint]]
    [malli-forms :as mf]
    [malli-forms.middleware :as mf.mw]
    [malli-forms.render.bootstrap :as bs]
    [malli-forms.test-schemas :refer [asn-pool-schema activity]]
    ;[malli.core :as m]
    ;[reitit.coercion :as coercion]
    [reitit.coercion.malli]
    ;[reitit.core :as r]
    [reitit.ring :as ring]
    [reitit.ring.middleware.muuntaja]
    [reitit.ring.middleware.parameters :as parameters]
    [ring.adapter.jetty :refer [run-jetty]]
    [ring.middleware.nested-params :refer [wrap-nested-params]]
    [rum.core :as rum]))

(defn pprint-pre
  "Pretty-print and wrap in a :pre"
  [data]
  [:pre (with-out-str (pprint data))])

(def router
  "Reitit router for testing above schema"
  (ring/router
    [[""
      ["/pools/new"
       {:name     ::new-pool
        ::mf/schema asn-pool-schema
        :handler mf.mw/stub-handler}]
      ["/enums"
       {::mf/schema [:enum :a "a"]
        :handler mf.mw/stub-handler}]
      ["/math"
       ["/echo"
        {::mf/schema [:map
                      [::my-number int?]]
         :handler    mf.mw/stub-handler}]
       ["/inc"
        {::mf/schema int?
         :handler #(-> % ::mf/value inc)
         :middleware [(fn [handler]
                        (fn [req]
                          [:span (handler req)]))]}]
       ["/sum"
        ;; won't actually respond to form submission
        {:get {:handler (fn [_]
                          (mf/render-form [:set number?]))}}]]
      ["/activities/new"
       {:name    ::new-activity
        ::mf/schema activity
        :handler mf.mw/stub-handler}]]]
    {:data {:middleware [parameters/parameters-middleware
                         mf.mw/remove-empty-params
                         wrap-nested-params
                         (fn [handler]
                           (fn [req]
                             (-> (handler req)
                                 (assoc-in [:headers "content-type"] "text/html")
                                 (update :body #(rum/render-static-markup
                                                  [:html [:body [:main %]]])))))
                         (fn [handler]
                           (fn [req]
                             (let [res (handler req)]
                               (if (map? res)
                                 res
                                 {:body res
                                  :status 200}))))
                         mf.mw/schema-endpoint-reitit
                         ]}}))

(def ^:private app (ring/ring-handler router))

(defonce ^:private server (atom nil))

(defn stop
  "Stop the running test server"
  []
  (when-some [s @server]
    (.stop s))
  (reset! server nil))

(defn start
  "Start a server with an endpoint at /pools/new"
  []
  (stop)
  (reset! server
          (run-jetty
            (fn [& args]
              (prn args)
              (apply app args))
            {:join? false
             :host "0.0.0.0"
             :port 8081})))
