(require '[cljs.pprint :as pprint]
         '[clojure.string :as string]
         '[reagent.core :as r]
         '[reagent.dom :as rdom]
         '[sci.core :as sci])

(def preloaded-code
  "(defn answer []
  ;; Better not obfuscate like this
  (->> js/Math.PI int (nth (iterate #(* % (inc %)) 1))))

(defn the-answer []
  [:div {:style {:font-family \"Georgia\"}}
   [:p \"The answer turns out to 🐝 ... \" (answer)]])

(render-with-reagent the-answer)

(answer)
")
;; end of preloaded-code

(def page-source (r/atom nil))
(def reagent-component (r/atom nil))
(def output (r/atom nil))
(defn print-to-output [s]
  (swap! output str s "\n"))

;; Originally went with just `binding` `*print-fn*` and `*print-err-fn*`, but
;; that failed to be sufficient for functions invoked (again) from Reagent.
(alter-var-root (var *print-fn*)
                (constantly print-to-output))
(alter-var-root (var *print-err-fn*)
                (constantly print-to-output))

(defn load-page-source []
  (-> (js/fetch js/window.location.href)
      (.then #(.text %))
      (.then #(reset! page-source %))))

(defn toggle-page-source []
  (if @page-source
    (reset! page-source nil)
    (load-page-source)))

(def editor-view
  (atom nil))

(defn editor-contents []
  (when-let [view @editor-view]
    (.toString (.-doc (.-state view)))))

(defn history []
  (when-let [view @editor-view]
    (.field (.-state view) js/cm_commands.historyField)))

(defn copy-to-clipboard [s]
  (let [temp-textarea (js/document.createElement "textarea")]
    (.appendChild js/document.body temp-textarea)
    (set! (.-value temp-textarea) s)
    (.select temp-textarea)
    (js/document.execCommand "copy")
    (.removeChild js/document.body temp-textarea)
    nil))

(defn encode [code]
  ;; Unfortunately the encoded URL takes quite a bit more bytes than
  ;; the code itself. I considered using Compression Streams API to
  ;; decrease URL size, but it seems relatively new and I'm not too keen
  ;; on dealing with the asynchrony.
  (-> code
      js/encodeURIComponent
      js/unescape
      js/btoa))

(defn decode [coded]
  (-> coded
      js/atob
      js/escape
      js/decodeURIComponent))

(defn location->url []
  (js/URL. js/window.location.href))

(def code-url-parameter
  "code")

(def checksum-url-parameter
  "checksum")

(defn code-checksum-id [code]
  (-> code hash js/btoa))

(defn share []
  (let [code (editor-contents)
        coded (encode code)
        url (location->url)]
    (.set (.-searchParams url)
          code-url-parameter
          coded)
    (.set (.-searchParams url)
          checksum-url-parameter
          (code-checksum-id code))
    (println "Sharing as" (str url))
    (copy-to-clipboard (str url))
    (js/alert "URL with code copied to clipboard.")))

(defn code-queryparam-value []
  (.get (.-searchParams (location->url))
        code-url-parameter))

(defn checksum-queryparam-value []
  (.get (.-searchParams (location->url))
        checksum-url-parameter))

(defn url-embedded-code []
  (when-let [coded (code-queryparam-value)]
    (let [code (decode coded)
          checksum (checksum-queryparam-value)]
      (if (or (string/blank? checksum)
              (not= checksum (code-checksum-id code)))
        (do
          (js/alert "Unfortunately, the opened URL is incomplete.\nThe embedded code fragment will be ignored.\nYou might want to try that again!")
          (.replace js/window.location (.-pathname (location->url)))
          nil)
        code))))

(def default-local-store-code-key
  "buffer-1")

(def local-store-code-key
  (atom default-local-store-code-key))

(defn local-store-history-key []
  (str @local-store-code-key ".history"))

(defn local-storage []
  (.-localStorage js/window))

(defn stored-code []
  (.getItem (local-storage) @local-store-code-key))

(defn store-code [code]
  (.setItem (local-storage)
            @local-store-code-key
            code))

(defn stored-history []
  (js/JSON.parse (.getItem (local-storage)
                           (local-store-history-key))))

(defn store-history [history]
  (.log js/console history)
  (.setItem (local-storage)
            (local-store-history-key)
            (js/JSON.stringify history)))

(def store-history-timeout-id
  (atom nil))

(defn planned-store-history []
  (when @store-history-timeout-id
    (js/clearTimeout @store-history-timeout-id)
    (reset! store-history-timeout-id nil))
  (-> (history)
      store-history))

(defn plan-store-history []
  (when @store-history-timeout-id
    (js/clearTimeout @store-history-timeout-id)
    (reset! store-history-timeout-id nil))
  (reset! store-history-timeout-id
          (js/setTimeout planned-store-history 400)))

(defn determine-initial-code []
  (if-let [url-embedded (url-embedded-code)]
    (do
      (reset! local-store-code-key
              (str "buffer-" (code-checksum-id url-embedded)))
      (let [locally-stored (stored-code)
            keep-local?
            (and locally-stored
                 (or (= locally-stored url-embedded)
                     (js/confirm "Keep previous changes for this URL?")))]
        (if keep-local?
          locally-stored
          (do
            (store-history "")
            (store-code url-embedded)
            url-embedded))))
    (or (stored-code)
        preloaded-code)))

(defn store-on-change []
  (.of (.-updateListener js/cm_view.EditorView)
       (fn [update]
         (when (.-docChanged update)
           (-> (editor-contents) store-code)
           (plan-store-history)))))

(defn new-editor-state [doc]
  (.create js/cm_state.EditorState
           #js {:doc doc
                :extensions #js[(.of js/cm_language.indentUnit " ")
                                js/codemirror.basicSetup
                                (js/lang_clojure.clojure)
                                (js/codemirror6_parinfer.parinferExtension)
                                (store-on-change)]}))

(defn new-editor [doc]
  (let [parent-div (.getElementById js/document "code-input")
        start-state (new-editor-state doc)]
    (js/cm_view.EditorView. #js{:state start-state
                                :parent parent-div})))

(defn initialize-code-mirror []
  (let [doc (determine-initial-code)
        view (new-editor doc)]
    (reset! editor-view view)
    (when-let [history (stored-history)])))
      ;; (.setHistory (.getDoc @code-mirror) history)))) ; TODO

(defn undo []
  (js/cm_commands.undo @editor-view)
  (.focus @editor-view))

(defn redo []
  (js/cm_commands.redo @editor-view)
  (.focus @editor-view))

(defn clear-code []
  (when (js/confirm "Really clear code buffer?")
    (.setState @editor-view (new-editor-state ""))
    (store-code ""))
  (.focus @editor-view))

(defn -clear-output! []
  (reset! output "")
  (reset! reagent-component nil))

(defn scroll-output-into-focus! []
  (let [dom-node (.getElementById js/document "output")]
    (.scrollIntoView dom-node {"block" "end", "behavior" "smooth"})))

;; available to user (renders within div)
(defn render-with-reagent [component]
  (reset! reagent-component component))

(defonce external-sources (atom {}))

(declare load-code)

(defn load-from-url
  "Experimental; necessarily asynchronous, so fetched code only becomes
   available on reload, which is forced as soon as code is loaded (so, any code
   before this invocation might be evaluated more than once).
   E.g. (load-from-url \"https://raw.githubusercontent.com/weavejester/medley/master/src/medley/core.cljc\")"
  [url]
  (if-let [source (get @external-sources url)]
    (load-string source)
    (do
      (println "Loading" url "...")
      (-> (js/fetch url)
          (.then #(.text %))
          (.then (fn [source]
                   (swap! external-sources assoc url source)
                   (load-string source)
                   (println "Loaded" url)
                   (load-code)))) ; a bit crude; let's see
      (throw (ex-info "loading" {:loading-url url})))))

(defn load-code []
  (let [c (editor-contents)]
    (-clear-output!)
    (js/setTimeout
     (fn []
       (try
         (let [value (load-string c)]
           (print-to-output (pr-str value)))
         (catch ^:sci/error js/Error e
           (when-not (contains? (ex-data e) :loading-url)
             (println "Error:" (.-message e))
             (run! println (-> (sci/stacktrace e) (sci/format-stacktrace))))))
       (js/setTimeout scroll-output-into-focus! 100)
       200))))

(defn render-output []
  (when-not (nil? @output)
    [:div {:style {:margin-top "1em"}}
     [:h3 "Output"]
     [:div {:id "output"}
      [:pre {:id "output-text"} @output]]
     [:button
      {:style {:margin "1px"}
       :on-click #(do (copy-to-clipboard @output)
                      (js/alert "Output copied to clipboard."))}
      "Copy to clipboard"]]))

(defn render-reagent-component []
  (when-let [component @reagent-component]
    [:div {:style {:margin-top "1em"
                   :margin-bottom "2em"}}
     [:h3 "Render"]
     [:div
      (if (vector? component)
        component
        [component])]]))

(defn github-link []
  [:a {:class "github-button"
       :href "https://github.com/jurjanpaul/ape-cljs-playground"
       :data-icon="octicon-repo"}
   ""])

(defn horizontal-separator []
  [:span
   {:style {:margin-left "0.2em"
            :margin-right "0.2em"
            :color "grey"}}
   "|"])

(defn editor []
  [:div
   [:div
    {:style {:display "flex"}}
    [:div
     [:h3
      "🦧 ape-cljs-playground "]]
    [:div
     {:style {:display "flex"
              :position "absolute"
              :right "0px"
              :top "0px"
              :margin-top "0.5em"}}
     [:button
      {:style {:margin-left "4px"
               :margin-right "4px"
               :margin-bottom "0.5em"}
       :on-click toggle-page-source}
      "source with docs " (if @page-source "▽" "▷")]
     [:span {:style {:padding-top "1px"
                     :margin-right "4px"}}
      [github-link]]]]
   [:p
    {:style {:font-size "0.9em"}}
    [:span [:i "Away from Preferred Editor"] " ClojureScript Playground"]]
   (when @page-source
     [:div
      {:style {:font-family "Source Code Pro, monospace"
               :font-size "0.6rem"
               :margin-bottom "1em"}}
      [:pre
       {:style {:padding "0.5em"
                :min-width "40em"
                :overflow "auto"
                :border-width "1px"
                :border-style "solid"
                :border-color "green"
                :color "green"}}
       @page-source]])
   [:div
    {:id "code-input"}]
   [:div
    [:button
     {:style {:margin "1px"}
      :on-click load-code}
     "Load"]
    [horizontal-separator]
    [:button
     {:style {:margin "0 1px 0 1px"
              :padding "0 4px 0 4px"
              :font-size "1.2em"
              :font-weight "bold"
              :transform "translate(0,2px)"}
      :on-click undo}
     "⟲"]
    [:button
     {:style {:margin "0 1px 0 1px"
              :padding "0 4px 0 4px"
              :font-size "1.2em"
              :font-weight "bold"
              :transform "translate(0,2px)"}
      :on-click redo}
     "⟳"]
    [horizontal-separator]
    [:button
     {:style {:margin "1px"}
      :on-click #(do (copy-to-clipboard (editor-contents))
                     (js/alert "Code copied to clipboard."))}
     "Copy"]
    [:button
     {:style {:margin "1px"}
      :on-click share}
     "Share"]
    [:button
     {:style {:margin "1px"}
      :on-click clear-code}
     "Clear"]
    (when-not (string/blank? (.-search (location->url)))
      [:<>
       [horizontal-separator]
       [:span
        {:style {:font-size "0.8em"}}
        [:a
         {:href (.-pathname (location->url))}
         "main buffer >"]]])]
   [render-output]
   [render-reagent-component]])

(rdom/render [editor] (.getElementById js/document "editor"))
(initialize-code-mirror)
