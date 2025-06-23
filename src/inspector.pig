(module inspector
  (:import
    [dom :from piglet:dom]
    [css :from piglet:css]))

(def data {:foo 123 :bar 345 :baz {:hello "world"}})

(defn expandable? [o]
  (and (not (string? o))
    (or
      (satisfies? Seqable o)
      (object? o))))

(declare collapsed inspector)

(defn expanded [nid k o]
  [:div.node.expanded
   {:id nid
}
   [:div.heading
    {:on-click (fn []
                 (dom:replace (dom:query-one (str "#" nid))
                   (dom:dom [collapsed nid k o])))}
    [:span.prefix "⏷ "]
    (when k
      [:span.value.key k " "])
    [:span.value (type-name o)]]
   [:div.body
    (if (or (satisfies? DictLike o) (object? o))
      (for [[k v] o]
        [inspector k v])
      (for [v o]
        [inspector v]))]])

(defn collapsed
  ([nid k o]
    [:div.node.collapsed
     {:id nid
      :on-click (fn []
                  (dom:replace (dom:query-one (str "#" nid))
                    (dom:dom [expanded nid k o])))}
     [:span.prefix
      (if (expandable? o)
        "⏵ "
        "  ")]
     (when k [:span.value.key k " "])
     [:span.value
      (if (expandable? o)
        (type-name o)
        (print-str o))]]))

(expandable? 3)

(defn inspector
  ([k v]
    [collapsed (gensym "n") k v])
  ([o]
    [collapsed (gensym "n") nil o]))


(dom:inner-html
  (dom:dom
    [inspect data]))

(def inspector-div (dom:dom [:div#inspector]))

(do
  (set! (.-innerHTML inspector-div) "")
  (dom:append inspector-div (dom:dom [inspector @engine:state])))

(js:console.log @engine:state)

(dom:append
  (dom:query-one "#app")
  inspector-div)

(dom:append
  (dom:query-one "head")
  (dom:dom [:style#inspector-styles]))

(set!
  (.-innerHTML (dom:query-one "#inspector-styles"))
  (css:css
    [["#app" {:display "flex" :flex-direction "row"}]
     ["#inspector" {:background-color "white"
                    :cursor "pointer"
                    :display "flex"
                    :flex-grow "1"
                    :font-family "monospace"}
      [:.prefix {:color "#999"}]
      [:.value {:color "#3b758a"}]]
     [:.node {:padding-left "1rem"}]
     [:.expanded {:display "flex" :flex-direction "column"}]
     ]))
