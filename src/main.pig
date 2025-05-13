(module main
  "Roguelike demo Piglet project"
  (:import
    [dialogs :from dialogs]
    [dom :from piglet:dom]
    [e :from engine]
    [gen :from generators]
    [rot :from "rot-js"]
    [str :from piglet:string]))

(def display-opts
  {:width 60
   :height 20
   :spacing 1
   :fontSize 60
   :fontFamily "'Victor Mono',monospace"})

(def tiles
  {:wall   ["#" "#874f15" "#573703"]
   :lava   ["$" "#ea1313" "#ff5733"]
   :air    ["·" "#1535a0" "#1f1f26"]
   :player ["@" "#DAF7A6"]
   :tree   ["Λ" "#0bc815"]
   :ram    ["Ꮘ" "#a19c6f"]
   :water  ["~" "#8ec4ff" "#4195ef"]
   :tweezers ["v" "#acb5b5"]})

(def entities
  {:tweezers {:name "Tweezers"
              :description "An old rusty pair of tweezers"
              :weight 1}
   :sassafras {:name "Sassafras Bark"
               :description "It emits a pleasant, herbal aroma. Makes you crave for rootbeer."
               :weight 1}
   :sandwich {:name "Sandwich"
              :description "A sandwich with an unidentifiable vegan spread, lettuce, and tomato. The bread has gone a little soggy."}
   })

(def base-keymap
  {:LEFT   :player/move-self
   :RIGHT  :player/move-self
   :UP     :player/move-self
   :DOWN   :player/move-self
   :ESCAPE :menu/show-global})

(defn build-map! []
  (let [gen (gen:cellular-gen)]
    (.randomize gen 0.5)
    (.create gen identity)
    (.create gen identity)
    (.connect gen
      (fn [x y val]
        (e:env-set! x y (if (= 0 val)
                          {:type :wall}
                          {:type :air})))
      1))

  (dotimes [_ 5]
    (doseq [[x y] (gen:gen-lake (+ 5 (rand-int 10)))]
      (e:env-set! x y {:type :water})))

  (dotimes [_ 10]
    (gen:add-tree!))
  )

(defn init! []
  (e:init!
    {:display-opts display-opts
     :tiles tiles
     :keymaps [base-keymap]
     :entities {}})

  (build-map!)

  (e:ent-set! :player
    (let [[x y] (e:empty-spot #{:air})]
      {:x x
       :y y
       :tile :player
       :passable #{:air}
       :inventory [[:sassafras (rand-int 12)]
                   [:tweezers 1]
                   [:tweezers 1]
                   [:tweezers 1]
                   [:tweezers 1]
                   [:sandwich 1]]}))

  (e:redraw!))

(defmethod e:do-action :player/move-self [{:keys [keyname]}]
  (cond
    (= :LEFT keyname)
    (e:try-move-by! :player -1 0)

    (= :RIGHT keyname)
    (e:try-move-by! :player 1 0)

    (= :UP keyname)
    (e:try-move-by! :player 0 -1)

    (= :DOWN keyname)
    (e:try-move-by! :player 0 1))
  (e:redraw!))


(def action-menu
  {:title "ACTIONS"
   :items [{:title "Inventory"
            :glyph ["I" "#0bc815"]
            :action :menu/inventory}
           {:title "Look"
            :glyph ["&" "#4195ef"]
            :action :menu/look}]})

(defmethod e:do-action :menu/show-global [_]
  (e:show-menu! action-menu)
  (e:redraw!))

(defmethod e:do-action :menu/inventory [_]
  (e:show-menu!
    {:title "  INVENTORY  "
     :items (for [[item qty] (:inventory (e:entv :player))]
              {:title (str (str:pad-start (str qty) 3 " ")
                        " "
                        (get-in entities [item :name]))})})
  (e:redraw!))

(defmethod e:do-action :menu/dispatch [_]
  (let [menu (last (:dialogs @e:state))
        item (nth (:items menu) (:selected menu))]
    (e:do-action {:type (:action item)})))

(init!)
