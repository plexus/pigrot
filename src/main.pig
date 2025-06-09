(module main
  "Roguelike demo Piglet project"
  (:import
    [dialogs :from dialogs]
    [dom :from piglet:dom]
    [e :from engine]
    [gen :from generators]
    [rot :from "rot-js"]
    [str :from piglet:string]
    [g :from tilegrids]))

(def display-opts
  {:width 60
   :height 20
   :spacing 1
   :fontSize 60
   :fontFamily "'Victor Mono',monospace"})

(def tiles
  {:wall   ["⨇" "#874f15" "#573703"]
   :lava   ["$" "#ea1313" "#ff5733"]
   :air    ["·" "#1535a0" "#1f1f26"]
   :player ["@" "#DAF7A6"]
   :tree   ["Λ" "#0bc815"]
   :ram    ["Ꮘ" "#a19c6f"]
   :water  ["≋" "#8ec4ff" "#4195ef"]
   :snake  ["ୡ" "#0bc815"]
   })

(def entity-types
  {:tweezers {:name "Rusty Tweezers"
              :description "An old rusty pair of tweezers, bent out of shape."
              :tile ["𒀹" "#b7b7c1"]
              :weight 1}
   :sassafras {:name "Sassafras Bark"
               :description "It emits a pleasant, herbal aroma. Makes you crave for rootbeer."
               :tile ["γ" "#a19c6f"]
               :weight 1}
   :sandwich {:name "Sandwich"
              :tile ["◣" "#ff5733"]
              :description "A sandwich with an unidentifiable vegan spread, lettuce, and tomato. The bread has gone a little soggy."}
   :horseshoe {:name "Horseshoe"
               :tile ["Ი" "#b7b7c1"]
               :description "A bent piece of metal. It makes for a fine shoe, if you're a horse."}
   :goblet {:name "Goblet"
            :tile ["ტ" "#8ec4ff"]}})

(def base-keymap
  {:LEFT   :player/move-self
   :RIGHT  :player/move-self
   :UP     :player/move-self
   :DOWN   :player/move-self
   :ESCAPE :menu/show-global
   :I      :menu/inventory})

(defn build-map! [grid]
  (gen:build-cellural-map grid
    {:wall-tile {:type :wall
                 :blocks-vision? true}
     :open-tile {:type :air}})


  ;; (dotimes [_ 5]
  ;;   (doseq [[x y] (gen:gen-lake (+ 5 (rand-int 10)))]
  ;;     (e:env-set! x y {:type :water})))

  (dotimes [_ 10]
    (gen:add-tree!)))

(defn init! []
  (e:init!
    {:display-opts display-opts
     :tiles (into tiles
              (update-vals entity-types :tile))
     :keymaps [base-keymap]
     :entities {}})

  (build-map! (e:map-grid))

  (e:ent-set! :player
    (let [[x y] (e:empty-spot #{:air})]
      {:x x
       :y y
       :type :player
       :passable #{:air}
       :traits #{:active}
       :state :controller
       :reputation {:snake -1}
       :speed 1
       :vision 20
       :hp 9
       :max-hp 9
       :inventory [[:sassafras (rand-int 12)]
                   [:tweezers 1]
                   [:tweezers 1]
                   [:tweezers 1]
                   [:tweezers 1]
                   [:sandwich 1]]}))

  (e:ent-set! :snake
    (let [[x y] (e:empty-spot #{:air})]
      {:x x
       :y y
       :type :snake
       :passable #{:air}
       :traits #{:active}
       :state :scanning
       :speed 0.9
       :vision 10
       :inventory [[:goblet 1]
                   [:horseshoe 1]]}))

  (e:start-engine!)

  (e:redraw!))

(defmethod e:do-action :player/move-self [{:keys [keyname]}]
  (when-let [controller (:controller @e:state)]
    (when
      (cond
        (= :LEFT keyname)
        (e:try-move-by! controller -1 0)

        (= :RIGHT keyname)
        (e:try-move-by! controller 1 0)

        (= :UP keyname)
        (e:try-move-by! controller 0 -1)

        (= :DOWN keyname)
        (e:try-move-by! controller 0 1))
      (e:tick! controller (/ 100 (:speed (e:entv controller))))
      (e:redraw!))))

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
  (let [controller (e:entv (:controller @e:state))]
    (e:show-menu!
    {:title "  INVENTORY  "
     :items (for [[item qty] (:inventory controller)]
              {:title (str (str:pad-start (str qty) 3 " ")
                        " "
                        (get-in entity-types [item :name]))
               :glyph (e:tile item)
               :qty qty
               :item item
               :from controller
               :entity (get entity-types item)
               :action :inventory/show-item})}))
  (e:redraw!))

(defmethod e:do-action :inventory/show-item [{:keys [entity from item qty] :as e}]
  (println "CONTROLLER" from)
  (let [{:keys [tile name description eid]} entity]
    (e:show-dialog!
      {:title [tile [" "] [name]]
       :text [[description]]
       :keymap {:D [:inventory/drop {:item item
                                     :from (:eid from)
                                     :qty qty}]}}))
  (e:redraw!))

(defmethod e:do-action :inventory/drop [{:keys [item qty from] :as action}]
  (println "ACTION" action)
  (let [{:keys [x y]} (e:entv from)]
    (e:ent-set! (e:new-eid) {:x x
                             :y y
                             :type item}))
  (e:redraw!))

(init!)
