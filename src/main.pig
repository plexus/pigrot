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
  {:tweezers {:description "An old rusty pair of tweezers"}})

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
       :passable #{:air}}))

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

(defmethod e:do-action :menu/nop [_])
(defmethod e:do-action :menu/prev [_]
  (swap! e:state update :dialogs
    (fn [dialogs]
      (update dialogs (dec (count dialogs))
        (fn [dialog]
          (if (= :menu (:type dialog))
            (assoc dialog :selected  (max 0 (dec (:secleced dialog))))
            dialog)))))
  (e:redraw!))

(defmethod e:do-action :menu/next [_]
  (swap! e:state update :dialogs
    (fn [dialogs]
      (update dialogs (dec (count dialogs))
        (fn [dialog]
          (if (= :menu (:type dialog))
            (assoc dialog :selected (min (dec (count (:items dialog))) (inc (:secleced dialog))))
            dialog)))))
  (e:redraw!))

(defmethod e:do-action :menu/close [_]
  (swap! e:state
    (fn [state]
      (-> state
        (update :dialogs butlast)
        (update :keymaps butlast))))
  (e:redraw!))

(defmethod e:do-action :menu/show-global [_]
  (swap! e:state
    (fn [state]
      (-> state
        (update :dialogs
          conj
          {:type :menu
           :selected 0
           :title "ACTIONS"
           :items [{:title "Inventory"
                    :glyph ["I" "#0bc815"]
                    :action :menu/inventory}
                   {:title "Look"
                    :glyph ["&" "#4195ef"]
                    :action :menu/look}]})
        (update :keymaps
          conj
          {:UP :menu/prev
           :DOWN :menu/next
           :ESCAPE :menu/close
           :RETURN :menu/dispatch
           :LEFT :nop
           :RIGHT :nop}))))
  (e:redraw!))

(defmethod e:do-action :menu/dispatch [_]
  (let [menu (last (:dialogs @e:state))
        item (nth (:items menu) (:selected menu))]
    (e:do-action {:type :menu/close})
    (e:do-action {:type (:action item)})))

(init!)
