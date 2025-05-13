(module main
  "Roguelike demo Piglet project"
  (:import
    [dialogs :from dialogs]
    [str :from piglet:string]
    [rot :from "rot-js"]
    [dom :from piglet:dom]))

(def display-opts
  #js
  {:width 60
   :height 20
   :fontSize 22})

(def tiles
  {:wall   ["#" "#874f15" "#573703"]
   :lava   ["$" "#ea1313" "#ff5733"]
   :air    ["·" "#1535a0" "#1f1f26"]
   :player ["@" "#DAF7A6"]
   :tree   ["Λ" "#0bc815"]
   :ram    ["Ꮘ" "#a19c6f"]
   :water  ["~" "#8ec4ff" "#4195ef"]
   :tweezers ["v" "#acb5b5"]
   })

(def entities
  {:tweezers {:description "An old rusty pair of tweezers"}})

(def display nil)
(def keycodes (into {}
                (map (fn [[k v]]
                       [v (keyword (str:subs k 3))]))
                rot:KEYS))

(def state
  (box
    {:keymaps [{:LEFT :player/move-self
                :RIGHT :player/move-self
                :UP :player/move-self
                :DOWN :player/move-self
                :ESCAPE :menu/show-global}]
     :dialogs []
     :environment []
     :entities {}}))

(defn entv [eid]
  (get-in @state [:entities eid]))

(defn entswap! [eid f & args]
  (apply swap! state update-in [:entities eid] f args))

(defn env-by-coords [x y]
  (get-in @state [:environment y x]))

(defn cellular-gen []
  (rot:Map.Cellular.
    (:width display-opts)
    (:height display-opts)
    #js {:born [5 6 7 8]
         :survive [4 5 6 7 8]}))

(declare empty-spot)

(defn gen-lake [size]
  (let [lake #{(empty-spot)}
        lake (reduce (fn [lake _]
                       (let [[x y] (rand-nth lake)
                             x (+ x (rand-nth [-1 0 1]))
                             y (+ y (rand-nth [-1 0 1]))]
                         (if (= :air (:type (env-by-coords x y)))
                           (conj lake [x y])
                           lake)))
               lake
               (range size))]
    (mapcat (fn [[x y]]
              (cons [x y]
                (for [dx [-1 0 1]
                      dy [-1 0 1]
                      :let [x (+ x dx)
                            y (+ y dy)]
                      :when (and
                              (< 0.5 (rand))
                              (= :air (:type (env-by-coords x y))))]
                  [x y]))) lake)))

(defn build-map! []
  (let [gen (cellular-gen)]
    (.randomize gen 0.5)
    (.create gen identity)
    (.create gen identity)
    (.connect gen
      (fn [x y val]
        (swap! state update
          :environment
          (fnil update [])
          y
          (fnil assoc [])
          x
          (if (= 0 val)
            {:type :wall}
            {:type :air})))
      1))

  (dotimes [_ 5]
    (doseq [[x y] (gen-lake (+ 5 (rand-int 10)))]
      (swap! state assoc-in [:environment y x :type] :water))))

(defn draw-env! [env dialog]
  (doseq [x (range (:width display-opts))
          y (range (:height display-opts))]
    (if-let [d (get-in dialog [y x])]
      (let [[char fg bg] d]
        (.draw display x y char fg bg))
      (let [{:keys [type]} (get-in env [y x])
            [char fg bg] (get tiles type)]
        (.draw display x y char fg bg)))))

(defn move-by! [eid dx dy]
  (entswap! eid
    (fn [pos]
      (-> pos
        (update :x + dx)
        (update :y + dy)))))

(defn passable? [eid x y]
  (get
    (:passable (entv eid))
    (:type (env-by-coords x y))))

(defn try-move-by! [eid dx dy]
  (entswap! eid
    (fn [pos]
      (let [x (+ (:x pos) dx)
            y (+ (:y pos) dy)]
        (if (passable? eid x y)
          (-> pos
            (update :x + dx)
            (update :y + dy))
          pos)))))

(defn draw-entities! [entities dialog]
  (doseq [[eid {:keys [tile x y]}] entities]
    (when (and tile x y (not (get-in dialog [y x])))
      (let [[char fg bg] (get tiles tile)
            env (env-by-coords x y)]
        (.draw display x y char fg (or bg (last (get tiles (:type env)))))))))

(defmulti do-action (fn [{:keys [type]}] type))

(defmulti render-dialog (fn [{:keys [type]}] type))
(defmethod render-dialog :menu [{:keys [items selected title]}]
  (dialogs:draw-center
    (dialogs:render-menu
      (concat
        (when title
          [[["  "] [title]]
           [[""]]])
        (for [{:keys [title glyph]} items]
          (into
            [(or glyph " ") " "]
            [[title]])))
      (cond-> selected title (+ 2)))
    (:width display-opts)
    (:height display-opts)))

(defn redraw! []
  (let [{:keys [environment entities dialogs]} @state
        dialog (some-> dialogs first render-dialog)]
    (draw-env! environment dialog)
    (draw-entities! entities dialog)))

(defn on-keydown [e]
  (let [c (.-keyCode e)
        k (get keycodes c)
        keymap (last (:keymaps @state :keymaps))]
    (if-let [action (get keymap k)]
      (do-action {:type action
                  :keyname k
                  :keycode c}))))

(defn empty-spot []
  (loop []
    (let [x (rand-int (:width display-opts))
          y (rand-int (:height display-opts))]
      (if (= :air (:type (env-by-coords x y)))
        [x y]
        (recur)))))

(defn init-player! []
  (let [[x y] (empty-spot)]
    (swap! state assoc-in
      [:entities :player]
      {:x x :y y
       :tile :player
       :passable #{:air}})))

(defn add-tree! []
  (let [x (rand-int (:width display-opts))
        y (rand-int (:height display-opts))]
    (swap! state assoc-in
      [:entities (rand-int (js:Math.pow 2 32))]
      {:x x :y y
       :tile :tree})))

(defn init []
  (set! display (rot:Display. display-opts))
  (dom:listen! js:window ::player-input "keydown" #'on-keydown)
  (dom:append
    (dom:query-one "body")
    (.getContainer display))
  (build-map!)
  (init-player!)
  (dotimes [_ 10]
    (add-tree!))
  (redraw!))

(defmethod do-action :player/move-self [{:keys [keyname]}]
  (cond
    (= :LEFT keyname)
    (try-move-by! :player -1 0)

    (= :RIGHT keyname)
    (try-move-by! :player 1 0)

    (= :UP keyname)
    (try-move-by! :player 0 -1)

    (= :DOWN keyname)
    (try-move-by! :player 0 1))
  (redraw!))

(defmethod do-action :menu/show-global [{:keys [keyname]}]
  (cond
    (= :LEFT keyname)
    (try-move-by! :player -1 0)

    (= :RIGHT keyname)
    (try-move-by! :player 1 0)

    (= :UP keyname)
    (try-move-by! :player 0 -1)

    (= :DOWN keyname)
    (try-move-by! :player 0 1))
  (redraw!))

(defmethod do-action :menu/nop [_])
(defmethod do-action :menu/prev [_]
  (swap! state update :dialogs
    (fn [dialogs]
      (update dialogs (dec (count dialogs))
        (fn [dialog]
          (if (= :menu (:type dialog))
            (assoc dialog :selected  (max 0 (dec (:secleced dialog))))
            dialog)))))
  (redraw!))

(defmethod do-action :menu/next [_]
  (swap! state update :dialogs
    (fn [dialogs]
      (update dialogs (dec (count dialogs))
        (fn [dialog]
          (if (= :menu (:type dialog))
            (assoc dialog :selected (min (dec (count (:items dialog))) (inc (:secleced dialog))))
            dialog)))))
  (redraw!))

(defmethod do-action :menu/close [_]
  (swap! state
    (fn [state]
      (-> state
        (update :dialogs butlast)
        (update :keymaps butlast))))
  (redraw!))

(defmethod do-action :menu/show-global [_]
  (swap! state
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
  (redraw!))

(defmethod do-action :menu/dispatch [_]
  (println "DISPATCH")
  (let [menu (last (:dialogs @state))
        item (nth (:items menu) (:selected menu))]
    (println "DISPATCH" (:action item))
    (do-action {:type :menu/close})
    (do-action {:type (:action item)})))

(init)
