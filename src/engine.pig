(module engine
  "Game state, generic logic, and rendering"
  (:import
    [dialogs :from dialogs]
    [dom :from piglet:dom]
    [g :from tilegrids]
    [rot :from "rot-js"]
    [str :from piglet:string]
    ))

(def display
  "Rot.js Display instance, wrapping a HTML Canvas"
  (box nil))

(def state
  "Game state"
  (box
    #js
    {:grid nil
     :map-grid nil
     :entities-grid nil
     :fov-grid nil
     :display-opts {}
     :keymaps []
     :dialogs []
     :environment []
     :entities {}
     :tiles {}
     :queue nil
     :controller nil}))

(def keycodes
  "numeric code -> keyword, e.g. 14 :ENTER"
  (into {}
    (map (fn [[k v]]
           [v (keyword (str:subs k 3))]))
    rot:KEYS))

;; Queries

(defn viewport-width []
  (get-in @state [:display-opts :width]))

(defn viewport-height []
  (get-in @state [:display-opts :height]))

(defn queue []
  (:queue @state))

(defn entv [eid]
  (get-in @state [:entities eid]))

(defn ent-swap! [eid f & args]
  (apply swap! state update-in [:entities eid] f args))

(defn grid []
  (:grid @state))

(defn map-grid []
  (:map-grid @state))

(defn entity-grid []
  (:entity-grid @state))

(defn env-by-coords [x y]
  (g:read (grid) x y))

(defn tile [t]
  (get-in @state [:tiles t]))

(defn passable? [eid x y]
  (get
    (:passable (entv eid))
    (:type (env-by-coords x y))))

(defn empty-spot [tile-pred]
  (loop []
    (let [x (rand-int (viewport-width))
          y (rand-int (viewport-height))]
      (if (tile-pred (:type (env-by-coords x y)))
        [x y]
        (recur)))))

;; State manipulation

(def eid-count (box 0))

(defn new-eid []
  (swap! eid-count inc))

(defn env-set!
  ([x y val]
    (g:write (grid) x y val))
  ([x y k v]
    (g:write (grid) x y (assoc (g:read (grid) x y) k v))))

(defn ent-set!
  ([eid val]
    (swap! state update :entities assoc eid (assoc val :eid eid)))
  ([eid k v]
    (swap! state assoc-in [:entities eid k] v)))

(defn eids-by-trait [t]
  (filter (comp t :traits) (vals (:entities @state))))

(defn move-by! [eid dx dy]
  (ent-swap! eid
    (fn [pos]
      (-> pos
        (update :x + dx)
        (update :y + dy)))))

(defn try-move-by! [eid dx dy]
  (let [{:keys [x y]} (entv eid)
        x (+ x dx)
        y (+ y dy)]
    (when (passable? eid x y)
      (ent-swap! eid
        #(-> % (update :x + dx) (update :y + dy)))
      true)))

;; Drawing

(defn fov-fn []
  (let [g #_(box #{}) (g:FixedTileGrid. (viewport-width) (viewport-height))]
    (g:reduce-xyt
      (grid)
      (fn [_ x y t]
        (when (:blocks-vision? t)
          #_(swap! g conj [x y])

          (g:write g x y true)))
      nil)
    (fn light-passes [x y]
      (not #_(get @g [x y]) (g:read g x y)))))

(defn fov-instance []
  (rot:FOV.PreciseShadowcasting. (fov-fn)))

(defn fov [x y r]
  (let [fov #js {}]
    (.compute (fov-instance)
      x y r
      (fn [x y r visible?]
        (when (and visible? (<= 0 x) (<= 0 y))
          (update! fov x (fnil assoc! #js {}) y true))))
    fov))

(defn efov [eid]
  (let [{:keys [x y vision]} (entv eid)]
    (fov x y vision)))

(defn visible-entities [eid]
  (let [fov (efov eid)]
    (filter (fn [{:keys [x y] :as e}]
              (and
                (not= (:eid e) eid)
                (get-in fov [x y])))
      (:entity-grid @state))))

(defn draw! [x y ch fg bg]
  (.draw (:display @state) x y ch fg bg))

(defn draw-env! [env dialog visible]
  (doseq [[row y] (map js:Array env (range))
          [env-tile x] (map js:Array row (range))
          :when (get-in visible [x y])]
    (if-let [d (get-in dialog [y x])]
      (let [[char fg bg] d]
        (draw! x y char fg bg))
      (let [{:keys [type]} env-tile
            [char fg bg] (tile type)]
        (draw! x y char fg bg)))))

(defn faded-fg-color [ticks]
  (let [rounds (/ ticks 210)
        gray-val (js:Math.round (max 0 (- 83 rounds)))
        hex-val (str:pad-start (.toString gray-val 16) 2 "0")]
    (str "#" hex-val hex-val hex-val)))

(defn faded-bg-color [ticks]
  (let [rounds (/ ticks 210)
        gray-val (js:Math.round (max 0 (- 31 rounds)))
        hex-val (str:pad-start (.toString gray-val 16) 2 "0")]
    (str "#" hex-val hex-val hex-val)))

(defn draw-grid! [tick dialog overlay-grid entity-grid base-grid]
  (g:reduce-xyt base-grid
    (fn [_ x y {:keys [type last-seen] :as t}]
      (if-let [d (get-in dialog [y x])]
        (let [[char fg bg] d]
          (draw! x y char fg bg))
        (when last-seen
          (let [[base-char _ base-bg] (tile type)
                t (or (g:read entity-grid x y) t)
                {:keys [type]} t
                [char fg bg] (tile type)]
            (if (= last-seen tick)
              (draw! x (inc y) char fg (or bg base-bg))
              (draw! x (inc y) base-char
                (faded-fg-color (- tick last-seen))
                (faded-bg-color (- tick last-seen))))))))
    nil))

(defn draw-hud! []
  (let [{:keys [hp max-hp]} (entv :player)]
    (.drawText (:display @state) 1 0 (str "HP " hp " / " max-hp))))

(defn draw-entities! [entities dialog visible]
  (doseq [[eid {:keys [x y] tile-type :tile}] entities
          :when (and tile x y (not (get-in dialog [y x])))
          :when (get-in visible [x y])
          :let [[char fg bg] (tile tile-type)
                env (env-by-coords x y)]]
    (draw! x y char fg (or bg (last (tile (:type env)))))))

(defmulti render-dialog (fn [{:keys [type]}] type))

(defmethod render-dialog :menu [{:keys [items selected title]}]
  (dialogs:draw-center
    (dialogs:render-menu
      (concat
        (when title
          [^:center [[title]]
           [[""]]])
        (for [{:keys [title glyph]} items]
          (into
            [" " (or glyph " ") " "]
            [[title]
             ["  "]])))
      (cond-> selected title (+ 2)))
    (viewport-width)
    (viewport-height)))

(defmethod render-dialog :dialog [{:keys [title text buttons] :as dialog}]
  (dialogs:draw-center
    (dialogs:render-box
      (map #(with-meta % {:center true})
        (into [title
               []]
          (dialogs:split-line text 35))))
    (viewport-width)
    (viewport-height)))


(defn redraw! []
  (let [start-ms (js:performance.now)
        {:keys [grid map-grid entity-grid overlay-grid entities dialogs display]} @state
        dialog (some-> dialogs last render-dialog)
        visible (some-> @state :controller efov)
        tick (.getTime (queue))]
    (doseq [[x col] visible
            [y] col]
      (g:update-xy map-grid (parse-long x) (parse-long y) assoc :last-seen tick))
    (.clear display)
    (.clear entity-grid)
    (into! entity-grid (vals entities))
    (draw-hud!)
    (draw-grid! tick dialog overlay-grid entity-grid map-grid)
    ;; (draw-env! environment dialog visible)
    ;; (draw-entities! entities dialog visible)
    (println "redraw took " (str (- (js:performance.now) start-ms) "ms") (str "(" (/ 1000 (- (js:performance.now) start-ms))" fps)"))))

;; Actions

(defmulti do-action (fn [{:keys [type]}] type))

(defn on-keydown [e]
  (let [c (.-keyCode e)
        k (get keycodes c)
        keymap (last (:keymaps @state))]
    ;; (println "KEY" k "ACTION" (get keymap k))
    (when-let [action (get keymap k)]
      (.preventDefault e)
      (if (vector? action)
        (do-action
          (merge (second action)
            {:type (first action)
             :keyname k
             :keycode c}))
        (do-action {:type action
                    :keyname k
                    :keycode c}))
      false)))

(defn init! [opts]
  (let [{:keys [width height]} (:display-opts opts)
        display (rot:Display. (into #js {} (:display-opts opts)))
        grid-class g:FixedTileGrid
        hud-grid    (new grid-class width height)
        dialog-grid (new grid-class width height)
        map-grid    (new grid-class width height)
        entity-grid (new grid-class width height)]
    (swap! state (fn [state]
                   (assoc
                     (merge state opts)
                     :display display
                     :queue (rot:EventQueue.)
                     :map-grid map-grid
                     :entity-grid entity-grid
                     :hud-grid hud-grid
                     :dialog-grid dialog-grid
                     :grid (g:LayeredGrid. [entity-grid map-grid])
                     :overlay-grid (g:LayeredGrid. [dialog-grid hud-grid]))))
    (dom:listen! js:window ::keyboard "keydown" #'on-keydown)
    (dom:append
      (dom:query-one "#app")
      (.getContainer display))))

(def menu-keymap
  {:UP :menu/prev
   :DOWN :menu/next
   :ESCAPE :dialog/close
   :RETURN :menu/dispatch
   :LEFT :nop
   :RIGHT :nop})

(def dialog-keymap
  {:ESCAPE :dialog/close
   :RETURN :dialog/close})

(defn show-menu! [menu-spec]
  (swap! state
    (fn [state]
      (-> state
        (update :dialogs conj (merge {:type :menu :selected 0} menu-spec))
        (update :keymaps conj menu-keymap)))))

(defn show-dialog! [dialog-spec]
  (swap! state
    (fn [state]
      (-> state
        (update :dialogs conj (merge {:type :dialog} dialog-spec))
        (update :keymaps conj (merge dialog-keymap (:keymap dialog-spec)))))))

(defmethod do-action :menu/nop [_])

(defmethod do-action :menu/prev [_]
  (swap! state update :dialogs
    (fn [dialogs]
      (update dialogs (dec (count dialogs))
        (fn [dialog]
          (if (= :menu (:type dialog))
            (assoc dialog :selected  (max 0 (dec (:selected dialog))))
            dialog)))))
  (redraw!))

(defmethod do-action :menu/next [_]
  (swap! state update :dialogs
    (fn [dialogs]
      (update dialogs (dec (count dialogs))
        (fn [dialog]
          (if (= :menu (:type dialog))
            (assoc dialog :selected (min (dec (count (:items dialog))) (inc (:selected dialog))))
            dialog)))))
  (redraw!))

(defmethod do-action :dialog/close [_]
  (swap! state
    (fn [state]
      (-> state
        (update :dialogs butlast)
        (update :keymaps butlast))))
  (redraw!))

(defmethod do-action :menu/dispatch [_]
  (let [menu (last (:dialogs @state))
        item (nth (:items menu) (:selected menu))]
    (do-action (assoc item :type (:action item)))))

(defmethod do-action :default [e]
  (println "Missing action for " e))

(defmulti handle-state (fn [entity] (:state entity)))

(defmethod handle-state :controller [{:keys [eid] :as e}]
  (swap! state assoc :controller eid))

(defn tick!
  ([]
    (let [q (queue)]
      (when-let [eid (.get q)]
        (handle-state (entv eid))
        (.add q eid (/ 100 (:speed (entv eid) 1))))))
  ([eid]
    (tick! eid 100))
  ([eid time]
    (let [q (queue)]
      (.add q eid (/ time (:speed (entv eid) 1)))
      (when-let [eid (.get q)]
        (handle-state (entv eid))))))

(defmethod handle-state :idle [{:keys [eid] :as e}]
  (tick! eid))

(defmethod handle-state :scanning [{:keys [eid type speed] :as e}]
  (doseq [e (visible-entities eid)]
    (when (< (get-in e [:reputation type]) 0)
      (ent-swap! eid assoc :state :aggro :target (:eid e))))
  (tick! eid 50))

(defn neighbor? [this that]
  (and
    (<= -1 (- (:x this) (:x that)) 1)
    (<= -1 (- (:y this) (:y that)) 1)))

(defmethod handle-state :aggro [{:keys [eid x y target] :as e}]
  (let [target (entv target)
        target-x (:x target)
        target-y (:y target)
        astar (rot:Path.AStar.
                target-x
                target-y
                (constantly true) #_
                (fn [x y]
                  (let [p (passable? eid x y)]
                    (println "PASS" eid x y p)
                    p)))
        path []]
    (.compute astar x y (fn [x y]
                          (println "GOT PATH"  x y)
                          (conj! path [x y])))
    (when (seq path)
      (let [[next-x next-y] (some (fn [loc]
                                    (when (and
                                            (not= loc [x y])
                                            (not= loc [target-x target-y]))
                                      loc))
                              path)]
        (try-move-by! eid (- next-x x) (- next-y y))))
    (when (neighbor? (entv eid) target)
      (ent-swap! eid assoc :state :attacking)))
  (tick! eid))

(defn shake! [n time]
  (let [cl (.-classList (dom:query-one "canvas"))
        kl (str "juicy__shake__" n)]
    (.add cl kl)
    (js:setTimeout #(.remove cl kl) time)))

(defmethod handle-state :attacking [{:keys [eid x y target] :as e}]
  (if (neighbor? (entv eid) (entv target))
    (do
      (shake! 4 450)
      (println
        (select-keys (entv eid) [:x :y])
        (select-keys (entv target) [:x :y])
        (neighbor? (entv eid) (entv target))
        "ATTACK"))
    (ent-swap! eid assoc :state :aggro))
  (tick! eid))

(defn start-engine! []
  (let [q (queue)]
    (doseq [{:keys [eid] :as e} (eids-by-trait :active)
            :let [ts (if (number? eid) eid 0)]]
      (println "INIT QUEUE" eid ts)
      (.add q eid ts)))
  (tick!))

(js:console.log @state)
