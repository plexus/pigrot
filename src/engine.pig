(module engine
  "Game state, generic logic, and rendering"
  (:import
    [dialogs :from dialogs]
    [str :from piglet:string]
    [rot :from "rot-js"]
    [dom :from piglet:dom]))

(def display
  "Rot.js Display instance, wrapping a HTML Canvas"
  (box nil))

(def state
  "Game state"
  (box
    {:display-opts {}
     :keymaps []
     :dialogs []
     :environment []
     :entities {}
     :tiles {}}))

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

(defn entv [eid]
  (get-in @state [:entities eid]))

(defn entswap! [eid f & args]
  (apply swap! state update-in [:entities eid] f args))

(defn env-by-coords [x y]
  (get-in @state [:environment y x]))

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
    (swap! state update
      :environment
      (fnil update [])
      y
      (fnil assoc [])
      x
      val))
  ([x y k v]
    (swap! state update
      :environment
      (fnil update [])
      y
      (fnil update [])
      x
      assoc k v)))

(defn ent-set!
  ([eid val]
    (swap! state update :entities assoc eid val))
  ([eid k v]
    (swap! state assoc-in [:entities eid k] v)))

(defn move-by! [eid dx dy]
  (entswap! eid
    (fn [pos]
      (-> pos
        (update :x + dx)
        (update :y + dy)))))

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

;; Drawing

(defn draw! [x y ch fg bg]
  (.draw (:display @state) x y ch fg bg))

(defn draw-env! [env dialog]
  (doseq [[row y] (map js:Array env (range))
          [env-tile x] (map js:Array row (range))]
    (if-let [d (get-in dialog [y x])]
      (let [[char fg bg] d]
        (draw! x y char fg bg))
      (let [{:keys [type]} env-tile
            [char fg bg] (tile type)]
        (draw! x y char fg bg)))))

(defn draw-entities! [entities dialog]
  (doseq [[eid {:keys [x y] tile-type :tile}] entities]
    (when (and tile x y (not (get-in dialog [y x])))
      (let [[char fg bg] (tile tile-type)
            env (env-by-coords x y)]
        (draw! x y char fg (or bg (last (tile (:type env)))))))))

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
            [" " (or glyph " ") " "]
            [[title]
             ["  "]])))
      (cond-> selected title (+ 2)))
    (viewport-width)
    (viewport-height)))

(defn redraw! []
  (let [start-ms (js:performance.now)
        {:keys [environment entities dialogs]} @state
        dialog (some-> dialogs last render-dialog)]
    (draw-env! environment dialog)
    (draw-entities! entities dialog)
    (println "redraw took " (str (- (js:performance.now) start-ms) "ms") (str "(" (/ 1000 (- (js:performance.now) start-ms))" fps)"))))

;; Actions

(defmulti do-action (fn [{:keys [type]}] type))

(defn on-keydown [e]
  (let [c (.-keyCode e)
        k (get keycodes c)
        keymap (last (:keymaps @state :keymaps))]
    (if-let [action (get keymap k)]
      (do-action {:type action
                  :keyname k
                  :keycode c}))))

(defn init! [opts]
  (let [display (rot:Display. (into #js {} (:display-opts opts)))]
    (swap! state (fn [state]
                   (assoc
                     (merge state opts)
                     :display display)))
    (dom:listen! js:window ::keyboard "keydown" #'on-keydown)
    (dom:append
      (dom:query-one "#app")
      (.getContainer display))))

(def menu-keymap
  {:UP :menu/prev
   :DOWN :menu/next
   :ESCAPE :menu/close
   :RETURN :menu/dispatch
   :LEFT :nop
   :RIGHT :nop})

(defn show-menu! [menu-spec]
  (swap! state
    (fn [state]
      (-> state
        (update :dialogs conj (merge {:type :menu :selected 0} menu-spec))
        (update :keymaps conj menu-keymap)))))

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

(defmethod do-action :menu/close [_]
  (swap! state
    (fn [state]
      (-> state
        (update :dialogs butlast)
        (update :keymaps butlast))))
  (redraw!))
