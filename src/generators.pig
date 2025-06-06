(module generators
  (:import
    [e :from engine]
    [rot :from "rot-js"]
    [g :from tilegrids]
    ))

(defn cellular-gen [{:keys [width height]}]
  (rot:Map.Cellular.
    width
    height
    #js {:born [5 6 7 8]
         :survive [4 5 6 7 8]}))

(defn build-cellural-map [grid {:keys [random wall-tile open-tile iterations]
                                :or {random 0.5 iterations 2}}]
  (let [gen (cellular-gen {:width (g:width grid) :height (g:height grid)})]
    (.randomize gen 0.5)
    (dotimes [_ iterations]
      (.create gen identity))
    (.connect gen
      (fn [x y val]
        (g:write grid x y (if (= 0 val) wall-tile open-tile)))
      1))
  grid)

(comment
  (build-cellural-map (g:FixedTileGrid. 20 20)
  {:wall-tile :wall
   :open-tile :air}))

(defn gen-lake [size]
  (let [lake #{(e:empty-spot #{:air})}
        lake (reduce (fn [lake _]
                       (let [[x y] (rand-nth lake)
                             x (+ x (rand-nth [-1 0 1]))
                             y (+ y (rand-nth [-1 0 1]))]
                         (if (= :air (:type (e:env-by-coords x y)))
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
                              (= :air (:type (e:env-by-coords x y))))]
                  [x y]))) lake)))

(defn add-tree! []
  (let [x (rand-int (e:viewport-width))
        y (rand-int (e:viewport-height))]
    (e:ent-set! (e:new-eid)
      {:x x
       :y y
       :blocks-vision? true
       :type :tree})))
