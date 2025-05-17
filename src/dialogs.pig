(module dialogs
  "Render menus/dialogs as boxes"
  (:import piglet:string))

(def chars (zipmap [:tl :tr :bl :br :h :v] "╭╮╰╯─│"))

(def border-color "#c4e7e7")
(def bg-color "#1f1f26")

(defn render-line [segments]
  (for [[s fg bg] segments
        ch s]
    [ch (or fg border-color) (or bg bg-color)]))

(defn render-box [line-specs]
  (let [lines (map render-line line-specs)
        width (apply max (map count lines))
        lines (map (fn [l s]
                     (if (:center (meta s))
                       (let [pad (/ (max 0 (- width (count l))) 2)
                             pad-start (js:Math.floor pad)
                             pad-end   (js:Math.ceil pad)]
                         (concat
                           (repeat pad-start [" " nil bg-color])
                           l
                           (repeat pad-end [" " nil bg-color])))
                       l))
                lines line-specs)]
    (for [j (range (+ (count lines) 2))]
      (for [i (range (+ width 2))]
        (cond
          (= i j 0)
          [(get chars :tl) border-color bg-color]

          (and (= i (inc width)) (= j 0))
          [(get chars :tr) border-color bg-color]

          (and (= i 0) (= j (inc (count lines))))
          [(get chars :bl) border-color bg-color]

          (and (= i (inc width)) (= j (inc (count lines))))
          [(get chars :br) border-color bg-color]

          (or (= i 0) (= i (inc width)))
          [(get chars :v) border-color bg-color]

          (or (= j 0) (= j (inc (count lines))))
          [(get chars :h) border-color bg-color]

          :else
          (or
            (nth (nth lines (dec j)) (dec i))
            [" " border-color bg-color]))))))

(defn render-menu [items selected-idx]
  (render-box
    (map (fn [i idx]
           (if (= idx selected-idx)
             (map (fn [[s fg bg]]
                    [s (or bg bg-color) (or fg border-color)])
               i)
             i))
      items
      (range))))

(defn draw-center [dialog width height]
  (let [res (mapv #(mapv (constantly nil) (range width)) (range height))
        lines (count dialog)
        length (count (first dialog))
        dx (js:Math.round (/ (- width length) 2))
        dy (js:Math.round (/ (- height lines) 2))]
    (doseq [x (range length)
            y (range lines)]
      (assoc-in! res [(+ y dy) (+ x dx)] (nth (nth dialog y) x)))
    res))

(defn split-line
  "Takes a line consisting of [string fg bg] segments, returns an array of
  lines, such that no line has more than max-width characters, by splitting on
  whitespace"
  [segments max-width]
  (let [word-segs (mapcat (fn [[s fg bg]]
                            (for [w (string:split #"(?<=\s)" s)]
                              [w fg bg]))
                    segments)]
    (let [[acc line] (reduce
                       (fn [[acc line] [w fg bg :as seg]]
                         (if (< max-width (apply + (count w) (map (comp count first) line)))
                           [(conj acc (update-in line [(dec (count line)) 0] string:trim))
                            [seg]]
                           [acc (conj line seg)]))
                       [[] []]
                       word-segs)]
      (conj acc line))))
