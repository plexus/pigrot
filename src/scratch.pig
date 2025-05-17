(module scratch
  (:import [rot :from "rot-js"])
  )


(extend-type rot:EventQueue
  MutableAssociative
  (-assoc! [this k v]
    (.add this v k))
  Empty
  (-empty? [this]
    (empty? (.-heap (.-_events this)))
    ))

(let [q (rot:EventQueue.)]
  (.-heap (.-_events q)))

(not nil)

(empty?  (rot:EventQueue.))

(.-length
  #js {})

(boolean
  (seq []))

(let [q (rot:EventQueue.)]
  (assoc! q 5 :a)
  (assoc! q 3 :b)
  (.get q)
  (assoc! q 9 :c)
  (.get q)
  (assoc! q 8 :d)
  (js:Array.from (.-heap (.-_events q)))
  #_(take-while (comp identity first)
      (repeatedly #(do [(.get q)
                      (js:Array.from (.-heap (.-_events q)))]))))

(.randomize map-gen 0.5)

(.create map-gen display.DEBUG)
(.connect map-gen display.DEBUG 1)

(.create (rot:Map.Uniform.
           (:width display-opts)
           (:height display-opts)
           )
  display.DEBUG)
rot:Display
rot:Map.Cellular

["Arena", "Uniform", "Cellular", "Digger", "EllerMaze", "DividedMaze", "IceyMaze", "Rogue"]

(.create map-gen prn)

;; rot:KEYS.VK_LEFT
;; (get rot:DIRS 4)

;; (.-keyCode last-event)
(comment
  (doseq [x (range 20)
        y (range 10)]
  (prn [x y])
  (.draw display x y "x"))
  (.draw display 1 2 "x")
  (.drawOver display 10 5 "x" )
  (.-drawOver display))

(init)
─ 	━ 	│ 	┃ 	┄ 	┅ 	┆ 	┇ 	┈ 	┉ 	┊ 	┋ 	┌ 	┍ 	┎ 	┏
U+251x 	┐ 	┑ 	┒ 	┓ 	└ 	┕ 	┖ 	┗ 	┘ 	┙ 	┚ 	┛ 	├ 	┝ 	┞ 	┟
U+252x 	┠ 	┡ 	┢ 	┣ 	┤ 	┥ 	┦ 	┧ 	┨ 	┩ 	┪ 	┫ 	┬ 	┭ 	┮ 	┯
U+253x 	┰ 	┱ 	┲ 	┳ 	┴ 	┵ 	┶ 	┷ 	┸ 	┹ 	┺ 	┻ 	┼ 	┽ 	┾ 	┿
U+254x 	╀ 	╁ 	╂ 	╃ 	╄ 	╅ 	╆ 	╇ 	╈ 	╉ 	╊ 	╋ 	╌ 	╍ 	╎ 	╏
U+255x 	═ 	║ 	╒ 	╓ 	╔ 	╕ 	╖ 	╗ 	╘ 	╙ 	╚ 	╛ 	╜ 	╝ 	╞ 	╟
U+256x 	╠ 	╡ 	╢ 	╣ 	╤ 	╥ 	╦ 	╧ 	╨ 	╩ 	╪ 	╫ 	╬ 	╭ 	╮ 	╯
U+257x 	╰ 	╱ 	╲ 	╳ 	╴ 	╵ 	╶ 	╷ 	╸ 	╹ 	╺ 	╻ 	╼ 	╽ 	╾ 	╿

(
  [[["◣", "#ff5733"], [" "], ["Sandwich"]]]
  []
  [["A ", nil, nil], ["sandwich ", nil, nil], ["with ", nil, nil], ["an ", nil, nil], ["unidentifiable", nil, nil]]
  [["vegan ", nil, nil], ["spread, ", nil, nil], ["lettuce, ", nil, nil], ["and ", nil, nil], ["tomato.", nil, nil]]
  [["The ", nil, nil], ["bread ", nil, nil], ["has ", nil, nil], ["gone ", nil, nil], ["a ", nil, nil], ["little ", nil, nil], ["soggy.", nil, nil]])


(:entities @e:state)
(e:redraw!)
(e:ent-set! (e:new-eid) {:tile :snake
                         :x 10
                         :y 10})
