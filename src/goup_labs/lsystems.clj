(ns goup-labs.lsystems
  (:require [quil.core :as q]
            [clojure.core.matrix :as mat]
            [instaparse.core :as insta]))

;; The Lsystem code itself is tiny
(defn lsystem
  "Takes a string and a map of strings to strings of replacements.
   The key for the replacement must only be 1 character.
   "
  [s rules]
  (apply str (map #(get rules (str %) (str %)) s)))

;; Math stuff
(defn round2
  "Rounds a number to two decimal places."
  [n]
  (/ (Math/round (* n 100)) 100))

(defn rotate
  "Takes a vector and an angle in degrees and rotates the vector."
  [[x y] angle]
  (let [s (Math/sin (Math/toRadians angle))
        c (Math/cos (Math/toRadians angle))]
    (mapv round2
          [(- (* x c) (* y s))
           (+ (* x s) (* y c))])))

;; Turtle graphics
(defn base-turtle-state [angle]
  {:angle     angle
   :position  [0 0]
   :direction [0 -1]
   :stack     (list)
   :segments  []
   :color     -1})

(def turtle-parser
  (insta/parser (clojure.java.io/resource "turtle-parser.bnf")))

(defn wrap-op
  [case]
  (let [op (first case)
        result {:op op}]
    (if (= op :set-color)
      (assoc result :color (second case))
      result)))

(defn parse-string
  "Parses the lsystem string into turtle commands."
  [str]
  (rest
    (insta/transform
      {:CASE      wrap-op
       :set-color (fn [_ n] [:set-color (Integer/parseInt n)])}
      (turtle-parser str))))

(defn turtle-line-segment
  "Turtle graphics line segment helper."
  [draw? {:keys [position direction color] :as turtle-state}]
  (let [[x y] position
        [dx dy] direction
        nx (+ x dx)
        ny (+ y dy)
        updated (assoc turtle-state
                  :position [nx ny])]
    (if draw?
      (update-in updated [:segments] conj
                 {:points [x y nx ny]
                  :color (:color turtle-state)})
      updated)))

(defn turtle-rotation
  "Turtle graphics rotation helper."
  [counterclockwise? turtle-state]
  (let [ang (:angle turtle-state)
        theta (if counterclockwise? ang (- ang))]
    (update-in turtle-state [:direction] rotate theta)))

(defmulti interpret
  "Takes a command and a turtle-graphics state and returns the next
   turtle graphics state. Characters are interpreted via the following
   rules.

   Upper case letters mean draw a line segment.
   Lower case letters mean move forward a line segment without drawing.
   + means rotate counterclockwise.
   - means rotate clockwise.
   [ means save current state onto stack
   ] means pop last saved state off of stack"
  (fn [op _] (:op op)))

(defmethod interpret :draw-line-segment [_ turtle-state]
  (turtle-line-segment true turtle-state))

(defmethod interpret :move-line-segment [_ turtle-state]
  (turtle-line-segment false turtle-state))

(defmethod interpret :rotate-clockwise [_ turtle-state]
  (turtle-rotation false turtle-state))

(defmethod interpret :rotate-counterclockwise [_ turtle-state]
  (turtle-rotation true turtle-state))

(defmethod interpret :push-stack [_ turtle-state]
  (update-in turtle-state [:stack] conj {:position (:position turtle-state)
                                         :direction (:direction turtle-state)
                                         :color (:color turtle-state)}))

(defmethod interpret :pop-stack [_ turtle-state]
  (let [stack (:stack turtle-state)
        {:keys [position direction color]} (first stack)]
    (assoc turtle-state
      :position position
      :direction direction
      :color color
      :stack (rest stack))))

(defmethod interpret :set-color [{:keys [color]} turtle-state]
  (assoc turtle-state :color color))

(defmethod interpret :no-op [_ turtle-state]
  turtle-state)

(defn calculate-bounding-box
  [lines]
  "Takes a sequence of line segments and returns a vector representing the
   bounding box.
   [a b c d] where
   [a b] is the upper left corner and
   [c d] is the lower right corner."
  (let [segments (map :points lines)]
    (reduce
      (fn [[a b c d] [x1 y1 x2 y2]]
        [(min a x1 x2)
         (min b y1 y2)
         (max c x1 x2)
         (max d y1 y2)])
      [0 0 0 0]
      segments)))

(defn calculate-position-transform
  "Computes the transformation matrix to translate the drawn l system with
   passed in bounding box to be displayed at x y."
  [x y [tlx tly _ _]]
  (let [movex (- x tlx)
        movey (- y tly)]
    ;; Translation
    [[1 0 movex]
     [0 1 movey]
     [0 0 1    ]]))

(defn calculate-scaling-transform
  "Computes the transformation matrix to scale the drawn l system with
   passed in bounding box to be as big as possible within width X height
   but scaled uniformly."
  [width height [_ _ brx bry]]
  (let [scalex (/ width brx)
        scaley (/ height bry)
        scale (min scalex scaley)]
    [[scale 0     0]
     [0     scale 0]
     [0     0     1]]))

(defn calculate-centering-transform
  "Computes the transformation matrix to translate the drawn l system with
   passed in bounding box to be centered in a width height box."
  [width height [tlx tly brx bry]]
  (let [movex (/ (- width (- brx tlx)) 2)
        movey (/ (- height (- bry tly)) 2)]
    [[1 0 movex]
     [0 1 movey]
     [0 0 1    ]]))

(defn transform-line
  "Takes a 3x3 transformation matrix and a 4 element vector representing two
   points that make a line and transforms the points by the matrix."
  [transform [a b c d]]
  (let [p1 (mat/mmul transform [a b 1])
        p2 (mat/mmul transform [c d 1])]
    (concat (take 2 p1) (take 2 p2))))

(defn transform-segment
  "Takes a sequence of matrix transformations and a turtle line segment map
   and transforms the segment through all the transformations."
  [transforms segment]
  (letfn [(reducer [seg tran]
            (update-in seg [:points] #(transform-line tran %)))]
    (reduce reducer segment transforms)))

;; TODO: Put color choices into turtle-state
(def colors
  {-1 [0 0 0]        ;black
   0 [136 89 61]     ;brown
   1 [36 173 29]     ;light green
   2 [107 255 107]}) ;dark green

(defn processing-draw-turtle
  "Draws the l system turtle in processing. Must be called from the processing
   thread in a setup or draw function."
  [turtle-state x y width height]
  (let [segments (:segments turtle-state)
        bounding-box (calculate-bounding-box segments)
        move-transform (calculate-position-transform x y bounding-box)
        new-bounds (transform-line move-transform bounding-box)
        scale-transform (calculate-scaling-transform width height new-bounds)
        final-bounds (transform-line scale-transform new-bounds)
        center-transform (calculate-centering-transform width height
                                                        final-bounds)
        transforms [move-transform scale-transform center-transform]
        lines (map #(transform-segment transforms %) segments)]
    (doseq [line lines]
      (apply q/line (:points line))
      (q/stroke
        (apply q/color (get colors (:color line)))))))

(defn display
  "Displays the l system string as a 2d turtle graphics interpretation of the
   characters in a processing sketch. Returns the processing sketch.
   "
  [s angle]
  (let [t (atom nil)]
    (letfn [(setup []
                   (q/smooth)
                   (q/background 200))
            (draw []
                  (q/background 200)
                  (if-let [turtle @t]
                    (processing-draw-turtle turtle 0 0 798 598)
                    (q/text "Calculating" 10 10)))]
      (future
        (reset! t (reduce #(interpret %2 %1)
                          (base-turtle-state angle)
                          (parse-string s))))
      (q/sketch
        :title "L System"
        :setup setup
        :draw draw
        :size [800 600]))))

(defn display-with-steps
  "Takes the initial string input, a map of substitution rules, the number
   of steps to run the lsystem and the rotation angle and returns a processing
   sketch that displays the l system after steps substitutions."
  [input rules steps angle]
  (let [result (loop [s input
                      step steps]
                 (if (> step 0)
                   (recur (lsystem s rules) (dec step))
                   s))]
    (display result angle)))

;; Examples
(comment
  ;; A Cool Tree
  (display-with-steps "FX" {"X" "F-[[X]+X]+F[+FX]-X"
                            "F" "FF"} 6 25)
  ;; Another Tree
  (display-with-steps "FX" {"F" "FF-[-F+F]+[+F-F]"
                            "X" "FF+[+F]+[-F]"} 4 25)
  ;; Dragon Curve
  (display-with-steps "FX" {"X" "X+YF+"
                            "Y" "-FX-Y"} 13 90)
  ;; Diamond Thingy
  (display-with-steps "L--F--L--F" {"L" "+R-F-R+"
                                    "R" "-L+F+L-"} 8 45)
  ;; Triangle Thingy
  (display-with-steps "-F" {"F" "F+F-F-F+F"} 4 90)

  ;; One with Skips, This one is dope!
  (display-with-steps "F+F+F+F" {"F" "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF"
                                 "f" "ffffff"} 2 90)
  ;; Neat pentagons star
  (display-with-steps "F-F-F-F-F" {"F" "F-F++F+F-F-F"} 4 72)

  ;; Tree with colors
  (display-with-steps "F" {"F" "C0FF-[C1-F+F+F]+[C2+F-F-F]"} 4 22)

)
