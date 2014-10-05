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
   :segments  []})

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
  [draw? {:keys [position direction] :as turtle-state}]
  (let [[x y] position
        [dx dy] direction
        nx (+ x dx)
        ny (+ y dy)
        updated (assoc turtle-state
                  :position [nx ny])]
    (if draw?
      (update-in updated [:segments] conj [x y] [nx ny])
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
                                         :direction (:direction turtle-state)}))

(defmethod interpret :pop-stack [_ turtle-state]
  (let [stack (:stack turtle-state)
        {:keys [position direction]} (first stack)]
    (assoc turtle-state
      :position position
      :direction direction
      :stack (rest stack))))

(defmethod interpret :set-color [{:keys [color]} turtle-state]
  ;TODO
  turtle-state
  )

(defmethod interpret :no-op [_ turtle-state]
  turtle-state)

(defn calculate-bounding-box
  [segments]
  "Takes a sequence of vectors and returns a vector representing the
   bounding box.
   [a b c d] where
   [a b] is the upper left corner and
   [c d] is the lower right corner."
  (reduce
    (fn [[a b c d] [x y]]
      [(min a x)
       (min b y)
       (max c x)
       (max d y)])
    [0 0 0 0]
    segments))

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

(defn transform-point
  "Multiplies a vector by a transformation matrix."
  [transform [x y]]
  (let [transformed (mat/mmul transform [x y 1])]
    (take 2 transformed)))

;; TODO: combine transforms to one multiplication.
(defn processing-draw-turtle
  "Draws the l system turtle in processing. Must be called from the processing
   thread in a setup or draw function."
  [turtle-state x y width height]
  (let [segments (:segments turtle-state)
        ;; Move to [0 0]
        bounding-box (calculate-bounding-box segments)
        move-transform (calculate-position-transform x y bounding-box)
        moved-points (map #(transform-point move-transform %) segments)
        ;; Scale to display size
        new-bounds (calculate-bounding-box moved-points)
        scale-transform (calculate-scaling-transform width height new-bounds)
        scaled-points (map #(transform-point scale-transform %) moved-points)
        ;; Center in viewport.
        final-bounds (calculate-bounding-box scaled-points)
        center-transform (calculate-centering-transform width height
                                                        final-bounds)
        points (map #(transform-point center-transform %) scaled-points)]
    ;; Scale the viewport so that this drawing will be centered.
    (doseq [[[a b] [c d]] (partition 2 points)]
      (q/line a b c d))))

(defn display
  "Displays the l system string as a 2d turtle graphics interpretation of the
   characters in a processing sketch. Returns the processing sketch.
   "
  [s angle]
  (letfn [(setup []
                 (q/smooth)
                 (q/background 200)
                 (let [turtle
                       (reduce
                         #(interpret %2 %1)
                         (base-turtle-state angle)
                         (parse-string s))]
                   (processing-draw-turtle turtle 0 0 798 598)))]
    (q/sketch
      :title "L System"
      :setup setup
      :size [800 600])))

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

  ;; Tree with colors TODO: color support
  (display-with-steps "F" {"F" "C0FF-[C1-F+F+F]+[C2+F-F-F]"} 5 22)

)
