(ns dla.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [constraints.core :as c]))

(defn intersections [c p1 p2]
  ;; http://mathworld.wolfram.com/Circle-LineIntersection.html
  (let [pc1 (c/.sub p1 (:c c)) ; move reference frame to center of circle
        pc2 (c/.sub p2 (:c c))
        d (c/.sub pc2 pc1)
        dr (c/mag d)
        D (- (* (.x pc1) (.y pc2)) (* (.x pc2) (.y pc1)))
        ; some convenience variables
        dx (.x d)
        dy (.y d)
        dr2 (Math/pow dr 2)
        r2 (Math/pow (:r c) 2)
        D2 (Math/pow D 2)
        sgn #(if (< % 0) -1 1)
        disc (- (* r2 dr2) D2) ; discriminant
        sqrt-disc (Math/sqrt disc)
        ]
    (println dx dy dr2 r2 D2 disc sqrt-disc)
    (when (< 0 disc) ; there is an intersection
      (if (= 0 disc) ; it's a tangent i.e. a single intersection point
        [(c/->Point (/ (+ (* D dy)) dr2)
                   (/ (+ (* (- D) dx) dr2)))]
        [(c/->Point (/ (+ (* D dy)     (* (sgn dy) dx sqrt-disc)) dr2)
                    (/ (+ (* (- D) dx) (* (Math/abs dy) sqrt-disc) dr2)))
         (c/->Point (/ (- (* D dy)     (* (sgn dy) dx sqrt-disc)) dr2)
                    (/ (- (* (- D) dx) (* (Math/abs dy) sqrt-disc) dr2)))]))))


;(defn circle-intersects? [c1 c2 p1 p2]
; "Projects c1 along p1->p2 and returns true if it intersects c2")
;
;
;(defn center []
;  (c/mk-circle 0 0 10))
;(defn shoot [r circles]
;  (let [a (q/random (* 2 Math/PI))
;        x (Math/cos a)
;        y (Math/sin a)
;        p1 (c/->Point x y)
;        hit-circle (-> circles
;                       (filter #(intersects? % p1 (c/->Point 0 0))
;                       (min-key #(x/dist p1 (:c %))))]
;    (when hit-circle
;      (let [p (k
;      (c/mk-circle k

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circle.
      (q/ellipse x y 100 100))))


(q/defsketch dla
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
