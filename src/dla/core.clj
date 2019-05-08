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
    (when (< 0 disc) ; there is an intersection
      (if (= 0 disc) ; it's a tangent i.e. a single intersection point
        [(.add (:c c) (c/->Point (/ (* D dy) dr2)
                                 (/ (* (- D) dx) dr2)))]
        [(.add (:c c) (c/->Point (/ (+ (* D dy)     (* (sgn dy) dx sqrt-disc)) dr2)
                                 (/ (+ (* (- D) dx) (* (Math/abs dy) sqrt-disc)) dr2)))
         (.add (:c c) (c/->Point (/ (- (* D dy)     (* (sgn dy) dx sqrt-disc)) dr2)
                                 (/ (- (* (- D) dx) (* (Math/abs dy) sqrt-disc)) dr2)))]))))

(defn center []
  (c/mk-circle 0 0 10))
(defn closest [p ps] (when ps (apply min-key #(.mag (.sub % p)) ps)))

(defn tree [r circles]
  (let [a (q/random (* 2 Math/PI))
        x (* 1000. (Math/cos a))
        y (* 1000. (Math/sin a))
        p (c/->Point x y)
        hit (->> circles
                 (map-indexed (fn [idx circ] "Find the two intersections and give the closest one back with the index"
                                [idx (closest p (intersections (assoc circ :r (+ r (:r circ))) p (c/->Point 0 0)))]))
                 (filter #(not (nil? (second %))))
                 (apply min-key #(.mag (.sub (second %) p))))]
    (if hit
      (conj circles 
            (assoc (c/mk-circle (.x (second hit)) (.y (second hit)) r) :link (first hit)))
      circles)))

(defn shoot [r circles]
  (let [a (q/random (* 2 Math/PI))
        x (* 1000. (Math/cos a))
        y (* 1000. (Math/sin a))
        p (c/->Point x y)
        hit (->> circles
                (mapcat #(intersections (assoc % :r (+ r (:r %))) p (c/->Point 0 0)))
                (apply min-key #(.mag (.sub p %))))]
    (if hit
      (conj circles (c/mk-circle (.x hit) (.y hit) r))
      circles)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  [(center)]
  )

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
;  (doseq [c state] (println (.x (:c c)) (.y (:c c))))
;  (println '>>>>>>>>>>>>>>>)
;  (loop [cs state]
;    (let [cs-next (tree 5. cs)]
;      (println cs-next)
;      (doseq [c cs-next] (println (.x (:c c)) (.y (:c c))))
;      (println '------------------------------------------)
;      (recur cs-next)))
;  (println '==========================================)
  (->> state
      ;(iterate (partial shoot (q/random 5.)))
      (iterate #(tree (q/random 10.) %))
      (drop 10)
      first
      ))


(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  (q/fill 150 150 150)
  (q/stroke 150 150 150)
  (doseq [c state]
    (when (:link c)
      ; Move origin point to the center of the sketch.
      (q/with-translation [(/ (q/width) 2)
                           (/ (q/height) 2)]
        ; Draw the circle.
        ;(q/ellipse (.x (:c c)) (.y (:c c)) (* 2. (:r c)) (* 2. (:r c)))
        (q/line (.x (:c c)) (.y (:c c))
                (.x (:c (state (:link c)))) (.y (:c (state (:link c)))))

        ))))


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
