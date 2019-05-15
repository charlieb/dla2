(ns dla.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [constraints.core :as c]
            [kdtree :as kd]))

(def radius 5.)
(def link-length 10.) ; 2x radius

(defn prnt [x] (println x) x)
(defn iterate-times [n f init]
 (first (drop n (iterate f init))))
(defn to-v2 [p] [(.x p) (.y p)])
(defn to-Point [[x y]] (c/->Point x y))


(defn mk-circle [x y r] {:c (c/->Point x y) :r r :id 0 :link-dist 0})
(defn add-circle [c cs] 
  (let [id (+ 1 (max (map :id cs)))]
    (conj (assoc c :id id) cs)))

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
  (assoc (mk-circle 0 0 radius)
         :id 0
         :layer 0))

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
            (assoc (mk-circle (.x (second hit)) (.y (second hit)) r) :link (first hit) :layer (+ 1 (:layer hit))))
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
      (conj circles (mk-circle (.x hit) (.y hit) r))
      circles)))

(defn sink [y circles]
  (cons
    (first circles) ; Don't move the origin circle
    (map 
      #(assoc % :c (.add (:c %) (c/->Point 0 y)))
      (rest circles))))

(defn exclude [circles factor]
  (let [tree (kd/build-tree (map #(with-meta (to-v2 (:c %)) %) circles))]
    (cons
      (first circles)
      (map (fn [c] 
              (let [c2 (meta (second (kd/nearest-neighbor tree (to-v2 (:c c)) 2)))]
                (assoc c 
                       :c (c/dist> (:c c) (:c c2)
                                   (+ (:r c) (:r c2)) ; desired-dist
                                   factor))))
           (rest circles)))))

(defn stick [circles factor] "Circles must stay a fixed distance from their :link"
  (map #(if (:link %) 
          (assoc %
                 :c (c/dist< (:c %) (:c (circles (:link %)))
                             (:link-dist %)
                             factor))
         %)
       circles))

;;;;;;;;;; KD-TREE Particle Approach ;;;;;;;;;;;;;;
(defn stick-particle [tree]
  (let [sz 500 -sz -500
        w sz -w -sz 
        h sz -h -sz
        touch-dist-sq (* 2 radius 2 radius)] 
    (loop [p [(q/random -w w) (q/random -h h)]]
      (let [near (kd/nearest-neighbor tree p 2)]
        (if (and (= 2 (count near))
                 (< (:dist-squared (first near)) touch-dist-sq)
                 (< (:dist-squared (second near)) touch-dist-sq))
          (recur [(q/random -w w) (q/random -h h)])
          (if (< (:dist-squared (first near)) touch-dist-sq)
            [p (first near)]
            (recur 
              (if (> (:dist-squared (first near)) (* 10 touch-dist-sq))
                [(q/random -w w) (q/random -h h)]
                [(+ (p 0) (q/random -2 2)) (+ (p 1) (q/random -2 2))]))))))))

(defn aggregate [nparts]
  (first 
    (iterate-times (- nparts 1)
                   (fn [[cs tree]] 
                     (let [[p hit] (stick-particle tree)
                           c (assoc (mk-circle (p 0) (p 1) radius) 
                                    :id (+ 1 (:id (last cs)))
                                    :link (:id (meta hit))
                                    :link-dist link-length
                                    :layer (+ 1 (:layer (meta hit))))]
                       [(conj cs c)
                        (kd/insert tree (with-meta (to-v2 (:c c)) c))]))
                   (let [c0 (center)
                         c1 (assoc c0 :c (.add (:c c0) (c/->Point 0 150)) :id 1)
                         c2 (assoc c0 :c (.add (:c c0) (c/->Point 150 0)) :id 2)
                         c3 (assoc c0 :c (.add (:c c0) (c/->Point 0 -150)) :id 3)
                         c4 (assoc c0 :c (.add (:c c0) (c/->Point -150 0)) :id 4)
                         cs [c0]]; c1 c2 c3 c4]]
                     [cs
                      (kd/build-tree (vec (map #(with-meta (to-v2 (:c %)) %) cs)))]))))


(defn randomize-radii [circles]
  (map (fn [c] (assoc c :r (q/random 2.5 10))) circles))

;;;;;;;;;; QUIL ;;;;;;;;;;;;;;
(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  
  ;  (->> [(center)]
  ;      ;(iterate (partial shoot (q/random 5.)))
  ;      (iterate #(tree (q/random 10.) %))
  ;      (drop 100)
  ;      first)
  (println 'setup)
  (let [circles (vec (aggregate 250))
        ;        circles-changed (vec (map #(assoc % 
        ;                                          :id (+ 1000 (:id %))
        ;                                          :link (when (:link %) (+ 1000 (:link %))))
        ;                                  (iterate-times 2 #(vec (stick %)) circles)))
        ]
;    (println (count circles) (count circles-changed))
;    (println ((vec (concat circles circles-changed)) 1001) )
    {:circles circles; (vec (concat circles circles-changed))
     :lit-layer 0
     :radius radius
     :link-len link-length
     :max-layer (apply max (map :layer circles))
     :frame 0 }
    ))

(defn settled? [cs1 cs2 max-delta]
  (not-any? #(> % max-delta)
            (map #(.mag (.sub (:c %1) (:c %2))) cs1 cs2)))

(defn settled-av? [cs1 cs2 max-delta]
  (> (* max-delta (count cs1))
     (reduce + (map #(.mag (.sub (:c %1) (:c %2))) cs1 cs2))))

(defn settle [cs f]
  (loop [cs cs
         cs-next (f cs)
         i 0]
    (print i '-)
    (if (settled-av? cs cs-next 0.5000)
      cs-next
      (recur cs-next (f cs-next) (inc i)))))

(defn expand-contract [state]
  (let [period 10
        phase (if (< (mod (:frame state) (* period 2)) period) 'expansion 'contraction)
        fr (mod (:frame state) period)

        ; Refers to expansion or contraction phase exclusion change
        exc-min 0.5
        exc-max (* 2. radius) 
        r (+ (:radius state)
               (/ (- (if (= phase 'expansion) exc-max exc-min) (:radius state))
                  (- period fr)))
                     

        ; Refers to expansion or contraction phase link-len change
        lin-min (+ (* 2 exc-min) 7.5)
        lin-max (* 2 exc-max) ;link-length

        lin (+ (:link-len state)
               (/ (- (if (= phase 'expansion) lin-max lin-min) (:link-len state))
                  (- period fr)))

        _ (println 'r (:r (second (:circles state))) r)
        _ (println 'li (:link-dist (second (:circles state))) lin)
        
        cs (map #(assoc % :r r :link-dist lin) (:circles state))

        _ (println phase 'r (:r (second cs)) r)
        _ (println phase 'lin (:link-dist (second cs)) lin)
        _ (println (> lin (* 2 r)))
        ]
    (assoc state
           :radius r
           :link-len lin
           :circles (vec (settle cs #(exclude (stick (vec %) 1.0) 0.2))))))

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
  (println (:frame state))
  (assoc (expand-contract state)
         :frame (inc (:frame state))
         :lit-layer (mod (inc (:lit-layer state)) (:max-layer state))))
             ;
              

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;(q/fill 150 150 150)
  (q/no-fill)
  (q/stroke 150 150 150)
  (doseq [c (:circles state)]
    (when (:link c)
      ; Move origin point to the center of the sketch.
      (q/with-translation [(/ (q/width) 2)
                           ;30
                           (/ (q/height) 2)
                           ]
        ; Draw the circle.
        ;(println (c 0) (c 1))
        ;        (q/ellipse (c 0) (c 1) 2 2)

 ;       (when (> (:id c) 1000) 
 ;         (q/stroke 150 150 50))
 ;       ;(q/stroke 150 150 (* 10 (:layer c)))
 ;       (when (= (:layer c) (:lit-layer state))
  ;       (q/ellipse (.x (:c c)) (.y (:c c)) (* 2. (:r c)) (* 2. (:r c)))
         ;)
        ;(when (or true (< (:layer c) (:lit-layer state)))
          (q/line (.x (:c c))
                  (.y (:c c))
                  (.x (:c ((:circles state) (:link c)))) 
                  (.y (:c ((:circles state) (:link c)))))
        ;  )

        )))
  ;(when (= (:frame state) (:lit-layer state))
  ;  (q/save-frame "dla5000-####.png"))
  )


(q/defsketch dla
  :title "You spin my circle right round"
  :size [1000 1000]
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
