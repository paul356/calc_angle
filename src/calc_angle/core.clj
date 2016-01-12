(ns calc-angle.core
  (:require [clojure.string :as string]
            [clojure.core.reducers :as r])
  (:gen-class))

(declare radian-to-degree)

(def PI 3.1415926)

(def base-high 10.3)

(def first-arm 14.)

(def second-arm 15.2)

(def first-angle-map {40 14.22404995529685, 45 20.043782041086256, 50 25.709566633950928, 55 31.292333102431485, 60 37.64978345241232, 65 43.95137401124416})

(def second-angle-map {60 9.390239675001839, 65 14.672252295919492, 70 20.917146108295576, 75 26.983126309334125, 80 32.827069109280586, 85 38.730492145419724})

(def base-angle-map {-20 -21.848553133651606 -10 -10.771402340447487 0 1.7763568394002505E-15 10 9.2752026167234 20 20.626695019341334 30 30.827271334625493})

;(defn map-angle [angle-map ang]
;  (let [ks     (keys angle-map)
;        kmax   (apply max ks)
;        kmin   (apply min ks)]
;    (cond
;      (> ang (angle-map kmax)) (+ ang (- kmax (angle-map kmax)))
;      (< ang (angle-map kmin)) (+ ang (- kmin (angle-map kmin)))
;      :else (let [[kn vn] (reduce (fn [[kmin min-dist] [k v]] 
;                                    (if (< (Math/abs (- ang v)) min-dist) 
;                                      [k v]
;                                      [kmin min-dist]))
;                                  [0 360]
;                                  angle-map)]
;              (+ ang (- kn vn))))))

(defn map-angle [angle-map ang]
  (int (+ ang 0.5)))

(defn get-first-angle [ang] 
  (map-angle first-angle-map ang))

(defn get-second-angle [ang] 
  (map-angle second-angle-map ang))

(defn get-base-angle [ang]
  (map-angle base-angle-map ang))

(defn normalize-angle [ang]
  (loop [a ang]
    (if (and (not (neg? a)) (< a (* 2 PI)))
      a
      (recur (+ a (if (neg? a) (* 2 PI) (* -2 PI)))))))

(defn calc-cos-beta [r z]
  (let [z2 (* z z)
        r2 (* r r)
        a (/ 
            (+ (- (* first-arm first-arm) (* second-arm second-arm)) z2 r2) 
            (* 2 first-arm))
        sqrt (Math/sqrt 
               (- 
                 (+ z2 r2) 
                 (* a a)))]
    (/ (- (* a r) (* z sqrt)) (+ z2 r2))))

(defn radian-to-degree [ang]
  (/ (* 180. ang) PI))

(defn degree-to-radian [ang]
  (/ (* PI ang) 180.))

(defn calc-alpha-beta [r z]
  (let [cos-beta (calc-cos-beta r z)
        sin-beta (Math/sqrt (- 1 (* cos-beta cos-beta)))
        cos-gamma (/ (- (* first-arm sin-beta) z) second-arm)]
    (list (- (/ PI 2.) (Math/acos cos-gamma)) (Math/acos cos-beta))))

(defn display-alpha-beta [alpha beta]
  (println (str "first-arm*cos(beta)=" (* first-arm (Math/cos beta))))
  (println (str "second-arm*cos(alpha)=" (* second-arm (Math/cos alpha))))
  (println (str "first-arm*sin(beta)=" (* first-arm (Math/sin beta))))
  (println (str "second-arm*sin(alpha)=" (* second-arm (Math/sin alpha)))))

(defn calc-r-theta [x y]
  (let [r (Math/sqrt (+ (* x x) (* y y)))]
    (list r (radian-to-degree (Math/asin (/ (- x) r))))))

;;; Code used to calculate the map between servo angle and arm angle
;(def exper-data {40 {60 [13.7 11.1]   65 [13.95 9.95]  70 [13.84 8.35]  75 [13.75 6.7]   80 [13.65 5.25]  85 [13.55 4.1]} 
;                 45 {60 [15.15 12.55] 65 [15.15 11.17] 70 [14.97 9.5]   75 [15.12 8.15]  80 [15.12 6.85]  85 [15.08 5.42]} 
;                 50 {60 [16.3 13.82]  65 [16.42 12.53] 70 [16.33 10.87] 75 [16.42 9.45]  80 [16.37 8.07]  85 [16.4 6.8]} 
;                 55 {60 [17.57 15.12] 65 [17.54 13.97] 70 [17.65 12.4]  75 [17.57 10.68] 80 [17.55 9.37]  85 [17.55 8.1]} 
;                 60 {60 [18.9 16.5]   65 [18.85 15.0]  70 [18.82 13.48] 75 [18.82 11.97] 80 [18.87 10.68] 85 [18.85 9.35]} 
;                 65 {60 [20.0 17.65]  65 [20.03 16.22] 70 [20.05 14.5]  75 [20.05 13.4]  80 [19.97 11.87] 85 [20.0 10.6]}})
;
;(defn calc-first-angle [data]
; (radian-to-degree (Math/asin (/ (- data base-high) first-arm))))
;
;(defn calc-second-angle [data]
; (radian-to-degree (Math/asin (/ data second-arm))))
;
;(defn calc-aver-for-first-angle [angle]
; (let [samples (exper-data angle)
;       leng    (count samples)]
;  (calc-first-angle (/ (reduce + (map (fn [[second-arm-ang [first-high second-high]]] first-high) samples)) leng))))
;
;(defn calc-aver-for-second-angle [angle]
; (let [samples (map 
;                (fn [[_ second-angle-hash]] 
;                 (- ((second-angle-hash angle) 0) ((second-angle-hash angle) 1))) 
;                exper-data)
;       leng    (count samples)]
;  (calc-second-angle (/ (reduce + samples) leng))))
;
;(println (reduce merge {} (map (fn [k] {k (calc-aver-for-first-angle k)}) (keys exper-data))))
;
;(println (reduce merge {} (map (fn [k] {k (calc-aver-for-second-angle k)}) (keys ((first exper-data) 1)))))
;
;;; Code used to calculate the map between real theta and servo theta
;(def exper-data1 [4.942 4.806 4.14 5.064 4.552])
;(def radius 25.602)
;(let [intervals (map #(* 2 (radian-to-degree (Math/asin (/ (/ % 2) radius)))) exper-data1)] 
;  (println intervals)
;  (println (map #(str %2 " " %1) (reduce #(conj %1 (+ (last %1) %2)) [(* -1 (+ (first intervals) (second intervals)))] intervals) (list -20 -10 0 10 20 30))))

(defn point-dist [pt1 pt2]
  (Math/sqrt (+ (* 
                  (- (first pt2) (first pt1)) 
                  (- (first pt2) (first pt1))) 
                (* 
                  (- (second pt2) (second pt1)) 
                  (- (second pt2) (second pt1))))))

(defn mix-pts [pt1 ratio1 pt2 ratio2]
  (list (+ 
          (* ratio1 (first pt1))
          (* ratio2 (first pt2)))
        (+
          (* ratio1 (second pt1))
          (* ratio2 (second pt2)))))

(defn preprocess-stroke [stroke]
  (defn append-point [stroke-arr new-tail]
    (let [dist (point-dist (last stroke-arr) new-tail)]
      (if (> dist 15)
        (let [iter-pts (rest (concat (range 0 1 (/ 15 dist)) '(1)))
              last-pt  (last stroke-arr)] 
          (reduce (fn [arr ptv] (conj arr (mix-pts last-pt (- 1 ptv) new-tail ptv))) stroke-arr iter-pts))
        (conj stroke-arr new-tail))))
  (loop [left-stroke (rest stroke)
         last-point  (first stroke)
         reslt-stroke [last-point]]
    (if (zero? (count left-stroke))
      reslt-stroke
      (recur (rest left-stroke) (first left-stroke) (append-point reslt-stroke (first left-stroke))))))

(defn fill-stroke-gap [strokes]
  (defn fill-gap [start-pt end-pt]
    (let [dist (point-dist start-pt end-pt)]
      (if (> dist 5)
        (reduce (fn [arr ptv] (conj arr (mix-pts start-pt (- 1 ptv) end-pt ptv))) [] (concat (range 0 1 (/ 5 dist)) '(1)))
        [start-pt end-pt])))
  (loop [left-strokes (rest strokes)
         last-stroke  (first strokes)
         reslt-strokes []]
    (if (zero? (count left-strokes))
      (conj reslt-strokes (fill-gap (last last-stroke) (first (first strokes))))
      (recur 
        (rest left-strokes) 
        (first left-strokes) 
        (conj reslt-strokes (fill-gap (last last-stroke) (first (first left-strokes))))))))

(defn process-stroke [stroke scale default-z]
  (map (fn [[x y]] (list (- (* 15. (/ x scale)) 7.5) (+ (* 15. (/ (- scale y) scale)) 7.5) default-z)) stroke))

(defn dump-strokes [strokes title]
  (println (str title " >>>"))
  (doseq [stroke strokes]
    (println (str "   " (doall stroke)))))

(defn calc-xyz [alpha beta theta]
  (let [alpha-rad (degree-to-radian alpha)
        beta-rad  (degree-to-radian beta)
        theta-rad (degree-to-radian theta)
        r (+ (* (Math/cos beta-rad) first-arm) (* (Math/cos alpha-rad) second-arm))
        y (+ (* (Math/sin beta-rad) first-arm) (* (Math/sin (- alpha-rad)) second-arm))]
    (list (* r (Math/cos theta-rad)) y (* r (Math/sin (- theta-rad))))))

(defn -main
  "main function"
  [& args]
  (with-open [fin (clojure.java.io/reader "resources/huan_strokes.txt")]
    (let [input (load-string (string/join "\n" (line-seq fin)))
          strokes (:strokes input)
          scale (:scale input)
          gaps (fill-stroke-gap strokes)
          strokes1 (map preprocess-stroke strokes)
          strokes-gaps (interleave (map (fn [x] (process-stroke x scale 0.0)) strokes1) 
                                   (map (fn [x] (process-stroke x scale 6.0)) gaps))
          strokes-z (reduce concat strokes-gaps)
          r-theta-list (map #(apply calc-r-theta %) (map (fn [x] (take 2 x)) strokes-z))
          alpha-beta-radian-list (map #(calc-alpha-beta (first %1) %2) r-theta-list (map (fn [x] (last x)) strokes-z))
          alpha-beta-degree-list (map (fn [[alpha beta]] (list (get-second-angle (radian-to-degree alpha)) (get-first-angle (radian-to-degree beta)))) alpha-beta-radian-list)
          reverse-xyz (map (fn [[alpha beta] [_ theta]] (calc-xyz alpha beta theta)) alpha-beta-degree-list r-theta-list)]
      (dump-strokes strokes "strokes")
      (dump-strokes gaps "gaps")
      (dump-strokes strokes1 "strokes after preprocess")
      (dump-strokes strokes-gaps "strokes gaps interleaved")
      (dump-strokes strokes-z "strokes-z")
      (dump-strokes alpha-beta-degree-list "alpha beta degree list")
      (dump-strokes r-theta-list "r theta list")
      (dump-strokes reverse-xyz "reverse-xyz")
      (with-open [fin (clojure.java.io/reader "resources/machine_arm.ino")
                  fout (clojure.java.io/writer "target/machine_arm.ino")]
        (doseq [line (line-seq fin)]
          (.write fout line)
          (.write fout "\n")
          (when (= line "// Insert Arrays Here")
            (.write fout 
                    (with-out-str 
                      (println (str "var alphas = [" (string/join "," (map #(str (first %)) alpha-beta-degree-list)) "];"))
                      (println (str "var betas = [" (string/join "," (map #(str (second %)) alpha-beta-degree-list)) "];"))
                      (println (str "var delta_thetas = [" (string/join "," (map #(str (get-base-angle (second %))) r-theta-list)) "];"))))
            ;(println (str "const int alphas[] PROGMEM = {" (string/join "," (map #(str (first %)) alpha-beta-degree-list)) "};"))
            ;(println (str "const int betas[] PROGMEM = {" (string/join "," (map #(str (second %)) alpha-beta-degree-list)) "};"))
            ;(println (str "const int delta_thetas[] PROGMEM = {" (string/join "," (map #(str (get-base-angle (second %))) r-theta-list)) "};"))))
            (.write fout "\n")))))))

;(defn -main1
;  "main function"
;  [& args]
;  (let [x-list (range -7 7 0.5)
;        y-list (range 10 24 0.5)
;        upper-arm (map #(list %1 %2) x-list (repeat 24))
;        right-arm (map #(list %1 %2) (repeat 7) (reverse y-list))
;        lower-arm (map #(list %1 %2) (reverse x-list) (repeat 10))
;        left-arm (map #(list %1 %2) (repeat -7) y-list)
;        square (concat upper-arm right-arm lower-arm left-arm)
;        r-theta-list (map #(apply calc-r-theta %) square)
;        alpha-beta-radian-list (map #(calc-alpha-beta (first %) 0) r-theta-list)
;        alpha-beta-degree-list (map (fn [[alpha beta]] (list (get-second-angle (radian-to-degree alpha)) (get-first-angle (radian-to-degree beta)))) alpha-beta-radian-list)]
;    (with-open [fin (clojure.java.io/reader "resources/machine_arm.ino")
;                fout (clojure.java.io/writer "target/machine_arm.ino")]
;      (doseq [line (line-seq fin)]
;        (.write fout line)
;        (.write fout "\n")
;        (when (= line "// Insert Arrays Here")
;          (.write fout 
;                  (with-out-str 
;                    (println (str "const int betas[] PROGMEM = {" (string/join "," (map #(str (first %)) alpha-beta-degree-list)) "};"))
;                    (println (str "const int alphas[] PROGMEM = {" (string/join "," (map #(str (second %)) alpha-beta-degree-list)) "};"))
;                    (println (str "const int delta_thetas[] PROGMEM = {" (string/join "," (map #(str (get-base-angle (second %))) r-theta-list)) "};"))))
;          (.write fout "\n"))))))
