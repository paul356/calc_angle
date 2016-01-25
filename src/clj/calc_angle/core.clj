(ns calc-angle.core
  (:require [clojure.string :as string]
            [clojure.core.reducers :as r]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [redirect]]
            [compojure.core :refer [GET defroutes]]
            [compojure.handler :refer [site]]
            [compojure.route :refer [resources not-found]]
            [clojure.java.io :as io])
  (:import  [jssc SerialPort SerialPortList])
  (:gen-class))

(declare radian-to-degree)

(def PI Math/PI)

(def base-high 10.3)

(def first-arm 14.)

(def second-arm 15.2)

(defn to-int [val]
  (int (+ 0.5 val)))

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

;;
;; beta is the angle of first-arm relative to x positive
;; alpha is the angle of x positive relative to second-arm
;;
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

;;
;; r is the distance to origin in the x-y plane
;; theta is the angle of the projection in x-y plane relative to y positive
;;
(defn calc-r-theta [x y]
  (let [r (Math/sqrt (+ (* x x) (* y y)))]
    (list r (radian-to-degree (Math/asin (/ (- x) r))))))

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

(defn fill-intra-stroke-gap [stroke]
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

(defn fill-inter-stroke-gap [strokes]
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

(def ^:dynamic *serial-conn*)

(defn set-angle [base-angle first-angle second-angle]
  (println (str "--> " "a=" base-angle ",b=" first-angle ",c=" second-angle ","))
  (.writeString *serial-conn* (str "a=" base-angle ",b=" first-angle ",c=" second-angle ","))
  (.readBytes *serial-conn* 1)
  (loop [reduce-str "<-- "
         curr-char (aget (.readBytes *serial-conn* 1) 0)]
    (if (= curr-char (byte \n))
      (println reduce-str)
      (recur (str reduce-str (char curr-char)) (aget (.readBytes *serial-conn* 1) 0)))))

(defn calc-angle-seq [input-str]
  (let [input (load-string input-str)
        strokes (:strokes input)
        scale (:scale input)
        gaps (fill-inter-stroke-gap strokes)
        dense-strokes (map fill-intra-stroke-gap strokes)
        strokes-plus-gaps (interleave (map (fn [x] (process-stroke x scale 0.0)) dense-strokes) 
                                      (map (fn [x] (process-stroke x scale 2.0)) gaps))
        strokes-z (reduce concat strokes-plus-gaps)
        r-theta-list (map #(apply calc-r-theta %) (map (fn [x] (take 2 x)) strokes-z))
        alpha-beta-radian-list (map #(calc-alpha-beta (first %1) %2) r-theta-list (map (fn [x] (last x)) strokes-z))
        alpha-beta-degree-list (map (fn [[alpha beta]] (list (radian-to-degree alpha) (radian-to-degree beta))) alpha-beta-radian-list)]
    (map #(fn [[_ theta] [alpha beta]] (list theta beta (- 180 alpha beta))) r-theta-list alpha-beta-degree-list)))

(def character-str "{:scale 480 :strokes [['(47 89) '(174 88) '(174 100) '(172 111) '(171 120) '(169 130) '(167 138) '(166 146) '(164, 154) '(161 164) '(158 176) '(154 189) '(149 202) '(145 213) '(141 223) '(135 236) '(119 267) '(113 275) '(106 286) '(100 294) '(94 304) '(87 314) '(81 321) '(75 329) '(68 338) '(62 344) '(56 350) '(50 357) '(43 363) '(26 364) '(15 365)] ['(62 172) '(70 182) '(76 189) '(82 196) '(86 200) '(91 206) '(95 211) '(100 216) '(104 222) '(108 228) '(112 234) '(117 238) '(136 266) '(142 273) '(148 282) '(154 292) '(159 299) '(163 306) '(166 310) '(170 317) '(174 323) '(179 324) '(184 324) '(189 326) '(194 326)] ['(270 43) '(267 52) '(264 62) '(262 70) '(260 78) '(258 87) '(257 96) '(254 106) '(250 115) '(248 122) '(246 129) '(243 138) '(240 146) '(238 154) '(234 162) '(231 169) '(227 178) '(224 185) '(220 193) '(216 201) '(212 209)] ['(257 108) '(416 108) '(412 114) '(411 122) '(410 130) '(406 140) '(403 148) '(400 157) '(397 166) '(394 175) '(393 178) '(387 182) '(380 184)] ['(311 186) '(310, 247) '(309 258) '(305 266) '(302 274) '(299 283) '(296 292) '(293 302) '(290 309) '(286 318) '(282 326) '(277 334) '(272 342) '(267 350) '(262 356) '(257 362) '(252 368) '(247 374) '(240 381) '(232 390) '(223 396) '(215 402) '(206 408) '(200 412) '(196 416) '(191 418) '(186 420)] ['(310 250) '(312 260) '(324 268) '(326 276) '(329 286) '(332 296) '(335 302) '(338 310) '(342 318) '(347 327) '(352 334) '(356 341) '(362 351) '(368 360) '(374 366) '(381 374) '(387 381) '(394 388) '(401 396) '(409 402) '(416 408) '(422 414) '(438 414) '(448 414)]]}")

(def curr-angle (atom {:a 0 :b 90 :c 180}))

(defn write-strokes [angles-seq]
  (doseq [[base-angle first-angle second-angle] angles-seq]
    (set-angle base-angle first-angle second-angle)
    (swap! curr-angle (fn [_] {:a base-angle :b first-angle :c second-angle})))
  "OK")

(defroutes app
           (GET "/" [] (redirect "index.html"))
           (GET "/set-angle" [a b c] (if (and a b c)
                                       (set-angle a b c)
                                       "/set-angle?a=<base>&b=<large>&c=<small>"))
           (POST "/write-character" [strokes] (write-strokes (calc-angle-seq strokes)))
           (resources "/")
           (not-found "<h1>not found</h1>"))

(def entry (site app))

(defn init
  "one time initialization work"
  []
  (let [ports (. SerialPortList getPortNames)
        ;myport "/dev/tty.usbmodem1421"]
        myport "/dev/tty.usbserial-A402XH6Q"]
    (if (and (pos? (alength ports)) (contains? (set ports) myport))
      (alter-var-root #'*serial-conn* 
                      (fn [_] 
                        (doto (SerialPort. myport)
                          (.openPort)
                          (.setParams 9600 8 1 0)
                          ((fn [_] (Thread/sleep 1000))))))
      (println "There is no serial connection!"))))

(defn destroy 
  "release acquired resources before service stops"
  []
  (.closePort *serial-conn*))

(defn -main1
  "main function"
  [& args]
  (let [x-list (range -7 7 0.5)
        y-list (range 10 24 0.5)
        upper-arm (map #(list %1 %2) x-list (repeat 24))
        right-arm (map #(list %1 %2) (repeat 7) (reverse y-list))
        lower-arm (map #(list %1 %2) (reverse x-list) (repeat 10))
        left-arm (map #(list %1 %2) (repeat -7) y-list)
        square (concat upper-arm right-arm lower-arm left-arm)
        r-theta-list (map #(apply calc-r-theta %) square)
        alpha-beta-radian-list (map #(calc-alpha-beta (first %) 0) r-theta-list)
        alpha-beta-degree-list (map (fn [[alpha beta]] (list (radian-to-degree alpha) (radian-to-degree beta))) alpha-beta-radian-list)]
    (with-open [fin (clojure.java.io/reader "resources/machine_arm.ino")
                fout (clojure.java.io/writer "target/machine_arm.ino")]
      (doseq [line (line-seq fin)]
        (.write fout line)
        (.write fout "\n")
        (when (= line "// Insert Arrays Here")
          (.write fout 
                  (with-out-str
                    (println (str "const int16_t alphas[] PROGMEM = {" (string/join ", " (map #(str (first %)) alpha-beta-degree-list)) "};"))
                    (println (str "const int16_t betas[] PROGMEM = {" (string/join ", " (map #(str (second %)) alpha-beta-degree-list)) "};"))
                    (println (str "const int16_t delta_thetas[] PROGMEM = {" (string/join ", " (map #(str (second %)) r-theta-list)) "};"))))
          (.write fout "\n"))))))
