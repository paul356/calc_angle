(ns handwrite_strokes.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.graphics :as graphics]
            [clojure.string :as string]
            [ajax.core :refer [GET POST]]
            [cljs.core.async :refer [<! chan put! timeout alts!]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce mouse-down (atom false))
(defonce strokes (atom []))
(defonce curr-stroke (atom []))
(defonce run-fixed-action (atom false))

(defn handle-mouseup [evt]
  (swap! strokes (fn [arr] (conj arr @curr-stroke)))
  (swap! curr-stroke (fn [_] []))
  (swap! mouse-down (fn [_] false)))

(defn handle-mousedown [evt]
  (swap! mouse-down (fn [_] true)))

(defn handle-mousemove [context2d text-output]
  (fn [evt]
    (when @mouse-down
      (when (and (> (count @curr-stroke) 0) 
                 (not= (last @curr-stroke) [(.-offsetX evt) (.-offsetY evt)]))
        (.beginPath context2d)
        (.moveTo context2d (first (last @curr-stroke)) (second (last @curr-stroke)))
        (.lineTo context2d (.-offsetX evt) (.-offsetY evt))
        (.stroke context2d))
      (when (not= (last @curr-stroke) [(.-offsetX evt) (.-offsetY evt)])
        (swap! curr-stroke (fn [arr] (conj arr [(.-offsetX evt) (.-offsetY evt)])))))
    (set! (.-innerText text-output) (str "(" (.-offsetX evt) "," (.-offsetY evt) ")"))))

(defn format-strokes [stroke-lst]
  (defn kick-repeat-off [stroke]
    (loop [new-stroke '()
           curr-stroke stroke
           last-pt '()]
      (if (== (count curr-stroke) 0)
        new-stroke
        (if (= (first curr-stroke) last-pt)
          (recur new-stroke (rest curr-stroke) last-pt)
          (recur (concat new-stroke (list (first curr-stroke))) (rest curr-stroke) (first curr-stroke))))))
  (defn format-stroke [arr] (str "(list " 
                                 (string/join " " (map (fn [[x y]] (str "'(" x " " y ")")) (kick-repeat-off arr)))
                                 ")"))
  (str "{" 
       :xscale " " (.-width (.getElementById js/document "canvas")) " " 
       :yscale " " (.-height (.getElementById js/document "canvas")) " "
       :strokes " (list " (string/join " " (map format-stroke stroke-lst)) ")}"))

(defn call-set-strokes [stroke-lst]
  (when (> (count stroke-lst) 0)
    (if (> (count (first stroke-lst)) 0)
      (POST "/write-character" {:format :url :params {:strokes (format-strokes (list (first stroke-lst)))} :handler (fn [reponse] (when (> (count stroke-lst) 1) (call-set-strokes (rest stroke-lst))))})
      (call-set-strokes (rest stroke-lst)))))

(defn clear-strokes [context2d image]
  (fn [_]
    (swap! strokes (fn [_] []))
    (swap! curr-stroke (fn [_] []))
    (.drawImage context2d image 0 0)))

(defn reset-angles []
  (GET "/reset-angles"))

(defn calib-action []
  (GET "/calib-action"))

(defn fixed-action [fixed-btn fixed-chan]
  (fn []
    (swap! run-fixed-action (fn [old-v] (not old-v)))
    (if @run-fixed-action
      (do 
        (set! (.-innerText fixed-btn) "固定动作 - 开") 
        (put! fixed-chan true))
      (set! (.-innerText fixed-btn) "固定动作 - 关"))))

(defn fixed-action-loop [fixed-chan]
  (go
    (while true
      (let [[_ _] (alts! [fixed-chan (timeout 120000)])]
        (when @run-fixed-action
          (GET "/fixed-action"))))))

(defn start []
  (let [canvas2d (.getElementById js/document "canvas")
        context2d (.getContext canvas2d "2d")
        image (.getElementById js/document "background")
        go-btn (.getElementById js/document "go-btn")
        clear-btn (.getElementById js/document "clear-btn")
        reset-btn (.getElementById js/document "reset-btn")
        calib-btn (.getElementById js/document "calib-btn")
        fixed-btn (.getElementById js/document "fixed-btn")
        info-bar (.getElementById js/document "info-bar")
        fixed-chan (chan)]
    (set! (.-strokeStyle context2d) "red")
    (.drawImage context2d image 0 0)
    (set! (.-onmousemove canvas2d) (handle-mousemove context2d info-bar))
    (set! (.-onmousedown canvas2d) handle-mousedown)
    (set! (.-onmouseup canvas2d) handle-mouseup)
    (set! (.-onclick go-btn) (fn [_] (call-set-strokes @strokes)))
    (set! (.-onclick clear-btn) (clear-strokes context2d image))
    (set! (.-onclick reset-btn) reset-angles)
    (set! (.-onclick calib-btn) calib-action)
    (set! (.-innerText fixed-btn) "固定动作 - 关")
    (set! (.-onclick fixed-btn) (fixed-action fixed-btn fixed-chan))
    (fixed-action-loop fixed-chan)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(set! (.-onload js/window) start)
