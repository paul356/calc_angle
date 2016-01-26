(ns handwrite_strokes.core
  (:require [goog.dom :as dom]
            [goog.graphics :as graphics]
            [clojure.string :as string]
            [ajax.core :refer [GET POST]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce mouse-down (atom false))
(defonce strokes (atom []))
(defonce curr-stroke (atom "["))

(defn handle-mouseup [evt]
  (swap! strokes (fn [arr] (conj arr (str @curr-stroke "]"))))
  (swap! curr-stroke (fn [_] "["))
  (swap! mouse-down (fn [_] false)))

(defn handle-mousedown [evt]
  (swap! mouse-down (fn [_] true)))

(defn handle-mousemove [evt]
  (when @mouse-down 
    (swap! curr-stroke (fn [curr-str] (str curr-str (str "'(" (.-offsetX evt) " " (.-offsetY evt) ") "))))))

(defn format-strokes []
  (str "{" :scale " " (.-width (.getElementById js/document "canvas")) " " :strokes " [" (string/join " " @strokes) "]}"))

(defn call-set-strokes []
  (POST "/write-character" {:format :url :params {:strokes (format-strokes)}}))

(defn start []
  (let [canvas2d (.getElementById js/document "canvas")
        context2d (.getContext canvas2d "2d")
        image (.getElementById js/document "background")
        button (.getElementById js/document "go-btn")]
    (.drawImage context2d image 0 0)
    (set! (.-onmousemove canvas2d) handle-mousemove)
    (set! (.-onmousedown canvas2d) handle-mousedown)
    (set! (.-onmouseup canvas2d) handle-mouseup)
    (set! (.-onclick button) call-set-strokes)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(set! (.-onload js/window) start)
