(ns clojure.gdx.space-invaders
  (:require [clojure.gdx :as gdx :refer [internal-file
                                         sprite-batch
                                         dispose
                                         delta-time
                                         clear-screen
                                         black
                                         begin
                                         end]]
            [clojure.gdx.lwjgl :refer [Application
                                       start]])
  (:import (com.badlogic.gdx Gdx Input$Keys)
           (com.badlogic.gdx.graphics OrthographicCamera)
           (com.badlogic.gdx.graphics Texture)
           (com.badlogic.gdx.graphics.g2d SpriteBatch)
           (com.badlogic.gdx.utils ScreenUtils)))

(def screen-width 800)
(def screen-height 600)

(defn texture [c path]
  (Texture. (internal-file c path)))

(defn make-player [c]
  {:x (/ screen-width 2)
   :y 50
   :width 64
   :height 64
   :speed 300
   :texture (texture c "ship/1.png")
   :bullets []})

(defn make-alien [c x y]
  {:x x
   :y y
   :width 48
   :height 48
   :texture (texture c "alien/1.png")})

(defn game-context [c]
  {:player (make-player c)
   :aliens (for [x (range 100 700 80)
                 y (range 400 500 60)]
             (make-alien c x y))
   :bullets []
   :batch (sprite-batch)
   :camera (OrthographicCamera. screen-width screen-height)}) ; 2x  camera!

(defn update-player [player delta c]
  (let [speed (:speed player)
        x (:x player)
        move-left (if (key-pressed? c :a)
                    (- x (* delta speed))
                    x)
        move-right (if (key-pressed? c :d)
                     (+ x (* delta speed))
                     move-left)
        new-x (max 0 (min move-right
                          (- screen-width (:width player))))]
    (assoc player :x new-x)))

(defn update-bullets [bullets delta]
  (->> bullets
       (map (fn [bullet]
              (update bullet :y #(+ % (* 500 delta)))))
       (filter #(> (:y %) 0))))

(defn check-collision [alien bullet]
  (and (< (:x bullet) (+ (:x alien) (:width alien)))
       (> (+ (:x bullet) 8) (:x alien))
       (< (:y bullet) (+ (:y alien) (:height alien)))
       (> (+ (:y bullet) 16) (:y alien))))

(defn update-aliens [aliens bullets]
  (reduce
   (fn [acc alien]
     (if (some #(check-collision alien %) bullets)
       acc
       (conj acc alien)))
   []
   aliens))

(defn handle-input [c]
  (if (key-just-pressed? c :space)
    (let [player (:player c)
          bullet {:x (+ (:x player) 28)
                  :y (+ (:y player) 64)}]
      (update c :bullets conj bullet))
    c))

(defn render-entity [batch entity]
  (.draw batch
         (:texture entity)
         (float (:x entity))
         (float (:y entity))))

(defn render-game [c]
  (clear-screen black)
  (let [batch (:batch c)
        player (:player c)
        aliens (:aliens c)
        bullets (:bullets c)]
    (begin batch)
    (render-entity batch player)
    (doseq [alien aliens]
      (render-entity batch alien))
    (doseq [bullet bullets]
      (render-entity batch
                     {:texture (texture c "bullet.png")
                      :x (:x bullet)
                      :y (:y bullet)}))
    (end batch)))

(defn update-game [c delta]
  (-> c
      handle-input
      (update :player update-player delta c)
      (update :bullets update-bullets delta)
      (update :aliens #(update-aliens % (:bullets c)))))

; this is only for inspection, otherwise we could pass through Application also
; and let-bound it there ?
(def state (atom nil))

(defn -main []
  (let [screen-width 800
        screen-height 600]
    (start {:title "Space Invaders"
            :width screen-width ; TODO can I see this in ctx ?
            :height screen-height
            :fps 60} ; default 60 ?
           (reify Application
             (create [_]
               (let [c (gdx/context)] ; pass through create ?
                 (reset! state (merge c (game-context c))))
               (swap! state assoc :camera (OrthographicCamera. screen-width screen-height))) ; duplicated camera

             (dispose [_]
               (let [c @state]
                 ; global textures !
                 (doseq [texture (concat [(-> c :player :texture)]
                                         (map :texture (:aliens c)))]
                   (dispose texture))))

             (render [_]
               (let [c @state]
                 (swap! state update-game c (delta-time c))
                 (render-game c)))

             (resize [_ width height]
               (.setToOrtho (:camera @state) false))))))
