(ns clojure.gdx.space-invaders
  (:require [clojure.gdx :as gdx]
            [clojure.gdx.lwjgl :refer [Application start]])
  (:import (com.badlogic.gdx Gdx Input$Keys)
           (com.badlogic.gdx.graphics OrthographicCamera)
           (com.badlogic.gdx.graphics Texture)
           (com.badlogic.gdx.graphics.g2d SpriteBatch)
           (com.badlogic.gdx.utils ScreenUtils)))

(def screen-width 800)
(def screen-height 600)

(defn texture [c path]
  (Texture. (gdx/internal-file c path)))

(defn make-player [context]
  {:x (/ screen-width 2)
   :y 50
   :width 64
   :height 64
   :speed 300
   :texture (texture context "ship/1.png")
   :bullets []})

(defn make-alien [context x y]
  {:x x
   :y y
   :width 48
   :height 48
   :texture (texture context "alien/1.png")})

(defn make-game-state [gdx-state]
  (merge gdx-state
         {:player (make-player gdx-state)
          :aliens (for [x (range 100 700 80)
                        y (range 400 500 60)]
                    (make-alien gdx-state x y))
          :bullets []
          :batch (SpriteBatch.)
          :camera (OrthographicCamera. screen-width screen-height)}))

(defn update-player [player delta]
  (let [speed (:speed player)
        x (:x player)
        move-left (if (.isKeyPressed Gdx/input Input$Keys/A) (- x (* delta speed)) x)
        move-right (if (.isKeyPressed Gdx/input Input$Keys/D) (+ x (* delta speed)) move-left)
        new-x (max 0 (min move-right (- screen-width (:width player))))]
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

(defn handle-input [game-state]
  (if (.isKeyJustPressed Gdx/input Input$Keys/SPACE)
    (let [player (:player game-state)
          bullet {:x (+ (:x player) 28)
                  :y (+ (:y player) 64)}]
      (update game-state :bullets conj bullet))
    game-state))

(defn render-entity [batch entity]
  (.draw batch
         (:texture entity)
         (float (:x entity))
         (float (:y entity))))

(defn render-game [game-state]
  (ScreenUtils/clear 0 0 0 1)
  (let [batch (:batch game-state)
        player (:player game-state)
        aliens (:aliens game-state)
        bullets (:bullets game-state)]
    (.begin batch)
    (render-entity batch player)
    (doseq [alien aliens]
      (render-entity batch alien))
    (doseq [bullet bullets]
      (render-entity batch
                     {:texture (texture game-state "bullet.png")
                      :x (:x bullet)
                      :y (:y bullet)}))
    (.end batch)))

(defn update-game [game-state delta]
  (-> game-state
      handle-input
      (update :player update-player delta)
      (update :bullets update-bullets delta)
      (update :aliens #(update-aliens % (:bullets game-state)))))

(def state (atom nil))

(defn -main []
  (start {:title "Space Invaders"
          :width screen-width
          :height screen-height
          :fps 60}
         (reify Application
           (create [_]
             (reset! state (make-game-state (gdx/context)))
             (swap! state assoc :camera (OrthographicCamera. screen-width screen-height)))

           (dispose [_]
             (doseq [texture (concat [(-> @state :player :texture)]
                                     (map :texture (:aliens @state)))]
               (.dispose texture)))

           (render [_]
             (let [delta (.getDeltaTime Gdx/graphics)]
               (swap! state update-game delta)
               (render-game @state)))

           (resize [_ width height]
             (.setToOrtho (:camera @state) false)))))
