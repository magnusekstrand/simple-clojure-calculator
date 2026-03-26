(ns oakbeach.core
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [clojure.string :as str]
            [oakbeach.calculator :as calc]))

;; --- 1. State Management ---
;; A single source of truth for the entire UI
(defonce app-state (r/atom {:input ""
                            :result "0"
                            :error? false}))

;; --- 2. Logic Wrappers ---
(defn- update-input! [char]
       (swap! app-state update :input str char))

(defn- clear-display! []
       (reset! app-state {:input "" :result "0" :error? false}))

(defn- run-calculation! []
       (let [current-input (:input @app-state)
             output (calc/handle-input current-input)]
            (if (str/starts-with? output "Error")
              (swap! app-state assoc :error? true :result output)
              (swap! app-state assoc :error? false :result output :input output))))

;; --- 3. UI Components ---

(defn- display-screen []
       (let [{:keys [input result error?]} @app-state]
            [:div {:class "w-full bg-black/40 rounded-lg p-6 mb-6 text-right font-mono shadow-inner"}
             [:div {:class "text-slate-400 text-sm h-6 overflow-hidden"} input]
             [:div {:class (str "text-3xl font-bold truncate "
                                (if error? "text-red-400" "text-emerald-400"))}
              result]]))

(defn- calc-button [label action color-class]
       [:button
        {:class (str "transition-all duration-150 active:scale-90 hover:brightness-110 "
                     "shadow-lg p-5 rounded-2xl font-bold text-xl text-white "
                     color-class)
         :on-click action}
        label])

(defn- keypad []
       (let [btns [["AC" clear-display! "bg-slate-600"] ["(" #(update-input! "(") "bg-slate-600"] [")" #(update-input! ")") "bg-slate-600"] ["/" #(update-input! "/") "bg-orange-500"]
                   ["7" #(update-input! "7") "bg-slate-700"] ["8" #(update-input! "8") "bg-slate-700"] ["9" #(update-input! "9") "bg-slate-700"] ["*" #(update-input! "*") "bg-orange-500"]
                   ["4" #(update-input! "4") "bg-slate-700"] ["5" #(update-input! "5") "bg-slate-700"] ["6" #(update-input! "6") "bg-slate-700"] ["-" #(update-input! "-") "bg-orange-500"]
                   ["1" #(update-input! "1") "bg-slate-700"] ["2" #(update-input! "2") "bg-slate-700"] ["3" #(update-input! "3") "bg-slate-700"] ["+" #(update-input! "+") "bg-orange-500"]
                   ["0" #(update-input! "0") "bg-slate-700"] ["." #(update-input! ".") "bg-slate-700"] ["^" #(update-input! "^") "bg-slate-700"] ["=" run-calculation! "bg-orange-600"]]]
            [:div {:class "grid grid-cols-4 gap-4"}
             (for [[label action col] btns]
                  ^{:key label} [calc-button label action col])]))

;; --- 4. Main View ---

(defn root-view []
      [:main {:class "min-h-screen bg-slate-900 flex items-center justify-center p-4"}
       ;; The Glass Card
       [:div {:class "relative z-10 bg-white/10 backdrop-blur-xl border border-white/20 rounded-3xl p-8 shadow-2xl w-full max-w-md"}
        [:h1 {:class "text-white/30 text-xs font-bold uppercase tracking-widest mb-4 text-center"}
         "Functional Clojure Calc"]
        [display-screen]
        [keypad]]])

;; --- 5. App Initialization ---
(defonce root (atom nil))

(defn ^:export init []
      (let [container (.getElementById js/document "app")]
           (if-not @root
                   ;; Första gången: Skapa en ny root
                   (let [new-root (rdom/create-root container)]
                        (reset! root new-root)
                        (rdom/render new-root [root-view]))
                   ;; Vid Hot Reload: Rendera bara om i befintlig root
                   (rdom/render @root [root-view]))))