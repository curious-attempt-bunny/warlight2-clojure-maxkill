(ns brain)
(require 'clojure.set)

; see rollouts/attack_map
; 2x1 -- (-1) --> 1
; 3x2 -- (-1) --> 2
; 5x3 -- (-2) --> 3
; 7x4 -- (-3) --> 4
; 9x5 -- (-4) --> 5
; 11x6 -- (-4) --> 7
; 13x7 -- (-5) --> 8
; 15x8 -- (-6) --> 9
; 17x9 -- (-7) --> 10
; 19x10 -- (-7) --> 12
; 21x11 -- (-8) --> 13
; 23x12 -- (-9) --> 14
; 25x13 -- (-10) --> 15
; 27x14 -- (-10) --> 17
; 29x15 -- (-11) --> 18
; 31x16 -- (-12) --> 19
; 33x17 -- (-13) --> 20
; 35x18 -- (-13) --> 22
; 37x19 -- (-14) --> 23
(defn armies_to_kill
    [defending_armies]
    (nth (concat [0 2 3] (iterate (partial + 2) 5)) defending_armies))

(defn armies_to_defend
    [attacking_armies]
    (nth (concat [0 0 2] (interleave (drop 3 (range)) (drop 3 (range)))) attacking_armies))

(defn attackers_killed
    [defending_armies]
    (java.lang.Math/round (+
        (* (* 0.7 defending_armies) (- 1 0.16))
        (* defending_armies 0.16))))

(defn regions
    [state]
        (vals (:regions state)))

(defn ours?
    [region]
    (= :us (:owner region)))

(defn enemy?
    [region]
    (= :them (:owner region)))

(defn neutral?
    [region]
    (= :neutral (:owner region)))

(defn neighbours
    [state region]
    (map (fn [region_id] (get-in state [:regions region_id])) (:neighbours region)))

(defn border?
    [state region]
    (and
        (ours? region)
        (some (comp ours?) (neighbours state region))))

(defn super_region
    [state region]
    (get-in state [:super_regions (:super_region_id region)]))

;; ----- picking regions

(defn pick_starting_region
    [state ids]
    (rand-nth ids))

;; ----- placement and attacking

(defn placement_to_defend
    [[state placements] region]
    (let [neighbours          (neighbours state region)
          enemy               (filter enemy? neighbours)
          enemy-armies        (map :armies enemy)
          attacking-armies    (apply max (conj enemy-armies 0))
          our-armies          (:armies region)
          minimum-defence     (armies_to_defend attacking-armies)
          armies_to_place     (min
                                (:starting_armies state)
                                (max 0 (- minimum-defence our-armies)))
          placement           {:region region :armies armies_to_place}]
        ; (bot/log (str "Place " armies_to_place " at region " (:id region)))
        ; (bot/log (str "  enemy neighbours " (pr-str (map :id enemy))))
        ; (bot/log minimum-defence)
        (if (or (empty? enemy) (zero? armies_to_place))
            [state placements]
            (let [state (update-in state [:starting_armies] #(- % armies_to_place))]
                (bot/log (str "Placing " armies_to_place " at region " (:id region) " to defend against " attacking-armies " armies from one of " (pr-str (map :id enemy))))
                [state (conj placements placement)]))))

(defn place_armies
    [state]
    (let [border-regions     (filter (partial border? state) (regions state))
          [state placements] (reduce placement_to_defend [state []] border-regions)
          final_region       (or (:from (first placements)) (first border-regions) (first (filter ours? (regions state))))
          final_placement    {:region final_region :armies (:starting_armies state)}]

        (if (zero? (:starting_armies state))
            placements
            (conj placements final_placement))))

(defn random_movement
    [state region]
    (let [neighbours  (neighbours state region)
          destination (rand-nth neighbours)
          armies      (dec (:armies region))
          movement    {:from region :to destination :armies armies}]
        movement))

(defn attack
    [state] 
    (->> (regions state)
        (filter ours?)
        (filter (fn [region] (> (:armies region) 1)))
        (map (partial random_movement state))))
