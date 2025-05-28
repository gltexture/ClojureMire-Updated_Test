(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))
(require '[clojure.set :as set])

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- remove-from-refs
  "Remove an object from ref."
  [obj from]
  (alter from disj obj)
)

(defn- add-in-refs
  "Remove an object from ref."
  [obj to]
  (alter to conj obj)
)

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents, all in one sentence."
  []
  (let [room @player/*current-room*
        desc (if (and (rooms/has-a-quest room) (@(:quest room) :completed?))
               (:desc_qc room)
               (:desc room))
        exits (keys @(:exits room))
        items @(:items room)
        quest-str (when (rooms/has-a-quest room)
                        (let [quest @(:quest room)]
                          (str "Quest: you need " (str/join ", " (:required-items quest)) ". You'll get " (:result quest) ".")))]
    (str desc " Exits: " (str/join ", " exits) ". "
     (when (seq items)
           (str (str/join " " (map #(str "There is " % " here") items)) ". "))
     (or quest-str "")
     )
    )
)


(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (when (= ((:exits @player/*current-room*) (keyword direction)) :FINAL)
     (do
       (println "Congratulations! You completed the game!")
       (System/exit 0)
     )
   )
   (let [target-name ((:exits @player/*current-room*) (keyword direction)) target (@rooms/rooms target-name)]
     (let [items-to-pass (rooms/room-needs-to-pass target)]
       (if (not-empty items-to-pass)
         (str "You can't go that way! You need: "
              (apply str (interpose ", " (seq items-to-pass)))
         "!")
         (do
           (if target
             (do
               (move-between-refs player/*name*
                                  (:inhabitants @player/*current-room*)
                                  (:inhabitants target))
               (ref-set player/*current-room* target)
               (look))
             "You can't go that way.")
         )
       )
     )
    ))
)

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn using-it
  "Use an item. Optionally, provide direction."
  ([thing] (using-it thing nil))
  ([thing direction]
   (dosync
    (if (player/carrying? thing)
      (if direction
        (let [target-name ((:exits @player/*current-room*) (keyword direction)) target (@rooms/rooms target-name)]
          (if (rooms/room-contains-to-pass? target thing)
            (do
              (rooms/remove-needs target thing)
              (remove-from-refs (keyword thing) player/*inventory*)
              (str "You used an item: " thing))
            (str "You can't use it there: " thing)
          )
        )
        (if (rooms/room-contains-to-pass? @player/*current-room* thing)
          (do
            (rooms/remove-needs @player/*current-room* thing)
            (remove-from-refs (keyword thing) player/*inventory*)
            (str "You used an item: " thing))
          (str "You can't use it there: " thing)
        )
      )
      (str "You're not carrying a " thing "."))
    )
   )
)

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn quest []
  (let [room @player/*current-room*]
    (if (rooms/has-a-quest room)
      (let [quest @(:quest room)]
        (if (set/subset? (:required-items quest) @player/*inventory*)
          (dosync
           (doseq [x (:required-items quest)] (remove-from-refs x player/*inventory*))
           (add-in-refs (:result quest) player/*inventory*)
           (ref-set (:quest room) (assoc quest :completed? true))
           (str "You completed the quest! You got: " (:result quest)))
          "You can't complete the quest yet!")
        )
      "There is no quest here."
      )
    )
)


;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "look" look
               "say" say
               "quest" quest
               "use" using-it
               "help" help})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
