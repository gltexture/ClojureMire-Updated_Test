(ns mire.rooms)

(def rooms (ref {}))

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :desc_qc (:desc_qc room)
            :exits (ref (:exits room))
            :items (ref (or (:items room) #{}))
            :needs (ref (or (:needs room) #{}))
            :quest (ref (or (:quest room) nil))
            :inhabitants (ref #{})}}))
)

(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms dir]
  (dosync
   (reduce load-room rooms
           (.listFiles (java.io.File. dir)))))

(defn add-rooms
  "Look through all the files in a dir for files describing rooms and add
  them to the mire.rooms/rooms map."
  [dir]
  (dosync
   (alter rooms load-rooms dir)))

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing))
)

(defn remove-needs [room thing]
  (alter (:needs room) disj (keyword thing))
)

(defn has-a-quest [room]
  (not (nil? @(:quest room)))
)

(defn room-contains-to-pass?
  [room thing]
  (@(:needs room) (keyword thing))
)

(defn room-needs-to-pass [room]
  (if-let [needs-ref (:needs room)]
    (let [needs @needs-ref]
      (when (not-empty needs) needs))
    nil)
)