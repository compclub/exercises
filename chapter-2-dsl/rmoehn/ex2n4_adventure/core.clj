;;; All objects have a state. All objects are interpreters. They have a
;;; transition function.
;;;
;;; handle : (State, Information) -> (State, Information)
;;;
;;; Commands by the player are handled as observations to the avatar he
;;; controls.
;;;
;;; Information propagates to other objects in the vicinity. How far it
;;; propagates depends on the type of information. Does this mean that
;;; information itself must have a state? That would be one way to do it. If it
;;; has signal strength. Well, it certainly has properties.
;;;
;;; The problem is that then it reverberates.
;;;
;;; The other way to do it is that rooms handle information and part of
;;; handling
;;; it is passing it on to the next rooms with reduced strength.
;;;
;;; The resulting information can be nil, in which case there is nothing to be
;;; propagated.
;;;
;;; Processing continues until there is no information to be propagated. Then
;;; the system requires external input in order to start processing again.
;;;
;;; I'm not sure how it will all work.
;;;
;;; We have one important concept, information propagation. How is this done?
;;; Room state must contain the objects in it and its relation to other rooms.
;;;
;;; Room → place. Different places might have different ways of handling
;;; information.

;;; Room ⊂ Place

(def objects
  {::right-home-101/richards-office
   ;; This should be constructed by a constructor.
   {::type ::room,
    ::place/directions {::direction/east ::right-home-101/oshiire,
                        ::direction/north ::right-home-101/living-room},
    ::place/contents []},
   {::type ::room,
    ::place/directions {::direction/north-east ::right-home-101/hallway,
                        ::direction/south-east ::right-home-101/cupboard,
                        ::direction/north ::right-home-101/kitchen,
                        ::direction/south ::right-home-101/office},
    ::place/contents [::player/richard]}
   ::player/richard,
   {::type ::creature/human, ::movable/location ::right-home-101/living-room}})

;;; Objects need to contain their own name.

(handle (get objects ::player/richard)
        {::information/type ::action/movement,
         ::movement/direction ::direction/south})

;;; What does this do?
;;; - Look up what is south of the player's location. Find out that it's the
;;;   office.
;;; - Update the location of the player to be office.
;;; - Return a movement information.
;;;
;;; How to decide who handles the movement information?
;;; - Could make all objects handle it and only those that have anything to do
;;;   with the player change state and return information.
;;; - But that would be expensive and unrealistic.
;;; - So it seems like the system does need to treat places in a special way. To
;;;   make it physical, it would be the places that are the system. They contain
;;;   objects and they propagate information between objects.

;;; Next thing would be handling a shout.
;;; Find more things that test the system most.
