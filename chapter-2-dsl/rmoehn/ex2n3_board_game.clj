(ns ex2n3_board_game
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [com.rpl.specter :as sp]))

;;; What do I still have to do to be fairly confident that I've practiced all
;;; the things that are relevant to the book?

;;; I'm not going to implement all the rules for checkers. At the bottom is a
;;; quick implementation of setting up the board, but that's as far as my
;;; patience goes.

;;; My main idea for the design is to separate the game state into the state on
;;; the board and the state off the board. And to distinguish between moves and
;;; plays.

(s/def ::game-state (s/keys ::board ::off-board ::player-in-control))

;;; - A move is the smallest action after which control returns to one of the
;;;   players.
;;; - A play is a sequence of moves, after which control returns to the other
;;;   play.
;;; - For example in chess, pushing a pawn to the end zone is a move.
;;;   Turning it into another piece is another move. Both moves together are a
;;;   play. Or, in checkers, a jump is a move and chained jumps form a play.
;;;
;;; The fact that a play hasn't ended yet is encoded in the off-board game
;;; state. I think this would let me address all the problems noted at the end
;;; of section 2.4.1.
;;;
;;;
;;; What they want in the book is an implementation of this function, as far as
;;; I understand:
;;;
;;;   generate-all-plays : (game, game-state) -> [play]
;;;
;;; This can be defined in terms of:
;;;
;;;   generate-all-moves : (game, game-state) -> [move]
;;;
;;; These functions are general. They rely on the game-specific rules being
;;; implemented as functions summarized by `game`. For a chess/checkers-like
;;; game, these top-level functions should be enough to define the game:
;;;
;;;   initial-state: -> game-state
;;;   generate-moves : (game-state, coords) -> [move]
;;;   process-move : (game-state, move) -> game-state
;;;
;;; The contents of the (get-piece board coords) and the contents of
;;; (off-board-state game-state) are specific to the game, too. Note that a
;;; piece doesn't contain its coords. – In a real chess game, the coordinates
;;; aren't written on each piece, either. And it feels like a good idea to
;;; follow the physical analogue.
;;; (https://lispcast.com/building-composable-abstractions/)
;;;
;;; There are some more supporting functions. For example, it would probably be
;;; useful to get all the coordinates in which there are pieces in a game state.
;;; But these are less important and would emerge if I implemented the thing.


;;;; Incomplete supporting functions

(defn empty-board
  [n-rows n-columns]
  (vec (repeat n-rows (vec (repeat n-columns nil)))))

(defn set-piece [board coords piece] (assoc-in board coords piece)); -> board
(defn get-piece [board coords]); -> piece

(declare render-piece)
(declare initial-state)

;; Note that render-piece would have to be passed in as part of the game
;; definition.
(defn render-board
  [board]
  (vec (reverse (sp/transform [sp/ALL sp/ALL]
                              #(some-> %
                                       render-piece)
                              board))))

;;;; Incomplete checkers definition

; (ns ex2n3-board-game.checkers)

(defn parity [n] (if (even? n) :even :odd))

(defn ->piece [color kind] {::color color, ::kind kind})

(defn spread-men
  [board color start-row end-row]
  (reduce (fn [board-so-far next-coords]
            (set-piece board-so-far next-coords (->piece color ::man)))
          board
          (for [row (range start-row end-row)
                col (range 0 8)
                :when (= (parity row) (parity col))]
            [row col])))

(def board (empty-board 8 8))

(defn initial-state
  []
  (let [board (empty-board 8 8)
        with-black-pieces (spread-men board ::black 0 3)
        with-all-pieces (spread-men with-black-pieces ::white 5 8)]
    with-all-pieces))

(defn render-piece
  [{::keys [color kind]}]
  (keyword (str (if (= ::black color) \b \w) (if (= ::man kind) \- \=))))