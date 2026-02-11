;;; -*- Gerbil -*-
;;; draw primitives test suite (pure-Scheme, no terminal needed)
(import :std/test
        :gerbil-termbox/draw
        :gerbil-termbox/termbox)
(export draw-test)

(def draw-test
  (test-suite "draw primitives"

    (test-case "box chars are integers"
      ;; Single
      (check (integer? BOX_SINGLE_H) => #t)
      (check (integer? BOX_SINGLE_V) => #t)
      (check (integer? BOX_SINGLE_TL) => #t)
      (check (integer? BOX_SINGLE_TR) => #t)
      (check (integer? BOX_SINGLE_BL) => #t)
      (check (integer? BOX_SINGLE_BR) => #t)
      (check (integer? BOX_SINGLE_CROSS) => #t)
      ;; Double
      (check (integer? BOX_DOUBLE_H) => #t)
      (check (integer? BOX_DOUBLE_V) => #t)
      (check (integer? BOX_DOUBLE_TL) => #t)
      ;; Heavy
      (check (integer? BOX_HEAVY_H) => #t)
      (check (integer? BOX_HEAVY_V) => #t)
      (check (integer? BOX_HEAVY_TL) => #t)
      ;; Rounded
      (check (integer? BOX_ROUNDED_TL) => #t)
      (check (integer? BOX_ROUNDED_TR) => #t)
      (check (integer? BOX_ROUNDED_BL) => #t)
      (check (integer? BOX_ROUNDED_BR) => #t))

    (test-case "box char Unicode codepoints"
      (check BOX_SINGLE_H => 9472)     ;; ─ U+2500
      (check BOX_SINGLE_V => 9474)     ;; │ U+2502
      (check BOX_SINGLE_TL => 9484)    ;; ┌ U+250C
      (check BOX_SINGLE_TR => 9488)    ;; ┐ U+2510
      (check BOX_SINGLE_BL => 9492)    ;; └ U+2514
      (check BOX_SINGLE_BR => 9496)    ;; ┘ U+2518
      (check BOX_DOUBLE_H => 9552)     ;; ═ U+2550
      (check BOX_DOUBLE_V => 9553)     ;; ║ U+2551
      (check BOX_HEAVY_H => 9473)      ;; ━ U+2501
      (check BOX_HEAVY_V => 9475)      ;; ┃ U+2503
      (check BOX_ROUNDED_TL => 9581)   ;; ╭ U+256D
      (check BOX_ROUNDED_BR => 9583))  ;; ╯ U+256F

    (test-case "box styles are vectors of 6 elements"
      (check (vector? box-style-single) => #t)
      (check (vector-length box-style-single) => 6)
      (check (vector? box-style-double) => #t)
      (check (vector-length box-style-double) => 6)
      (check (vector? box-style-heavy) => #t)
      (check (vector-length box-style-heavy) => 6)
      (check (vector? box-style-rounded) => #t)
      (check (vector-length box-style-rounded) => 6))

    (test-case "box style accessors"
      (check (box-style-h box-style-single) => BOX_SINGLE_H)
      (check (box-style-v box-style-single) => BOX_SINGLE_V)
      (check (box-style-tl box-style-single) => BOX_SINGLE_TL)
      (check (box-style-tr box-style-single) => BOX_SINGLE_TR)
      (check (box-style-bl box-style-single) => BOX_SINGLE_BL)
      (check (box-style-br box-style-single) => BOX_SINGLE_BR)
      ;; Rounded uses single H/V but rounded corners
      (check (box-style-h box-style-rounded) => BOX_SINGLE_H)
      (check (box-style-v box-style-rounded) => BOX_SINGLE_V)
      (check (box-style-tl box-style-rounded) => BOX_ROUNDED_TL)
      (check (box-style-br box-style-rounded) => BOX_ROUNDED_BR))

    (test-case "text alignment math"
      ;; Test the alignment/truncation logic used by draw-text!
      ;; Left align: text starts at x=0
      (let* ((text "Hi")
             (max-w 10)
             (len (string-length text))
             (pad (- max-w len)))
        (check pad => 8)
        (check (quotient pad 2) => 4))  ;; center offset

      ;; Truncation: long text gets cut
      (let* ((text "Hello World!!")
             (max-w 5)
             (visible (substring text 0 (min (string-length text) max-w))))
        (check visible => "Hello")
        (check (string-length visible) => 5))

      ;; Center alignment: odd padding splits left-biased
      (let* ((text "AB")
             (max-w 5)
             (pad (- max-w (string-length text)))
             (left-pad (quotient pad 2)))
        (check left-pad => 1)))  ;; "AB" in 5 cols: " AB  "

    (test-case "tee and cross constants"
      (check BOX_SINGLE_TEE_D => 9516)  ;; ┬
      (check BOX_SINGLE_TEE_U => 9524)  ;; ┴
      (check BOX_SINGLE_TEE_R => 9500)  ;; ├
      (check BOX_SINGLE_TEE_L => 9508)  ;; ┤
      (check BOX_SINGLE_CROSS => 9532)  ;; ┼
      (check BOX_DOUBLE_CROSS => 9580)  ;; ╬
      (check BOX_HEAVY_CROSS => 9547))  ;; ╋
))
