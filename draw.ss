;;; -*- Gerbil -*-
;;; termbox2 — drawing primitives (box, line, text, fill)
(export
  ;; Box-drawing character sets (Unicode codepoints as integers)
  ;; Single (light)
  BOX_SINGLE_H BOX_SINGLE_V
  BOX_SINGLE_TL BOX_SINGLE_TR BOX_SINGLE_BL BOX_SINGLE_BR
  BOX_SINGLE_TEE_D BOX_SINGLE_TEE_U BOX_SINGLE_TEE_R BOX_SINGLE_TEE_L
  BOX_SINGLE_CROSS
  ;; Double
  BOX_DOUBLE_H BOX_DOUBLE_V
  BOX_DOUBLE_TL BOX_DOUBLE_TR BOX_DOUBLE_BL BOX_DOUBLE_BR
  BOX_DOUBLE_TEE_D BOX_DOUBLE_TEE_U BOX_DOUBLE_TEE_R BOX_DOUBLE_TEE_L
  BOX_DOUBLE_CROSS
  ;; Heavy
  BOX_HEAVY_H BOX_HEAVY_V
  BOX_HEAVY_TL BOX_HEAVY_TR BOX_HEAVY_BL BOX_HEAVY_BR
  BOX_HEAVY_TEE_D BOX_HEAVY_TEE_U BOX_HEAVY_TEE_R BOX_HEAVY_TEE_L
  BOX_HEAVY_CROSS
  ;; Rounded
  BOX_ROUNDED_TL BOX_ROUNDED_TR BOX_ROUNDED_BL BOX_ROUNDED_BR
  ;; Box style accessors
  box-style-h box-style-v
  box-style-tl box-style-tr box-style-bl box-style-br
  box-style-single box-style-double box-style-heavy box-style-rounded
  ;; Drawing functions
  draw-box! draw-hline! draw-vline! draw-text! fill-rect!)

(import :gerbil-termbox/termbox)

;; --- Box-drawing character constants (Unicode codepoints) ---

;; Single (light) ─ │ ┌ ┐ └ ┘ ┬ ┴ ├ ┤ ┼
(def BOX_SINGLE_H     9472)  ;; ─ U+2500
(def BOX_SINGLE_V     9474)  ;; │ U+2502
(def BOX_SINGLE_TL    9484)  ;; ┌ U+250C
(def BOX_SINGLE_TR    9488)  ;; ┐ U+2510
(def BOX_SINGLE_BL    9492)  ;; └ U+2514
(def BOX_SINGLE_BR    9496)  ;; ┘ U+2518
(def BOX_SINGLE_TEE_D 9516)  ;; ┬ U+252C
(def BOX_SINGLE_TEE_U 9524)  ;; ┴ U+2534
(def BOX_SINGLE_TEE_R 9500)  ;; ├ U+251C
(def BOX_SINGLE_TEE_L 9508)  ;; ┤ U+2524
(def BOX_SINGLE_CROSS 9532)  ;; ┼ U+253C

;; Double ═ ║ ╔ ╗ ╚ ╝ ╦ ╩ ╠ ╣ ╬
(def BOX_DOUBLE_H     9552)  ;; ═ U+2550
(def BOX_DOUBLE_V     9553)  ;; ║ U+2551
(def BOX_DOUBLE_TL    9556)  ;; ╔ U+2554
(def BOX_DOUBLE_TR    9559)  ;; ╗ U+2557
(def BOX_DOUBLE_BL    9562)  ;; ╚ U+255A
(def BOX_DOUBLE_BR    9565)  ;; ╝ U+255D
(def BOX_DOUBLE_TEE_D 9574)  ;; ╦ U+2566
(def BOX_DOUBLE_TEE_U 9577)  ;; ╩ U+2569
(def BOX_DOUBLE_TEE_R 9568)  ;; ╠ U+2560
(def BOX_DOUBLE_TEE_L 9571)  ;; ╣ U+2563
(def BOX_DOUBLE_CROSS 9580)  ;; ╬ U+256C

;; Heavy ━ ┃ ┏ ┓ ┗ ┛ ┳ ┻ ┣ ┫ ╋
(def BOX_HEAVY_H     9473)  ;; ━ U+2501
(def BOX_HEAVY_V     9475)  ;; ┃ U+2503
(def BOX_HEAVY_TL    9487)  ;; ┏ U+250F
(def BOX_HEAVY_TR    9491)  ;; ┓ U+2513
(def BOX_HEAVY_BL    9495)  ;; ┗ U+2517
(def BOX_HEAVY_BR    9499)  ;; ┛ U+251B
(def BOX_HEAVY_TEE_D 9523)  ;; ┳ U+2533
(def BOX_HEAVY_TEE_U 9531)  ;; ┻ U+253B
(def BOX_HEAVY_TEE_R 9507)  ;; ┣ U+2523
(def BOX_HEAVY_TEE_L 9515)  ;; ┫ U+252B
(def BOX_HEAVY_CROSS 9547)  ;; ╋ U+254B

;; Rounded ╭ ╮ ╰ ╯ (uses single H/V for lines)
(def BOX_ROUNDED_TL   9581)  ;; ╭ U+256D
(def BOX_ROUNDED_TR   9582)  ;; ╮ U+256E
(def BOX_ROUNDED_BL   9584)  ;; ╰ U+2570
(def BOX_ROUNDED_BR   9583)  ;; ╯ U+256F

;; --- Box style accessors ---
;; A box style is a vector: #(h v tl tr bl br)

(def (box-style-h s)  (vector-ref s 0))
(def (box-style-v s)  (vector-ref s 1))
(def (box-style-tl s) (vector-ref s 2))
(def (box-style-tr s) (vector-ref s 3))
(def (box-style-bl s) (vector-ref s 4))
(def (box-style-br s) (vector-ref s 5))

(def box-style-single
  (vector BOX_SINGLE_H BOX_SINGLE_V
          BOX_SINGLE_TL BOX_SINGLE_TR BOX_SINGLE_BL BOX_SINGLE_BR))

(def box-style-double
  (vector BOX_DOUBLE_H BOX_DOUBLE_V
          BOX_DOUBLE_TL BOX_DOUBLE_TR BOX_DOUBLE_BL BOX_DOUBLE_BR))

(def box-style-heavy
  (vector BOX_HEAVY_H BOX_HEAVY_V
          BOX_HEAVY_TL BOX_HEAVY_TR BOX_HEAVY_BL BOX_HEAVY_BR))

(def box-style-rounded
  (vector BOX_SINGLE_H BOX_SINGLE_V
          BOX_ROUNDED_TL BOX_ROUNDED_TR BOX_ROUNDED_BL BOX_ROUNDED_BR))

;; --- Drawing functions ---

(def (draw-hline! x y len
                  fg: (fg TB_DEFAULT) bg: (bg TB_DEFAULT)
                  char: (ch BOX_SINGLE_H))
  (let loop ((i 0))
    (when (< i len)
      (tb-set-cell! (+ x i) y ch fg bg)
      (loop (+ i 1)))))

(def (draw-vline! x y len
                  fg: (fg TB_DEFAULT) bg: (bg TB_DEFAULT)
                  char: (ch BOX_SINGLE_V))
  (let loop ((i 0))
    (when (< i len)
      (tb-set-cell! x (+ y i) ch fg bg)
      (loop (+ i 1)))))

(def (fill-rect! x y w h
                 fg: (fg TB_DEFAULT) bg: (bg TB_DEFAULT)
                 char: (ch (char->integer #\space)))
  (let loop-y ((row 0))
    (when (< row h)
      (let loop-x ((col 0))
        (when (< col w)
          (tb-set-cell! (+ x col) (+ y row) ch fg bg)
          (loop-x (+ col 1))))
      (loop-y (+ row 1)))))

(def (draw-text! x y text max-width
                 fg: (fg TB_DEFAULT) bg: (bg TB_DEFAULT)
                 align: (align 'left))
  (let* ((len (string-length text))
         (visible (if (> len max-width)
                    (substring text 0 max-width)
                    text))
         (vlen (string-length visible))
         (pad (- max-width vlen))
         (left-pad (case align
                     ((left)   0)
                     ((right)  pad)
                     ((center) (quotient pad 2))
                     (else     0))))
    ;; Fill background for alignment padding
    (when (> pad 0)
      (fill-rect! x y max-width 1 fg: fg bg: bg))
    ;; Print the visible text at the aligned position
    (tb-print! (+ x left-pad) y visible fg bg)))

(def (draw-box! x y w h
                fg: (fg TB_DEFAULT) bg: (bg TB_DEFAULT)
                style: (style box-style-single)
                title: (title #f)
                title-align: (title-align 'left))
  (when (and (>= w 2) (>= h 2))
    (let ((ch-h  (box-style-h style))
          (ch-v  (box-style-v style))
          (ch-tl (box-style-tl style))
          (ch-tr (box-style-tr style))
          (ch-bl (box-style-bl style))
          (ch-br (box-style-br style)))
      ;; Corners
      (tb-set-cell! x y ch-tl fg bg)
      (tb-set-cell! (+ x w -1) y ch-tr fg bg)
      (tb-set-cell! x (+ y h -1) ch-bl fg bg)
      (tb-set-cell! (+ x w -1) (+ y h -1) ch-br fg bg)
      ;; Horizontal edges
      (draw-hline! (+ x 1) y (- w 2) fg: fg bg: bg char: ch-h)
      (draw-hline! (+ x 1) (+ y h -1) (- w 2) fg: fg bg: bg char: ch-h)
      ;; Vertical edges
      (draw-vline! x (+ y 1) (- h 2) fg: fg bg: bg char: ch-v)
      (draw-vline! (+ x w -1) (+ y 1) (- h 2) fg: fg bg: bg char: ch-v)
      ;; Optional title on top edge
      (when (and title (> w 4))
        (let* ((max-title-w (- w 4))
               (ttext (if (> (string-length title) max-title-w)
                        (substring title 0 max-title-w)
                        title))
               (tlen (string-length ttext))
               (tpad (- max-title-w tlen))
               (tx (+ x 2
                      (case title-align
                        ((left) 0)
                        ((right) tpad)
                        ((center) (quotient tpad 2))
                        (else 0)))))
          (tb-print! tx y (string-append " " ttext " ") fg bg))))))
