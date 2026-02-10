;;; -*- Gerbil -*-
;;; termbox2 test suite
(import :std/test
        :gerbil-termbox/termbox
        :gerbil-termbox/libtermbox)
(export termbox-test)

(def termbox-test
  (test-suite "termbox2 bindings"

    (test-case "constants are integers"
      (check (integer? TB_OK) => #t)
      (check (integer? TB_ERR) => #t)
      (check (integer? TB_EVENT_KEY) => #t)
      (check (integer? TB_EVENT_RESIZE) => #t)
      (check (integer? TB_EVENT_MOUSE) => #t)
      (check (integer? TB_KEY_ENTER) => #t)
      (check (integer? TB_KEY_ESC) => #t)
      (check (integer? TB_KEY_SPACE) => #t)
      (check (integer? TB_KEY_F1) => #t)
      (check (integer? TB_KEY_ARROW_UP) => #t)
      (check (integer? TB_MOD_ALT) => #t)
      (check (integer? TB_MOD_CTRL) => #t)
      (check (integer? TB_INPUT_ESC) => #t)
      (check (integer? TB_OUTPUT_NORMAL) => #t)
      (check (integer? TB_OUTPUT_TRUECOLOR) => #t)
      (check (integer? TB_DEFAULT) => #t)
      (check (integer? TB_RED) => #t)
      (check (integer? TB_BOLD) => #t)
      (check (integer? TB_UNDERLINE) => #t))

    (test-case "error codes are negative"
      (check (= TB_OK 0) => #t)
      (check (< TB_ERR 0) => #t)
      (check (< TB_ERR_NOT_INIT 0) => #t)
      (check (< TB_ERR_MEM 0) => #t)
      (check (< TB_ERR_NO_EVENT 0) => #t))

    (test-case "tb-rgb color construction"
      (check (tb-rgb 0 0 0) => 0)
      (check (tb-rgb 255 0 0) => #xff0000)
      (check (tb-rgb 0 255 0) => #x00ff00)
      (check (tb-rgb 0 0 255) => #x0000ff)
      (check (tb-rgb 255 255 255) => #xffffff)
      (check (tb-rgb 18 52 86) => #x123456))

    (test-case "tb-color with attributes"
      (check (tb-color TB_RED) => TB_RED)
      (check (tb-color TB_RED TB_BOLD)
             => (bitwise-ior TB_RED TB_BOLD))
      (check (tb-color TB_GREEN TB_BOLD TB_UNDERLINE)
             => (bitwise-ior TB_GREEN TB_BOLD TB_UNDERLINE)))

    (test-case "tb-event struct"
      (let ((ev (make-tb-event 1 0 13 0 0 0 0 0)))
        (check (tb-event? ev) => #t)
        (check (tb-event-type ev) => 1)
        (check (tb-event-mod ev) => 0)
        (check (tb-event-key ev) => 13)
        (check (tb-event-ch ev) => 0)
        (check (tb-event-w ev) => 0)
        (check (tb-event-h ev) => 0)
        (check (tb-event-x ev) => 0)
        (check (tb-event-y ev) => 0)))

    (test-case "tb-event type predicates"
      (let ((key-ev (make-tb-event TB_EVENT_KEY 0 0 65 0 0 0 0))
            (resize-ev (make-tb-event TB_EVENT_RESIZE 0 0 0 80 24 0 0))
            (mouse-ev (make-tb-event TB_EVENT_MOUSE 0 0 0 0 0 10 5)))
        (check (tb-event-key? key-ev) => #t)
        (check (tb-event-resize? key-ev) => #f)
        (check (tb-event-mouse? key-ev) => #f)
        (check (tb-event-resize? resize-ev) => #t)
        (check (tb-event-mouse? mouse-ev) => #t)))

    (test-case "tb-strerror returns string"
      (check (string? (tb-strerror TB_OK)) => #t)
      (check (string? (tb-strerror TB_ERR)) => #t)
      (check (string? (tb-strerror TB_ERR_NOT_INIT)) => #t))

    (test-case "tb_version returns string"
      (check (string? (tb_version)) => #t))
))
