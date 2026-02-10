;;; -*- Gerbil -*-
;;; termbox2 — high-level Gerbil wrapper API
(export
  ;; Lifecycle
  with-termbox

  ;; Screen
  tb-width tb-height
  tb-clear! tb-present! tb-invalidate!
  tb-set-clear-attrs!

  ;; Cell/print
  tb-set-cell! tb-print!

  ;; Cursor
  tb-set-cursor! tb-hide-cursor!

  ;; Modes
  tb-set-input-mode! tb-set-output-mode!

  ;; Events
  tb-poll-event tb-peek-event
  (struct-out tb-event)
  tb-event-key? tb-event-resize? tb-event-mouse?

  ;; Color helpers
  tb-rgb tb-color

  ;; Error/info
  tb-strerror tb-last-errno

  ;; Re-export all constants
  TB_KEY_CTRL_TILDE
  TB_KEY_CTRL_A TB_KEY_CTRL_B TB_KEY_CTRL_C
  TB_KEY_CTRL_D TB_KEY_CTRL_E TB_KEY_CTRL_F
  TB_KEY_CTRL_G TB_KEY_CTRL_J TB_KEY_CTRL_K
  TB_KEY_CTRL_L TB_KEY_CTRL_N TB_KEY_CTRL_O
  TB_KEY_CTRL_P TB_KEY_CTRL_Q TB_KEY_CTRL_R
  TB_KEY_CTRL_S TB_KEY_CTRL_T TB_KEY_CTRL_U
  TB_KEY_CTRL_V TB_KEY_CTRL_W TB_KEY_CTRL_X
  TB_KEY_CTRL_Y TB_KEY_CTRL_Z
  TB_KEY_BACKSPACE TB_KEY_TAB TB_KEY_ENTER
  TB_KEY_ESC TB_KEY_SPACE TB_KEY_BACKSPACE2
  TB_KEY_F1 TB_KEY_F2 TB_KEY_F3 TB_KEY_F4
  TB_KEY_F5 TB_KEY_F6 TB_KEY_F7 TB_KEY_F8
  TB_KEY_F9 TB_KEY_F10 TB_KEY_F11 TB_KEY_F12
  TB_KEY_INSERT TB_KEY_DELETE
  TB_KEY_HOME TB_KEY_END
  TB_KEY_PGUP TB_KEY_PGDN
  TB_KEY_ARROW_UP TB_KEY_ARROW_DOWN
  TB_KEY_ARROW_LEFT TB_KEY_ARROW_RIGHT
  TB_KEY_BACK_TAB
  TB_KEY_MOUSE_LEFT TB_KEY_MOUSE_RIGHT
  TB_KEY_MOUSE_MIDDLE TB_KEY_MOUSE_RELEASE
  TB_KEY_MOUSE_WHEEL_UP TB_KEY_MOUSE_WHEEL_DOWN
  TB_EVENT_KEY TB_EVENT_RESIZE TB_EVENT_MOUSE
  TB_MOD_ALT TB_MOD_CTRL TB_MOD_SHIFT TB_MOD_MOTION
  TB_INPUT_CURRENT TB_INPUT_ESC TB_INPUT_ALT TB_INPUT_MOUSE
  TB_OUTPUT_CURRENT TB_OUTPUT_NORMAL
  TB_OUTPUT_256 TB_OUTPUT_216
  TB_OUTPUT_GRAYSCALE TB_OUTPUT_TRUECOLOR
  TB_DEFAULT TB_BLACK TB_RED TB_GREEN
  TB_YELLOW TB_BLUE TB_MAGENTA TB_CYAN TB_WHITE
  TB_BOLD TB_UNDERLINE TB_REVERSE
  TB_ITALIC TB_BLINK TB_HI_BLACK
  TB_BRIGHT TB_DIM
  TB_OK TB_ERR
  TB_ERR_NEED_MORE TB_ERR_INIT_ALREADY
  TB_ERR_INIT_OPEN TB_ERR_MEM
  TB_ERR_NO_EVENT TB_ERR_NO_TERM
  TB_ERR_NOT_INIT TB_ERR_OUT_OF_BOUNDS
  TB_ERR_READ TB_ERR_RESIZE_IOCTL
  TB_ERR_RESIZE_PIPE TB_ERR_RESIZE_SIGACTION
  TB_ERR_POLL TB_ERR_TCGETATTR
  TB_ERR_TCSETATTR TB_ERR_UNSUPPORTED_TERM
  TB_ERR_RESIZE_WRITE TB_ERR_RESIZE_POLL
  TB_ERR_RESIZE_READ TB_ERR_RESIZE_SSCANF
  TB_ERR_CAP_COLLISION)

(import :gerbil-termbox/libtermbox)

;; --- Event struct ---

(defstruct tb-event (type mod key ch w h x y)
  transparent: #t)

(def (tb-event-key? ev)
  (= (tb-event-type ev) TB_EVENT_KEY))

(def (tb-event-resize? ev)
  (= (tb-event-type ev) TB_EVENT_RESIZE))

(def (tb-event-mouse? ev)
  (= (tb-event-type ev) TB_EVENT_MOUSE))

;; --- Lifecycle ---

(defrule (with-termbox body ...)
  (let ((rc (tb_init)))
    (unless (= rc TB_OK)
      (error "termbox: init failed" (tb_strerror rc) rc))
    (try (begin body ...)
      (finally (tb_shutdown)))))

;; --- Screen ---

(def (tb-width) (tb_width))
(def (tb-height) (tb_height))
(def (tb-clear!) (tb_clear))
(def (tb-present!) (tb_present))
(def (tb-invalidate!) (tb_invalidate))
(def (tb-set-clear-attrs! fg bg) (tb_set_clear_attrs fg bg))

;; --- Cell/print ---

(def (tb-set-cell! x y ch (fg TB_DEFAULT) (bg TB_DEFAULT))
  (tb_set_cell x y ch fg bg))

(def (tb-print! x y str (fg TB_DEFAULT) (bg TB_DEFAULT))
  (tb_print x y fg bg str))

;; --- Cursor ---

(def (tb-set-cursor! x y)
  (tb_set_cursor x y))

(def (tb-hide-cursor!)
  (tb_hide_cursor))

;; --- Modes ---

(def (tb-set-input-mode! mode)
  (tb_set_input_mode mode))

(def (tb-set-output-mode! mode)
  (tb_set_output_mode mode))

;; --- Events ---

(def (tb-poll-event)
  (let ((rc (ffi_tb_poll_event)))
    (if (>= rc TB_OK)
      (make-tb-event
        (ffi_tb_event_type)
        (ffi_tb_event_mod)
        (ffi_tb_event_key)
        (ffi_tb_event_ch)
        (ffi_tb_event_w)
        (ffi_tb_event_h)
        (ffi_tb_event_x)
        (ffi_tb_event_y))
      (values #f rc))))

(def (tb-peek-event timeout-ms)
  (let ((rc (ffi_tb_peek_event timeout-ms)))
    (if (>= rc TB_OK)
      (make-tb-event
        (ffi_tb_event_type)
        (ffi_tb_event_mod)
        (ffi_tb_event_key)
        (ffi_tb_event_ch)
        (ffi_tb_event_w)
        (ffi_tb_event_h)
        (ffi_tb_event_x)
        (ffi_tb_event_y))
      (values #f rc))))

;; --- Color helpers ---

;; Construct a 24-bit truecolor value from r, g, b (0-255)
(def (tb-rgb r g b)
  (bitwise-ior
    (arithmetic-shift (bitwise-and r #xff) 16)
    (arithmetic-shift (bitwise-and g #xff) 8)
    (bitwise-and b #xff)))

;; Construct a color with optional style attributes
(def (tb-color color . attrs)
  (foldl bitwise-ior color attrs))

;; --- Error/info ---

(def (tb-strerror err)
  (tb_strerror err))

(def (tb-last-errno)
  (tb_last_errno))
