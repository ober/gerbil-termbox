;;; -*- Gerbil -*-
;;; termbox2 raw FFI bindings
(export #t)
(import :std/foreign)

(begin-ffi
    (;; Init/shutdown
     tb_init tb_shutdown

     ;; Screen
     tb_width tb_height
     tb_clear tb_set_clear_attrs
     tb_present tb_invalidate

     ;; Cell
     tb_set_cell tb_print

     ;; Cursor
     tb_set_cursor tb_hide_cursor

     ;; Input/output modes
     tb_set_input_mode tb_set_output_mode

     ;; Event handling (via C wrappers)
     ffi_tb_poll_event ffi_tb_peek_event
     ffi_tb_event_type ffi_tb_event_mod
     ffi_tb_event_key ffi_tb_event_ch
     ffi_tb_event_w ffi_tb_event_h
     ffi_tb_event_x ffi_tb_event_y

     ;; Error/info
     tb_strerror tb_last_errno
     tb_has_truecolor tb_has_egc
     tb_attr_width tb_version

     ;; Key constants
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

     ;; Event types
     TB_EVENT_KEY TB_EVENT_RESIZE TB_EVENT_MOUSE

     ;; Modifiers
     TB_MOD_ALT TB_MOD_CTRL TB_MOD_SHIFT TB_MOD_MOTION

     ;; Input modes
     TB_INPUT_CURRENT TB_INPUT_ESC TB_INPUT_ALT TB_INPUT_MOUSE

     ;; Output modes
     TB_OUTPUT_CURRENT TB_OUTPUT_NORMAL
     TB_OUTPUT_256 TB_OUTPUT_216
     TB_OUTPUT_GRAYSCALE TB_OUTPUT_TRUECOLOR

     ;; Colors
     TB_DEFAULT TB_BLACK TB_RED TB_GREEN
     TB_YELLOW TB_BLUE TB_MAGENTA TB_CYAN TB_WHITE

     ;; Attributes (32-bit truecolor mode)
     TB_BOLD TB_UNDERLINE TB_REVERSE
     TB_ITALIC TB_BLINK TB_HI_BLACK
     TB_BRIGHT TB_DIM

     ;; Error codes
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

  (declare (not safe))

  (c-declare #<<END-C
#define TB_OPT_ATTR_W 32
#define TB_IMPL
#include "termbox2.h"

/* --- Event struct wrapper ---
 * Store the last event in a static variable and provide accessors.
 * This avoids passing struct pointers across the FFI boundary.
 */
static struct tb_event ffi_last_event;

static int ffi_tb_poll_event_wrapper(void)
{
  return tb_poll_event(&ffi_last_event);
}

static int ffi_tb_peek_event_wrapper(int timeout_ms)
{
  return tb_peek_event(&ffi_last_event, timeout_ms);
}

static int ffi_tb_event_type(void) { return (int)ffi_last_event.type; }
static int ffi_tb_event_mod(void)  { return (int)ffi_last_event.mod; }
static int ffi_tb_event_key(void)  { return (int)ffi_last_event.key; }
static int ffi_tb_event_ch(void)   { return (int)ffi_last_event.ch; }
static int ffi_tb_event_w(void)    { return (int)ffi_last_event.w; }
static int ffi_tb_event_h(void)    { return (int)ffi_last_event.h; }
static int ffi_tb_event_x(void)    { return (int)ffi_last_event.x; }
static int ffi_tb_event_y(void)    { return (int)ffi_last_event.y; }
END-C
  )

  ;; --- Init/shutdown ---
  (define-c-lambda tb_init () int)
  (define-c-lambda tb_shutdown () int)

  ;; --- Screen ---
  (define-c-lambda tb_width () int)
  (define-c-lambda tb_height () int)
  (define-c-lambda tb_clear () int)
  (define-c-lambda tb_set_clear_attrs (unsigned-int32 unsigned-int32) int)
  (define-c-lambda tb_present () int)
  (define-c-lambda tb_invalidate () int)

  ;; --- Cell ---
  (define-c-lambda tb_set_cell (int int unsigned-int32 unsigned-int32 unsigned-int32) int)
  (define-c-lambda tb_print (int int unsigned-int32 unsigned-int32 char-string) int)

  ;; --- Cursor ---
  (define-c-lambda tb_set_cursor (int int) int)
  (define-c-lambda tb_hide_cursor () int)

  ;; --- Input/output modes ---
  (define-c-lambda tb_set_input_mode (int) int)
  (define-c-lambda tb_set_output_mode (int) int)

  ;; --- Event handling ---
  (define-c-lambda ffi_tb_poll_event () int "ffi_tb_poll_event_wrapper")
  (define-c-lambda ffi_tb_peek_event (int) int "ffi_tb_peek_event_wrapper")
  (define-c-lambda ffi_tb_event_type () int)
  (define-c-lambda ffi_tb_event_mod () int)
  (define-c-lambda ffi_tb_event_key () int)
  (define-c-lambda ffi_tb_event_ch () int)
  (define-c-lambda ffi_tb_event_w () int)
  (define-c-lambda ffi_tb_event_h () int)
  (define-c-lambda ffi_tb_event_x () int)
  (define-c-lambda ffi_tb_event_y () int)

  ;; --- Error/info ---
  (define-c-lambda tb_strerror (int) char-string)
  (define-c-lambda tb_last_errno () int)
  (define-c-lambda tb_has_truecolor () int)
  (define-c-lambda tb_has_egc () int)
  (define-c-lambda tb_attr_width () int)
  (define-c-lambda tb_version () char-string)

  ;; --- Key constants ---
  (define-const TB_KEY_CTRL_TILDE)
  (define-const TB_KEY_CTRL_A)
  (define-const TB_KEY_CTRL_B)
  (define-const TB_KEY_CTRL_C)
  (define-const TB_KEY_CTRL_D)
  (define-const TB_KEY_CTRL_E)
  (define-const TB_KEY_CTRL_F)
  (define-const TB_KEY_CTRL_G)
  (define-const TB_KEY_CTRL_J)
  (define-const TB_KEY_CTRL_K)
  (define-const TB_KEY_CTRL_L)
  (define-const TB_KEY_CTRL_N)
  (define-const TB_KEY_CTRL_O)
  (define-const TB_KEY_CTRL_P)
  (define-const TB_KEY_CTRL_Q)
  (define-const TB_KEY_CTRL_R)
  (define-const TB_KEY_CTRL_S)
  (define-const TB_KEY_CTRL_T)
  (define-const TB_KEY_CTRL_U)
  (define-const TB_KEY_CTRL_V)
  (define-const TB_KEY_CTRL_W)
  (define-const TB_KEY_CTRL_X)
  (define-const TB_KEY_CTRL_Y)
  (define-const TB_KEY_CTRL_Z)
  (define-const TB_KEY_BACKSPACE)
  (define-const TB_KEY_TAB)
  (define-const TB_KEY_ENTER)
  (define-const TB_KEY_ESC)
  (define-const TB_KEY_SPACE)
  (define-const TB_KEY_BACKSPACE2)

  ;; Terminal-dependent key constants
  (define-const TB_KEY_F1)
  (define-const TB_KEY_F2)
  (define-const TB_KEY_F3)
  (define-const TB_KEY_F4)
  (define-const TB_KEY_F5)
  (define-const TB_KEY_F6)
  (define-const TB_KEY_F7)
  (define-const TB_KEY_F8)
  (define-const TB_KEY_F9)
  (define-const TB_KEY_F10)
  (define-const TB_KEY_F11)
  (define-const TB_KEY_F12)
  (define-const TB_KEY_INSERT)
  (define-const TB_KEY_DELETE)
  (define-const TB_KEY_HOME)
  (define-const TB_KEY_END)
  (define-const TB_KEY_PGUP)
  (define-const TB_KEY_PGDN)
  (define-const TB_KEY_ARROW_UP)
  (define-const TB_KEY_ARROW_DOWN)
  (define-const TB_KEY_ARROW_LEFT)
  (define-const TB_KEY_ARROW_RIGHT)
  (define-const TB_KEY_BACK_TAB)
  (define-const TB_KEY_MOUSE_LEFT)
  (define-const TB_KEY_MOUSE_RIGHT)
  (define-const TB_KEY_MOUSE_MIDDLE)
  (define-const TB_KEY_MOUSE_RELEASE)
  (define-const TB_KEY_MOUSE_WHEEL_UP)
  (define-const TB_KEY_MOUSE_WHEEL_DOWN)

  ;; --- Event types ---
  (define-const TB_EVENT_KEY)
  (define-const TB_EVENT_RESIZE)
  (define-const TB_EVENT_MOUSE)

  ;; --- Modifiers ---
  (define-const TB_MOD_ALT)
  (define-const TB_MOD_CTRL)
  (define-const TB_MOD_SHIFT)
  (define-const TB_MOD_MOTION)

  ;; --- Input modes ---
  (define-const TB_INPUT_CURRENT)
  (define-const TB_INPUT_ESC)
  (define-const TB_INPUT_ALT)
  (define-const TB_INPUT_MOUSE)

  ;; --- Output modes ---
  (define-const TB_OUTPUT_CURRENT)
  (define-const TB_OUTPUT_NORMAL)
  (define-const TB_OUTPUT_256)
  (define-const TB_OUTPUT_216)
  (define-const TB_OUTPUT_GRAYSCALE)
  (define-const TB_OUTPUT_TRUECOLOR)

  ;; --- Colors ---
  (define-const TB_DEFAULT)
  (define-const TB_BLACK)
  (define-const TB_RED)
  (define-const TB_GREEN)
  (define-const TB_YELLOW)
  (define-const TB_BLUE)
  (define-const TB_MAGENTA)
  (define-const TB_CYAN)
  (define-const TB_WHITE)

  ;; --- Attributes (32-bit values with TB_OPT_ATTR_W=32) ---
  (define-const TB_BOLD)
  (define-const TB_UNDERLINE)
  (define-const TB_REVERSE)
  (define-const TB_ITALIC)
  (define-const TB_BLINK)
  (define-const TB_HI_BLACK)
  (define-const TB_BRIGHT)
  (define-const TB_DIM)

  ;; --- Error codes ---
  (define-const TB_OK)
  (define-const TB_ERR)
  (define-const TB_ERR_NEED_MORE)
  (define-const TB_ERR_INIT_ALREADY)
  (define-const TB_ERR_INIT_OPEN)
  (define-const TB_ERR_MEM)
  (define-const TB_ERR_NO_EVENT)
  (define-const TB_ERR_NO_TERM)
  (define-const TB_ERR_NOT_INIT)
  (define-const TB_ERR_OUT_OF_BOUNDS)
  (define-const TB_ERR_READ)
  (define-const TB_ERR_RESIZE_IOCTL)
  (define-const TB_ERR_RESIZE_PIPE)
  (define-const TB_ERR_RESIZE_SIGACTION)
  (define-const TB_ERR_POLL)
  (define-const TB_ERR_TCGETATTR)
  (define-const TB_ERR_TCSETATTR)
  (define-const TB_ERR_UNSUPPORTED_TERM)
  (define-const TB_ERR_RESIZE_WRITE)
  (define-const TB_ERR_RESIZE_POLL)
  (define-const TB_ERR_RESIZE_READ)
  (define-const TB_ERR_RESIZE_SSCANF)
  (define-const TB_ERR_CAP_COLLISION)

) ;; end begin-ffi
