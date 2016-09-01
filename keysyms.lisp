;; This file is an copy (except for package name) from stumpwm.
;;
;; Copyright (C) 2006-2008 Matthew Kennedy
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Mapping a keysym to a name is a client side activity in X11.  Some
;; of the code here was taken from the CMUCL Hemlocks code base.  The
;; actual mappings were taken from Xorg's keysymdefs.h.
;;
;; Code:

(in-package #:cl-fm)

;;; we need to map gtk character set to lisp characters.
;;; In addition, we need to process all keyboard codes to something useful:
;;; right-shift and left shift should convert to shift, which is not a
;;; lisp character...


;hashtable mapping gtk key # to cl character code
(defvar *gtkkey-clcode* (make-hash-table))

(defvar *gtkkey-name-translations* (make-hash-table))
(defvar *name-gtkkey-translations* (make-hash-table :test #'equal))

(defun define-gtkkey (gtkkey name)
  "Define a mapping from a gtkkey name to a gtkkey."
  (setf (gethash gtkkey *gtkkey-name-translations*) name
        (gethash name *name-gtkkey-translations*) gtkkey))

(defun gtkkey-name->gtkkey (name)
  "Return the gtkkey corresponding to NAME."
  (multiple-value-bind (value present-p)
      (gethash name *name-gtkkey-translations*)
    (declare (ignore present-p))
    value))

(defun gtkkey->gtkkey-name (gtkkey)
  "Return the name corresponding to GTKKEY or nil"
  (gethash gtkkey *gtkkey-name-translations*))

;; A quick macro to substitute gtkkey for name
(defmacro gtkkey-of (name)
  "Insert the numeric value of the named gtkkey"
  (let ((gtkkey (gtkkey-name->gtkkey name)))
    `,gtkkey))

;;OK, I will rip out X11/GTK gtkkeys and name them the Emacs way...
(define-gtkkey #xff0d "Return")         ;Return, enter
(define-gtkkey #xff1b "ESC")
(define-gtkkey #xff09 "TAB")
(define-gtkkey #xff08 "BS")      ;Back space, back char
(define-gtkkey #xffff "DEL")         ;Delete, rubout
(define-gtkkey #x0020 "SPC")        ;U+0020 SPACE
(define-gtkkey #x0021 "!")         ;U+0021 EXCLAMATION MAR
(define-gtkkey #x0022 "\"")       ;U+0022 QUOTATION MARK
(define-gtkkey #x0023 "#")     ;U+0023 NUMBER SIGN
(define-gtkkey #x0024 "$")         ;U+0024 DOLLAR SIGN
(define-gtkkey #x0025 "%")        ;U+0025 PERCENT SIGN
(define-gtkkey #x0026 "&")      ;U+0026 AMPERSAND
(define-gtkkey #x0027 "'")     ;U+0027 APOSTROPHE
(define-gtkkey #x0028 "(")      ;U+0028 LEFT PARENTHESIS
(define-gtkkey #x0029 ")")     ;U+0029 RIGHT PARENTHESIS
(define-gtkkey #x002a "*")       ;U+002A ASTERISK
(define-gtkkey #x002b "+")           ;U+002B PLUS SIGN
(define-gtkkey #x002c ",")          ;U+002C COMMA
(define-gtkkey #x002d "-")          ;U+002D HYPHEN-MINUS
(define-gtkkey #x002e ".")         ;U+002E FULL STOP
(define-gtkkey #x002f "/")          ;U+002F SOLIDUS
(define-gtkkey #x003a ":")          ;U+003A COLON
(define-gtkkey #x003b ";")      ;U+003B SEMICOLON
(define-gtkkey #x003c "<")           ;U+003C LESS-THAN SIGN
(define-gtkkey #x003d "=")          ;U+003D EQUALS SIGN
(define-gtkkey #x003e ">")        ;U+003E GREATER-THAN SIGN
(define-gtkkey #x003f "?")       ;U+003F QUESTION MARK
(define-gtkkey #x0040 "@")             ;U+0040 COMMERCIAL AT
(define-gtkkey #x005b "[")    ;U+005B LEFT SQUARE BRACKET
(define-gtkkey #x005c "\\")      ;U+005C REVERSE SOLIDUS
(define-gtkkey #x005d "]")   ;U+005D RIGHT SQUARE BRACKET
(define-gtkkey #x005e "^")    ;U+005E CIRCUMFLEX ACCENT
(define-gtkkey #x005f "_")     ;U+005F LOW LINE
(define-gtkkey #x0060 "`")          ;U+0060 GRAVE ACCENT
(define-gtkkey #x007b "{")      ;U+007B LEFT CURLY BRACKET
(define-gtkkey #x007c "|")            ;U+007C VERTICAL LINE
(define-gtkkey #x007d "}")     ;U+007D RIGHT CURLY BRACKET
(define-gtkkey #x007e "~")     ;U+007E TILDE



(define-gtkkey #xffffff "VoidSymbol")   ;Void symbol


(define-gtkkey #xff0a "Linefeed")       ;Linefeed, LF
(define-gtkkey #xff0b "Clear")
(define-gtkkey #xff13 "Pause")          ;Pause, hold
(define-gtkkey #xff14 "Scroll_Lock")
(define-gtkkey #xff15 "Sys_Req")


(define-gtkkey #xff20 "Multi_key")      ;Multi-key character compose
(define-gtkkey #xff37 "Codeinput")
(define-gtkkey #xff3c "SingleCandidate")
(define-gtkkey #xff3d "MultipleCandidate")
(define-gtkkey #xff3e "PreviousCandidate")
(define-gtkkey #xff21 "Kanji")          ;Kanji, Kanji convert
(define-gtkkey #xff22 "Muhenkan")       ;Cancel Conversion
(define-gtkkey #xff23 "Henkan_Mode")    ;Start/Stop Conversion
(define-gtkkey #xff23 "Henkan")         ;Alias for Henkan_Mode
(define-gtkkey #xff24 "Romaji")         ;to Romaji
(define-gtkkey #xff25 "Hiragana")       ;to Hiragana
(define-gtkkey #xff26 "Katakana")       ;to Katakana
(define-gtkkey #xff27 "Hiragana_Katakana") ;Hiragana/Katakana toggle
(define-gtkkey #xff28 "Zenkaku")        ;to Zenkaku
(define-gtkkey #xff29 "Hankaku")        ;to Hankaku
(define-gtkkey #xff2a "Zenkaku_Hankaku") ;Zenkaku/Hankaku toggle
(define-gtkkey #xff2b "Touroku")        ;Add to Dictionary
(define-gtkkey #xff2c "Massyo")         ;Delete from Dictionary
(define-gtkkey #xff2d "Kana_Lock")      ;Kana Lock
(define-gtkkey #xff2e "Kana_Shift")     ;Kana Shift
(define-gtkkey #xff2f "Eisu_Shift")     ;Alphanumeric Shift
(define-gtkkey #xff30 "Eisu_toggle")    ;Alphanumeric toggle
(define-gtkkey #xff37 "Kanji_Bangou")   ;Codeinput
(define-gtkkey #xff3d "Zen_Koho")       ;Multiple/All Candidate(s)
(define-gtkkey #xff3e "Mae_Koho")       ;Previous Candidate
(define-gtkkey #xff50 "Home")
(define-gtkkey #xff51 "Left")           ;Move left, left arrow
(define-gtkkey #xff52 "Up")             ;Move up, up arrow
(define-gtkkey #xff53 "Right")          ;Move right, right arrow
(define-gtkkey #xff54 "Down")           ;Move down, down arrow
(define-gtkkey #xff55 "Prior")          ;Prior, previous
(define-gtkkey #xff55 "Page_Up")
(define-gtkkey #xff56 "Next")           ;Next
(define-gtkkey #xff56 "Page_Down")
(define-gtkkey #xff57 "End")            ;EOL
(define-gtkkey #xff58 "Begin")          ;BOL
(define-gtkkey #xff60 "Select")         ;Select, mark
(define-gtkkey #xff61 "Print")
(define-gtkkey #xff62 "Execute")        ;Execute, run, do
(define-gtkkey #xff63 "Insert")         ;Insert, insert here
(define-gtkkey #xff65 "Undo")
(define-gtkkey #xff66 "Redo")           ;Redo, again
(define-gtkkey #xff67 "Menu")
(define-gtkkey #xff68 "Find")           ;Find, search
(define-gtkkey #xff69 "Cancel")         ;Cancel, stop, abort, exit
(define-gtkkey #xff6a "Help")           ;Help
(define-gtkkey #xff6b "Break")
(define-gtkkey #xff7e "Mode_switch")    ;Character set switch
(define-gtkkey #xff7e "script_switch")  ;Alias for mode_switch
(define-gtkkey #xff7f "Num_Lock")
(define-gtkkey #xff80 "KP_Space")       ;Space
(define-gtkkey #xff89 "KP_Tab")
(define-gtkkey #xff8d "KP_Enter")       ;Enter
(define-gtkkey #xff91 "KP_F1")          ;PF1, KP_A, ...
(define-gtkkey #xff92 "KP_F2")
(define-gtkkey #xff93 "KP_F3")
(define-gtkkey #xff94 "KP_F4")
(define-gtkkey #xff95 "KP_Home")
(define-gtkkey #xff96 "KP_Left")
(define-gtkkey #xff97 "KP_Up")
(define-gtkkey #xff98 "KP_Right")
(define-gtkkey #xff99 "KP_Down")
(define-gtkkey #xff9a "KP_Prior")
(define-gtkkey #xff9a "KP_Page_Up")
(define-gtkkey #xff9b "KP_Next")
(define-gtkkey #xff9b "KP_Page_Down")
(define-gtkkey #xff9c "KP_End")
(define-gtkkey #xff9d "KP_Begin")
(define-gtkkey #xff9e "KP_Insert")
(define-gtkkey #xff9f "KP_Delete")
(define-gtkkey #xffbd "KP_Equal")       ;Equals
(define-gtkkey #xffaa "KP_Multiply")
(define-gtkkey #xffab "KP_Add")
(define-gtkkey #xffac "KP_Separator")   ;Separator, often comma
(define-gtkkey #xffad "KP_Subtract")
(define-gtkkey #xffae "KP_Decimal")
(define-gtkkey #xffaf "KP_Divide")
(define-gtkkey #xffb0 "KP_0")
(define-gtkkey #xffb1 "KP_1")
(define-gtkkey #xffb2 "KP_2")
(define-gtkkey #xffb3 "KP_3")
(define-gtkkey #xffb4 "KP_4")
(define-gtkkey #xffb5 "KP_5")
(define-gtkkey #xffb6 "KP_6")
(define-gtkkey #xffb7 "KP_7")
(define-gtkkey #xffb8 "KP_8")
(define-gtkkey #xffb9 "KP_9")
(define-gtkkey #xffbe "F1")
(define-gtkkey #xffbf "F2")
(define-gtkkey #xffc0 "F3")
(define-gtkkey #xffc1 "F4")
(define-gtkkey #xffc2 "F5")
(define-gtkkey #xffc3 "F6")
(define-gtkkey #xffc4 "F7")
(define-gtkkey #xffc5 "F8")
(define-gtkkey #xffc6 "F9")
(define-gtkkey #xffc7 "F10")
(define-gtkkey #xffc8 "F11")
(define-gtkkey #xffc9 "F12")
(define-gtkkey #xffca "F13")
(define-gtkkey #xffcb "F14")
(define-gtkkey #xffcc "F15")
(define-gtkkey #xffcd "F16")
(define-gtkkey #xffce "F17")
(define-gtkkey #xffcf "F18")
(define-gtkkey #xffd0 "F19")
(define-gtkkey #xffd1 "F20")
(define-gtkkey #xffd2 "F21")
(define-gtkkey #xffd3 "F22")
(define-gtkkey #xffd4 "F23")
(define-gtkkey #xffd5 "F24")
(define-gtkkey #xffd6 "F25")
(define-gtkkey #xffd7 "F26")
(define-gtkkey #xffd8 "F27")
(define-gtkkey #xffd9 "F28")
(define-gtkkey #xffda "F29")
(define-gtkkey #xffdb "F30")
(define-gtkkey #xffdc "F31")
(define-gtkkey #xffdd "F32")
(define-gtkkey #xffde "F33")
(define-gtkkey #xffdf "F34")
(define-gtkkey #xffe0 "F35")
(define-gtkkey #xffe1 "Shift_L")        ;Left shift
(define-gtkkey #xffe2 "Shift_R")        ;Right shift
(define-gtkkey #xffe3 "Control_L")      ;Left control
(define-gtkkey #xffe4 "Control_R")      ;Right control
(define-gtkkey #xffe5 "Caps_Lock")      ;Caps lock
(define-gtkkey #xffe6 "Shift_Lock")     ;Shift lock
(define-gtkkey #xffe7 "Meta_L")         ;Left meta
(define-gtkkey #xffe8 "Meta_R")         ;Right meta
(define-gtkkey #xffe9 "Alt_L")          ;Left alt
(define-gtkkey #xffea "Alt_R")          ;Right alt
(define-gtkkey #xffeb "Super_L")        ;Left super
(define-gtkkey #xffec "Super_R")        ;Right super
(define-gtkkey #xffed "Hyper_L")        ;Left hyper
(define-gtkkey #xffee "Hyper_R")        ;Right hyper
(define-gtkkey #xfe01 "ISO_Lock")
(define-gtkkey #xfe02 "ISO_Level2_Latch")
(define-gtkkey #xfe03 "ISO_Level3_Shift")
(define-gtkkey #xfe04 "ISO_Level3_Latch")
(define-gtkkey #xfe05 "ISO_Level3_Lock")
(define-gtkkey #xff7e "ISO_Group_Shift") ;Alias for mode_switch
(define-gtkkey #xfe06 "ISO_Group_Latch")
(define-gtkkey #xfe07 "ISO_Group_Lock")
(define-gtkkey #xfe08 "ISO_Next_Group")
(define-gtkkey #xfe09 "ISO_Next_Group_Lock")
(define-gtkkey #xfe0a "ISO_Prev_Group")
(define-gtkkey #xfe0b "ISO_Prev_Group_Lock")
(define-gtkkey #xfe0c "ISO_First_Group")
(define-gtkkey #xfe0d "ISO_First_Group_Lock")
(define-gtkkey #xfe0e "ISO_Last_Group")
(define-gtkkey #xfe0f "ISO_Last_Group_Lock")
(define-gtkkey #xfe20 "ISO_Left_Tab")
(define-gtkkey #xfe21 "ISO_Move_Line_Up")
(define-gtkkey #xfe22 "ISO_Move_Line_Down")
(define-gtkkey #xfe23 "ISO_Partial_Line_Up")
(define-gtkkey #xfe24 "ISO_Partial_Line_Down")
(define-gtkkey #xfe25 "ISO_Partial_Space_Left")
(define-gtkkey #xfe26 "ISO_Partial_Space_Right")
(define-gtkkey #xfe27 "ISO_Set_Margin_Left")
(define-gtkkey #xfe28 "ISO_Set_Margin_Right")
(define-gtkkey #xfe29 "ISO_Release_Margin_Left")
(define-gtkkey #xfe2a "ISO_Release_Margin_Right")
(define-gtkkey #xfe2b "ISO_Release_Both_Margins")
(define-gtkkey #xfe2c "ISO_Fast_Cursor_Left")
(define-gtkkey #xfe2d "ISO_Fast_Cursor_Right")
(define-gtkkey #xfe2e "ISO_Fast_Cursor_Up")
(define-gtkkey #xfe2f "ISO_Fast_Cursor_Down")
(define-gtkkey #xfe30 "ISO_Continuous_Underline")
(define-gtkkey #xfe31 "ISO_Discontinuous_Underline")
(define-gtkkey #xfe32 "ISO_Emphasize")
(define-gtkkey #xfe33 "ISO_Center_Object")
(define-gtkkey #xfe34 "ISO_Enter")
(define-gtkkey #xfe50 "dead_grave")
(define-gtkkey #xfe51 "dead_acute")
(define-gtkkey #xfe52 "dead_circumflex")
(define-gtkkey #xfe53 "dead_tilde")
(define-gtkkey #xfe54 "dead_macron")
(define-gtkkey #xfe55 "dead_breve")
(define-gtkkey #xfe56 "dead_abovedot")
(define-gtkkey #xfe57 "dead_diaeresis")
(define-gtkkey #xfe58 "dead_abovering")
(define-gtkkey #xfe59 "dead_doubleacute")
(define-gtkkey #xfe5a "dead_caron")
(define-gtkkey #xfe5b "dead_cedilla")
(define-gtkkey #xfe5c "dead_ogonek")
(define-gtkkey #xfe5d "dead_iota")
(define-gtkkey #xfe5e "dead_voiced_sound")
(define-gtkkey #xfe5f "dead_semivoiced_sound")
(define-gtkkey #xfe60 "dead_belowdot")
(define-gtkkey #xfe61 "dead_hook")
(define-gtkkey #xfe62 "dead_horn")
(define-gtkkey #xfed0 "First_Virtual_Screen")
(define-gtkkey #xfed1 "Prev_Virtual_Screen")
(define-gtkkey #xfed2 "Next_Virtual_Screen")
(define-gtkkey #xfed4 "Last_Virtual_Screen")
(define-gtkkey #xfed5 "Terminate_Server")
(define-gtkkey #xfe70 "AccessX_Enable")
(define-gtkkey #xfe71 "AccessX_Feedback_Enable")
(define-gtkkey #xfe72 "RepeatKeys_Enable")
(define-gtkkey #xfe73 "SlowKeys_Enable")
(define-gtkkey #xfe74 "BounceKeys_Enable")
(define-gtkkey #xfe75 "StickyKeys_Enable")
(define-gtkkey #xfe76 "MouseKeys_Enable")
(define-gtkkey #xfe77 "MouseKeys_Accel_Enable")
(define-gtkkey #xfe78 "Overlay1_Enable")
(define-gtkkey #xfe79 "Overlay2_Enable")
(define-gtkkey #xfe7a "AudibleBell_Enable")
(define-gtkkey #xfee0 "Pointer_Left")
(define-gtkkey #xfee1 "Pointer_Right")
(define-gtkkey #xfee2 "Pointer_Up")
(define-gtkkey #xfee3 "Pointer_Down")
(define-gtkkey #xfee4 "Pointer_UpLeft")
(define-gtkkey #xfee5 "Pointer_UpRight")
(define-gtkkey #xfee6 "Pointer_DownLeft")
(define-gtkkey #xfee7 "Pointer_DownRight")
(define-gtkkey #xfee8 "Pointer_Button_Dflt")
(define-gtkkey #xfee9 "Pointer_Button1")
(define-gtkkey #xfeea "Pointer_Button2")
(define-gtkkey #xfeeb "Pointer_Button3")
(define-gtkkey #xfeec "Pointer_Button4")
(define-gtkkey #xfeed "Pointer_Button5")
(define-gtkkey #xfeee "Pointer_DblClick_Dflt")
(define-gtkkey #xfeef "Pointer_DblClick1")
(define-gtkkey #xfef0 "Pointer_DblClick2")
(define-gtkkey #xfef1 "Pointer_DblClick3")
(define-gtkkey #xfef2 "Pointer_DblClick4")
(define-gtkkey #xfef3 "Pointer_DblClick5")
(define-gtkkey #xfef4 "Pointer_Drag_Dflt")
(define-gtkkey #xfef5 "Pointer_Drag1")
(define-gtkkey #xfef6 "Pointer_Drag2")
(define-gtkkey #xfef7 "Pointer_Drag3")
(define-gtkkey #xfef8 "Pointer_Drag4")
(define-gtkkey #xfefd "Pointer_Drag5")
(define-gtkkey #xfef9 "Pointer_EnableKeys")
(define-gtkkey #xfefa "Pointer_Accelerate")
(define-gtkkey #xfefb "Pointer_DfltBtnNext")
(define-gtkkey #xfefc "Pointer_DfltBtnPrev")
(define-gtkkey #xfd01 "3270_Duplicate")
(define-gtkkey #xfd02 "3270_FieldMark")
(define-gtkkey #xfd03 "3270_Right2")
(define-gtkkey #xfd04 "3270_Left2")
(define-gtkkey #xfd05 "3270_BackTab")
(define-gtkkey #xfd06 "3270_EraseEOF")
(define-gtkkey #xfd07 "3270_EraseInput")
(define-gtkkey #xfd08 "3270_Reset")
(define-gtkkey #xfd09 "3270_Quit")
(define-gtkkey #xfd0a "3270_PA1")
(define-gtkkey #xfd0b "3270_PA2")
(define-gtkkey #xfd0c "3270_PA3")
(define-gtkkey #xfd0d "3270_Test")
(define-gtkkey #xfd0e "3270_Attn")
(define-gtkkey #xfd0f "3270_CursorBlink")
(define-gtkkey #xfd10 "3270_AltCursor")
(define-gtkkey #xfd11 "3270_KeyClick")
(define-gtkkey #xfd12 "3270_Jump")
(define-gtkkey #xfd13 "3270_Ident")
(define-gtkkey #xfd14 "3270_Rule")
(define-gtkkey #xfd15 "3270_Copy")
(define-gtkkey #xfd16 "3270_Play")
(define-gtkkey #xfd17 "3270_Setup")
(define-gtkkey #xfd18 "3270_Record")
(define-gtkkey #xfd19 "3270_ChangeScreen")
(define-gtkkey #xfd1a "3270_DeleteWord")
(define-gtkkey #xfd1b "3270_ExSelect")
(define-gtkkey #xfd1c "3270_CursorSelect")
(define-gtkkey #xfd1d "3270_PrintScreen")
(define-gtkkey #xfd1e "3270_Enter")



(define-gtkkey #x0030 "0")              ;U+0030 DIGIT ZERO
(define-gtkkey #x0031 "1")              ;U+0031 DIGIT ONE
(define-gtkkey #x0032 "2")              ;U+0032 DIGIT TWO
(define-gtkkey #x0033 "3")              ;U+0033 DIGIT THREE
(define-gtkkey #x0034 "4")              ;U+0034 DIGIT FOUR
(define-gtkkey #x0035 "5")              ;U+0035 DIGIT FIVE
(define-gtkkey #x0036 "6")              ;U+0036 DIGIT SIX
(define-gtkkey #x0037 "7")              ;U+0037 DIGIT SEVEN
(define-gtkkey #x0038 "8")              ;U+0038 DIGIT EIGHT
(define-gtkkey #x0039 "9")              ;U+0039 DIGIT NINE

(define-gtkkey #x0041 "A")              ;U+0041 LATIN CAPITAL LETTER A
(define-gtkkey #x0042 "B")              ;U+0042 LATIN CAPITAL LETTER B
(define-gtkkey #x0043 "C")              ;U+0043 LATIN CAPITAL LETTER C
(define-gtkkey #x0044 "D")              ;U+0044 LATIN CAPITAL LETTER D
(define-gtkkey #x0045 "E")              ;U+0045 LATIN CAPITAL LETTER E
(define-gtkkey #x0046 "F")              ;U+0046 LATIN CAPITAL LETTER F
(define-gtkkey #x0047 "G")              ;U+0047 LATIN CAPITAL LETTER G
(define-gtkkey #x0048 "H")              ;U+0048 LATIN CAPITAL LETTER H
(define-gtkkey #x0049 "I")              ;U+0049 LATIN CAPITAL LETTER I
(define-gtkkey #x004a "J")              ;U+004A LATIN CAPITAL LETTER J
(define-gtkkey #x004b "K")              ;U+004B LATIN CAPITAL LETTER K
(define-gtkkey #x004c "L")              ;U+004C LATIN CAPITAL LETTER L
(define-gtkkey #x004d "M")              ;U+004D LATIN CAPITAL LETTER M
(define-gtkkey #x004e "N")              ;U+004E LATIN CAPITAL LETTER N
(define-gtkkey #x004f "O")              ;U+004F LATIN CAPITAL LETTER O
(define-gtkkey #x0050 "P")              ;U+0050 LATIN CAPITAL LETTER P
(define-gtkkey #x0051 "Q")              ;U+0051 LATIN CAPITAL LETTER Q
(define-gtkkey #x0052 "R")              ;U+0052 LATIN CAPITAL LETTER R
(define-gtkkey #x0053 "S")              ;U+0053 LATIN CAPITAL LETTER S
(define-gtkkey #x0054 "T")              ;U+0054 LATIN CAPITAL LETTER T
(define-gtkkey #x0055 "U")              ;U+0055 LATIN CAPITAL LETTER U
(define-gtkkey #x0056 "V")              ;U+0056 LATIN CAPITAL LETTER V
(define-gtkkey #x0057 "W")              ;U+0057 LATIN CAPITAL LETTER W
(define-gtkkey #x0058 "X")              ;U+0058 LATIN CAPITAL LETTER X
(define-gtkkey #x0059 "Y")              ;U+0059 LATIN CAPITAL LETTER Y
(define-gtkkey #x005a "Z")              ;U+005A LATIN CAPITAL LETTER Z
(define-gtkkey #x0061 "a")              ;U+0061 LATIN SMALL LETTER A
(define-gtkkey #x0062 "b")              ;U+0062 LATIN SMALL LETTER B
(define-gtkkey #x0063 "c")              ;U+0063 LATIN SMALL LETTER C
(define-gtkkey #x0064 "d")              ;U+0064 LATIN SMALL LETTER D
(define-gtkkey #x0065 "e")              ;U+0065 LATIN SMALL LETTER E
(define-gtkkey #x0066 "f")              ;U+0066 LATIN SMALL LETTER F
(define-gtkkey #x0067 "g")              ;U+0067 LATIN SMALL LETTER G
(define-gtkkey #x0068 "h")              ;U+0068 LATIN SMALL LETTER H
(define-gtkkey #x0069 "i")              ;U+0069 LATIN SMALL LETTER I
(define-gtkkey #x006a "j")              ;U+006A LATIN SMALL LETTER J
(define-gtkkey #x006b "k")              ;U+006B LATIN SMALL LETTER K
(define-gtkkey #x006c "l")              ;U+006C LATIN SMALL LETTER L
(define-gtkkey #x006d "m")              ;U+006D LATIN SMALL LETTER M
(define-gtkkey #x006e "n")              ;U+006E LATIN SMALL LETTER N
(define-gtkkey #x006f "o")              ;U+006F LATIN SMALL LETTER O
(define-gtkkey #x0070 "p")              ;U+0070 LATIN SMALL LETTER P
(define-gtkkey #x0071 "q")              ;U+0071 LATIN SMALL LETTER Q
(define-gtkkey #x0072 "r")              ;U+0072 LATIN SMALL LETTER R
(define-gtkkey #x0073 "s")              ;U+0073 LATIN SMALL LETTER S
(define-gtkkey #x0074 "t")              ;U+0074 LATIN SMALL LETTER T
(define-gtkkey #x0075 "u")              ;U+0075 LATIN SMALL LETTER U
(define-gtkkey #x0076 "v")              ;U+0076 LATIN SMALL LETTER V
(define-gtkkey #x0077 "w")              ;U+0077 LATIN SMALL LETTER W
(define-gtkkey #x0078 "x")              ;U+0078 LATIN SMALL LETTER X
(define-gtkkey #x0079 "y")              ;U+0079 LATIN SMALL LETTER Y
(define-gtkkey #x007a "z")              ;U+007A LATIN SMALL LETTER Z
(define-gtkkey #x00a0 "nobreakspace")   ;U+00A0 NO-BREAK SPACE
(define-gtkkey #x00a1 "exclamdown")  ;U+00A1 INVERTED EXCLAMATION MARK
(define-gtkkey #x00a2 "cent")           ;U+00A2 CENT SIGN
(define-gtkkey #x00a3 "sterling")       ;U+00A3 POUND SIGN
(define-gtkkey #x00a4 "currency")       ;U+00A4 CURRENCY SIGN
(define-gtkkey #x00a5 "yen")            ;U+00A5 YEN SIGN
(define-gtkkey #x00a6 "brokenbar")      ;U+00A6 BROKEN BAR
(define-gtkkey #x00a7 "section")        ;U+00A7 SECTION SIGN
(define-gtkkey #x00a8 "diaeresis")      ;U+00A8 DIAERESIS
(define-gtkkey #x00a9 "copyright")      ;U+00A9 COPYRIGHT SIGN
(define-gtkkey #x00aa "ordfeminine") ;U+00AA FEMININE ORDINAL INDICATOR
(define-gtkkey #x00ab "guillemotleft") ;U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-gtkkey #x00ac "notsign")        ;U+00AC NOT SIGN
(define-gtkkey #x00ad "hyphen")         ;U+00AD SOFT HYPHEN
(define-gtkkey #x00ae "registered")     ;U+00AE REGISTERED SIGN
(define-gtkkey #x00af "macron")         ;U+00AF MACRON
(define-gtkkey #x00b0 "degree")         ;U+00B0 DEGREE SIGN
(define-gtkkey #x00b1 "plusminus")      ;U+00B1 PLUS-MINUS SIGN
(define-gtkkey #x00b2 "twosuperior")    ;U+00B2 SUPERSCRIPT TWO
(define-gtkkey #x00b3 "threesuperior")  ;U+00B3 SUPERSCRIPT THREE
(define-gtkkey #x00b4 "acute")          ;U+00B4 ACUTE ACCENT
(define-gtkkey #x00b5 "mu")             ;U+00B5 MICRO SIGN
(define-gtkkey #x00b6 "paragraph")      ;U+00B6 PILCROW SIGN
(define-gtkkey #x00b7 "periodcentered") ;U+00B7 MIDDLE DOT
(define-gtkkey #x00b8 "cedilla")        ;U+00B8 CEDILLA
(define-gtkkey #x00b9 "onesuperior")    ;U+00B9 SUPERSCRIPT ONE
(define-gtkkey #x00ba "masculine") ;U+00BA MASCULINE ORDINAL INDICATOR
(define-gtkkey #x00bb "guillemotright") ;U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-gtkkey #x00bc "onequarter") ;U+00BC VULGAR FRACTION ONE QUARTER
(define-gtkkey #x00bd "onehalf")      ;U+00BD VULGAR FRACTION ONE HALF
(define-gtkkey #x00be "threequarters") ;U+00BE VULGAR FRACTION THREE QUARTERS
(define-gtkkey #x00bf "questiondown")   ;U+00BF INVERTED QUESTION MARK
(define-gtkkey #x00c0 "Agrave") ;U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
(define-gtkkey #x00c1 "Aacute") ;U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
(define-gtkkey #x00c2 "Acircumflex") ;U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
(define-gtkkey #x00c3 "Atilde") ;U+00C3 LATIN CAPITAL LETTER A WITH TILDE
(define-gtkkey #x00c4 "Adiaeresis") ;U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
(define-gtkkey #x00c5 "Aring") ;U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
(define-gtkkey #x00c6 "AE")            ;U+00C6 LATIN CAPITAL LETTER AE
(define-gtkkey #x00c7 "Ccedilla") ;U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
(define-gtkkey #x00c8 "Egrave") ;U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
(define-gtkkey #x00c9 "Eacute") ;U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
(define-gtkkey #x00ca "Ecircumflex") ;U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
(define-gtkkey #x00cb "Ediaeresis") ;U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
(define-gtkkey #x00cc "Igrave") ;U+00CC LATIN CAPITAL LETTER I WITH GRAVE
(define-gtkkey #x00cd "Iacute") ;U+00CD LATIN CAPITAL LETTER I WITH ACUTE
(define-gtkkey #x00ce "Icircumflex") ;U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
(define-gtkkey #x00cf "Idiaeresis") ;U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
(define-gtkkey #x00d0 "ETH")          ;U+00D0 LATIN CAPITAL LETTER ETH
(define-gtkkey #x00d0 "Eth")            ;deprecated
(define-gtkkey #x00d1 "Ntilde") ;U+00D1 LATIN CAPITAL LETTER N WITH TILDE
(define-gtkkey #x00d2 "Ograve") ;U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
(define-gtkkey #x00d3 "Oacute") ;U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
(define-gtkkey #x00d4 "Ocircumflex") ;U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
(define-gtkkey #x00d5 "Otilde") ;U+00D5 LATIN CAPITAL LETTER O WITH TILDE
(define-gtkkey #x00d6 "Odiaeresis") ;U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
(define-gtkkey #x00d7 "multiply")       ;U+00D7 MULTIPLICATION SIGN
(define-gtkkey #x00d8 "Oslash") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-gtkkey #x00d8 "Ooblique") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-gtkkey #x00d9 "Ugrave") ;U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
(define-gtkkey #x00da "Uacute") ;U+00DA LATIN CAPITAL LETTER U WITH ACUTE
(define-gtkkey #x00db "Ucircumflex") ;U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
(define-gtkkey #x00dc "Udiaeresis") ;U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
(define-gtkkey #x00dd "Yacute") ;U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
(define-gtkkey #x00de "THORN")      ;U+00DE LATIN CAPITAL LETTER THORN
(define-gtkkey #x00de "Thorn")          ;deprecated
(define-gtkkey #x00df "ssharp")     ;U+00DF LATIN SMALL LETTER SHARP S
(define-gtkkey #x00e0 "agrave") ;U+00E0 LATIN SMALL LETTER A WITH GRAVE
(define-gtkkey #x00e1 "aacute") ;U+00E1 LATIN SMALL LETTER A WITH ACUTE
(define-gtkkey #x00e2 "acircumflex") ;U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX
(define-gtkkey #x00e3 "atilde") ;U+00E3 LATIN SMALL LETTER A WITH TILDE
(define-gtkkey #x00e4 "adiaeresis") ;U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
(define-gtkkey #x00e5 "aring") ;U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
(define-gtkkey #x00e6 "ae")             ;U+00E6 LATIN SMALL LETTER AE
(define-gtkkey #x00e7 "ccedilla") ;U+00E7 LATIN SMALL LETTER C WITH CEDILLA
(define-gtkkey #x00e8 "egrave") ;U+00E8 LATIN SMALL LETTER E WITH GRAVE
(define-gtkkey #x00e9 "eacute") ;U+00E9 LATIN SMALL LETTER E WITH ACUTE
(define-gtkkey #x00ea "ecircumflex") ;U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
(define-gtkkey #x00eb "ediaeresis") ;U+00EB LATIN SMALL LETTER E WITH DIAERESIS
(define-gtkkey #x00ec "igrave") ;U+00EC LATIN SMALL LETTER I WITH GRAVE
(define-gtkkey #x00ed "iacute") ;U+00ED LATIN SMALL LETTER I WITH ACUTE
(define-gtkkey #x00ee "icircumflex") ;U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
(define-gtkkey #x00ef "idiaeresis") ;U+00EF LATIN SMALL LETTER I WITH DIAERESIS
(define-gtkkey #x00f0 "eth")            ;U+00F0 LATIN SMALL LETTER ETH
(define-gtkkey #x00f1 "ntilde") ;U+00F1 LATIN SMALL LETTER N WITH TILDE
(define-gtkkey #x00f2 "ograve") ;U+00F2 LATIN SMALL LETTER O WITH GRAVE
(define-gtkkey #x00f3 "oacute") ;U+00F3 LATIN SMALL LETTER O WITH ACUTE
(define-gtkkey #x00f4 "ocircumflex") ;U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
(define-gtkkey #x00f5 "otilde") ;U+00F5 LATIN SMALL LETTER O WITH TILDE
(define-gtkkey #x00f6 "odiaeresis") ;U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
(define-gtkkey #x00f7 "division")       ;U+00F7 DIVISION SIGN
(define-gtkkey #x00f8 "oslash") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-gtkkey #x00f8 "ooblique") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-gtkkey #x00f9 "ugrave") ;U+00F9 LATIN SMALL LETTER U WITH GRAVE
(define-gtkkey #x00fa "uacute") ;U+00FA LATIN SMALL LETTER U WITH ACUTE
(define-gtkkey #x00fb "ucircumflex") ;U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
(define-gtkkey #x00fc "udiaeresis") ;U+00FC LATIN SMALL LETTER U WITH DIAERESIS
(define-gtkkey #x00fd "yacute") ;U+00FD LATIN SMALL LETTER Y WITH ACUTE
(define-gtkkey #x00fe "thorn")        ;U+00FE LATIN SMALL LETTER THORN
(define-gtkkey #x00ff "ydiaeresis") ;U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
(define-gtkkey #x01a1 "Aogonek") ;U+0104 LATIN CAPITAL LETTER A WITH OGONEK
(define-gtkkey #x01a2 "breve")          ;U+02D8 BREVE
(define-gtkkey #x01a3 "Lstroke") ;U+0141 LATIN CAPITAL LETTER L WITH STROKE
(define-gtkkey #x01a5 "Lcaron") ;U+013D LATIN CAPITAL LETTER L WITH CARON
(define-gtkkey #x01a6 "Sacute") ;U+015A LATIN CAPITAL LETTER S WITH ACUTE
(define-gtkkey #x01a9 "Scaron") ;U+0160 LATIN CAPITAL LETTER S WITH CARON
(define-gtkkey #x01aa "Scedilla") ;U+015E LATIN CAPITAL LETTER S WITH CEDILLA
(define-gtkkey #x01ab "Tcaron") ;U+0164 LATIN CAPITAL LETTER T WITH CARON
(define-gtkkey #x01ac "Zacute") ;U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
(define-gtkkey #x01ae "Zcaron") ;U+017D LATIN CAPITAL LETTER Z WITH CARON
(define-gtkkey #x01af "Zabovedot") ;U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
(define-gtkkey #x01b1 "aogonek") ;U+0105 LATIN SMALL LETTER A WITH OGONEK
(define-gtkkey #x01b2 "ogonek")         ;U+02DB OGONEK
(define-gtkkey #x01b3 "lstroke") ;U+0142 LATIN SMALL LETTER L WITH STROKE
(define-gtkkey #x01b5 "lcaron") ;U+013E LATIN SMALL LETTER L WITH CARON
(define-gtkkey #x01b6 "sacute") ;U+015B LATIN SMALL LETTER S WITH ACUTE
(define-gtkkey #x01b7 "caron")          ;U+02C7 CARON
(define-gtkkey #x01b9 "scaron") ;U+0161 LATIN SMALL LETTER S WITH CARON
(define-gtkkey #x01ba "scedilla") ;U+015F LATIN SMALL LETTER S WITH CEDILLA
(define-gtkkey #x01bb "tcaron") ;U+0165 LATIN SMALL LETTER T WITH CARON
(define-gtkkey #x01bc "zacute") ;U+017A LATIN SMALL LETTER Z WITH ACUTE
(define-gtkkey #x01bd "doubleacute")    ;U+02DD DOUBLE ACUTE ACCENT
(define-gtkkey #x01be "zcaron") ;U+017E LATIN SMALL LETTER Z WITH CARON
(define-gtkkey #x01bf "zabovedot") ;U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
(define-gtkkey #x01c0 "Racute") ;U+0154 LATIN CAPITAL LETTER R WITH ACUTE
(define-gtkkey #x01c3 "Abreve") ;U+0102 LATIN CAPITAL LETTER A WITH BREVE
(define-gtkkey #x01c5 "Lacute") ;U+0139 LATIN CAPITAL LETTER L WITH ACUTE
(define-gtkkey #x01c6 "Cacute") ;U+0106 LATIN CAPITAL LETTER C WITH ACUTE
(define-gtkkey #x01c8 "Ccaron") ;U+010C LATIN CAPITAL LETTER C WITH CARON
(define-gtkkey #x01ca "Eogonek") ;U+0118 LATIN CAPITAL LETTER E WITH OGONEK
(define-gtkkey #x01cc "Ecaron") ;U+011A LATIN CAPITAL LETTER E WITH CARON
(define-gtkkey #x01cf "Dcaron") ;U+010E LATIN CAPITAL LETTER D WITH CARON
(define-gtkkey #x01d0 "Dstroke") ;U+0110 LATIN CAPITAL LETTER D WITH STROKE
(define-gtkkey #x01d1 "Nacute") ;U+0143 LATIN CAPITAL LETTER N WITH ACUTE
(define-gtkkey #x01d2 "Ncaron") ;U+0147 LATIN CAPITAL LETTER N WITH CARON
(define-gtkkey #x01d5 "Odoubleacute") ;U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
(define-gtkkey #x01d8 "Rcaron") ;U+0158 LATIN CAPITAL LETTER R WITH CARON
(define-gtkkey #x01d9 "Uring") ;U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
(define-gtkkey #x01db "Udoubleacute") ;U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
(define-gtkkey #x01de "Tcedilla") ;U+0162 LATIN CAPITAL LETTER T WITH CEDILLA
(define-gtkkey #x01e0 "racute") ;U+0155 LATIN SMALL LETTER R WITH ACUTE
(define-gtkkey #x01e3 "abreve") ;U+0103 LATIN SMALL LETTER A WITH BREVE
(define-gtkkey #x01e5 "lacute") ;U+013A LATIN SMALL LETTER L WITH ACUTE
(define-gtkkey #x01e6 "cacute") ;U+0107 LATIN SMALL LETTER C WITH ACUTE
(define-gtkkey #x01e8 "ccaron") ;U+010D LATIN SMALL LETTER C WITH CARON
(define-gtkkey #x01ea "eogonek") ;U+0119 LATIN SMALL LETTER E WITH OGONEK
(define-gtkkey #x01ec "ecaron") ;U+011B LATIN SMALL LETTER E WITH CARON
(define-gtkkey #x01ef "dcaron") ;U+010F LATIN SMALL LETTER D WITH CARON
(define-gtkkey #x01f0 "dstroke") ;U+0111 LATIN SMALL LETTER D WITH STROKE
(define-gtkkey #x01f1 "nacute") ;U+0144 LATIN SMALL LETTER N WITH ACUTE
(define-gtkkey #x01f2 "ncaron") ;U+0148 LATIN SMALL LETTER N WITH CARON
(define-gtkkey #x01f5 "odoubleacute") ;U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
(define-gtkkey #x01fb "udoubleacute") ;U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
(define-gtkkey #x01f8 "rcaron") ;U+0159 LATIN SMALL LETTER R WITH CARON
(define-gtkkey #x01f9 "uring") ;U+016F LATIN SMALL LETTER U WITH RING ABOVE
(define-gtkkey #x01fe "tcedilla") ;U+0163 LATIN SMALL LETTER T WITH CEDILLA
(define-gtkkey #x01ff "abovedot")       ;U+02D9 DOT ABOVE
(define-gtkkey #x02a1 "Hstroke") ;U+0126 LATIN CAPITAL LETTER H WITH STROKE
(define-gtkkey #x02a6 "Hcircumflex") ;U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX
(define-gtkkey #x02a9 "Iabovedot") ;U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
(define-gtkkey #x02ab "Gbreve") ;U+011E LATIN CAPITAL LETTER G WITH BREVE
(define-gtkkey #x02ac "Jcircumflex") ;U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX
(define-gtkkey #x02b1 "hstroke") ;U+0127 LATIN SMALL LETTER H WITH STROKE
(define-gtkkey #x02b6 "hcircumflex") ;U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX
(define-gtkkey #x02b9 "idotless") ;U+0131 LATIN SMALL LETTER DOTLESS I
(define-gtkkey #x02bb "gbreve") ;U+011F LATIN SMALL LETTER G WITH BREVE
(define-gtkkey #x02bc "jcircumflex") ;U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX
(define-gtkkey #x02c5 "Cabovedot") ;U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE
(define-gtkkey #x02c6 "Ccircumflex") ;U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX
(define-gtkkey #x02d5 "Gabovedot") ;U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE
(define-gtkkey #x02d8 "Gcircumflex") ;U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX
(define-gtkkey #x02dd "Ubreve") ;U+016C LATIN CAPITAL LETTER U WITH BREVE
(define-gtkkey #x02de "Scircumflex") ;U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX
(define-gtkkey #x02e5 "cabovedot") ;U+010B LATIN SMALL LETTER C WITH DOT ABOVE
(define-gtkkey #x02e6 "ccircumflex") ;U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX
(define-gtkkey #x02f5 "gabovedot") ;U+0121 LATIN SMALL LETTER G WITH DOT ABOVE
(define-gtkkey #x02f8 "gcircumflex") ;U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX
(define-gtkkey #x02fd "ubreve") ;U+016D LATIN SMALL LETTER U WITH BREVE
(define-gtkkey #x02fe "scircumflex") ;U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX
(define-gtkkey #x03a2 "kra")            ;U+0138 LATIN SMALL LETTER KRA
(define-gtkkey #x03a2 "kappa")          ;deprecated
(define-gtkkey #x03a3 "Rcedilla") ;U+0156 LATIN CAPITAL LETTER R WITH CEDILLA
(define-gtkkey #x03a5 "Itilde") ;U+0128 LATIN CAPITAL LETTER I WITH TILDE
(define-gtkkey #x03a6 "Lcedilla") ;U+013B LATIN CAPITAL LETTER L WITH CEDILLA
(define-gtkkey #x03aa "Emacron") ;U+0112 LATIN CAPITAL LETTER E WITH MACRON
(define-gtkkey #x03ab "Gcedilla") ;U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
(define-gtkkey #x03ac "Tslash") ;U+0166 LATIN CAPITAL LETTER T WITH STROKE
(define-gtkkey #x03b3 "rcedilla") ;U+0157 LATIN SMALL LETTER R WITH CEDILLA
(define-gtkkey #x03b5 "itilde") ;U+0129 LATIN SMALL LETTER I WITH TILDE
(define-gtkkey #x03b6 "lcedilla") ;U+013C LATIN SMALL LETTER L WITH CEDILLA
(define-gtkkey #x03ba "emacron") ;U+0113 LATIN SMALL LETTER E WITH MACRON
(define-gtkkey #x03bb "gcedilla") ;U+0123 LATIN SMALL LETTER G WITH CEDILLA
(define-gtkkey #x03bc "tslash") ;U+0167 LATIN SMALL LETTER T WITH STROKE
(define-gtkkey #x03bd "ENG")          ;U+014A LATIN CAPITAL LETTER ENG
(define-gtkkey #x03bf "eng")            ;U+014B LATIN SMALL LETTER ENG
(define-gtkkey #x03c0 "Amacron") ;U+0100 LATIN CAPITAL LETTER A WITH MACRON
(define-gtkkey #x03c7 "Iogonek") ;U+012E LATIN CAPITAL LETTER I WITH OGONEK
(define-gtkkey #x03cc "Eabovedot") ;U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE
(define-gtkkey #x03cf "Imacron") ;U+012A LATIN CAPITAL LETTER I WITH MACRON
(define-gtkkey #x03d1 "Ncedilla") ;U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
(define-gtkkey #x03d2 "Omacron") ;U+014C LATIN CAPITAL LETTER O WITH MACRON
(define-gtkkey #x03d3 "Kcedilla") ;U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
(define-gtkkey #x03d9 "Uogonek") ;U+0172 LATIN CAPITAL LETTER U WITH OGONEK
(define-gtkkey #x03dd "Utilde") ;U+0168 LATIN CAPITAL LETTER U WITH TILDE
(define-gtkkey #x03de "Umacron") ;U+016A LATIN CAPITAL LETTER U WITH MACRON
(define-gtkkey #x03e0 "amacron") ;U+0101 LATIN SMALL LETTER A WITH MACRON
(define-gtkkey #x03e7 "iogonek") ;U+012F LATIN SMALL LETTER I WITH OGONEK
(define-gtkkey #x03ec "eabovedot") ;U+0117 LATIN SMALL LETTER E WITH DOT ABOVE
(define-gtkkey #x03ef "imacron") ;U+012B LATIN SMALL LETTER I WITH MACRON
(define-gtkkey #x03f1 "ncedilla") ;U+0146 LATIN SMALL LETTER N WITH CEDILLA
(define-gtkkey #x03f2 "omacron") ;U+014D LATIN SMALL LETTER O WITH MACRON
(define-gtkkey #x03f3 "kcedilla") ;U+0137 LATIN SMALL LETTER K WITH CEDILLA
(define-gtkkey #x03f9 "uogonek") ;U+0173 LATIN SMALL LETTER U WITH OGONEK
(define-gtkkey #x03fd "utilde") ;U+0169 LATIN SMALL LETTER U WITH TILDE
(define-gtkkey #x03fe "umacron") ;U+016B LATIN SMALL LETTER U WITH MACRON
(define-gtkkey #x1001e02 "Babovedot") ;U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE
(define-gtkkey #x1001e03 "babovedot") ;U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE
(define-gtkkey #x1001e0a "Dabovedot") ;U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE
(define-gtkkey #x1001e80 "Wgrave") ;U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
(define-gtkkey #x1001e82 "Wacute") ;U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
(define-gtkkey #x1001e0b "dabovedot") ;U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE
(define-gtkkey #x1001ef2 "Ygrave") ;U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE
(define-gtkkey #x1001e1e "Fabovedot") ;U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE
(define-gtkkey #x1001e1f "fabovedot") ;U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE
(define-gtkkey #x1001e40 "Mabovedot") ;U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE
(define-gtkkey #x1001e41 "mabovedot") ;U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE
(define-gtkkey #x1001e56 "Pabovedot") ;U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE
(define-gtkkey #x1001e81 "wgrave") ;U+1E81 LATIN SMALL LETTER W WITH GRAVE
(define-gtkkey #x1001e57 "pabovedot") ;U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE
(define-gtkkey #x1001e83 "wacute") ;U+1E83 LATIN SMALL LETTER W WITH ACUTE
(define-gtkkey #x1001e60 "Sabovedot") ;U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE
(define-gtkkey #x1001ef3 "ygrave") ;U+1EF3 LATIN SMALL LETTER Y WITH GRAVE
(define-gtkkey #x1001e84 "Wdiaeresis") ;U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS
(define-gtkkey #x1001e85 "wdiaeresis") ;U+1E85 LATIN SMALL LETTER W WITH DIAERESIS
(define-gtkkey #x1001e61 "sabovedot") ;U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE
(define-gtkkey #x1000174 "Wcircumflex") ;U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX
(define-gtkkey #x1001e6a "Tabovedot") ;U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE
(define-gtkkey #x1000176 "Ycircumflex") ;U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
(define-gtkkey #x1000175 "wcircumflex") ;U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX
(define-gtkkey #x1001e6b "tabovedot") ;U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE
(define-gtkkey #x1000177 "ycircumflex") ;U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX
(define-gtkkey #x13bc "OE")          ;U+0152 LATIN CAPITAL LIGATURE OE
(define-gtkkey #x13bd "oe")            ;U+0153 LATIN SMALL LIGATURE OE
(define-gtkkey #x13be "Ydiaeresis") ;U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
(define-gtkkey #x047e "overline")       ;U+203E OVERLINE
(define-gtkkey #x04a1 "kana_fullstop")  ;U+3002 IDEOGRAPHIC FULL STOP
(define-gtkkey #x04a2 "kana_openingbracket") ;U+300C LEFT CORNER BRACKET
(define-gtkkey #x04a3 "kana_closingbracket") ;U+300D RIGHT CORNER BRACKET
(define-gtkkey #x04a4 "kana_comma")     ;U+3001 IDEOGRAPHIC COMMA
(define-gtkkey #x04a5 "kana_conjunctive") ;U+30FB KATAKANA MIDDLE DOT
(define-gtkkey #x04a5 "kana_middledot") ;deprecated
(define-gtkkey #x04a6 "kana_WO")        ;U+30F2 KATAKANA LETTER WO
(define-gtkkey #x04a7 "kana_a")        ;U+30A1 KATAKANA LETTER SMALL A
(define-gtkkey #x04a8 "kana_i")        ;U+30A3 KATAKANA LETTER SMALL I
(define-gtkkey #x04a9 "kana_u")        ;U+30A5 KATAKANA LETTER SMALL U
(define-gtkkey #x04aa "kana_e")        ;U+30A7 KATAKANA LETTER SMALL E
(define-gtkkey #x04ab "kana_o")        ;U+30A9 KATAKANA LETTER SMALL O
(define-gtkkey #x04ac "kana_ya")      ;U+30E3 KATAKANA LETTER SMALL YA
(define-gtkkey #x04ad "kana_yu")      ;U+30E5 KATAKANA LETTER SMALL YU
(define-gtkkey #x04ae "kana_yo")      ;U+30E7 KATAKANA LETTER SMALL YO
(define-gtkkey #x04af "kana_tsu")     ;U+30C3 KATAKANA LETTER SMALL TU
(define-gtkkey #x04af "kana_tu")        ;deprecated
(define-gtkkey #x04b0 "prolongedsound") ;U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK
(define-gtkkey #x04b1 "kana_A")         ;U+30A2 KATAKANA LETTER A
(define-gtkkey #x04b2 "kana_I")         ;U+30A4 KATAKANA LETTER I
(define-gtkkey #x04b3 "kana_U")         ;U+30A6 KATAKANA LETTER U
(define-gtkkey #x04b4 "kana_E")         ;U+30A8 KATAKANA LETTER E
(define-gtkkey #x04b5 "kana_O")         ;U+30AA KATAKANA LETTER O
(define-gtkkey #x04b6 "kana_KA")        ;U+30AB KATAKANA LETTER KA
(define-gtkkey #x04b7 "kana_KI")        ;U+30AD KATAKANA LETTER KI
(define-gtkkey #x04b8 "kana_KU")        ;U+30AF KATAKANA LETTER KU
(define-gtkkey #x04b9 "kana_KE")        ;U+30B1 KATAKANA LETTER KE
(define-gtkkey #x04ba "kana_KO")        ;U+30B3 KATAKANA LETTER KO
(define-gtkkey #x04bb "kana_SA")        ;U+30B5 KATAKANA LETTER SA
(define-gtkkey #x04bc "kana_SHI")       ;U+30B7 KATAKANA LETTER SI
(define-gtkkey #x04bd "kana_SU")        ;U+30B9 KATAKANA LETTER SU
(define-gtkkey #x04be "kana_SE")        ;U+30BB KATAKANA LETTER SE
(define-gtkkey #x04bf "kana_SO")        ;U+30BD KATAKANA LETTER SO
(define-gtkkey #x04c0 "kana_TA")        ;U+30BF KATAKANA LETTER TA
(define-gtkkey #x04c1 "kana_CHI")       ;U+30C1 KATAKANA LETTER TI
(define-gtkkey #x04c1 "kana_TI")        ;deprecated
(define-gtkkey #x04c2 "kana_TSU")       ;U+30C4 KATAKANA LETTER TU
(define-gtkkey #x04c2 "kana_TU")        ;deprecated
(define-gtkkey #x04c3 "kana_TE")        ;U+30C6 KATAKANA LETTER TE
(define-gtkkey #x04c4 "kana_TO")        ;U+30C8 KATAKANA LETTER TO
(define-gtkkey #x04c5 "kana_NA")        ;U+30CA KATAKANA LETTER NA
(define-gtkkey #x04c6 "kana_NI")        ;U+30CB KATAKANA LETTER NI
(define-gtkkey #x04c7 "kana_NU")        ;U+30CC KATAKANA LETTER NU
(define-gtkkey #x04c8 "kana_NE")        ;U+30CD KATAKANA LETTER NE
(define-gtkkey #x04c9 "kana_NO")        ;U+30CE KATAKANA LETTER NO
(define-gtkkey #x04ca "kana_HA")        ;U+30CF KATAKANA LETTER HA
(define-gtkkey #x04cb "kana_HI")        ;U+30D2 KATAKANA LETTER HI
(define-gtkkey #x04cc "kana_FU")        ;U+30D5 KATAKANA LETTER HU
(define-gtkkey #x04cc "kana_HU")        ;deprecated
(define-gtkkey #x04cd "kana_HE")        ;U+30D8 KATAKANA LETTER HE
(define-gtkkey #x04ce "kana_HO")        ;U+30DB KATAKANA LETTER HO
(define-gtkkey #x04cf "kana_MA")        ;U+30DE KATAKANA LETTER MA
(define-gtkkey #x04d0 "kana_MI")        ;U+30DF KATAKANA LETTER MI
(define-gtkkey #x04d1 "kana_MU")        ;U+30E0 KATAKANA LETTER MU
(define-gtkkey #x04d2 "kana_ME")        ;U+30E1 KATAKANA LETTER ME
(define-gtkkey #x04d3 "kana_MO")        ;U+30E2 KATAKANA LETTER MO
(define-gtkkey #x04d4 "kana_YA")        ;U+30E4 KATAKANA LETTER YA
(define-gtkkey #x04d5 "kana_YU")        ;U+30E6 KATAKANA LETTER YU
(define-gtkkey #x04d6 "kana_YO")        ;U+30E8 KATAKANA LETTER YO
(define-gtkkey #x04d7 "kana_RA")        ;U+30E9 KATAKANA LETTER RA
(define-gtkkey #x04d8 "kana_RI")        ;U+30EA KATAKANA LETTER RI
(define-gtkkey #x04d9 "kana_RU")        ;U+30EB KATAKANA LETTER RU
(define-gtkkey #x04da "kana_RE")        ;U+30EC KATAKANA LETTER RE
(define-gtkkey #x04db "kana_RO")        ;U+30ED KATAKANA LETTER RO
(define-gtkkey #x04dc "kana_WA")        ;U+30EF KATAKANA LETTER WA
(define-gtkkey #x04dd "kana_N")         ;U+30F3 KATAKANA LETTER N
(define-gtkkey #x04de "voicedsound") ;U+309B KATAKANA-HIRAGANA VOICED SOUND MARK
(define-gtkkey #x04df "semivoicedsound") ;U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
(define-gtkkey #xff7e "kana_switch")    ;Alias for mode_switch
(define-gtkkey #x10006f0 "Farsi_0") ;U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO
(define-gtkkey #x10006f1 "Farsi_1") ;U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE
(define-gtkkey #x10006f2 "Farsi_2") ;U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO
(define-gtkkey #x10006f3 "Farsi_3") ;U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE
(define-gtkkey #x10006f4 "Farsi_4") ;U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR
(define-gtkkey #x10006f5 "Farsi_5") ;U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE
(define-gtkkey #x10006f6 "Farsi_6") ;U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX
(define-gtkkey #x10006f7 "Farsi_7") ;U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN
(define-gtkkey #x10006f8 "Farsi_8") ;U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT
(define-gtkkey #x10006f9 "Farsi_9") ;U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE
(define-gtkkey #x100066a "Arabic_percent") ;U+066A ARABIC PERCENT SIGN
(define-gtkkey #x1000670 "Arabic_superscript_alef") ;U+0670 ARABIC LETTER SUPERSCRIPT ALEF
(define-gtkkey #x1000679 "Arabic_tteh") ;U+0679 ARABIC LETTER TTEH
(define-gtkkey #x100067e "Arabic_peh")  ;U+067E ARABIC LETTER PEH
(define-gtkkey #x1000686 "Arabic_tcheh") ;U+0686 ARABIC LETTER TCHEH
(define-gtkkey #x1000688 "Arabic_ddal") ;U+0688 ARABIC LETTER DDAL
(define-gtkkey #x1000691 "Arabic_rreh") ;U+0691 ARABIC LETTER RREH
(define-gtkkey #x05ac "Arabic_comma")   ;U+060C ARABIC COMMA
(define-gtkkey #x10006d4 "Arabic_fullstop") ;U+06D4 ARABIC FULL STOP
(define-gtkkey #x1000660 "Arabic_0")   ;U+0660 ARABIC-INDIC DIGIT ZERO
(define-gtkkey #x1000661 "Arabic_1")    ;U+0661 ARABIC-INDIC DIGIT ONE
(define-gtkkey #x1000662 "Arabic_2")    ;U+0662 ARABIC-INDIC DIGIT TWO
(define-gtkkey #x1000663 "Arabic_3")  ;U+0663 ARABIC-INDIC DIGIT THREE
(define-gtkkey #x1000664 "Arabic_4")   ;U+0664 ARABIC-INDIC DIGIT FOUR
(define-gtkkey #x1000665 "Arabic_5")   ;U+0665 ARABIC-INDIC DIGIT FIVE
(define-gtkkey #x1000666 "Arabic_6")    ;U+0666 ARABIC-INDIC DIGIT SIX
(define-gtkkey #x1000667 "Arabic_7")  ;U+0667 ARABIC-INDIC DIGIT SEVEN
(define-gtkkey #x1000668 "Arabic_8")  ;U+0668 ARABIC-INDIC DIGIT EIGHT
(define-gtkkey #x1000669 "Arabic_9")   ;U+0669 ARABIC-INDIC DIGIT NINE
(define-gtkkey #x05bb "Arabic_semicolon") ;U+061B ARABIC SEMICOLON
(define-gtkkey #x05bf "Arabic_question_mark") ;U+061F ARABIC QUESTION MARK
(define-gtkkey #x05c1 "Arabic_hamza")   ;U+0621 ARABIC LETTER HAMZA
(define-gtkkey #x05c2 "Arabic_maddaonalef") ;U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE
(define-gtkkey #x05c3 "Arabic_hamzaonalef") ;U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE
(define-gtkkey #x05c4 "Arabic_hamzaonwaw") ;U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE
(define-gtkkey #x05c5 "Arabic_hamzaunderalef") ;U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW
(define-gtkkey #x05c6 "Arabic_hamzaonyeh") ;U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE
(define-gtkkey #x05c7 "Arabic_alef")    ;U+0627 ARABIC LETTER ALEF
(define-gtkkey #x05c8 "Arabic_beh")     ;U+0628 ARABIC LETTER BEH
(define-gtkkey #x05c9 "Arabic_tehmarbuta") ;U+0629 ARABIC LETTER TEH MARBUTA
(define-gtkkey #x05ca "Arabic_teh")     ;U+062A ARABIC LETTER TEH
(define-gtkkey #x05cb "Arabic_theh")    ;U+062B ARABIC LETTER THEH
(define-gtkkey #x05cc "Arabic_jeem")    ;U+062C ARABIC LETTER JEEM
(define-gtkkey #x05cd "Arabic_hah")     ;U+062D ARABIC LETTER HAH
(define-gtkkey #x05ce "Arabic_khah")    ;U+062E ARABIC LETTER KHAH
(define-gtkkey #x05cf "Arabic_dal")     ;U+062F ARABIC LETTER DAL
(define-gtkkey #x05d0 "Arabic_thal")    ;U+0630 ARABIC LETTER THAL
(define-gtkkey #x05d1 "Arabic_ra")      ;U+0631 ARABIC LETTER REH
(define-gtkkey #x05d2 "Arabic_zain")    ;U+0632 ARABIC LETTER ZAIN
(define-gtkkey #x05d3 "Arabic_seen")    ;U+0633 ARABIC LETTER SEEN
(define-gtkkey #x05d4 "Arabic_sheen")   ;U+0634 ARABIC LETTER SHEEN
(define-gtkkey #x05d5 "Arabic_sad")     ;U+0635 ARABIC LETTER SAD
(define-gtkkey #x05d6 "Arabic_dad")     ;U+0636 ARABIC LETTER DAD
(define-gtkkey #x05d7 "Arabic_tah")     ;U+0637 ARABIC LETTER TAH
(define-gtkkey #x05d8 "Arabic_zah")     ;U+0638 ARABIC LETTER ZAH
(define-gtkkey #x05d9 "Arabic_ain")     ;U+0639 ARABIC LETTER AIN
(define-gtkkey #x05da "Arabic_ghain")   ;U+063A ARABIC LETTER GHAIN
(define-gtkkey #x05e0 "Arabic_tatweel") ;U+0640 ARABIC TATWEEL
(define-gtkkey #x05e1 "Arabic_feh")     ;U+0641 ARABIC LETTER FEH
(define-gtkkey #x05e2 "Arabic_qaf")     ;U+0642 ARABIC LETTER QAF
(define-gtkkey #x05e3 "Arabic_kaf")     ;U+0643 ARABIC LETTER KAF
(define-gtkkey #x05e4 "Arabic_lam")     ;U+0644 ARABIC LETTER LAM
(define-gtkkey #x05e5 "Arabic_meem")    ;U+0645 ARABIC LETTER MEEM
(define-gtkkey #x05e6 "Arabic_noon")    ;U+0646 ARABIC LETTER NOON
(define-gtkkey #x05e7 "Arabic_ha")      ;U+0647 ARABIC LETTER HEH
(define-gtkkey #x05e7 "Arabic_heh")     ;deprecated
(define-gtkkey #x05e8 "Arabic_waw")     ;U+0648 ARABIC LETTER WAW
(define-gtkkey #x05e9 "Arabic_alefmaksura") ;U+0649 ARABIC LETTER ALEF MAKSURA
(define-gtkkey #x05ea "Arabic_yeh")     ;U+064A ARABIC LETTER YEH
(define-gtkkey #x05eb "Arabic_fathatan") ;U+064B ARABIC FATHATAN
(define-gtkkey #x05ec "Arabic_dammatan") ;U+064C ARABIC DAMMATAN
(define-gtkkey #x05ed "Arabic_kasratan") ;U+064D ARABIC KASRATAN
(define-gtkkey #x05ee "Arabic_fatha")   ;U+064E ARABIC FATHA
(define-gtkkey #x05ef "Arabic_damma")   ;U+064F ARABIC DAMMA
(define-gtkkey #x05f0 "Arabic_kasra")   ;U+0650 ARABIC KASRA
(define-gtkkey #x05f1 "Arabic_shadda")  ;U+0651 ARABIC SHADDA
(define-gtkkey #x05f2 "Arabic_sukun")   ;U+0652 ARABIC SUKUN
(define-gtkkey #x1000653 "Arabic_madda_above") ;U+0653 ARABIC MADDAH ABOVE
(define-gtkkey #x1000654 "Arabic_hamza_above") ;U+0654 ARABIC HAMZA ABOVE
(define-gtkkey #x1000655 "Arabic_hamza_below") ;U+0655 ARABIC HAMZA BELOW
(define-gtkkey #x1000698 "Arabic_jeh")  ;U+0698 ARABIC LETTER JEH
(define-gtkkey #x10006a4 "Arabic_veh")  ;U+06A4 ARABIC LETTER VEH
(define-gtkkey #x10006a9 "Arabic_keheh") ;U+06A9 ARABIC LETTER KEHEH
(define-gtkkey #x10006af "Arabic_gaf")  ;U+06AF ARABIC LETTER GAF
(define-gtkkey #x10006ba "Arabic_noon_ghunna") ;U+06BA ARABIC LETTER NOON GHUNNA
(define-gtkkey #x10006be "Arabic_heh_doachashmee") ;U+06BE ARABIC LETTER HEH DOACHASHMEE
(define-gtkkey #x10006cc "Farsi_yeh")  ;U+06CC ARABIC LETTER FARSI YEH
(define-gtkkey #x10006cc "Arabic_farsi_yeh") ;U+06CC ARABIC LETTER FARSI YEH
(define-gtkkey #x10006d2 "Arabic_yeh_baree") ;U+06D2 ARABIC LETTER YEH BARREE
(define-gtkkey #x10006c1 "Arabic_heh_goal") ;U+06C1 ARABIC LETTER HEH GOAL
(define-gtkkey #xff7e "Arabic_switch")  ;Alias for mode_switch
(define-gtkkey #x1000492 "Cyrillic_GHE_bar") ;U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE
(define-gtkkey #x100093 "Cyrillic_ghe_bar") ;U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE
(define-gtkkey #x1000496 "Cyrillic_ZHE_descender") ;U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
(define-gtkkey #x1000497 "Cyrillic_zhe_descender") ;U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER
(define-gtkkey #x100049a "Cyrillic_KA_descender") ;U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER
(define-gtkkey #x100049b "Cyrillic_ka_descender") ;U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER
(define-gtkkey #x100049c "Cyrillic_KA_vertstroke") ;U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
(define-gtkkey #x100049d "Cyrillic_ka_vertstroke") ;U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
(define-gtkkey #x10004a2 "Cyrillic_EN_descender") ;U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER
(define-gtkkey #x10004a3 "Cyrillic_en_descender") ;U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER
(define-gtkkey #x10004ae "Cyrillic_U_straight") ;U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U
(define-gtkkey #x10004af "Cyrillic_u_straight") ;U+04AF CYRILLIC SMALL LETTER STRAIGHT U
(define-gtkkey #x10004b0 "Cyrillic_U_straight_bar") ;U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
(define-gtkkey #x10004b1 "Cyrillic_u_straight_bar") ;U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
(define-gtkkey #x10004b2 "Cyrillic_HA_descender") ;U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER
(define-gtkkey #x10004b3 "Cyrillic_ha_descender") ;U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER
(define-gtkkey #x10004b6 "Cyrillic_CHE_descender") ;U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
(define-gtkkey #x10004b7 "Cyrillic_che_descender") ;U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER
(define-gtkkey #x10004b8 "Cyrillic_CHE_vertstroke") ;U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
(define-gtkkey #x10004b9 "Cyrillic_che_vertstroke") ;U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
(define-gtkkey #x10004ba "Cyrillic_SHHA") ;U+04BA CYRILLIC CAPITAL LETTER SHHA
(define-gtkkey #x10004bb "Cyrillic_shha") ;U+04BB CYRILLIC SMALL LETTER SHHA
(define-gtkkey #x10004d8 "Cyrillic_SCHWA") ;U+04D8 CYRILLIC CAPITAL LETTER SCHWA
(define-gtkkey #x10004d9 "Cyrillic_schwa") ;U+04D9 CYRILLIC SMALL LETTER SCHWA
(define-gtkkey #x10004e2 "Cyrillic_I_macron") ;U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON
(define-gtkkey #x10004e3 "Cyrillic_i_macron") ;U+04E3 CYRILLIC SMALL LETTER I WITH MACRON
(define-gtkkey #x10004e8 "Cyrillic_O_bar") ;U+04E8 CYRILLIC CAPITAL LETTER BARRED O
(define-gtkkey #x10004e9 "Cyrillic_o_bar") ;U+04E9 CYRILLIC SMALL LETTER BARRED O
(define-gtkkey #x10004ee "Cyrillic_U_macron") ;U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON
(define-gtkkey #x10004ef "Cyrillic_u_macron") ;U+04EF CYRILLIC SMALL LETTER U WITH MACRON
(define-gtkkey #x06a1 "Serbian_dje") ;U+0452 CYRILLIC SMALL LETTER DJE
(define-gtkkey #x06a2 "Macedonia_gje") ;U+0453 CYRILLIC SMALL LETTER GJE
(define-gtkkey #x06a3 "Cyrillic_io")  ;U+0451 CYRILLIC SMALL LETTER IO
(define-gtkkey #x06a4 "Ukrainian_ie") ;U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE
(define-gtkkey #x06a4 "Ukranian_je")    ;deprecated
(define-gtkkey #x06a5 "Macedonia_dse") ;U+0455 CYRILLIC SMALL LETTER DZE
(define-gtkkey #x06a6 "Ukrainian_i") ;U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
(define-gtkkey #x06a6 "Ukranian_i")     ;deprecated
(define-gtkkey #x06a7 "Ukrainian_yi") ;U+0457 CYRILLIC SMALL LETTER YI
(define-gtkkey #x06a7 "Ukranian_yi")    ;deprecated
(define-gtkkey #x06a8 "Cyrillic_je")  ;U+0458 CYRILLIC SMALL LETTER JE
(define-gtkkey #x06a8 "Serbian_je")     ;deprecated
(define-gtkkey #x06a9 "Cyrillic_lje") ;U+0459 CYRILLIC SMALL LETTER LJE
(define-gtkkey #x06a9 "Serbian_lje")    ;deprecated
(define-gtkkey #x06aa "Cyrillic_nje") ;U+045A CYRILLIC SMALL LETTER NJE
(define-gtkkey #x06aa "Serbian_nje")    ;deprecated
(define-gtkkey #x06ab "Serbian_tshe") ;U+045B CYRILLIC SMALL LETTER TSHE
(define-gtkkey #x06ac "Macedonia_kje") ;U+045C CYRILLIC SMALL LETTER KJE
(define-gtkkey #x06ad "Ukrainian_ghe_with_upturn") ;U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN
(define-gtkkey #x06ae "Byelorussian_shortu") ;U+045E CYRILLIC SMALL LETTER SHORT U
(define-gtkkey #x06af "Cyrillic_dzhe") ;U+045F CYRILLIC SMALL LETTER DZHE
(define-gtkkey #x06af "Serbian_dze")    ;deprecated
(define-gtkkey #x06b0 "numerosign")     ;U+2116 NUMERO SIGN
(define-gtkkey #x06b1 "Serbian_DJE") ;U+0402 CYRILLIC CAPITAL LETTER DJE
(define-gtkkey #x06b2 "Macedonia_GJE") ;U+0403 CYRILLIC CAPITAL LETTER GJE
(define-gtkkey #x06b3 "Cyrillic_IO") ;U+0401 CYRILLIC CAPITAL LETTER IO
(define-gtkkey #x06b4 "Ukrainian_IE") ;U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE
(define-gtkkey #x06b4 "Ukranian_JE")    ;deprecated
(define-gtkkey #x06b5 "Macedonia_DSE") ;U+0405 CYRILLIC CAPITAL LETTER DZE
(define-gtkkey #x06b6 "Ukrainian_I") ;U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
(define-gtkkey #x06b6 "Ukranian_I")     ;deprecated
(define-gtkkey #x06b7 "Ukrainian_YI") ;U+0407 CYRILLIC CAPITAL LETTER YI
(define-gtkkey #x06b7 "Ukranian_YI")    ;deprecated
(define-gtkkey #x06b8 "Cyrillic_JE") ;U+0408 CYRILLIC CAPITAL LETTER JE
(define-gtkkey #x06b8 "Serbian_JE")     ;deprecated
(define-gtkkey #x06b9 "Cyrillic_LJE") ;U+0409 CYRILLIC CAPITAL LETTER LJE
(define-gtkkey #x06b9 "Serbian_LJE")    ;deprecated
(define-gtkkey #x06ba "Cyrillic_NJE") ;U+040A CYRILLIC CAPITAL LETTER NJE
(define-gtkkey #x06ba "Serbian_NJE")    ;deprecated
(define-gtkkey #x06bb "Serbian_TSHE") ;U+040B CYRILLIC CAPITAL LETTER TSHE
(define-gtkkey #x06bc "Macedonia_KJE") ;U+040C CYRILLIC CAPITAL LETTER KJE
(define-gtkkey #x06bd "Ukrainian_GHE_WITH_UPTURN") ;U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN
(define-gtkkey #x06be "Byelorussian_SHORTU") ;U+040E CYRILLIC CAPITAL LETTER SHORT U
(define-gtkkey #x06bf "Cyrillic_DZHE") ;U+040F CYRILLIC CAPITAL LETTER DZHE
(define-gtkkey #x06bf "Serbian_DZE")    ;deprecated
(define-gtkkey #x06c0 "Cyrillic_yu")  ;U+044E CYRILLIC SMALL LETTER YU
(define-gtkkey #x06c1 "Cyrillic_a")    ;U+0430 CYRILLIC SMALL LETTER A
(define-gtkkey #x06c2 "Cyrillic_be")  ;U+0431 CYRILLIC SMALL LETTER BE
(define-gtkkey #x06c3 "Cyrillic_tse") ;U+0446 CYRILLIC SMALL LETTER TSE
(define-gtkkey #x06c4 "Cyrillic_de")  ;U+0434 CYRILLIC SMALL LETTER DE
(define-gtkkey #x06c5 "Cyrillic_ie")  ;U+0435 CYRILLIC SMALL LETTER IE
(define-gtkkey #x06c6 "Cyrillic_ef")  ;U+0444 CYRILLIC SMALL LETTER EF
(define-gtkkey #x06c7 "Cyrillic_ghe") ;U+0433 CYRILLIC SMALL LETTER GHE
(define-gtkkey #x06c8 "Cyrillic_ha")  ;U+0445 CYRILLIC SMALL LETTER HA
(define-gtkkey #x06c9 "Cyrillic_i")    ;U+0438 CYRILLIC SMALL LETTER I
(define-gtkkey #x06ca "Cyrillic_shorti") ;U+0439 CYRILLIC SMALL LETTER SHORT I
(define-gtkkey #x06cb "Cyrillic_ka")  ;U+043A CYRILLIC SMALL LETTER KA
(define-gtkkey #x06cc "Cyrillic_el")  ;U+043B CYRILLIC SMALL LETTER EL
(define-gtkkey #x06cd "Cyrillic_em")  ;U+043C CYRILLIC SMALL LETTER EM
(define-gtkkey #x06ce "Cyrillic_en")  ;U+043D CYRILLIC SMALL LETTER EN
(define-gtkkey #x06cf "Cyrillic_o")    ;U+043E CYRILLIC SMALL LETTER O
(define-gtkkey #x06d0 "Cyrillic_pe")  ;U+043F CYRILLIC SMALL LETTER PE
(define-gtkkey #x06d1 "Cyrillic_ya")  ;U+044F CYRILLIC SMALL LETTER YA
(define-gtkkey #x06d2 "Cyrillic_er")  ;U+0440 CYRILLIC SMALL LETTER ER
(define-gtkkey #x06d3 "Cyrillic_es")  ;U+0441 CYRILLIC SMALL LETTER ES
(define-gtkkey #x06d4 "Cyrillic_te")  ;U+0442 CYRILLIC SMALL LETTER TE
(define-gtkkey #x06d5 "Cyrillic_u")    ;U+0443 CYRILLIC SMALL LETTER U
(define-gtkkey #x06d6 "Cyrillic_zhe") ;U+0436 CYRILLIC SMALL LETTER ZHE
(define-gtkkey #x06d7 "Cyrillic_ve")  ;U+0432 CYRILLIC SMALL LETTER VE
(define-gtkkey #x06d8 "Cyrillic_softsign") ;U+044C CYRILLIC SMALL LETTER SOFT SIGN
(define-gtkkey #x06d9 "Cyrillic_yeru") ;U+044B CYRILLIC SMALL LETTER YERU
(define-gtkkey #x06da "Cyrillic_ze")  ;U+0437 CYRILLIC SMALL LETTER ZE
(define-gtkkey #x06db "Cyrillic_sha") ;U+0448 CYRILLIC SMALL LETTER SHA
(define-gtkkey #x06dc "Cyrillic_e")    ;U+044D CYRILLIC SMALL LETTER E
(define-gtkkey #x06dd "Cyrillic_shcha") ;U+0449 CYRILLIC SMALL LETTER SHCHA
(define-gtkkey #x06de "Cyrillic_che") ;U+0447 CYRILLIC SMALL LETTER CHE
(define-gtkkey #x06df "Cyrillic_hardsign") ;U+044A CYRILLIC SMALL LETTER HARD SIGN
(define-gtkkey #x06e0 "Cyrillic_YU") ;U+042E CYRILLIC CAPITAL LETTER YU
(define-gtkkey #x06e1 "Cyrillic_A")  ;U+0410 CYRILLIC CAPITAL LETTER A
(define-gtkkey #x06e2 "Cyrillic_BE") ;U+0411 CYRILLIC CAPITAL LETTER BE
(define-gtkkey #x06e3 "Cyrillic_TSE") ;U+0426 CYRILLIC CAPITAL LETTER TSE
(define-gtkkey #x06e4 "Cyrillic_DE") ;U+0414 CYRILLIC CAPITAL LETTER DE
(define-gtkkey #x06e5 "Cyrillic_IE") ;U+0415 CYRILLIC CAPITAL LETTER IE
(define-gtkkey #x06e6 "Cyrillic_EF") ;U+0424 CYRILLIC CAPITAL LETTER EF
(define-gtkkey #x06e7 "Cyrillic_GHE") ;U+0413 CYRILLIC CAPITAL LETTER GHE
(define-gtkkey #x06e8 "Cyrillic_HA") ;U+0425 CYRILLIC CAPITAL LETTER HA
(define-gtkkey #x06e9 "Cyrillic_I")  ;U+0418 CYRILLIC CAPITAL LETTER I
(define-gtkkey #x06ea "Cyrillic_SHORTI") ;U+0419 CYRILLIC CAPITAL LETTER SHORT I
(define-gtkkey #x06eb "Cyrillic_KA") ;U+041A CYRILLIC CAPITAL LETTER KA
(define-gtkkey #x06ec "Cyrillic_EL") ;U+041B CYRILLIC CAPITAL LETTER EL
(define-gtkkey #x06ed "Cyrillic_EM") ;U+041C CYRILLIC CAPITAL LETTER EM
(define-gtkkey #x06ee "Cyrillic_EN") ;U+041D CYRILLIC CAPITAL LETTER EN
(define-gtkkey #x06ef "Cyrillic_O")  ;U+041E CYRILLIC CAPITAL LETTER O
(define-gtkkey #x06f0 "Cyrillic_PE") ;U+041F CYRILLIC CAPITAL LETTER PE
(define-gtkkey #x06f1 "Cyrillic_YA") ;U+042F CYRILLIC CAPITAL LETTER YA
(define-gtkkey #x06f2 "Cyrillic_ER") ;U+0420 CYRILLIC CAPITAL LETTER ER
(define-gtkkey #x06f3 "Cyrillic_ES") ;U+0421 CYRILLIC CAPITAL LETTER ES
(define-gtkkey #x06f4 "Cyrillic_TE") ;U+0422 CYRILLIC CAPITAL LETTER TE
(define-gtkkey #x06f5 "Cyrillic_U")  ;U+0423 CYRILLIC CAPITAL LETTER U
(define-gtkkey #x06f6 "Cyrillic_ZHE") ;U+0416 CYRILLIC CAPITAL LETTER ZHE
(define-gtkkey #x06f7 "Cyrillic_VE") ;U+0412 CYRILLIC CAPITAL LETTER VE
(define-gtkkey #x06f8 "Cyrillic_SOFTSIGN") ;U+042C CYRILLIC CAPITAL LETTER SOFT SIGN
(define-gtkkey #x06f9 "Cyrillic_YERU") ;U+042B CYRILLIC CAPITAL LETTER YERU
(define-gtkkey #x06fa "Cyrillic_ZE") ;U+0417 CYRILLIC CAPITAL LETTER ZE
(define-gtkkey #x06fb "Cyrillic_SHA") ;U+0428 CYRILLIC CAPITAL LETTER SHA
(define-gtkkey #x06fc "Cyrillic_E")  ;U+042D CYRILLIC CAPITAL LETTER E
(define-gtkkey #x06fd "Cyrillic_SHCHA") ;U+0429 CYRILLIC CAPITAL LETTER SHCHA
(define-gtkkey #x06fe "Cyrillic_CHE") ;U+0427 CYRILLIC CAPITAL LETTER CHE
(define-gtkkey #x06ff "Cyrillic_HARDSIGN") ;U+042A CYRILLIC CAPITAL LETTER HARD SIGN
(define-gtkkey #x07a1 "Greek_ALPHAaccent") ;U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS
(define-gtkkey #x07a2 "Greek_EPSILONaccent") ;U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS
(define-gtkkey #x07a3 "Greek_ETAaccent") ;U+0389 GREEK CAPITAL LETTER ETA WITH TONOS
(define-gtkkey #x07a4 "Greek_IOTAaccent") ;U+038A GREEK CAPITAL LETTER IOTA WITH TONOS
(define-gtkkey #x07a5 "Greek_IOTAdieresis") ;U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
(define-gtkkey #x07a5 "Greek_IOTAdiaeresis") ;old typo
(define-gtkkey #x07a7 "Greek_OMICRONaccent") ;U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS
(define-gtkkey #x07a8 "Greek_UPSILONaccent") ;U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS
(define-gtkkey #x07a9 "Greek_UPSILONdieresis") ;U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
(define-gtkkey #x07ab "Greek_OMEGAaccent") ;U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS
(define-gtkkey #x07ae "Greek_accentdieresis") ;U+0385 GREEK DIALYTIKA TONOS
(define-gtkkey #x07af "Greek_horizbar") ;U+2015 HORIZONTAL BAR
(define-gtkkey #x07b1 "Greek_alphaaccent") ;U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
(define-gtkkey #x07b2 "Greek_epsilonaccent") ;U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
(define-gtkkey #x07b3 "Greek_etaaccent") ;U+03AE GREEK SMALL LETTER ETA WITH TONOS
(define-gtkkey #x07b4 "Greek_iotaaccent") ;U+03AF GREEK SMALL LETTER IOTA WITH TONOS
(define-gtkkey #x07b5 "Greek_iotadieresis") ;U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
(define-gtkkey #x07b6 "Greek_iotaaccentdieresis") ;U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
(define-gtkkey #x07b7 "Greek_omicronaccent") ;U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
(define-gtkkey #x07b8 "Greek_upsilonaccent") ;U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
(define-gtkkey #x07b9 "Greek_upsilondieresis") ;U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
(define-gtkkey #x07ba "Greek_upsilonaccentdieresis") ;U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
(define-gtkkey #x07bb "Greek_omegaaccent") ;U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
(define-gtkkey #x07c1 "Greek_ALPHA") ;U+0391 GREEK CAPITAL LETTER ALPHA
(define-gtkkey #x07c2 "Greek_BETA")  ;U+0392 GREEK CAPITAL LETTER BETA
(define-gtkkey #x07c3 "Greek_GAMMA") ;U+0393 GREEK CAPITAL LETTER GAMMA
(define-gtkkey #x07c4 "Greek_DELTA") ;U+0394 GREEK CAPITAL LETTER DELTA
(define-gtkkey #x07c5 "Greek_EPSILON") ;U+0395 GREEK CAPITAL LETTER EPSILON
(define-gtkkey #x07c6 "Greek_ZETA")  ;U+0396 GREEK CAPITAL LETTER ZETA
(define-gtkkey #x07c7 "Greek_ETA")    ;U+0397 GREEK CAPITAL LETTER ETA
(define-gtkkey #x07c8 "Greek_THETA") ;U+0398 GREEK CAPITAL LETTER THETA
(define-gtkkey #x07c9 "Greek_IOTA")  ;U+0399 GREEK CAPITAL LETTER IOTA
(define-gtkkey #x07ca "Greek_KAPPA") ;U+039A GREEK CAPITAL LETTER KAPPA
(define-gtkkey #x07cb "Greek_LAMDA") ;U+039B GREEK CAPITAL LETTER LAMDA
(define-gtkkey #x07cb "Greek_LAMBDA") ;U+039B GREEK CAPITAL LETTER LAMDA
(define-gtkkey #x07cc "Greek_MU")      ;U+039C GREEK CAPITAL LETTER MU
(define-gtkkey #x07cd "Greek_NU")      ;U+039D GREEK CAPITAL LETTER NU
(define-gtkkey #x07ce "Greek_XI")      ;U+039E GREEK CAPITAL LETTER XI
(define-gtkkey #x07cf "Greek_OMICRON") ;U+039F GREEK CAPITAL LETTER OMICRON
(define-gtkkey #x07d0 "Greek_PI")      ;U+03A0 GREEK CAPITAL LETTER PI
(define-gtkkey #x07d1 "Greek_RHO")    ;U+03A1 GREEK CAPITAL LETTER RHO
(define-gtkkey #x07d2 "Greek_SIGMA") ;U+03A3 GREEK CAPITAL LETTER SIGMA
(define-gtkkey #x07d4 "Greek_TAU")    ;U+03A4 GREEK CAPITAL LETTER TAU
(define-gtkkey #x07d5 "Greek_UPSILON") ;U+03A5 GREEK CAPITAL LETTER UPSILON
(define-gtkkey #x07d6 "Greek_PHI")    ;U+03A6 GREEK CAPITAL LETTER PHI
(define-gtkkey #x07d7 "Greek_CHI")    ;U+03A7 GREEK CAPITAL LETTER CHI
(define-gtkkey #x07d8 "Greek_PSI")    ;U+03A8 GREEK CAPITAL LETTER PSI
(define-gtkkey #x07d9 "Greek_OMEGA") ;U+03A9 GREEK CAPITAL LETTER OMEGA
(define-gtkkey #x07e1 "Greek_alpha")  ;U+03B1 GREEK SMALL LETTER ALPHA
(define-gtkkey #x07e2 "Greek_beta")    ;U+03B2 GREEK SMALL LETTER BETA
(define-gtkkey #x07e3 "Greek_gamma")  ;U+03B3 GREEK SMALL LETTER GAMMA
(define-gtkkey #x07e4 "Greek_delta")  ;U+03B4 GREEK SMALL LETTER DELTA
(define-gtkkey #x07e5 "Greek_epsilon") ;U+03B5 GREEK SMALL LETTER EPSILON
(define-gtkkey #x07e6 "Greek_zeta")    ;U+03B6 GREEK SMALL LETTER ZETA
(define-gtkkey #x07e7 "Greek_eta")      ;U+03B7 GREEK SMALL LETTER ETA
(define-gtkkey #x07e8 "Greek_theta")  ;U+03B8 GREEK SMALL LETTER THETA
(define-gtkkey #x07e9 "Greek_iota")    ;U+03B9 GREEK SMALL LETTER IOTA
(define-gtkkey #x07ea "Greek_kappa")  ;U+03BA GREEK SMALL LETTER KAPPA
(define-gtkkey #x07eb "Greek_lamda")  ;U+03BB GREEK SMALL LETTER LAMDA
(define-gtkkey #x07eb "Greek_lambda") ;U+03BB GREEK SMALL LETTER LAMDA
(define-gtkkey #x07ec "Greek_mu")       ;U+03BC GREEK SMALL LETTER MU
(define-gtkkey #x07ed "Greek_nu")       ;U+03BD GREEK SMALL LETTER NU
(define-gtkkey #x07ee "Greek_xi")       ;U+03BE GREEK SMALL LETTER XI
(define-gtkkey #x07ef "Greek_omicron") ;U+03BF GREEK SMALL LETTER OMICRON
(define-gtkkey #x07f0 "Greek_pi")       ;U+03C0 GREEK SMALL LETTER PI
(define-gtkkey #x07f1 "Greek_rho")      ;U+03C1 GREEK SMALL LETTER RHO
(define-gtkkey #x07f2 "Greek_sigma")  ;U+03C3 GREEK SMALL LETTER SIGMA
(define-gtkkey #x07f3 "Greek_finalsmallsigma") ;U+03C2 GREEK SMALL LETTER FINAL SIGMA
(define-gtkkey #x07f4 "Greek_tau")      ;U+03C4 GREEK SMALL LETTER TAU
(define-gtkkey #x07f5 "Greek_upsilon") ;U+03C5 GREEK SMALL LETTER UPSILON
(define-gtkkey #x07f6 "Greek_phi")      ;U+03C6 GREEK SMALL LETTER PHI
(define-gtkkey #x07f7 "Greek_chi")      ;U+03C7 GREEK SMALL LETTER CHI
(define-gtkkey #x07f8 "Greek_psi")      ;U+03C8 GREEK SMALL LETTER PSI
(define-gtkkey #x07f9 "Greek_omega")  ;U+03C9 GREEK SMALL LETTER OMEGA
(define-gtkkey #xff7e "Greek_switch")   ;Alias for mode_switch
(define-gtkkey #x08a1 "leftradical")    ;U+23B7 RADICAL SYMBOL BOTTOM
(define-gtkkey #x08a2 "topleftradical") ;(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
(define-gtkkey #x08a3 "horizconnector") ;(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
(define-gtkkey #x08a4 "topintegral")    ;U+2320 TOP HALF INTEGRAL
(define-gtkkey #x08a5 "botintegral")    ;U+2321 BOTTOM HALF INTEGRAL
(define-gtkkey #x08a6 "vertconnector") ;(U+2502 BOX DRAWINGS LIGHT VERTICAL)
(define-gtkkey #x08a7 "topleftsqbracket") ;U+23A1 LEFT SQUARE BRACKET UPPER CORNER
(define-gtkkey #x08a8 "botleftsqbracket") ;U+23A3 LEFT SQUARE BRACKET LOWER CORNER
(define-gtkkey #x08a9 "toprightsqbracket") ;U+23A4 RIGHT SQUARE BRACKET UPPER CORNER
(define-gtkkey #x08aa "botrightsqbracket") ;U+23A6 RIGHT SQUARE BRACKET LOWER CORNER
(define-gtkkey #x08ab "topleftparens") ;U+239B LEFT PARENTHESIS UPPER HOOK
(define-gtkkey #x08ac "botleftparens") ;U+239D LEFT PARENTHESIS LOWER HOOK
(define-gtkkey #x08ad "toprightparens") ;U+239E RIGHT PARENTHESIS UPPER HOOK
(define-gtkkey #x08ae "botrightparens") ;U+23A0 RIGHT PARENTHESIS LOWER HOOK
(define-gtkkey #x08af "leftmiddlecurlybrace") ;U+23A8 LEFT CURLY BRACKET MIDDLE PIECE
(define-gtkkey #x08b0 "rightmiddlecurlybrace") ;U+23AC RIGHT CURLY BRACKET MIDDLE PIECE
(define-gtkkey #x08b1 "topleftsummation")
(define-gtkkey #x08b2 "botleftsummation")
(define-gtkkey #x08b3 "topvertsummationconnector")
(define-gtkkey #x08b4 "botvertsummationconnector")
(define-gtkkey #x08b5 "toprightsummation")
(define-gtkkey #x08b6 "botrightsummation")
(define-gtkkey #x08b7 "rightmiddlesummation")
(define-gtkkey #x08bc "lessthanequal")  ;U+2264 LESS-THAN OR EQUAL TO
(define-gtkkey #x08bd "notequal")       ;U+2260 NOT EQUAL TO
(define-gtkkey #x08be "greaterthanequal") ;U+2265 GREATER-THAN OR EQUAL TO
(define-gtkkey #x08bf "integral")       ;U+222B INTEGRAL
(define-gtkkey #x08c0 "therefore")      ;U+2234 THEREFORE
(define-gtkkey #x08c1 "variation")      ;U+221D PROPORTIONAL TO
(define-gtkkey #x08c2 "infinity")       ;U+221E INFINITY
(define-gtkkey #x08c5 "nabla")          ;U+2207 NABLA
(define-gtkkey #x08c8 "approximate")    ;U+223C TILDE OPERATOR
(define-gtkkey #x08c9 "similarequal")  ;U+2243 ASYMPTOTICALLY EQUAL TO
(define-gtkkey #x08cd "ifonlyif")      ;U+21D4 LEFT RIGHT DOUBLE ARROW
(define-gtkkey #x08ce "implies")       ;U+21D2 RIGHTWARDS DOUBLE ARROW
(define-gtkkey #x08cf "identical")      ;U+2261 IDENTICAL TO
(define-gtkkey #x08d6 "radical")        ;U+221A SQUARE ROOT
(define-gtkkey #x08da "includedin")     ;U+2282 SUBSET OF
(define-gtkkey #x08db "includes")       ;U+2283 SUPERSET OF
(define-gtkkey #x08dc "intersection")   ;U+2229 INTERSECTION
(define-gtkkey #x08dd "union")          ;U+222A UNION
(define-gtkkey #x08de "logicaland")     ;U+2227 LOGICAL AND
(define-gtkkey #x08df "logicalor")      ;U+2228 LOGICAL OR
(define-gtkkey #x08ef "partialderivative") ;U+2202 PARTIAL DIFFERENTIAL
(define-gtkkey #x08f6 "function") ;U+0192 LATIN SMALL LETTER F WITH HOOK
(define-gtkkey #x08fb "leftarrow")      ;U+2190 LEFTWARDS ARROW
(define-gtkkey #x08fc "uparrow")        ;U+2191 UPWARDS ARROW
(define-gtkkey #x08fd "rightarrow")     ;U+2192 RIGHTWARDS ARROW
(define-gtkkey #x08fe "downarrow")      ;U+2193 DOWNWARDS ARROW
(define-gtkkey #x09df "blank")
(define-gtkkey #x09e0 "soliddiamond")   ;U+25C6 BLACK DIAMOND
(define-gtkkey #x09e1 "checkerboard")   ;U+2592 MEDIUM SHADE
(define-gtkkey #x09e2 "ht")   ;U+2409 SYMBOL FOR HORIZONTAL TABULATION
(define-gtkkey #x09e3 "ff")             ;U+240C SYMBOL FOR FORM FEED
(define-gtkkey #x09e4 "cr")         ;U+240D SYMBOL FOR CARRIAGE RETURN
(define-gtkkey #x09e5 "lf")             ;U+240A SYMBOL FOR LINE FEED
(define-gtkkey #x09e8 "nl")             ;U+2424 SYMBOL FOR NEWLINE
(define-gtkkey #x09e9 "vt")     ;U+240B SYMBOL FOR VERTICAL TABULATION
(define-gtkkey #x09ea "lowrightcorner") ;U+2518 BOX DRAWINGS LIGHT UP AND LEFT
(define-gtkkey #x09eb "uprightcorner") ;U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
(define-gtkkey #x09ec "upleftcorner") ;U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
(define-gtkkey #x09ed "lowleftcorner") ;U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
(define-gtkkey #x09ee "crossinglines") ;U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
(define-gtkkey #x09ef "horizlinescan1") ;U+23BA HORIZONTAL SCAN LINE-1
(define-gtkkey #x09f0 "horizlinescan3") ;U+23BB HORIZONTAL SCAN LINE-3
(define-gtkkey #x09f1 "horizlinescan5") ;U+2500 BOX DRAWINGS LIGHT HORIZONTAL
(define-gtkkey #x09f2 "horizlinescan7") ;U+23BC HORIZONTAL SCAN LINE-7
(define-gtkkey #x09f3 "horizlinescan9") ;U+23BD HORIZONTAL SCAN LINE-9
(define-gtkkey #x09f4 "leftt") ;U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
(define-gtkkey #x09f5 "rightt") ;U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
(define-gtkkey #x09f6 "bott") ;U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
(define-gtkkey #x09f7 "topt") ;U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
(define-gtkkey #x09f8 "vertbar")   ;U+2502 BOX DRAWINGS LIGHT VERTICAL
(define-gtkkey #x0aa1 "emspace")        ;U+2003 EM SPACE
(define-gtkkey #x0aa2 "enspace")        ;U+2002 EN SPACE
(define-gtkkey #x0aa3 "em3space")       ;U+2004 THREE-PER-EM SPACE
(define-gtkkey #x0aa4 "em4space")       ;U+2005 FOUR-PER-EM SPACE
(define-gtkkey #x0aa5 "digitspace")     ;U+2007 FIGURE SPACE
(define-gtkkey #x0aa6 "punctspace")     ;U+2008 PUNCTUATION SPACE
(define-gtkkey #x0aa7 "thinspace")      ;U+2009 THIN SPACE
(define-gtkkey #x0aa8 "hairspace")      ;U+200A HAIR SPACE
(define-gtkkey #x0aa9 "emdash")         ;U+2014 EM DASH
(define-gtkkey #x0aaa "endash")         ;U+2013 EN DASH
(define-gtkkey #x0aac "signifblank")    ;(U+2423 OPEN BOX)
(define-gtkkey #x0aae "ellipsis")       ;U+2026 HORIZONTAL ELLIPSIS
(define-gtkkey #x0aaf "doubbaselinedot") ;U+2025 TWO DOT LEADER
(define-gtkkey #x0ab0 "onethird")    ;U+2153 VULGAR FRACTION ONE THIRD
(define-gtkkey #x0ab1 "twothirds")  ;U+2154 VULGAR FRACTION TWO THIRDS
(define-gtkkey #x0ab2 "onefifth")    ;U+2155 VULGAR FRACTION ONE FIFTH
(define-gtkkey #x0ab3 "twofifths")  ;U+2156 VULGAR FRACTION TWO FIFTHS
(define-gtkkey #x0ab4 "threefifths") ;U+2157 VULGAR FRACTION THREE FIFTHS
(define-gtkkey #x0ab5 "fourfifths") ;U+2158 VULGAR FRACTION FOUR FIFTHS
(define-gtkkey #x0ab6 "onesixth")    ;U+2159 VULGAR FRACTION ONE SIXTH
(define-gtkkey #x0ab7 "fivesixths") ;U+215A VULGAR FRACTION FIVE SIXTHS
(define-gtkkey #x0ab8 "careof")         ;U+2105 CARE OF
(define-gtkkey #x0abb "figdash")        ;U+2012 FIGURE DASH
(define-gtkkey #x0abc "leftanglebracket") ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(define-gtkkey #x0abd "decimalpoint")   ;(U+002E FULL STOP)
(define-gtkkey #x0abe "rightanglebracket") ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(define-gtkkey #x0abf "marker")
(define-gtkkey #x0ac3 "oneeighth")  ;U+215B VULGAR FRACTION ONE EIGHTH
(define-gtkkey #x0ac4 "threeeighths") ;U+215C VULGAR FRACTION THREE EIGHTHS
(define-gtkkey #x0ac5 "fiveeighths") ;U+215D VULGAR FRACTION FIVE EIGHTHS
(define-gtkkey #x0ac6 "seveneighths") ;U+215E VULGAR FRACTION SEVEN EIGHTHS
(define-gtkkey #x0ac9 "trademark")      ;U+2122 TRADE MARK SIGN
(define-gtkkey #x0aca "signaturemark")  ;(U+2613 SALTIRE)
(define-gtkkey #x0acb "trademarkincircle")
(define-gtkkey #x0acc "leftopentriangle") ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
(define-gtkkey #x0acd "rightopentriangle") ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(define-gtkkey #x0ace "emopencircle")   ;(U+25CB WHITE CIRCLE)
(define-gtkkey #x0acf "emopenrectangle") ;(U+25AF WHITE VERTICAL RECTANGLE)
(define-gtkkey #x0ad0 "leftsinglequotemark") ;U+2018 LEFT SINGLE QUOTATION MARK
(define-gtkkey #x0ad1 "rightsinglequotemark") ;U+2019 RIGHT SINGLE QUOTATION MARK
(define-gtkkey #x0ad2 "leftdoublequotemark") ;U+201C LEFT DOUBLE QUOTATION MARK
(define-gtkkey #x0ad3 "rightdoublequotemark") ;U+201D RIGHT DOUBLE QUOTATION MARK
(define-gtkkey #x0ad4 "prescription")   ;U+211E PRESCRIPTION TAKE
(define-gtkkey #x0ad6 "minutes")        ;U+2032 PRIME
(define-gtkkey #x0ad7 "seconds")        ;U+2033 DOUBLE PRIME
(define-gtkkey #x0ad9 "latincross")     ;U+271D LATIN CROSS
(define-gtkkey #x0ada "hexagram")
(define-gtkkey #x0adb "filledrectbullet") ;(U+25AC BLACK RECTANGLE)
(define-gtkkey #x0adc "filledlefttribullet") ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
(define-gtkkey #x0add "filledrighttribullet") ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(define-gtkkey #x0ade "emfilledcircle") ;(U+25CF BLACK CIRCLE)
(define-gtkkey #x0adf "emfilledrect") ;(U+25AE BLACK VERTICAL RECTANGLE)
(define-gtkkey #x0ae0 "enopencircbullet") ;(U+25E6 WHITE BULLET)
(define-gtkkey #x0ae1 "enopensquarebullet") ;(U+25AB WHITE SMALL SQUARE)
(define-gtkkey #x0ae2 "openrectbullet") ;(U+25AD WHITE RECTANGLE)
(define-gtkkey #x0ae3 "opentribulletup") ;(U+25B3 WHITE UP-POINTING TRIANGLE)
(define-gtkkey #x0ae4 "opentribulletdown") ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
(define-gtkkey #x0ae5 "openstar")       ;(U+2606 WHITE STAR)
(define-gtkkey #x0ae6 "enfilledcircbullet") ;(U+2022 BULLET)
(define-gtkkey #x0ae7 "enfilledsqbullet") ;(U+25AA BLACK SMALL SQUARE)
(define-gtkkey #x0ae8 "filledtribulletup") ;(U+25B2 BLACK UP-POINTING TRIANGLE)
(define-gtkkey #x0ae9 "filledtribulletdown") ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
(define-gtkkey #x0aea "leftpointer") ;(U+261C WHITE LEFT POINTING INDEX)
(define-gtkkey #x0aeb "rightpointer") ;(U+261E WHITE RIGHT POINTING INDEX)
(define-gtkkey #x0aec "club")           ;U+2663 BLACK CLUB SUIT
(define-gtkkey #x0aed "diamond")        ;U+2666 BLACK DIAMOND SUIT
(define-gtkkey #x0aee "heart")          ;U+2665 BLACK HEART SUIT
(define-gtkkey #x0af0 "maltesecross")   ;U+2720 MALTESE CROSS
(define-gtkkey #x0af1 "dagger")         ;U+2020 DAGGER
(define-gtkkey #x0af2 "doubledagger")   ;U+2021 DOUBLE DAGGER
(define-gtkkey #x0af3 "checkmark")      ;U+2713 CHECK MARK
(define-gtkkey #x0af4 "ballotcross")    ;U+2717 BALLOT X
(define-gtkkey #x0af5 "musicalsharp")   ;U+266F MUSIC SHARP SIGN
(define-gtkkey #x0af6 "musicalflat")    ;U+266D MUSIC FLAT SIGN
(define-gtkkey #x0af7 "malesymbol")     ;U+2642 MALE SIGN
(define-gtkkey #x0af8 "femalesymbol")   ;U+2640 FEMALE SIGN
(define-gtkkey #x0af9 "telephone")      ;U+260E BLACK TELEPHONE
(define-gtkkey #x0afa "telephonerecorder") ;U+2315 TELEPHONE RECORDER
(define-gtkkey #x0afb "phonographcopyright") ;U+2117 SOUND RECORDING COPYRIGHT
(define-gtkkey #x0afc "caret")          ;U+2038 CARET
(define-gtkkey #x0afd "singlelowquotemark") ;U+201A SINGLE LOW-9 QUOTATION MARK
(define-gtkkey #x0afe "doublelowquotemark") ;U+201E DOUBLE LOW-9 QUOTATION MARK
(define-gtkkey #x0aff "cursor")
(define-gtkkey #x0ba3 "leftcaret")      ;(U+003C LESS-THAN SIGN)
(define-gtkkey #x0ba6 "rightcaret")     ;(U+003E GREATER-THAN SIGN)
(define-gtkkey #x0ba8 "downcaret")      ;(U+2228 LOGICAL OR)
(define-gtkkey #x0ba9 "upcaret")        ;(U+2227 LOGICAL AND)
(define-gtkkey #x0bc0 "overbar")        ;(U+00AF MACRON)
(define-gtkkey #x0bc2 "downtack")       ;U+22A5 UP TACK
(define-gtkkey #x0bc3 "upshoe")         ;(U+2229 INTERSECTION)
(define-gtkkey #x0bc4 "downstile")      ;U+230A LEFT FLOOR
(define-gtkkey #x0bc6 "underbar")       ;(U+005F LOW LINE)
(define-gtkkey #x0bca "jot")            ;U+2218 RING OPERATOR
(define-gtkkey #x0bcc "quad")       ;U+2395 APL FUNCTIONAL SYMBOL QUAD
(define-gtkkey #x0bce "uptack")         ;U+22A4 DOWN TACK
(define-gtkkey #x0bcf "circle")         ;U+25CB WHITE CIRCLE
(define-gtkkey #x0bd3 "upstile")        ;U+2308 LEFT CEILING
(define-gtkkey #x0bd6 "downshoe")       ;(U+222A UNION)
(define-gtkkey #x0bd8 "rightshoe")      ;(U+2283 SUPERSET OF)
(define-gtkkey #x0bda "leftshoe")       ;(U+2282 SUBSET OF)
(define-gtkkey #x0bdc "lefttack")       ;U+22A2 RIGHT TACK
(define-gtkkey #x0bfc "righttack")      ;U+22A3 LEFT TACK
(define-gtkkey #x0cdf "hebrew_doublelowline") ;U+2017 DOUBLE LOW LINE
(define-gtkkey #x0ce0 "hebrew_aleph")   ;U+05D0 HEBREW LETTER ALEF
(define-gtkkey #x0ce1 "hebrew_bet")     ;U+05D1 HEBREW LETTER BET
(define-gtkkey #x0ce1 "hebrew_beth")    ;deprecated
(define-gtkkey #x0ce2 "hebrew_gimel")   ;U+05D2 HEBREW LETTER GIMEL
(define-gtkkey #x0ce2 "hebrew_gimmel")  ;deprecated
(define-gtkkey #x0ce3 "hebrew_dalet")   ;U+05D3 HEBREW LETTER DALET
(define-gtkkey #x0ce3 "hebrew_daleth")  ;deprecated
(define-gtkkey #x0ce4 "hebrew_he")      ;U+05D4 HEBREW LETTER HE
(define-gtkkey #x0ce5 "hebrew_waw")     ;U+05D5 HEBREW LETTER VAV
(define-gtkkey #x0ce6 "hebrew_zain")    ;U+05D6 HEBREW LETTER ZAYIN
(define-gtkkey #x0ce6 "hebrew_zayin")   ;deprecated
(define-gtkkey #x0ce7 "hebrew_chet")    ;U+05D7 HEBREW LETTER HET
(define-gtkkey #x0ce7 "hebrew_het")     ;deprecated
(define-gtkkey #x0ce8 "hebrew_tet")     ;U+05D8 HEBREW LETTER TET
(define-gtkkey #x0ce8 "hebrew_teth")    ;deprecated
(define-gtkkey #x0ce9 "hebrew_yod")     ;U+05D9 HEBREW LETTER YOD
(define-gtkkey #x0cea "hebrew_finalkaph") ;U+05DA HEBREW LETTER FINAL KAF
(define-gtkkey #x0ceb "hebrew_kaph")    ;U+05DB HEBREW LETTER KAF
(define-gtkkey #x0cec "hebrew_lamed")   ;U+05DC HEBREW LETTER LAMED
(define-gtkkey #x0ced "hebrew_finalmem") ;U+05DD HEBREW LETTER FINAL MEM
(define-gtkkey #x0cee "hebrew_mem")     ;U+05DE HEBREW LETTER MEM
(define-gtkkey #x0cef "hebrew_finalnun") ;U+05DF HEBREW LETTER FINAL NUN
(define-gtkkey #x0cf0 "hebrew_nun")     ;U+05E0 HEBREW LETTER NUN
(define-gtkkey #x0cf1 "hebrew_samech")  ;U+05E1 HEBREW LETTER SAMEKH
(define-gtkkey #x0cf1 "hebrew_samekh")  ;deprecated
(define-gtkkey #x0cf2 "hebrew_ayin")    ;U+05E2 HEBREW LETTER AYIN
(define-gtkkey #x0cf3 "hebrew_finalpe") ;U+05E3 HEBREW LETTER FINAL PE
(define-gtkkey #x0cf4 "hebrew_pe")      ;U+05E4 HEBREW LETTER PE
(define-gtkkey #x0cf5 "hebrew_finalzade") ;U+05E5 HEBREW LETTER FINAL TSADI
(define-gtkkey #x0cf5 "hebrew_finalzadi") ;deprecated
(define-gtkkey #x0cf6 "hebrew_zade")    ;U+05E6 HEBREW LETTER TSADI
(define-gtkkey #x0cf6 "hebrew_zadi")    ;deprecated
(define-gtkkey #x0cf7 "hebrew_qoph")    ;U+05E7 HEBREW LETTER QOF
(define-gtkkey #x0cf7 "hebrew_kuf")     ;deprecated
(define-gtkkey #x0cf8 "hebrew_resh")    ;U+05E8 HEBREW LETTER RESH
(define-gtkkey #x0cf9 "hebrew_shin")    ;U+05E9 HEBREW LETTER SHIN
(define-gtkkey #x0cfa "hebrew_taw")     ;U+05EA HEBREW LETTER TAV
(define-gtkkey #x0cfa "hebrew_taf")     ;deprecated
(define-gtkkey #xff7e "Hebrew_switch")  ;Alias for mode_switch
(define-gtkkey #x0da1 "Thai_kokai")     ;U+0E01 THAI CHARACTER KO KAI
(define-gtkkey #x0da2 "Thai_khokhai")  ;U+0E02 THAI CHARACTER KHO KHAI
(define-gtkkey #x0da3 "Thai_khokhuat") ;U+0E03 THAI CHARACTER KHO KHUAT
(define-gtkkey #x0da4 "Thai_khokhwai") ;U+0E04 THAI CHARACTER KHO KHWAI
(define-gtkkey #x0da5 "Thai_khokhon")  ;U+0E05 THAI CHARACTER KHO KHON
(define-gtkkey #x0da6 "Thai_khorakhang") ;U+0E06 THAI CHARACTER KHO RAKHANG
(define-gtkkey #x0da7 "Thai_ngongu")    ;U+0E07 THAI CHARACTER NGO NGU
(define-gtkkey #x0da8 "Thai_chochan")  ;U+0E08 THAI CHARACTER CHO CHAN
(define-gtkkey #x0da9 "Thai_choching") ;U+0E09 THAI CHARACTER CHO CHING
(define-gtkkey #x0daa "Thai_chochang") ;U+0E0A THAI CHARACTER CHO CHANG
(define-gtkkey #x0dab "Thai_soso")      ;U+0E0B THAI CHARACTER SO SO
(define-gtkkey #x0dac "Thai_chochoe")  ;U+0E0C THAI CHARACTER CHO CHOE
(define-gtkkey #x0dad "Thai_yoying")    ;U+0E0D THAI CHARACTER YO YING
(define-gtkkey #x0dae "Thai_dochada")  ;U+0E0E THAI CHARACTER DO CHADA
(define-gtkkey #x0daf "Thai_topatak")  ;U+0E0F THAI CHARACTER TO PATAK
(define-gtkkey #x0db0 "Thai_thothan")  ;U+0E10 THAI CHARACTER THO THAN
(define-gtkkey #x0db1 "Thai_thonangmontho") ;U+0E11 THAI CHARACTER THO NANGMONTHO
(define-gtkkey #x0db2 "Thai_thophuthao") ;U+0E12 THAI CHARACTER THO PHUTHAO
(define-gtkkey #x0db3 "Thai_nonen")     ;U+0E13 THAI CHARACTER NO NEN
(define-gtkkey #x0db4 "Thai_dodek")     ;U+0E14 THAI CHARACTER DO DEK
(define-gtkkey #x0db5 "Thai_totao")     ;U+0E15 THAI CHARACTER TO TAO
(define-gtkkey #x0db6 "Thai_thothung") ;U+0E16 THAI CHARACTER THO THUNG
(define-gtkkey #x0db7 "Thai_thothahan") ;U+0E17 THAI CHARACTER THO THAHAN
(define-gtkkey #x0db8 "Thai_thothong") ;U+0E18 THAI CHARACTER THO THONG
(define-gtkkey #x0db9 "Thai_nonu")      ;U+0E19 THAI CHARACTER NO NU
(define-gtkkey #x0dba "Thai_bobaimai") ;U+0E1A THAI CHARACTER BO BAIMAI
(define-gtkkey #x0dbb "Thai_popla")     ;U+0E1B THAI CHARACTER PO PLA
(define-gtkkey #x0dbc "Thai_phophung") ;U+0E1C THAI CHARACTER PHO PHUNG
(define-gtkkey #x0dbd "Thai_fofa")      ;U+0E1D THAI CHARACTER FO FA
(define-gtkkey #x0dbe "Thai_phophan")  ;U+0E1E THAI CHARACTER PHO PHAN
(define-gtkkey #x0dbf "Thai_fofan")     ;U+0E1F THAI CHARACTER FO FAN
(define-gtkkey #x0dc0 "Thai_phosamphao") ;U+0E20 THAI CHARACTER PHO SAMPHAO
(define-gtkkey #x0dc1 "Thai_moma")      ;U+0E21 THAI CHARACTER MO MA
(define-gtkkey #x0dc2 "Thai_yoyak")     ;U+0E22 THAI CHARACTER YO YAK
(define-gtkkey #x0dc3 "Thai_rorua")     ;U+0E23 THAI CHARACTER RO RUA
(define-gtkkey #x0dc4 "Thai_ru")        ;U+0E24 THAI CHARACTER RU
(define-gtkkey #x0dc5 "Thai_loling")    ;U+0E25 THAI CHARACTER LO LING
(define-gtkkey #x0dc6 "Thai_lu")        ;U+0E26 THAI CHARACTER LU
(define-gtkkey #x0dc7 "Thai_wowaen")    ;U+0E27 THAI CHARACTER WO WAEN
(define-gtkkey #x0dc8 "Thai_sosala")    ;U+0E28 THAI CHARACTER SO SALA
(define-gtkkey #x0dc9 "Thai_sorusi")    ;U+0E29 THAI CHARACTER SO RUSI
(define-gtkkey #x0dca "Thai_sosua")     ;U+0E2A THAI CHARACTER SO SUA
(define-gtkkey #x0dcb "Thai_hohip")     ;U+0E2B THAI CHARACTER HO HIP
(define-gtkkey #x0dcc "Thai_lochula")  ;U+0E2C THAI CHARACTER LO CHULA
(define-gtkkey #x0dcd "Thai_oang")      ;U+0E2D THAI CHARACTER O ANG
(define-gtkkey #x0dce "Thai_honokhuk") ;U+0E2E THAI CHARACTER HO NOKHUK
(define-gtkkey #x0dcf "Thai_paiyannoi") ;U+0E2F THAI CHARACTER PAIYANNOI
(define-gtkkey #x0dd0 "Thai_saraa")     ;U+0E30 THAI CHARACTER SARA A
(define-gtkkey #x0dd1 "Thai_maihanakat") ;U+0E31 THAI CHARACTER MAI HAN-AKAT
(define-gtkkey #x0dd2 "Thai_saraaa")    ;U+0E32 THAI CHARACTER SARA AA
(define-gtkkey #x0dd3 "Thai_saraam")    ;U+0E33 THAI CHARACTER SARA AM
(define-gtkkey #x0dd4 "Thai_sarai")     ;U+0E34 THAI CHARACTER SARA I
(define-gtkkey #x0dd5 "Thai_saraii")    ;U+0E35 THAI CHARACTER SARA II
(define-gtkkey #x0dd6 "Thai_saraue")    ;U+0E36 THAI CHARACTER SARA UE
(define-gtkkey #x0dd7 "Thai_sarauee")  ;U+0E37 THAI CHARACTER SARA UEE
(define-gtkkey #x0dd8 "Thai_sarau")     ;U+0E38 THAI CHARACTER SARA U
(define-gtkkey #x0dd9 "Thai_sarauu")    ;U+0E39 THAI CHARACTER SARA UU
(define-gtkkey #x0dda "Thai_phinthu")   ;U+0E3A THAI CHARACTER PHINTHU
(define-gtkkey #x0dde "Thai_maihanakat_maitho")
(define-gtkkey #x0ddf "Thai_baht")   ;U+0E3F THAI CURRENCY SYMBOL BAHT
(define-gtkkey #x0de0 "Thai_sarae")     ;U+0E40 THAI CHARACTER SARA E
(define-gtkkey #x0de1 "Thai_saraae")    ;U+0E41 THAI CHARACTER SARA AE
(define-gtkkey #x0de2 "Thai_sarao")     ;U+0E42 THAI CHARACTER SARA O
(define-gtkkey #x0de3 "Thai_saraaimaimuan") ;U+0E43 THAI CHARACTER SARA AI MAIMUAN
(define-gtkkey #x0de4 "Thai_saraaimaimalai") ;U+0E44 THAI CHARACTER SARA AI MAIMALAI
(define-gtkkey #x0de5 "Thai_lakkhangyao") ;U+0E45 THAI CHARACTER LAKKHANGYAO
(define-gtkkey #x0de6 "Thai_maiyamok") ;U+0E46 THAI CHARACTER MAIYAMOK
(define-gtkkey #x0de7 "Thai_maitaikhu") ;U+0E47 THAI CHARACTER MAITAIKHU
(define-gtkkey #x0de8 "Thai_maiek")     ;U+0E48 THAI CHARACTER MAI EK
(define-gtkkey #x0de9 "Thai_maitho")    ;U+0E49 THAI CHARACTER MAI THO
(define-gtkkey #x0dea "Thai_maitri")    ;U+0E4A THAI CHARACTER MAI TRI
(define-gtkkey #x0deb "Thai_maichattawa") ;U+0E4B THAI CHARACTER MAI CHATTAWA
(define-gtkkey #x0dec "Thai_thanthakhat") ;U+0E4C THAI CHARACTER THANTHAKHAT
(define-gtkkey #x0ded "Thai_nikhahit") ;U+0E4D THAI CHARACTER NIKHAHIT
(define-gtkkey #x0df0 "Thai_leksun")    ;U+0E50 THAI DIGIT ZERO
(define-gtkkey #x0df1 "Thai_leknung")   ;U+0E51 THAI DIGIT ONE
(define-gtkkey #x0df2 "Thai_leksong")   ;U+0E52 THAI DIGIT TWO
(define-gtkkey #x0df3 "Thai_leksam")    ;U+0E53 THAI DIGIT THREE
(define-gtkkey #x0df4 "Thai_leksi")     ;U+0E54 THAI DIGIT FOUR
(define-gtkkey #x0df5 "Thai_lekha")     ;U+0E55 THAI DIGIT FIVE
(define-gtkkey #x0df6 "Thai_lekhok")    ;U+0E56 THAI DIGIT SIX
(define-gtkkey #x0df7 "Thai_lekchet")   ;U+0E57 THAI DIGIT SEVEN
(define-gtkkey #x0df8 "Thai_lekpaet")   ;U+0E58 THAI DIGIT EIGHT
(define-gtkkey #x0df9 "Thai_lekkao")    ;U+0E59 THAI DIGIT NINE
(define-gtkkey #xff31 "Hangul")         ;Hangul start/stop(toggle)
(define-gtkkey #xff32 "Hangul_Start")   ;Hangul start
(define-gtkkey #xff33 "Hangul_End")     ;Hangul end, English start
(define-gtkkey #xff34 "Hangul_Hanja")  ;Start Hangul->Hanja Conversion
(define-gtkkey #xff35 "Hangul_Jamo")    ;Hangul Jamo mode
(define-gtkkey #xff36 "Hangul_Romaja")  ;Hangul Romaja mode
(define-gtkkey #xff37 "Hangul_Codeinput") ;Hangul code input mode
(define-gtkkey #xff38 "Hangul_Jeonja")  ;Jeonja mode
(define-gtkkey #xff39 "Hangul_Banja")   ;Banja mode
(define-gtkkey #xff3a "Hangul_PreHanja") ;Pre Hanja conversion
(define-gtkkey #xff3b "Hangul_PostHanja") ;Post Hanja conversion
(define-gtkkey #xff3c "Hangul_SingleCandidate") ;Single candidate
(define-gtkkey #xff3d "Hangul_MultipleCandidate") ;Multiple candidate
(define-gtkkey #xff3e "Hangul_PreviousCandidate") ;Previous candidate
(define-gtkkey #xff3f "Hangul_Special") ;Special symbols
(define-gtkkey #xff7e "Hangul_switch")  ;Alias for mode_switch
(define-gtkkey #x0ea1 "Hangul_Kiyeog")
(define-gtkkey #x0ea2 "Hangul_SsangKiyeog")
(define-gtkkey #x0ea3 "Hangul_KiyeogSios")
(define-gtkkey #x0ea4 "Hangul_Nieun")
(define-gtkkey #x0ea5 "Hangul_NieunJieuj")
(define-gtkkey #x0ea6 "Hangul_NieunHieuh")
(define-gtkkey #x0ea7 "Hangul_Dikeud")
(define-gtkkey #x0ea8 "Hangul_SsangDikeud")
(define-gtkkey #x0ea9 "Hangul_Rieul")
(define-gtkkey #x0eaa "Hangul_RieulKiyeog")
(define-gtkkey #x0eab "Hangul_RieulMieum")
(define-gtkkey #x0eac "Hangul_RieulPieub")
(define-gtkkey #x0ead "Hangul_RieulSios")
(define-gtkkey #x0eae "Hangul_RieulTieut")
(define-gtkkey #x0eaf "Hangul_RieulPhieuf")
(define-gtkkey #x0eb0 "Hangul_RieulHieuh")
(define-gtkkey #x0eb1 "Hangul_Mieum")
(define-gtkkey #x0eb2 "Hangul_Pieub")
(define-gtkkey #x0eb3 "Hangul_SsangPieub")
(define-gtkkey #x0eb4 "Hangul_PieubSios")
(define-gtkkey #x0eb5 "Hangul_Sios")
(define-gtkkey #x0eb6 "Hangul_SsangSios")
(define-gtkkey #x0eb7 "Hangul_Ieung")
(define-gtkkey #x0eb8 "Hangul_Jieuj")
(define-gtkkey #x0eb9 "Hangul_SsangJieuj")
(define-gtkkey #x0eba "Hangul_Cieuc")
(define-gtkkey #x0ebb "Hangul_Khieuq")
(define-gtkkey #x0ebc "Hangul_Tieut")
(define-gtkkey #x0ebd "Hangul_Phieuf")
(define-gtkkey #x0ebe "Hangul_Hieuh")
(define-gtkkey #x0ebf "Hangul_A")
(define-gtkkey #x0ec0 "Hangul_AE")
(define-gtkkey #x0ec1 "Hangul_YA")
(define-gtkkey #x0ec2 "Hangul_YAE")
(define-gtkkey #x0ec3 "Hangul_EO")
(define-gtkkey #x0ec4 "Hangul_E")
(define-gtkkey #x0ec5 "Hangul_YEO")
(define-gtkkey #x0ec6 "Hangul_YE")
(define-gtkkey #x0ec7 "Hangul_O")
(define-gtkkey #x0ec8 "Hangul_WA")
(define-gtkkey #x0ec9 "Hangul_WAE")
(define-gtkkey #x0eca "Hangul_OE")
(define-gtkkey #x0ecb "Hangul_YO")
(define-gtkkey #x0ecc "Hangul_U")
(define-gtkkey #x0ecd "Hangul_WEO")
(define-gtkkey #x0ece "Hangul_WE")
(define-gtkkey #x0ecf "Hangul_WI")
(define-gtkkey #x0ed0 "Hangul_YU")
(define-gtkkey #x0ed1 "Hangul_EU")
(define-gtkkey #x0ed2 "Hangul_YI")
(define-gtkkey #x0ed3 "Hangul_I")
(define-gtkkey #x0ed4 "Hangul_J_Kiyeog")
(define-gtkkey #x0ed5 "Hangul_J_SsangKiyeog")
(define-gtkkey #x0ed6 "Hangul_J_KiyeogSios")
(define-gtkkey #x0ed7 "Hangul_J_Nieun")
(define-gtkkey #x0ed8 "Hangul_J_NieunJieuj")
(define-gtkkey #x0ed9 "Hangul_J_NieunHieuh")
(define-gtkkey #x0eda "Hangul_J_Dikeud")
(define-gtkkey #x0edb "Hangul_J_Rieul")
(define-gtkkey #x0edc "Hangul_J_RieulKiyeog")
(define-gtkkey #x0edd "Hangul_J_RieulMieum")
(define-gtkkey #x0ede "Hangul_J_RieulPieub")
(define-gtkkey #x0edf "Hangul_J_RieulSios")
(define-gtkkey #x0ee0 "Hangul_J_RieulTieut")
(define-gtkkey #x0ee1 "Hangul_J_RieulPhieuf")
(define-gtkkey #x0ee2 "Hangul_J_RieulHieuh")
(define-gtkkey #x0ee3 "Hangul_J_Mieum")
(define-gtkkey #x0ee4 "Hangul_J_Pieub")
(define-gtkkey #x0ee5 "Hangul_J_PieubSios")
(define-gtkkey #x0ee6 "Hangul_J_Sios")
(define-gtkkey #x0ee7 "Hangul_J_SsangSios")
(define-gtkkey #x0ee8 "Hangul_J_Ieung")
(define-gtkkey #x0ee9 "Hangul_J_Jieuj")
(define-gtkkey #x0eea "Hangul_J_Cieuc")
(define-gtkkey #x0eeb "Hangul_J_Khieuq")
(define-gtkkey #x0eec "Hangul_J_Tieut")
(define-gtkkey #x0eed "Hangul_J_Phieuf")
(define-gtkkey #x0eee "Hangul_J_Hieuh")
(define-gtkkey #x0eef "Hangul_RieulYeorinHieuh")
(define-gtkkey #x0ef0 "Hangul_SunkyeongeumMieum")
(define-gtkkey #x0ef1 "Hangul_SunkyeongeumPieub")
(define-gtkkey #x0ef2 "Hangul_PanSios")
(define-gtkkey #x0ef3 "Hangul_KkogjiDalrinIeung")
(define-gtkkey #x0ef4 "Hangul_SunkyeongeumPhieuf")
(define-gtkkey #x0ef5 "Hangul_YeorinHieuh")
(define-gtkkey #x0ef6 "Hangul_AraeA")
(define-gtkkey #x0ef7 "Hangul_AraeAE")
(define-gtkkey #x0ef8 "Hangul_J_PanSios")
(define-gtkkey #x0ef9 "Hangul_J_KkogjiDalrinIeung")
(define-gtkkey #x0efa "Hangul_J_YeorinHieuh")
(define-gtkkey #x0eff "Korean_Won")     ;(U+20A9 WON SIGN)
(define-gtkkey #x1000587 "Armenian_ligature_ew") ;U+0587 ARMENIAN SMALL LIGATURE ECH YIWN
(define-gtkkey #x1000589 "Armenian_full_stop") ;U+0589 ARMENIAN FULL STOP
(define-gtkkey #x1000589 "Armenian_verjaket") ;U+0589 ARMENIAN FULL STOP
(define-gtkkey #x100055d "Armenian_separation_mark") ;U+055D ARMENIAN COMMA
(define-gtkkey #x100055d "Armenian_but") ;U+055D ARMENIAN COMMA
(define-gtkkey #x100058a "Armenian_hyphen") ;U+058A ARMENIAN HYPHEN
(define-gtkkey #x100058a "Armenian_yentamna") ;U+058A ARMENIAN HYPHEN
(define-gtkkey #x100055c "Armenian_exclam") ;U+055C ARMENIAN EXCLAMATION MARK
(define-gtkkey #x100055c "Armenian_amanak") ;U+055C ARMENIAN EXCLAMATION MARK
(define-gtkkey #x100055b "Armenian_accent") ;U+055B ARMENIAN EMPHASIS MARK
(define-gtkkey #x100055b "Armenian_shesht") ;U+055B ARMENIAN EMPHASIS MARK
(define-gtkkey #x100055e "Armenian_question") ;U+055E ARMENIAN QUESTION MARK
(define-gtkkey #x100055e "Armenian_paruyk") ;U+055E ARMENIAN QUESTION MARK
(define-gtkkey #x1000531 "Armenian_AYB") ;U+0531 ARMENIAN CAPITAL LETTER AYB
(define-gtkkey #x1000561 "Armenian_ayb") ;U+0561 ARMENIAN SMALL LETTER AYB
(define-gtkkey #x1000532 "Armenian_BEN") ;U+0532 ARMENIAN CAPITAL LETTER BEN
(define-gtkkey #x1000562 "Armenian_ben") ;U+0562 ARMENIAN SMALL LETTER BEN
(define-gtkkey #x1000533 "Armenian_GIM") ;U+0533 ARMENIAN CAPITAL LETTER GIM
(define-gtkkey #x1000563 "Armenian_gim") ;U+0563 ARMENIAN SMALL LETTER GIM
(define-gtkkey #x1000534 "Armenian_DA") ;U+0534 ARMENIAN CAPITAL LETTER DA
(define-gtkkey #x1000564 "Armenian_da") ;U+0564 ARMENIAN SMALL LETTER DA
(define-gtkkey #x1000535 "Armenian_YECH") ;U+0535 ARMENIAN CAPITAL LETTER ECH
(define-gtkkey #x1000565 "Armenian_yech") ;U+0565 ARMENIAN SMALL LETTER ECH
(define-gtkkey #x1000536 "Armenian_ZA") ;U+0536 ARMENIAN CAPITAL LETTER ZA
(define-gtkkey #x1000566 "Armenian_za") ;U+0566 ARMENIAN SMALL LETTER ZA
(define-gtkkey #x1000537 "Armenian_E") ;U+0537 ARMENIAN CAPITAL LETTER EH
(define-gtkkey #x1000567 "Armenian_e") ;U+0567 ARMENIAN SMALL LETTER EH
(define-gtkkey #x1000538 "Armenian_AT") ;U+0538 ARMENIAN CAPITAL LETTER ET
(define-gtkkey #x1000568 "Armenian_at") ;U+0568 ARMENIAN SMALL LETTER ET
(define-gtkkey #x1000539 "Armenian_TO") ;U+0539 ARMENIAN CAPITAL LETTER TO
(define-gtkkey #x1000569 "Armenian_to") ;U+0569 ARMENIAN SMALL LETTER TO
(define-gtkkey #x100053a "Armenian_ZHE") ;U+053A ARMENIAN CAPITAL LETTER ZHE
(define-gtkkey #x100056a "Armenian_zhe") ;U+056A ARMENIAN SMALL LETTER ZHE
(define-gtkkey #x100053b "Armenian_INI") ;U+053B ARMENIAN CAPITAL LETTER INI
(define-gtkkey #x100056b "Armenian_ini") ;U+056B ARMENIAN SMALL LETTER INI
(define-gtkkey #x100053c "Armenian_LYUN") ;U+053C ARMENIAN CAPITAL LETTER LIWN
(define-gtkkey #x100056c "Armenian_lyun") ;U+056C ARMENIAN SMALL LETTER LIWN
(define-gtkkey #x100053d "Armenian_KHE") ;U+053D ARMENIAN CAPITAL LETTER XEH
(define-gtkkey #x100056d "Armenian_khe") ;U+056D ARMENIAN SMALL LETTER XEH
(define-gtkkey #x100053e "Armenian_TSA") ;U+053E ARMENIAN CAPITAL LETTER CA
(define-gtkkey #x100056e "Armenian_tsa") ;U+056E ARMENIAN SMALL LETTER CA
(define-gtkkey #x100053f "Armenian_KEN") ;U+053F ARMENIAN CAPITAL LETTER KEN
(define-gtkkey #x100056f "Armenian_ken") ;U+056F ARMENIAN SMALL LETTER KEN
(define-gtkkey #x1000540 "Armenian_HO") ;U+0540 ARMENIAN CAPITAL LETTER HO
(define-gtkkey #x1000570 "Armenian_ho") ;U+0570 ARMENIAN SMALL LETTER HO
(define-gtkkey #x1000541 "Armenian_DZA") ;U+0541 ARMENIAN CAPITAL LETTER JA
(define-gtkkey #x1000571 "Armenian_dza") ;U+0571 ARMENIAN SMALL LETTER JA
(define-gtkkey #x1000542 "Armenian_GHAT") ;U+0542 ARMENIAN CAPITAL LETTER GHAD
(define-gtkkey #x1000572 "Armenian_ghat") ;U+0572 ARMENIAN SMALL LETTER GHAD
(define-gtkkey #x1000543 "Armenian_TCHE") ;U+0543 ARMENIAN CAPITAL LETTER CHEH
(define-gtkkey #x1000573 "Armenian_tche") ;U+0573 ARMENIAN SMALL LETTER CHEH
(define-gtkkey #x1000544 "Armenian_MEN") ;U+0544 ARMENIAN CAPITAL LETTER MEN
(define-gtkkey #x1000574 "Armenian_men") ;U+0574 ARMENIAN SMALL LETTER MEN
(define-gtkkey #x1000545 "Armenian_HI") ;U+0545 ARMENIAN CAPITAL LETTER YI
(define-gtkkey #x1000575 "Armenian_hi") ;U+0575 ARMENIAN SMALL LETTER YI
(define-gtkkey #x1000546 "Armenian_NU") ;U+0546 ARMENIAN CAPITAL LETTER NOW
(define-gtkkey #x1000576 "Armenian_nu") ;U+0576 ARMENIAN SMALL LETTER NOW
(define-gtkkey #x1000547 "Armenian_SHA") ;U+0547 ARMENIAN CAPITAL LETTER SHA
(define-gtkkey #x1000577 "Armenian_sha") ;U+0577 ARMENIAN SMALL LETTER SHA
(define-gtkkey #x1000548 "Armenian_VO") ;U+0548 ARMENIAN CAPITAL LETTER VO
(define-gtkkey #x1000578 "Armenian_vo") ;U+0578 ARMENIAN SMALL LETTER VO
(define-gtkkey #x1000549 "Armenian_CHA") ;U+0549 ARMENIAN CAPITAL LETTER CHA
(define-gtkkey #x1000579 "Armenian_cha") ;U+0579 ARMENIAN SMALL LETTER CHA
(define-gtkkey #x100054a "Armenian_PE") ;U+054A ARMENIAN CAPITAL LETTER PEH
(define-gtkkey #x100057a "Armenian_pe") ;U+057A ARMENIAN SMALL LETTER PEH
(define-gtkkey #x100054b "Armenian_JE") ;U+054B ARMENIAN CAPITAL LETTER JHEH
(define-gtkkey #x100057b "Armenian_je") ;U+057B ARMENIAN SMALL LETTER JHEH
(define-gtkkey #x100054c "Armenian_RA") ;U+054C ARMENIAN CAPITAL LETTER RA
(define-gtkkey #x100057c "Armenian_ra") ;U+057C ARMENIAN SMALL LETTER RA
(define-gtkkey #x100054d "Armenian_SE") ;U+054D ARMENIAN CAPITAL LETTER SEH
(define-gtkkey #x100057d "Armenian_se") ;U+057D ARMENIAN SMALL LETTER SEH
(define-gtkkey #x100054e "Armenian_VEV") ;U+054E ARMENIAN CAPITAL LETTER VEW
(define-gtkkey #x100057e "Armenian_vev") ;U+057E ARMENIAN SMALL LETTER VEW
(define-gtkkey #x100054f "Armenian_TYUN") ;U+054F ARMENIAN CAPITAL LETTER TIWN
(define-gtkkey #x100057f "Armenian_tyun") ;U+057F ARMENIAN SMALL LETTER TIWN
(define-gtkkey #x1000550 "Armenian_RE") ;U+0550 ARMENIAN CAPITAL LETTER REH
(define-gtkkey #x1000580 "Armenian_re") ;U+0580 ARMENIAN SMALL LETTER REH
(define-gtkkey #x1000551 "Armenian_TSO") ;U+0551 ARMENIAN CAPITAL LETTER CO
(define-gtkkey #x1000581 "Armenian_tso") ;U+0581 ARMENIAN SMALL LETTER CO
(define-gtkkey #x1000552 "Armenian_VYUN") ;U+0552 ARMENIAN CAPITAL LETTER YIWN
(define-gtkkey #x1000582 "Armenian_vyun") ;U+0582 ARMENIAN SMALL LETTER YIWN
(define-gtkkey #x1000553 "Armenian_PYUR") ;U+0553 ARMENIAN CAPITAL LETTER PIWR
(define-gtkkey #x1000583 "Armenian_pyur") ;U+0583 ARMENIAN SMALL LETTER PIWR
(define-gtkkey #x1000554 "Armenian_KE") ;U+0554 ARMENIAN CAPITAL LETTER KEH
(define-gtkkey #x1000584 "Armenian_ke") ;U+0584 ARMENIAN SMALL LETTER KEH
(define-gtkkey #x1000555 "Armenian_O") ;U+0555 ARMENIAN CAPITAL LETTER OH
(define-gtkkey #x1000585 "Armenian_o") ;U+0585 ARMENIAN SMALL LETTER OH
(define-gtkkey #x1000556 "Armenian_FE") ;U+0556 ARMENIAN CAPITAL LETTER FEH
(define-gtkkey #x1000586 "Armenian_fe") ;U+0586 ARMENIAN SMALL LETTER FEH
(define-gtkkey #x100055a "Armenian_apostrophe") ;U+055A ARMENIAN APOSTROPHE
(define-gtkkey #x10010d0 "Georgian_an") ;U+10D0 GEORGIAN LETTER AN
(define-gtkkey #x10010d1 "Georgian_ban") ;U+10D1 GEORGIAN LETTER BAN
(define-gtkkey #x10010d2 "Georgian_gan") ;U+10D2 GEORGIAN LETTER GAN
(define-gtkkey #x10010d3 "Georgian_don") ;U+10D3 GEORGIAN LETTER DON
(define-gtkkey #x10010d4 "Georgian_en") ;U+10D4 GEORGIAN LETTER EN
(define-gtkkey #x10010d5 "Georgian_vin") ;U+10D5 GEORGIAN LETTER VIN
(define-gtkkey #x10010d6 "Georgian_zen") ;U+10D6 GEORGIAN LETTER ZEN
(define-gtkkey #x10010d7 "Georgian_tan") ;U+10D7 GEORGIAN LETTER TAN
(define-gtkkey #x10010d8 "Georgian_in") ;U+10D8 GEORGIAN LETTER IN
(define-gtkkey #x10010d9 "Georgian_kan") ;U+10D9 GEORGIAN LETTER KAN
(define-gtkkey #x10010da "Georgian_las") ;U+10DA GEORGIAN LETTER LAS
(define-gtkkey #x10010db "Georgian_man") ;U+10DB GEORGIAN LETTER MAN
(define-gtkkey #x10010dc "Georgian_nar") ;U+10DC GEORGIAN LETTER NAR
(define-gtkkey #x10010dd "Georgian_on") ;U+10DD GEORGIAN LETTER ON
(define-gtkkey #x10010de "Georgian_par") ;U+10DE GEORGIAN LETTER PAR
(define-gtkkey #x10010df "Georgian_zhar") ;U+10DF GEORGIAN LETTER ZHAR
(define-gtkkey #x10010e0 "Georgian_rae") ;U+10E0 GEORGIAN LETTER RAE
(define-gtkkey #x10010e1 "Georgian_san") ;U+10E1 GEORGIAN LETTER SAN
(define-gtkkey #x10010e2 "Georgian_tar") ;U+10E2 GEORGIAN LETTER TAR
(define-gtkkey #x10010e3 "Georgian_un") ;U+10E3 GEORGIAN LETTER UN
(define-gtkkey #x10010e4 "Georgian_phar") ;U+10E4 GEORGIAN LETTER PHAR
(define-gtkkey #x10010e5 "Georgian_khar") ;U+10E5 GEORGIAN LETTER KHAR
(define-gtkkey #x10010e6 "Georgian_ghan") ;U+10E6 GEORGIAN LETTER GHAN
(define-gtkkey #x10010e7 "Georgian_qar") ;U+10E7 GEORGIAN LETTER QAR
(define-gtkkey #x10010e8 "Georgian_shin") ;U+10E8 GEORGIAN LETTER SHIN
(define-gtkkey #x10010e9 "Georgian_chin") ;U+10E9 GEORGIAN LETTER CHIN
(define-gtkkey #x10010ea "Georgian_can") ;U+10EA GEORGIAN LETTER CAN
(define-gtkkey #x10010eb "Georgian_jil") ;U+10EB GEORGIAN LETTER JIL
(define-gtkkey #x10010ec "Georgian_cil") ;U+10EC GEORGIAN LETTER CIL
(define-gtkkey #x10010ed "Georgian_char") ;U+10ED GEORGIAN LETTER CHAR
(define-gtkkey #x10010ee "Georgian_xan") ;U+10EE GEORGIAN LETTER XAN
(define-gtkkey #x10010ef "Georgian_jhan") ;U+10EF GEORGIAN LETTER JHAN
(define-gtkkey #x10010f0 "Georgian_hae") ;U+10F0 GEORGIAN LETTER HAE
(define-gtkkey #x10010f1 "Georgian_he") ;U+10F1 GEORGIAN LETTER HE
(define-gtkkey #x10010f2 "Georgian_hie") ;U+10F2 GEORGIAN LETTER HIE
(define-gtkkey #x10010f3 "Georgian_we") ;U+10F3 GEORGIAN LETTER WE
(define-gtkkey #x10010f4 "Georgian_har") ;U+10F4 GEORGIAN LETTER HAR
(define-gtkkey #x10010f5 "Georgian_hoe") ;U+10F5 GEORGIAN LETTER HOE
(define-gtkkey #x10010f6 "Georgian_fi") ;U+10F6 GEORGIAN LETTER FI
(define-gtkkey #x1001e8a "Xabovedot") ;U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE
(define-gtkkey #x100012c "Ibreve") ;U+012C LATIN CAPITAL LETTER I WITH BREVE
(define-gtkkey #x10001b5 "Zstroke") ;U+01B5 LATIN CAPITAL LETTER Z WITH STROKE
(define-gtkkey #x10001e6 "Gcaron") ;U+01E6 LATIN CAPITAL LETTER G WITH CARON
(define-gtkkey #x10001d1 "Ocaron") ;U+01D2 LATIN CAPITAL LETTER O WITH CARON
(define-gtkkey #x100019f "Obarred") ;U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE
(define-gtkkey #x1001e8b "xabovedot") ;U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE
(define-gtkkey #x100012d "ibreve") ;U+012D LATIN SMALL LETTER I WITH BREVE
(define-gtkkey #x10001b6 "zstroke") ;U+01B6 LATIN SMALL LETTER Z WITH STROKE
(define-gtkkey #x10001e7 "gcaron") ;U+01E7 LATIN SMALL LETTER G WITH CARON
(define-gtkkey #x10001d2 "ocaron") ;U+01D2 LATIN SMALL LETTER O WITH CARON
(define-gtkkey #x1000275 "obarred") ;U+0275 LATIN SMALL LETTER BARRED O
(define-gtkkey #x100018f "SCHWA")   ;U+018F LATIN CAPITAL LETTER SCHWA
(define-gtkkey #x1000259 "schwa")     ;U+0259 LATIN SMALL LETTER SCHWA
(define-gtkkey #x1001e36 "Lbelowdot") ;U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW
(define-gtkkey #x1001e37 "lbelowdot") ;U+1E37 LATIN SMALL LETTER L WITH DOT BELOW
(define-gtkkey #x1001ea0 "Abelowdot") ;U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW
(define-gtkkey #x1001ea1 "abelowdot") ;U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW
(define-gtkkey #x1001ea2 "Ahook") ;U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE
(define-gtkkey #x1001ea3 "ahook") ;U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE
(define-gtkkey #x1001ea4 "Acircumflexacute") ;U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
(define-gtkkey #x1001ea5 "acircumflexacute") ;U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
(define-gtkkey #x1001ea6 "Acircumflexgrave") ;U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
(define-gtkkey #x1001ea7 "acircumflexgrave") ;U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
(define-gtkkey #x1001ea8 "Acircumflexhook") ;U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
(define-gtkkey #x1001ea9 "acircumflexhook") ;U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
(define-gtkkey #x1001eaa "Acircumflextilde") ;U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
(define-gtkkey #x1001eab "acircumflextilde") ;U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
(define-gtkkey #x1001eac "Acircumflexbelowdot") ;U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
(define-gtkkey #x1001ead "acircumflexbelowdot") ;U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
(define-gtkkey #x1001eae "Abreveacute") ;U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
(define-gtkkey #x1001eaf "abreveacute") ;U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE
(define-gtkkey #x1001eb0 "Abrevegrave") ;U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
(define-gtkkey #x1001eb1 "abrevegrave") ;U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE
(define-gtkkey #x1001eb2 "Abrevehook") ;U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
(define-gtkkey #x1001eb3 "abrevehook") ;U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
(define-gtkkey #x1001eb4 "Abrevetilde") ;U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE
(define-gtkkey #x1001eb5 "abrevetilde") ;U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE
(define-gtkkey #x1001eb6 "Abrevebelowdot") ;U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
(define-gtkkey #x1001eb7 "abrevebelowdot") ;U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
(define-gtkkey #x1001eb8 "Ebelowdot") ;U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW
(define-gtkkey #x1001eb9 "ebelowdot") ;U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW
(define-gtkkey #x1001eba "Ehook") ;U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE
(define-gtkkey #x1001ebb "ehook") ;U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE
(define-gtkkey #x1001ebc "Etilde") ;U+1EBC LATIN CAPITAL LETTER E WITH TILDE
(define-gtkkey #x1001ebd "etilde") ;U+1EBD LATIN SMALL LETTER E WITH TILDE
(define-gtkkey #x1001ebe "Ecircumflexacute") ;U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
(define-gtkkey #x1001ebf "ecircumflexacute") ;U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
(define-gtkkey #x1001ec0 "Ecircumflexgrave") ;U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
(define-gtkkey #x1001ec1 "ecircumflexgrave") ;U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
(define-gtkkey #x1001ec2 "Ecircumflexhook") ;U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
(define-gtkkey #x1001ec3 "ecircumflexhook") ;U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
(define-gtkkey #x1001ec4 "Ecircumflextilde") ;U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
(define-gtkkey #x1001ec5 "ecircumflextilde") ;U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
(define-gtkkey #x1001ec6 "Ecircumflexbelowdot") ;U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
(define-gtkkey #x1001ec7 "ecircumflexbelowdot") ;U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
(define-gtkkey #x1001ec8 "Ihook") ;U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE
(define-gtkkey #x1001ec9 "ihook") ;U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE
(define-gtkkey #x1001eca "Ibelowdot") ;U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW
(define-gtkkey #x1001ecb "ibelowdot") ;U+1ECB LATIN SMALL LETTER I WITH DOT BELOW
(define-gtkkey #x1001ecc "Obelowdot") ;U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW
(define-gtkkey #x1001ecd "obelowdot") ;U+1ECD LATIN SMALL LETTER O WITH DOT BELOW
(define-gtkkey #x1001ece "Ohook") ;U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE
(define-gtkkey #x1001ecf "ohook") ;U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE
(define-gtkkey #x1001ed0 "Ocircumflexacute") ;U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
(define-gtkkey #x1001ed1 "ocircumflexacute") ;U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
(define-gtkkey #x1001ed2 "Ocircumflexgrave") ;U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
(define-gtkkey #x1001ed3 "ocircumflexgrave") ;U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
(define-gtkkey #x1001ed4 "Ocircumflexhook") ;U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
(define-gtkkey #x1001ed5 "ocircumflexhook") ;U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
(define-gtkkey #x1001ed6 "Ocircumflextilde") ;U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
(define-gtkkey #x1001ed7 "ocircumflextilde") ;U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
(define-gtkkey #x1001ed8 "Ocircumflexbelowdot") ;U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
(define-gtkkey #x1001ed9 "ocircumflexbelowdot") ;U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
(define-gtkkey #x1001eda "Ohornacute") ;U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE
(define-gtkkey #x1001edb "ohornacute") ;U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE
(define-gtkkey #x1001edc "Ohorngrave") ;U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE
(define-gtkkey #x1001edd "ohorngrave") ;U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE
(define-gtkkey #x1001ede "Ohornhook") ;U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
(define-gtkkey #x1001edf "ohornhook") ;U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
(define-gtkkey #x1001ee0 "Ohorntilde") ;U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE
(define-gtkkey #x1001ee1 "ohorntilde") ;U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE
(define-gtkkey #x1001ee2 "Ohornbelowdot") ;U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
(define-gtkkey #x1001ee3 "ohornbelowdot") ;U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW
(define-gtkkey #x1001ee4 "Ubelowdot") ;U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW
(define-gtkkey #x1001ee5 "ubelowdot") ;U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW
(define-gtkkey #x1001ee6 "Uhook") ;U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE
(define-gtkkey #x1001ee7 "uhook") ;U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE
(define-gtkkey #x1001ee8 "Uhornacute") ;U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE
(define-gtkkey #x1001ee9 "uhornacute") ;U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE
(define-gtkkey #x1001eea "Uhorngrave") ;U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE
(define-gtkkey #x1001eeb "uhorngrave") ;U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE
(define-gtkkey #x1001eec "Uhornhook") ;U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
(define-gtkkey #x1001eed "uhornhook") ;U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
(define-gtkkey #x1001eee "Uhorntilde") ;U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE
(define-gtkkey #x1001eef "uhorntilde") ;U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE
(define-gtkkey #x1001ef0 "Uhornbelowdot") ;U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
(define-gtkkey #x1001ef1 "uhornbelowdot") ;U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW
(define-gtkkey #x1001ef4 "Ybelowdot") ;U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW
(define-gtkkey #x1001ef5 "ybelowdot") ;U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW
(define-gtkkey #x1001ef6 "Yhook") ;U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE
(define-gtkkey #x1001ef7 "yhook") ;U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE
(define-gtkkey #x1001ef8 "Ytilde") ;U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE
(define-gtkkey #x1001ef9 "ytilde") ;U+1EF9 LATIN SMALL LETTER Y WITH TILDE
(define-gtkkey #x10001a0 "Ohorn") ;U+01A0 LATIN CAPITAL LETTER O WITH HORN
(define-gtkkey #x10001a1 "ohorn") ;U+01A1 LATIN SMALL LETTER O WITH HORN
(define-gtkkey #x10001af "Uhorn") ;U+01AF LATIN CAPITAL LETTER U WITH HORN
(define-gtkkey #x10001b0 "uhorn") ;U+01B0 LATIN SMALL LETTER U WITH HORN
(define-gtkkey #x10020a0 "EcuSign")     ;U+20A0 EURO-CURRENCY SIGN
(define-gtkkey #x10020a1 "ColonSign")   ;U+20A1 COLON SIGN
(define-gtkkey #x10020a2 "CruzeiroSign") ;U+20A2 CRUZEIRO SIGN
(define-gtkkey #x10020a3 "FFrancSign")  ;U+20A3 FRENCH FRANC SIGN
(define-gtkkey #x10020a4 "LiraSign")    ;U+20A4 LIRA SIGN
(define-gtkkey #x10020a5 "MillSign")    ;U+20A5 MILL SIGN
(define-gtkkey #x10020a6 "NairaSign")   ;U+20A6 NAIRA SIGN
(define-gtkkey #x10020a7 "PesetaSign")  ;U+20A7 PESETA SIGN
(define-gtkkey #x10020a8 "RupeeSign")   ;U+20A8 RUPEE SIGN
(define-gtkkey #x10020a9 "WonSign")     ;U+20A9 WON SIGN
(define-gtkkey #x10020aa "NewSheqelSign") ;U+20AA NEW SHEQEL SIGN
(define-gtkkey #x10020ab "DongSign")    ;U+20AB DONG SIGN
(define-gtkkey #x20ac "EuroSign")       ;U+20AC EURO SIGN
(define-gtkkey #x1002070 "zerosuperior") ;U+2070 SUPERSCRIPT ZERO
(define-gtkkey #x1002074 "foursuperior") ;U+2074 SUPERSCRIPT FOUR
(define-gtkkey #x1002075 "fivesuperior") ;U+2075 SUPERSCRIPT FIVE
(define-gtkkey #x1002076 "sixsuperior") ;U+2076 SUPERSCRIPT SIX
(define-gtkkey #x1002077 "sevensuperior") ;U+2077 SUPERSCRIPT SEVEN
(define-gtkkey #x1002078 "eightsuperior") ;U+2078 SUPERSCRIPT EIGHT
(define-gtkkey #x1002079 "ninesuperior") ;U+2079 SUPERSCRIPT NINE
(define-gtkkey #x1002080 "zerosubscript") ;U+2080 SUBSCRIPT ZERO
(define-gtkkey #x1002081 "onesubscript") ;U+2081 SUBSCRIPT ONE
(define-gtkkey #x1002082 "twosubscript") ;U+2082 SUBSCRIPT TWO
(define-gtkkey #x1002083 "threesubscript") ;U+2083 SUBSCRIPT THREE
(define-gtkkey #x1002084 "foursubscript") ;U+2084 SUBSCRIPT FOUR
(define-gtkkey #x1002085 "fivesubscript") ;U+2085 SUBSCRIPT FIVE
(define-gtkkey #x1002086 "sixsubscript") ;U+2086 SUBSCRIPT SIX
(define-gtkkey #x1002087 "sevensubscript") ;U+2087 SUBSCRIPT SEVEN
(define-gtkkey #x1002088 "eightsubscript") ;U+2088 SUBSCRIPT EIGHT
(define-gtkkey #x1002089 "ninesubscript") ;U+2089 SUBSCRIPT NINE
(define-gtkkey #x1002202 "partdifferential") ;U+2202 PARTIAL DIFFERENTIAL
(define-gtkkey #x1002205 "emptyset")    ;U+2205 NULL SET
(define-gtkkey #x1002208 "elementof")   ;U+2208 ELEMENT OF
(define-gtkkey #x1002209 "notelementof") ;U+2209 NOT AN ELEMENT OF
(define-gtkkey #x100220B "containsas")  ;U+220B CONTAINS AS MEMBER
(define-gtkkey #x100221A "squareroot")  ;U+221A SQUARE ROOT
(define-gtkkey #x100221B "cuberoot")    ;U+221B CUBE ROOT
(define-gtkkey #x100221C "fourthroot")  ;U+221C FOURTH ROOT
(define-gtkkey #x100222C "dintegral")   ;U+222C DOUBLE INTEGRAL
(define-gtkkey #x100222D "tintegral")   ;U+222D TRIPLE INTEGRAL
(define-gtkkey #x1002235 "because")     ;U+2235 BECAUSE
(define-gtkkey #x1002248 "approxeq")    ;U+2245 ALMOST EQUAL TO
(define-gtkkey #x1002247 "notapproxeq") ;U+2247 NOT ALMOST EQUAL TO
(define-gtkkey #x1002262 "notidentical") ;U+2262 NOT IDENTICAL TO
(define-gtkkey #x1002263 "stricteq")    ;U+2263 STRICTLY EQUIVALENT TO

;; A bunch of extended gtkkeys

(define-gtkkey #x100000A8 "hpmute_acute")
(define-gtkkey #x100000A9 "hpmute_grave")
(define-gtkkey #x100000AA "hpmute_asciicircum")
(define-gtkkey #x100000AB "hpmute_diaeresis")
(define-gtkkey #x100000AC "hpmute_asciitilde")
(define-gtkkey #x100000AF "hplira")
(define-gtkkey #x100000BE "hpguilder")
(define-gtkkey #x100000EE "hpYdiaeresis")
(define-gtkkey #x100000EE "hpIO")
(define-gtkkey #x100000F6 "hplongminus")
(define-gtkkey #x100000FC "hpblock")
(define-gtkkey #x1000FF00 "apLineDel")
(define-gtkkey #x1000FF01 "apCharDel")
(define-gtkkey #x1000FF02 "apCopy")
(define-gtkkey #x1000FF03 "apCut")
(define-gtkkey #x1000FF04 "apPaste")
(define-gtkkey #x1000FF05 "apMove")
(define-gtkkey #x1000FF06 "apGrow")
(define-gtkkey #x1000FF07 "apCmd")
(define-gtkkey #x1000FF08 "apShell")
(define-gtkkey #x1000FF09 "apLeftBar")
(define-gtkkey #x1000FF0A "apRightBar")
(define-gtkkey #x1000FF0B "apLeftBox")
(define-gtkkey #x1000FF0C "apRightBox")
(define-gtkkey #x1000FF0D "apUpBox")
(define-gtkkey #x1000FF0E "apDownBox")
(define-gtkkey #x1000FF0F "apPop")
(define-gtkkey #x1000FF10 "apRead")
(define-gtkkey #x1000FF11 "apEdit")
(define-gtkkey #x1000FF12 "apSave")
(define-gtkkey #x1000FF13 "apExit")
(define-gtkkey #x1000FF14 "apRepeat")
(define-gtkkey #x1000FF48 "hpModelock1")
(define-gtkkey #x1000FF49 "hpModelock2")
(define-gtkkey #x1000FF6C "hpReset")
(define-gtkkey #x1000FF6D "hpSystem")
(define-gtkkey #x1000FF6E "hpUser")
(define-gtkkey #x1000FF6F "hpClearLine")
(define-gtkkey #x1000FF70 "hpInsertLine")
(define-gtkkey #x1000FF71 "hpDeleteLine")
(define-gtkkey #x1000FF72 "hpInsertChar")
(define-gtkkey #x1000FF73 "hpDeleteChar")
(define-gtkkey #x1000FF74 "hpBackTab")
(define-gtkkey #x1000FF75 "hpKP_BackTab")
(define-gtkkey #x1000FFA8 "apKP_parenleft")
(define-gtkkey #x1000FFA9 "apKP_parenright")
(define-gtkkey #x10004001 "I2ND_FUNC_L")
(define-gtkkey #x10004002 "I2ND_FUNC_R")
(define-gtkkey #x10004003 "IREMOVE")
(define-gtkkey #x10004004 "IREPEAT")
(define-gtkkey #x10004101 "IA1")
(define-gtkkey #x10004102 "IA2")
(define-gtkkey #x10004103 "IA3")
(define-gtkkey #x10004104 "IA4")
(define-gtkkey #x10004105 "IA5")
(define-gtkkey #x10004106 "IA6")
(define-gtkkey #x10004107 "IA7")
(define-gtkkey #x10004108 "IA8")
(define-gtkkey #x10004109 "IA9")
(define-gtkkey #x1000410A "IA10")
(define-gtkkey #x1000410B "IA11")
(define-gtkkey #x1000410C "IA12")
(define-gtkkey #x1000410D "IA13")
(define-gtkkey #x1000410E "IA14")
(define-gtkkey #x1000410F "IA15")
(define-gtkkey #x10004201 "IB1")
(define-gtkkey #x10004202 "IB2")
(define-gtkkey #x10004203 "IB3")
(define-gtkkey #x10004204 "IB4")
(define-gtkkey #x10004205 "IB5")
(define-gtkkey #x10004206 "IB6")
(define-gtkkey #x10004207 "IB7")
(define-gtkkey #x10004208 "IB8")
(define-gtkkey #x10004209 "IB9")
(define-gtkkey #x1000420A "IB10")
(define-gtkkey #x1000420B "IB11")
(define-gtkkey #x1000420C "IB12")
(define-gtkkey #x1000420D "IB13")
(define-gtkkey #x1000420E "IB14")
(define-gtkkey #x1000420F "IB15")
(define-gtkkey #x10004210 "IB16")
(define-gtkkey #x1000FF00 "DRemove")
(define-gtkkey #x1000FEB0 "Dring_accent")
(define-gtkkey #x1000FE5E "Dcircumflex_accent")
(define-gtkkey #x1000FE2C "Dcedilla_accent")
(define-gtkkey #x1000FE27 "Dacute_accent")
(define-gtkkey #x1000FE60 "Dgrave_accent")
(define-gtkkey #x1000FE7E "Dtilde")
(define-gtkkey #x1000FE22 "Ddiaeresis")
(define-gtkkey #x1004FF02 "osfCopy")
(define-gtkkey #x1004FF03 "osfCut")
(define-gtkkey #x1004FF04 "osfPaste")
(define-gtkkey #x1004FF07 "osfBackTab")
(define-gtkkey #x1004FF08 "osfBackSpace")
(define-gtkkey #x1004FF0B "osfClear")
(define-gtkkey #x1004FF1B "osfEscape")
(define-gtkkey #x1004FF31 "osfAddMode")
(define-gtkkey #x1004FF32 "osfPrimaryPaste")
(define-gtkkey #x1004FF33 "osfQuickPaste")
(define-gtkkey #x1004FF40 "osfPageLeft")
(define-gtkkey #x1004FF41 "osfPageUp")
(define-gtkkey #x1004FF42 "osfPageDown")
(define-gtkkey #x1004FF43 "osfPageRight")
(define-gtkkey #x1004FF44 "osfActivate")
(define-gtkkey #x1004FF45 "osfMenuBar")
(define-gtkkey #x1004FF51 "osfLeft")
(define-gtkkey #x1004FF52 "osfUp")
(define-gtkkey #x1004FF53 "osfRight")
(define-gtkkey #x1004FF54 "osfDown")
(define-gtkkey #x1004FF55 "osfPrior")
(define-gtkkey #x1004FF56 "osfNext")
(define-gtkkey #x1004FF57 "osfEndLine")
(define-gtkkey #x1004FF58 "osfBeginLine")
(define-gtkkey #x1004FF59 "osfEndData")
(define-gtkkey #x1004FF5A "osfBeginData")
(define-gtkkey #x1004FF5B "osfPrevMenu")
(define-gtkkey #x1004FF5C "osfNextMenu")
(define-gtkkey #x1004FF5D "osfPrevField")
(define-gtkkey #x1004FF5E "osfNextField")
(define-gtkkey #x1004FF60 "osfSelect")
(define-gtkkey #x1004FF63 "osfInsert")
(define-gtkkey #x1004FF65 "osfUndo")
(define-gtkkey #x1004FF67 "osfMenu")
(define-gtkkey #x1004FF69 "osfCancel")
(define-gtkkey #x1004FF6A "osfHelp")
(define-gtkkey #x1004FF71 "osfSelectAll")
(define-gtkkey #x1004FF72 "osfDeselectAll")
(define-gtkkey #x1004FF73 "osfReselect")
(define-gtkkey #x1004FF74 "osfExtend")
(define-gtkkey #x1004FF78 "osfRestore")
(define-gtkkey #x1004FF7E "osfSwitchDirection")
(define-gtkkey #x1004FFF5 "osfPriorMinor")
(define-gtkkey #x1004FFF6 "osfNextMinor")
(define-gtkkey #x1004FFF7 "osfRightLine")
(define-gtkkey #x1004FFF8 "osfLeftLine")
(define-gtkkey #x1004FFFF "osfDelete")
(define-gtkkey #x1005FF00 "SunFA_Grave")
(define-gtkkey #x1005FF01 "SunFA_Circum")
(define-gtkkey #x1005FF02 "SunFA_Tilde")
(define-gtkkey #x1005FF03 "SunFA_Acute")
(define-gtkkey #x1005FF04 "SunFA_Diaeresis")
(define-gtkkey #x1005FF05 "SunFA_Cedilla")
(define-gtkkey #x1005FF10 "SunF36")
(define-gtkkey #x1005FF11 "SunF37")
(define-gtkkey #x1005FF60 "SunSys_Req")
(define-gtkkey #x1005FF70 "SunProps")
(define-gtkkey #x1005FF71 "SunFront")
(define-gtkkey #x1005FF72 "SunCopy")
(define-gtkkey #x1005FF73 "SunOpen")
(define-gtkkey #x1005FF74 "SunPaste")
(define-gtkkey #x1005FF75 "SunCut")
(define-gtkkey #x1005FF76 "SunPowerSwitch")
(define-gtkkey #x1005FF77 "SunAudioLowerVolume")
(define-gtkkey #x1005FF78 "SunAudioMute")
(define-gtkkey #x1005FF79 "SunAudioRaiseVolume")
(define-gtkkey #x1005FF7A "SunVideoDegauss")
(define-gtkkey #x1005FF7B "SunVideoLowerBrightness")
(define-gtkkey #x1005FF7C "SunVideoRaiseBrightness")
(define-gtkkey #x1005FF7D "SunPowerSwitchShift")
(define-gtkkey #xFF20 "SunCompose")
(define-gtkkey #xFF55 "SunPageUp")
(define-gtkkey #xFF56 "SunPageDown")
(define-gtkkey #xFF61 "SunPrint_Screen")
(define-gtkkey #xFF65 "SunUndo")
(define-gtkkey #xFF66 "SunAgain")
(define-gtkkey #xFF68 "SunFind")
(define-gtkkey #xFF69 "SunStop")
(define-gtkkey #xFF7E "SunAltGraph")
(define-gtkkey #x1006FF00 "WYSetup")
(define-gtkkey #x1006FF00 "ncdSetup")
(define-gtkkey #x10070001 "XeroxPointerButton1")
(define-gtkkey #x10070002 "XeroxPointerButton2")
(define-gtkkey #x10070003 "XeroxPointerButton3")
(define-gtkkey #x10070004 "XeroxPointerButton4")
(define-gtkkey #x10070005 "XeroxPointerButton5")
(define-gtkkey #x1008FF01 "XF86ModeLock")
(define-gtkkey #x1008FF02 "XF86MonBrightnessUp")
(define-gtkkey #x1008FF03 "XF86MonBrightnessDown")
(define-gtkkey #x1008FF04 "XF86KbdLightOnOff")
(define-gtkkey #x1008FF05 "XF86KbdBrightnessUp")
(define-gtkkey #x1008FF06 "XF86KbdBrightnessDown")
(define-gtkkey #x1008FF10 "XF86Standby")
(define-gtkkey #x1008FF11 "XF86AudioLowerVolume")
(define-gtkkey #x1008FF12 "XF86AudioMute")
(define-gtkkey #x1008FF13 "XF86AudioRaiseVolume")
(define-gtkkey #x1008FF14 "XF86AudioPlay")
(define-gtkkey #x1008FF15 "XF86AudioStop")
(define-gtkkey #x1008FF16 "XF86AudioPrev")
(define-gtkkey #x1008FF17 "XF86AudioNext")
(define-gtkkey #x1008FF18 "XF86HomePage")
(define-gtkkey #x1008FF19 "XF86Mail")
(define-gtkkey #x1008FF1A "XF86Start")
(define-gtkkey #x1008FF1B "XF86Search")
(define-gtkkey #x1008FF1C "XF86AudioRecord")
(define-gtkkey #x1008FF1D "XF86Calculator")
(define-gtkkey #x1008FF1E "XF86Memo")
(define-gtkkey #x1008FF1F "XF86ToDoList")
(define-gtkkey #x1008FF20 "XF86Calendar")
(define-gtkkey #x1008FF21 "XF86PowerDown")
(define-gtkkey #x1008FF22 "XF86ContrastAdjust")
(define-gtkkey #x1008FF23 "XF86RockerUp")
(define-gtkkey #x1008FF24 "XF86RockerDown")
(define-gtkkey #x1008FF25 "XF86RockerEnter")
(define-gtkkey #x1008FF26 "XF86Back")
(define-gtkkey #x1008FF27 "XF86Forward")
(define-gtkkey #x1008FF28 "XF86Stop")
(define-gtkkey #x1008FF29 "XF86Refresh")
(define-gtkkey #x1008FF2A "XF86PowerOff")
(define-gtkkey #x1008FF2B "XF86WakeUp")
(define-gtkkey #x1008FF2C "XF86Eject")
(define-gtkkey #x1008FF2D "XF86ScreenSaver")
(define-gtkkey #x1008FF2E "XF86WWW")
(define-gtkkey #x1008FF2F "XF86Sleep")
(define-gtkkey #x1008FF30 "XF86Favorites")
(define-gtkkey #x1008FF31 "XF86AudioPause")
(define-gtkkey #x1008FF32 "XF86AudioMedia")
(define-gtkkey #x1008FF33 "XF86MyComputer")
(define-gtkkey #x1008FF34 "XF86VendorHome")
(define-gtkkey #x1008FF35 "XF86LightBulb")
(define-gtkkey #x1008FF36 "XF86Shop")
(define-gtkkey #x1008FF37 "XF86History")
(define-gtkkey #x1008FF38 "XF86OpenURL")
(define-gtkkey #x1008FF39 "XF86AddFavorite")
(define-gtkkey #x1008FF3A "XF86HotLinks")
(define-gtkkey #x1008FF3B "XF86BrightnessAdjust")
(define-gtkkey #x1008FF3C "XF86Finance")
(define-gtkkey #x1008FF3D "XF86Community")
(define-gtkkey #x1008FF3E "XF86AudioRewind")
(define-gtkkey #x1008FF3F "XF86BackForward")
(define-gtkkey #x1008FF40 "XF86Launch0")
(define-gtkkey #x1008FF41 "XF86Launch1")
(define-gtkkey #x1008FF42 "XF86Launch2")
(define-gtkkey #x1008FF43 "XF86Launch3")
(define-gtkkey #x1008FF44 "XF86Launch4")
(define-gtkkey #x1008FF45 "XF86Launch5")
(define-gtkkey #x1008FF46 "XF86Launch6")
(define-gtkkey #x1008FF47 "XF86Launch7")
(define-gtkkey #x1008FF48 "XF86Launch8")
(define-gtkkey #x1008FF49 "XF86Launch9")
(define-gtkkey #x1008FF4A "XF86LaunchA")
(define-gtkkey #x1008FF4B "XF86LaunchB")
(define-gtkkey #x1008FF4C "XF86LaunchC")
(define-gtkkey #x1008FF4D "XF86LaunchD")
(define-gtkkey #x1008FF4E "XF86LaunchE")
(define-gtkkey #x1008FF4F "XF86LaunchF")
(define-gtkkey #x1008FF50 "XF86ApplicationLeft")
(define-gtkkey #x1008FF51 "XF86ApplicationRight")
(define-gtkkey #x1008FF52 "XF86Book")
(define-gtkkey #x1008FF53 "XF86CD")
(define-gtkkey #x1008FF54 "XF86Calculater")
(define-gtkkey #x1008FF55 "XF86Clear")
(define-gtkkey #x1008FF56 "XF86Close")
(define-gtkkey #x1008FF57 "XF86Copy")
(define-gtkkey #x1008FF58 "XF86Cut")
(define-gtkkey #x1008FF59 "XF86Display")
(define-gtkkey #x1008FF5A "XF86DOS")
(define-gtkkey #x1008FF5B "XF86Documents")
(define-gtkkey #x1008FF5C "XF86Excel")
(define-gtkkey #x1008FF5D "XF86Explorer")
(define-gtkkey #x1008FF5E "XF86Game")
(define-gtkkey #x1008FF5F "XF86Go")
(define-gtkkey #x1008FF60 "XF86iTouch")
(define-gtkkey #x1008FF61 "XF86LogOff")
(define-gtkkey #x1008FF62 "XF86Market")
(define-gtkkey #x1008FF63 "XF86Meeting")
(define-gtkkey #x1008FF65 "XF86MenuKB")
(define-gtkkey #x1008FF66 "XF86MenuPB")
(define-gtkkey #x1008FF67 "XF86MySites")
(define-gtkkey #x1008FF68 "XF86New")
(define-gtkkey #x1008FF69 "XF86News")
(define-gtkkey #x1008FF6A "XF86OfficeHome")
(define-gtkkey #x1008FF6B "XF86Open")
(define-gtkkey #x1008FF6C "XF86Option")
(define-gtkkey #x1008FF6D "XF86Paste")
(define-gtkkey #x1008FF6E "XF86Phone")
(define-gtkkey #x1008FF70 "XF86Q")
(define-gtkkey #x1008FF72 "XF86Reply")
(define-gtkkey #x1008FF73 "XF86Reload")
(define-gtkkey #x1008FF74 "XF86RotateWindows")
(define-gtkkey #x1008FF75 "XF86RotationPB")
(define-gtkkey #x1008FF76 "XF86RotationKB")
(define-gtkkey #x1008FF77 "XF86Save")
(define-gtkkey #x1008FF78 "XF86ScrollUp")
(define-gtkkey #x1008FF79 "XF86ScrollDown")
(define-gtkkey #x1008FF7A "XF86ScrollClick")
(define-gtkkey #x1008FF7B "XF86Send")
(define-gtkkey #x1008FF7C "XF86Spell")
(define-gtkkey #x1008FF7D "XF86SplitScreen")
(define-gtkkey #x1008FF7E "XF86Support")
(define-gtkkey #x1008FF7F "XF86TaskPane")
(define-gtkkey #x1008FF80 "XF86Terminal")
(define-gtkkey #x1008FF81 "XF86Tools")
(define-gtkkey #x1008FF82 "XF86Travel")
(define-gtkkey #x1008FF84 "XF86UserPB")
(define-gtkkey #x1008FF85 "XF86User1KB")
(define-gtkkey #x1008FF86 "XF86User2KB")
(define-gtkkey #x1008FF87 "XF86Video")
(define-gtkkey #x1008FF88 "XF86WheelButton")
(define-gtkkey #x1008FF89 "XF86Word")
(define-gtkkey #x1008FF8A "XF86Xfer")
(define-gtkkey #x1008FF8B "XF86ZoomIn")
(define-gtkkey #x1008FF8C "XF86ZoomOut")
(define-gtkkey #x1008FF8D "XF86Away")
(define-gtkkey #x1008FF8E "XF86Messenger")
(define-gtkkey #x1008FF8F "XF86WebCam")
(define-gtkkey #x1008FF90 "XF86MailForward")
(define-gtkkey #x1008FF91 "XF86Pictures")
(define-gtkkey #x1008FF92 "XF86Music")
(define-gtkkey #x1008FF93 "XF86Battery")
(define-gtkkey #x1008FF94 "XF86Bluetooth")
(define-gtkkey #x1008FF95 "XF86WLAN")
(define-gtkkey #x1008FF96 "XF86UWB")
(define-gtkkey #x1008FF97 "XF86AudioForward")
(define-gtkkey #x1008FF98 "XF86AudioRepeat")
(define-gtkkey #x1008FF99 "XF86AudioRandomPlay")
(define-gtkkey #x1008FF9A "XF86Subtitle")
(define-gtkkey #x1008FF9B "XF86AudioCycleTrack")
(define-gtkkey #x1008FF9C "XF86CycleAngle")
(define-gtkkey #x1008FF9D "XF86FrameBack")
(define-gtkkey #x1008FF9E "XF86FrameForward")
(define-gtkkey #x1008FF9F "XF86Time")
(define-gtkkey #x1008FFA0 "XF86Select")
(define-gtkkey #x1008FFA1 "XF86View")
(define-gtkkey #x1008FFA2 "XF86TopMenu")
(define-gtkkey #x1008FFA3 "XF86Red")
(define-gtkkey #x1008FFA4 "XF86Green")
(define-gtkkey #x1008FFA5 "XF86Yellow")
(define-gtkkey #x1008FFA6 "XF86Blue")
(define-gtkkey #x1008FFA7 "XF86Suspend")
(define-gtkkey #x1008FFA8 "XF86Hibernate")
(define-gtkkey #x1008FFA9 "XF86TouchpadToggle")
(define-gtkkey #x1008FFB0 "XF86TouchpadOn")
(define-gtkkey #x1008FFB1 "XF86TouchpadOff")
(define-gtkkey #x1008FFB2 "XF86AudioMicMute")
(define-gtkkey #x1008FE01 "XF86_Switch_VT_1")
(define-gtkkey #x1008FE02 "XF86_Switch_VT_2")
(define-gtkkey #x1008FE03 "XF86_Switch_VT_3")
(define-gtkkey #x1008FE04 "XF86_Switch_VT_4")
(define-gtkkey #x1008FE05 "XF86_Switch_VT_5")
(define-gtkkey #x1008FE06 "XF86_Switch_VT_6")
(define-gtkkey #x1008FE07 "XF86_Switch_VT_7")
(define-gtkkey #x1008FE08 "XF86_Switch_VT_8")
(define-gtkkey #x1008FE09 "XF86_Switch_VT_9")
(define-gtkkey #x1008FE0A "XF86_Switch_VT_10")
(define-gtkkey #x1008FE0B "XF86_Switch_VT_11")
(define-gtkkey #x1008FE0C "XF86_Switch_VT_12")
(define-gtkkey #x1008FE20 "XF86_Ungrab")
(define-gtkkey #x1008FE21 "XF86_ClearGrab")
(define-gtkkey #x1008FE22 "XF86_Next_VMode")
(define-gtkkey #x1008FE23 "XF86_Prev_VMode")
(define-gtkkey #x100000A8 "usldead_acute")
(define-gtkkey #x100000A9 "usldead_grave")
(define-gtkkey #x100000AB "usldead_diaeresis")
(define-gtkkey #x100000AA "usldead_asciicircum")
(define-gtkkey #x100000AC "usldead_asciitilde")
(define-gtkkey #x1000FE2C "usldead_cedilla")
(define-gtkkey #x1000FEB0 "usldead_ring")

(defun modifier-p (gtkkey)
  "check if the gtkkey is a modifier (shift, etc)"
  ;; TODO: these should probably be mapped individually...
  (and (>= gtkkey #xffe1)
       (<= gtkkey #xffee)))
