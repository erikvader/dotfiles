; header stuffs
; Save with BOM to make autohotkey realize this file is UTF-8
#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

keysEnabled := true ; toggle all keys
state := false ; modifiers state, for rebinding caps

; remap key but keep original on AltGr
altgrmove(orig, repl, replShift)
{
  if (getKeyState("LCtrl") and getKeyState("RAlt")) ; AltGr
  {
    Send {Blind}{RAlt up}{LCtrl up}%orig%{RAlt down}
  }
  else
  {
    beg := ""
    end := ""
    key := repl
    if (getKeyState("LCtrl"))
    {
      beg := beg "{RCtrl down}{LCtrl up}"
      end := "{LCtrl down}{RCtrl up}" end
    }
    if (getKeyState("Shift"))
    {
      key := replShift
      beg := beg "{Shift up}"
      end := "{Shift down}" end
    }
    Send {Blind}%beg%%key%%end%
  }
}

f12::keysEnabled := !keysEnabled
#If keysEnabled
$*ä::altgrmove("ä", "]", "{}}")
$*ö::altgrmove("ö", "[", "{{}")
$*å::altgrmove("å", "@", "\")
$*4::altgrmove("4", "4", "$")
$*¨::altgrmove("¨{Space}", "{^}{Space}", "{~}{Space}")
$*´::altgrmove("´", "``{Space}", "´{Space}")
#If

; caps lock = esc + control
; https://vim.fandom.com/wiki/Map_caps_lock_to_escape_in_Windows#AutoHotkey

; remap caps lock to control+esc
;#InstallKeybdHook
;SetCapsLockState, alwaysoff
;*Capslock::
;Send {LControl Down}
;KeyWait, CapsLock
;Send {LControl Up}
;if ( A_PriorKey = "CapsLock" )
;{
;  Send {Esc}
;}
;return

; remap control to control+esc
; Set caps lock to control: setxkbmap -option ctrl:nocaps
$~*LCtrl::
if !state
  state := (GetKeyState("Shift", "P") ||  GetKeyState("Alt", "P") || GetKeyState("LWin", "P") || GetKeyState("RWin", "P"))
return

$~LCtrl up::
if instr(A_PriorKey, "control") && !state
  send {esc}
state := false
return