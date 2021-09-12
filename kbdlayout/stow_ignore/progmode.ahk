#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

#InstallKeybdHook
SetCapsLockState, alwaysoff
*Capslock::
Send {LControl Down}
KeyWait, CapsLock
Send {LControl Up}
if ( A_PriorKey = "CapsLock" )
{
    Send {Esc}
}
return

isAltgrPressed()
{
  return getKeyState("LCtrl") and getKeyState("RAlt")
}

altgrmove(orig, repl, replShift)
{
  if (isAltgrPressed())
  {
    Send {Blind}{RAlt up}%orig%{RAlt down}
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

$*ä::altgrmove("ä", "]", "{}}")
$*ö::altgrmove("ö", "[", "{{}")
$*å::altgrmove("å", "@", "\")
$*4::altgrmove("4", "4", "$")
$*¨::altgrmove("¨{Space}", "{^}{Space}", "{~}{Space}")
$*´::altgrmove("´", "``{Space}", "´{Space}")
