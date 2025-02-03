unit UnitCommon;

interface

uses Winapi.Windows, Vcl.Controls;

{ ComputerOff }

{ Center the mouse cursor in the given control }
procedure CenterMouse(AControl: TWinControl; const AFocus: Boolean = True);

{ Others }

{ No-op function }
procedure noop; inline;

{ WinApi }

{ Get window title }
function CoGetWindowText(const AHwnd: HWND): string;

type
  CoTypeInfo = class
    class function GetName<T>(const AValue: T): string; inline;
  end;

implementation

uses System.Types, System.Math, System.SysUtils, System.TypInfo;

{ ComputerOff }

procedure CenterMouse(AControl: TWinControl; const AFocus: Boolean);
var
  LPoint: TPoint;
  LRoundModeOriginal: TRoundingMode;
begin
  // Nothing to do, the control is disabled, also don't center mouse in this case
  if not AControl.CanFocus then
    Exit;

  // Set focus if requested
  if AFocus then
    AControl.SetFocus;

  LRoundModeOriginal := GetRoundMode;
  SetRoundMode(rmDown);

  // Computer the new mouse position
  with AControl do
    LPoint := Parent.ClientToScreen(
      Point(Left + Round(Width / 2), Top + Round(Height / 2)));

  SetCursorPos(LPoint.X, LPoint.Y);

  // Restore
  SetRoundMode(LRoundModeOriginal);
end;

{ Others }

procedure noop; inline;
begin
  // Do nothing
end;

class function CoTypeInfo.GetName<T>(const AValue: T): string;
var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := TypeInfo(T);
  Result := string(LTypeInfo.Name);
end;

{ WinApi }

function CoGetWindowText(const AHwnd: HWND): string;
var
  LWindowTextLength: Integer;
  LWindowTextResult: Integer;
begin
  // Prepare the buffer
  LWindowTextLength := GetWindowTextLength(AHwnd);
  SetLength(Result, LWindowTextLength + SizeOf(Char)); // +1 for #0 character
  // Actual title
  LWindowTextResult := GetWindowText(AHwnd, PChar(Result), Length(Result));
  SetLength(Result, LWindowTextResult);
end;

end.
