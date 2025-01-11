unit UnitMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, IniFiles, Vcl.XPMan, Vcl.AppEvnts;

type
  TFormMainForm = class(TForm)
    Start: TButton;
    Stop: TButton;
    Options: TButton;
    About: TButton;
    Quit: TButton;
    p: TBevel;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    XPManifest1: TXPManifest;
    Timer2: TTimer;
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    procedure WndProc(var Message:TMessage); override;
    procedure FormDestroy(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure StopMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrayIcon1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ApplicationEvents1Minimize(Sender: TObject);
  private
    { Private declarations }
    hour, min, sec : integer;
    FWindowsTerminateType: integer;
    FEndDateTime: TDateTime;
//    FCursorPosition: TPoint;
    procedure SetWindowsTerminateType(const Value: integer);
    procedure ComputerOff;
    procedure CloseAbortComputerOffModal;
    procedure AbortComputerOff;
    procedure PauseVideoBeforeOff;
    { Get window title }
    function MUGetWindowText(const AHwnd: HWND): string;
    property WindowsTerminateType: integer read FWindowsTerminateType write SetWindowsTerminateType;
    procedure ShowComputerOff;
    procedure HideComputerOff;
  public
    { Public declarations }
    procedure SetTime;
    procedure ShowTime;
  end;

const
  IdM_Show = 2;

var
  FormMainForm: TFormMainForm;
  HasPrivateCmd: Boolean = False;
  gForegroundWindow: HWND;
  RM_CoMain: Cardinal;

implementation

{$R *.dfm}

uses
  Winapi.ShellAPI, System.DateUtils, System.Types, System.UITypes,
  UnitOptionsDialog, UnitAbout;

procedure TFormMainForm.AboutClick(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TFormMainForm.ApplicationEvents1Minimize(Sender: TObject);
begin
  HideComputerOff;
end;

procedure TFormMainForm.FormActivate(Sender: TObject);
begin
  ShowTime;
end;

procedure TFormMainForm.FormCreate(Sender: TObject);
begin
  { Remeber the current foreground window }
  gForegroundWindow := GetForegroundWindow;

  ScaleBy(4, 3);
end;

procedure TFormMainForm.FormDestroy(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  IniFile.WriteInteger('Main', 'Type', WindowsTerminateType);
  IniFile.Free;
end;

procedure TFormMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Sender = FormMainForm) and (Key = #27) then
  begin
    if not Timer1.Enabled then
      Application.Terminate
    else
      SendToBack;
  end;
end;

procedure TFormMainForm.FormShow(Sender: TObject);
begin
  if Options.Enabled then
    FocusControl(Options);
end;

procedure TFormMainForm.OptionsClick(Sender: TObject);
begin
  { Save current cursor position }
//  GetCursorPos(FCursorPosition);

  if Sender = Options then
    FormOptionsDialog.Position := poScreenCenter;

  if FormOptionsDialog.ShowModal = mrOk then
  begin
    { Restore the previously recorded foreground window }
    SetForegroundWindow(gForegroundWindow);

    { Restore cursor position }
//    SetCursorPos(FCursorPosition.X, FCursorPosition.Y);

    WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
    if ((FormOptionsDialog.SpinEdit1.Value = 0) and
      (FormOptionsDialog.SpinEdit2.Value = 0) and
      (FormOptionsDialog.SpinEdit3.Value = 0))
    then
    begin
      FocusControl(Options);
      case FormOptionsDialog.ComboBox1.ItemIndex of
        0:
          begin
            WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
            Start.Caption := 'S&uspend';
            Label1.Caption := 'Time to Suspend2Ram';
          end;
        1:
          begin
            WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
            Start.Caption := '&Hibernation';
            Label1.Caption := 'Time to Hibernation';
          end;
        2:
          begin
            WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
            Start.Caption := '&Turn Off';
            Label1.Caption := 'Time to Turn Off';
          end;
      end;
    end
    else
    begin
      FocusControl(Start);
      Start.Caption := '&Start';
    end;

    SetTime;
    Start.Click;

    { When I click OK in an Options dialog, then the MainWindow can disappear. }
    HideComputerOff;
  end
  else
  begin
    if Label2.Caption = '00:00:00' then
      FocusControl(Options)
    else
      FocusControl(Start);
  end;

  case FormOptionsDialog.ComboBox1.ItemIndex of
    0: Label1.Caption := 'Time to Suspend2Ram';
    1: Label1.Caption := 'Time to Hibernation';
    2: Label1.Caption := 'Time to Turn Off';
  end;

  ShowTime;
end;

procedure TFormMainForm.QuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMainForm.SetTime;
begin
  hour := FormOptionsDialog.SpinEdit1.Value;
  min := FormOptionsDialog.SpinEdit2.Value;
  sec := FormOptionsDialog.SpinEdit3.Value;
end;

procedure TFormMainForm.SetWindowsTerminateType(const Value: integer);
begin
  FWindowsTerminateType := Value;
end;

procedure TFormMainForm.ShowTime;
begin
  if hour >= 10 then
    Label2.Caption := IntToStr(hour) + ':'
  else
    Label2.Caption := '0' + IntToStr(hour) + ':';

  if min >= 10 then
    Label2.Caption := Label2.Caption + IntToStr(min) + ':'
  else
    Label2.Caption := Label2.Caption + '0' + IntToStr(min) + ':';

  if sec >= 10 then
    Label2.Caption := Label2.Caption + IntToStr(sec)
  else
    Label2.Caption := Label2.Caption + '0' + IntToStr(sec);
end;

procedure TFormMainForm.HideComputerOff;
begin
  Hide;
  WindowState := TWindowState.wsMinimized;
  TrayIcon1.Visible := True;
end;

procedure TFormMainForm.ShowComputerOff;
begin
  TrayIcon1.Visible := False;
  Show;
  WindowState := TWindowState.wsNormal;
  Application.BringToFront;
end;

procedure TFormMainForm.StartClick(Sender: TObject);
begin
  if ((Start.Caption = 'S&uspend') or (Start.Caption = '&Hibernation') or
    (Start.Caption = '&Turn Off'))
  then
  begin
    ComputerOff;
    Application.Terminate;
  end
  else
  begin
    if HasPrivateCmd then
      FEndDateTime := IncHour(SysUtils.Now, hour + 3);

    Stop.Enabled := true;
    Options.Enabled := false;
    case FormOptionsDialog.ComboBox1.ItemIndex of
      0:
        begin
          WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
          Start.Caption := 'S&uspend';
          Label1.Caption := 'Time to Suspend2Ram';
        end;
      1:
        begin
          WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
          Start.Caption := '&Hibernation';
          Label1.Caption := 'Time to Hibernation';
        end;
      2:
        begin
          WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
          Start.Caption := '&Turn Off';
          Label1.Caption := 'Time to Turn Off';
        end;
    end;
    Timer1.Enabled := True;
    ProgressBar1.Enabled := true;
    ProgressBar1.Max := ((hour * 60 * 60) + (min * 60) + (sec));
    ProgressBar1.Position := ((hour * 60 * 60) + (min * 60) + (sec));
    FocusControl(Stop);
  end;
end;

procedure TFormMainForm.StopClick(Sender: TObject);
begin
  { Stop hide ComputerOff timer }
  if Timer2.Interval = 2000 then
    Timer2.Enabled := False;

  ProgressBar1.Enabled := false;
  ProgressBar1.Position := 0;
  Stop.Enabled := false;
  Options.Enabled := true;
  FocusControl(Options);
  Timer1.Enabled := False;
  hour := 0; min := 0; sec := 0;
  ShowTime;
  case FormOptionsDialog.ComboBox1.ItemIndex of
    0:
      begin
        WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
        Start.Caption := 'S&uspend';
        Label1.Caption := 'Time to Suspend2Ram';
      end;
    1:
      begin
        WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
        Start.Caption := '&Hibernation';
        Label1.Caption := 'Time to Hibernation';
      end;
    2:
      begin
        WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
        Start.Caption := '&Turn Off';
        Label1.Caption := 'Time to Turn Off';
      end;
  end;
end;

procedure TFormMainForm.StopMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    OptionsClick(Sender);
end;

procedure TFormMainForm.ComputerOff;
var
  htoken: THandle;
  ret, dw : Cardinal;
  tp,tps : TOKEN_PRIVILEGES;

begin
  if OpenProcessToken(GetCurrentProcess(),
    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, htoken)
  then
  begin
    if LookupPrivilegeValue(nil, 'SeShutdownPrivilege',tp.Privileges[0].Luid) then
    begin
      tp.PrivilegeCount := 1;
      tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      dw := SizeOf(tp) ;
      Windows.AdjustTokenPrivileges(htoken,false,tp,dw,tps,ret);
      closehandle(htoken);
    end;
  end;

  case WindowsTerminateType of
    { Suspend }
    0: SetSystemPowerState(true, false);
    { Hibernate }
    1: SetSystemPowerState(false, false);
    2: ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, 0);
  end;
end;

procedure TFormMainForm.AbortComputerOff;
var
  ConfirmAction: string;
  DialogResult: Integer;
begin
  { Remeber the current foreground window }
  gForegroundWindow := GetForegroundWindow;
  Application.BringToFront;

  case FormOptionsDialog.ComboBox1.ItemIndex of
    0: ConfirmAction := 'Suspend';
    1: ConfirmAction := 'Hibernation';
    2: ConfirmAction := 'Turn Off';
  end;

  ConfirmAction := Format('Abort %s that will occur in 2 minutes?', [ConfirmAction]);
  DialogResult := MessageDlg(ConfirmAction, mtConfirmation, mbYesNo, 0, mbYes);

  { Restore the previously recorded foreground window }
  SetForegroundWindow(gForegroundWindow);

  if DialogResult = mrYes then
    Close;
end;

procedure TFormMainForm.CloseAbortComputerOffModal;
var
  ActiveWindow: HWND;
  LWindowText: string;
begin
  ActiveWindow := GetActiveWindow;
  { Získať názov okna }
  LWindowText := MUGetWindowText(ActiveWindow);

  { Send WM_CLOSE only if the Confirm modal is in the foreground }
  if IsWindow(ActiveWindow) and (LWindowText = 'Confirm') then
    SendMessage(ActiveWindow, WM_CLOSE, 0, 0);

  FormMainForm.SendToBack;
end;

procedure TFormMainForm.PauseVideoBeforeOff;
begin
    { Pause the Skylink/YouTube video if it's in the foreground, it sends
      the ctrl+alt+shift+p keyboard shortcut that is handled by
      the Tampermonkey. }
    ShellExecute(0, nil,
      PChar('E:\autohotkey\os-global\Src\ComputerOff\PauseVideoAtSuspend.ahk'),
      nil, nil, SW_HIDE);
end;

procedure TFormMainForm.Timer1Timer(Sender: TObject);
begin
  if (HasPrivateCmd
    and (CompareDateTime(Now, FEndDateTime) = GreaterThanValue)) then
  begin
    Application.Terminate;
    Timer1.Enabled := False;
  end;

  dec(sec);

  if sec = -1 then
  begin
    sec := 59;
    dec(min);
  end;

  if min = -1 then
  begin
    min := 59;
    dec(hour);
  end;

  if hour = -1 then
  begin
    Timer1.Enabled := False;
    ComputerOff;
    Application.Terminate;
  end;

  ShowTime;
  ProgressBar1.Position := ProgressBar1.Position - 1;

  { Prepare for PauseVideoBeforeOff }
  if (hour = 0) and (min = 0) and (sec = 10) then
    CloseAbortComputerOffModal;

  { Pause video 8 seconds before a suspend }
  if (hour = 0) and (min = 0) and (sec = 8) then
    PauseVideoBeforeOff;

  { Show dialog that allows to abort off action 2m55s before, 5s after LG TV }
  if (hour = 0) and (min = 2) and (sec = 55) then
    AbortComputerOff;
end;

procedure TFormMainForm.Timer2Timer(Sender: TObject);
var
  IniFile: TIniFile;
begin
  { Application started }
  if Timer2.Interval = 1 then
  begin
    IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
    FormOptionsDialog.ComboBox1.ItemIndex := IniFile.ReadInteger('Main', 'Type', 0);
    IniFile.Free;

    case FormOptionsDialog.ComboBox1.ItemIndex of
      0:
        begin
          WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
          Start.Caption := 'S&uspend';
          Label1.Caption := 'Time to Suspend2Ram';
        end;
      1:
        begin
          WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
          Start.Caption := '&Hibernation';
          Label1.Caption := 'Time to Hibernation';
        end;
      2:
        begin
          WindowsTerminateType := FormOptionsDialog.ComboBox1.ItemIndex;
          Start.Caption := '&Turn Off';
          Label1.Caption := 'Time to Turn Off';
        end;
    end;

    if HasPrivateCmd then
    begin
      Timer2.Interval := 0;

      hour := 2;
      min := 0;
      sec := 0;

      Start.Caption := '&Start';
      ShowTime;
      Start.Click;
    end
    { To show Options dialog, I'm trying to avoid Options dialog shows below
      the MainForm once out of ten. }
    else
      Timer2.Interval := 90;

  end
  else if Timer2.Interval = 90 then
  begin
    Timer2.Interval := 0;

    { Show an Options dialog on the first run }
    Options.Click;
  end

  { Lookup to ComputerOff application for 2s }
  else if Timer2.Interval = 2000 then
  begin
    Timer2.Enabled := False;
    { Don't hide if countdown isn't running }
    if Timer1.Enabled then
      HideComputerOff;
  end;
end;

procedure TFormMainForm.TrayIcon1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowComputerOff;
end;

function TFormMainForm.MUGetWindowText(const AHwnd: HWND): string;
var
  LWindowTextLength: Integer;
  LWindowTextResult: Integer;
begin
  LWindowTextLength := GetWindowTextLength(AHwnd);
  Inc(LWindowTextLength);
  SetLength(Result, LWindowTextLength);
  LWindowTextResult := GetWindowText(AHwnd, PChar(Result), Length(Result));
  SetLength(Result, LWindowTextResult);
end;

procedure TFormMainForm.WndProc(var Message: TMessage);
begin
  with Message do
  begin
    if Msg = RM_CoMain then
      case WParam of
        IdM_Show:
        begin
          ShowComputerOff;
          { Hide after 2s }
          Timer2.Interval := 2000;
          Timer2.Enabled := True;
        end;
      end;
  end; { with Message do }

  inherited;
end;

end.
