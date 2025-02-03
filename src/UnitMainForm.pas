unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils, System.IniFiles, System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.AppEvnts;

type
  TFormMainForm = class(TForm)
    BevelCountDown: TBevel;
    About: TButton;
    ApplicationEventsMain: TApplicationEvents;
    ButtonComputerOff: TButton;
    LabelComputerOff: TLabel;
    LabelCountDown: TLabel;
    Options: TButton;
    CountDownBar: TProgressBar;
    Quit: TButton;
    Stop: TButton;
    TimerCommon: TTimer;
    TimerCountDown: TTimer;
    TrayIconMain: TTrayIcon;
    TimerThrottleActivate: TTimer;

    { Constructors }
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    { Windows Messages }
    procedure WndProc(var Message: TMessage); override;
    procedure ApplicationEventsMainMinimize(Sender: TObject);

    { Timers }
    procedure TimerCommonTimer(Sender: TObject);
    procedure TimerCountDownTimer(Sender: TObject);

    { TApplication Events }
    procedure ApplicationActivate(Sender: TObject);
    procedure ApplicationMinimize(Sender: TObject);

    { TForm Events }
    procedure FormKeyPress(Sender: TObject; var Key: Char);

    { TControl Events }
    procedure AboutClick(Sender: TObject);
    procedure ButtonComputerOffClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure StopMouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    { Tray Icon Events }
    procedure TrayIconMainMouseDown(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerThrottleActivateTimer(Sender: TObject);

  public
    { UI CountDown related }
    procedure PrepareComputerOffType;

  strict private type
    { Types }
    TComputerOffTypeString = record
      Name: string;
      NameWithAccelerator: string;
      constructor Create(const AName: string; const ANameWithAccelerator: string);
    end;

  strict private
    FCountDownTime: TDateTime;
    FShuttingDown: Boolean;

    FComputerOffType: Integer;
    FComputerOffTypeString: TComputerOffTypeString;
    FComputerOffTypesHash: TDictionary<Integer, TComputerOffTypeString>;

    FComputerOffTimeout: TDateTime;
    { Secured quit }
    FComputerOffTimeout4h: TDateTime;
    { Secured ComputerOff }
    FComputerOffTimeout1s: TDateTime;
    { Secured Abort modal }
    FComputerOffTimeout_SecuredAbortModal: TDateTime;

    FAbortModal: TForm;
    FAbortModalShown: Boolean;

    FhForegroundWindow: HWND;
    FCursorPosition: TPoint;
    FhPowerNotify: HDEVNOTIFY;

    { Initialization }
    procedure InitComputerOffTypesHash;

    { Persitent Storage }
    procedure LoadIniFile;
    procedure SaveIniFile;

    { Startup }
    procedure Startup;
    procedure StartupNormal;
    procedure StartupPrivate;

    { CountDown }
    procedure HandleMissedTimeouts;

    { OptionsClick }
    function IsZeroCountDownTime: Boolean;

    { UI CountDown related }
    procedure RestartCountDown;
    procedure ResetCountDown;
    procedure StartCountDown;

    procedure UpdateLabelComputerOff;
    procedure UpdateButtonComputerOff;

    procedure PepareAllCountDownControls;
    procedure PepareCountDown;
    procedure PrepareCountDownBar;
    procedure PepareComputerOffTimeouts;
    procedure UpdateLabelCountDown;

    function ComputeCountDownBarMax: Integer; inline;
    function ComputeCountDownBarPosition: Integer;
    function UpdateCountDownBarPosition: Integer; inline;

    { ComputerOff }
    procedure PreComputerOff;
    procedure PauseVideo; inline;

    procedure ComputerOff;
    function EnableSeShutdownPrivilege: Boolean;
    procedure InvokeComputerOff;

    { Abort Modal }
    procedure ShowAbortComputerOffModal;
    function GetAbortModalMessage: string;
    procedure FreeAbortModal;

    { Show/Hide/Quit }
    procedure ShowComputerOff(ARememberForegroundWindow: Boolean = True);
    procedure HideComputerOff(ARestorePreviousWindow: Boolean = True);
    procedure FocusAndCenterMouseOnActivate;
    procedure RestorePreviousWindow;

    procedure QuitApplication;

    { Windows Messages actions }
    procedure ShowComputerOffFor2s;
    procedure HideComputerOffAfter2s;

    procedure HandleApmResume; inline;
    procedure SynchronizeCountDownTimer;
    procedure QuitOrShowAbortModal;

    { TForm Events }
    procedure FormShowCenterMouse(Sender: TObject);
  end;

const
  MsgId_Show = WPARAM(2);

var
  FormMainForm: TFormMainForm;
  HasPrivateCmd: Boolean = False;
  RmShowMainForm: Cardinal;

implementation

{$R *.dfm}

uses
  Winapi.ShellAPI, System.DateUtils,
  UnitConstants, UnitCommon, UnitOptionsDialog, UnitAbout;

{ External }

const
  powrprof = 'powrprof.dll';

function RegisterSuspendResumeNotification(hRecipient: THandle; Flags: DWORD): HDEVNOTIFY;
  external user32;
function AdjustTokenPrivileges(TokenHandle: THandle; DisableAllPrivileges: BOOL;
  const NewState: TTokenPrivileges; BufferLength: DWORD; PreviousState: PTokenPrivileges;
  ReturnLength: PDWORD): BOOL; external advapi32;
function SetSuspendState(Hibernate, Force, WakeupEventsDisabled: ByteBool): ByteBool;
  external powrprof;

{ published }

{ Constructors }

procedure TFormMainForm.FormCreate(Sender: TObject);
begin
  { Remember the current foreground window }
  FhForegroundWindow := GetForegroundWindow;

  InitComputerOffTypesHash;

  if HasPrivateCmd then
    TrayIconMain.Visible := True;

  RegisterSuspendResumeNotification(Handle, DEVICE_NOTIFY_WINDOW_HANDLE);
end;

procedure TFormMainForm.FormDestroy(Sender: TObject);
begin
  if FhPowerNotify <> nil then
    UnregisterDeviceNotification(FhPowerNotify);

  FreeAndNil(FComputerOffTypesHash);

  { ChangeFileExt can throw EOutOfMemory, also, the exception handler is necessary
    because it's called from the destructor). }
  try
    SaveIniFile;
  finally
  end;
end;

{ Windows Messages }

procedure TFormMainForm.WndProc(var Message: TMessage);
begin
  with Message do
  begin
    case Msg of
      { Don't accidentally call ComputerOff the next day if PC was suspended prematurely
        the previous night. }
      WM_POWERBROADCAST: // See https://learn.microsoft.com/en-us/windows/win32/power/wm-powerbroadcast
        if WParam = PBT_APMRESUMEAUTOMATIC then
          HandleApmResume;
    end; { case Msg of }

    { Our User registered messages }
    { Cannot be in the case statement because RM_CoMain is not a constant. }
    if Msg = RmShowMainForm then
      case WParam of
        MsgId_Show: ShowComputerOffFor2s;
      end;
  end; { with Message do }

  inherited;
end;

procedure TFormMainForm.ApplicationEventsMainMinimize(Sender: TObject);
begin
  HideComputerOff;
end;

procedure TFormMainForm.ApplicationMinimize(Sender: TObject);
begin
  { Restore the previously recorded foreground window }
  RestorePreviousWindow;
end;

{ Timers }

procedure TFormMainForm.TimerCommonTimer(Sender: TObject);
begin
  case TimerCommon.Interval of
    cStartupOneShot: Startup;
    cHideComputerOffAfter2s: HideComputerOffAfter2s;
  end;
end;

// Never call SynchronizeCountDownTimer in this timer directly
procedure TFormMainForm.TimerCountDownTimer(Sender: TObject);
begin
  HandleMissedTimeouts;

  FCountDownTime := FCountDownTime.IncSecond(-1);
  UpdateLabelCountDown;
  CountDownBar.StepIt;

  if SameTime(FCountDownTime.GetTime, ZeroDateTime.GetTime) then
  begin
    { Without this the countdown doesn't show 00:00, it quits at 00:01 }
    Application.ProcessMessages;
    ComputerOff
  end
  else
    PreComputerOff;
end;

{ TApplication Events }

procedure TFormMainForm.ApplicationActivate(Sender: TObject);
begin
  with TimerThrottleActivate do
  begin
    Enabled := False;
    Enabled := True;
  end;
end;

procedure TFormMainForm.TimerThrottleActivateTimer(Sender: TObject);
begin
  if (Application.ModalLevel > 0) or not FormMainForm.Visible or
     (WindowState = wsMinimized)  or TrayIconMain.Visible
  then
    Exit;

  UpdateLabelCountDown;
  FocusAndCenterMouseOnActivate;

  TimerThrottleActivate.Enabled := False;
end;

{ TForm Events }

procedure TFormMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  { Nothing to do }
  if Key <> Char(VK_ESCAPE) then
    Exit;

  { Quit if the ComputerOff timer is not running }
  if TimerCountDown.Enabled then
    SendToBack
  else
    QuitApplication;
end;

{ TControl Events }

procedure TFormMainForm.AboutClick(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TFormMainForm.ButtonComputerOffClick(Sender: TObject);
begin
  ComputerOff;
end;

procedure TFormMainForm.OptionsClick(Sender: TObject);
begin
  { Nothing to do, Options modal has been canceled }
  if FormOptionsDialog.ShowModal <> mrOk then
    Exit;

  { Restore the previously recorded foreground window }
  RestorePreviousWindow;

  { Nothing to do, Options modal was submitted with zero countdown time (00:00) }
  if IsZeroCountDownTime then
    Exit;

  StartCountDown;

  { MainWindow may disappear after clicking OK }
  HideComputerOff(False);
end;

procedure TFormMainForm.QuitClick(Sender: TObject);
begin
  QuitApplication;
end;

procedure TFormMainForm.StopClick(Sender: TObject);
begin
  { Stop the timer to hide the ComputerOff application }
  if TimerCommon.Interval = cHideComputerOffAfter2s then
    TimerCommon.Enabled := False;

  ResetCountDown;
  CenterMouse(Options);
end;

procedure TFormMainForm.StopMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    OptionsClick(Sender);
end;

{ Tray Icon Events }

procedure TFormMainForm.TrayIconMainMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowComputerOff(False);
end;

{ public }

{ UI CountDown related }

procedure TFormMainForm.PrepareComputerOffType;
begin
  FComputerOffType := FormOptionsDialog.ComputerOffType.ItemIndex;
  FComputerOffTypeString := FComputerOffTypesHash.Items[FComputerOffType];

  UpdateLabelComputerOff;
  UpdateButtonComputerOff;
end;

{ private }

{ Types }

constructor TFormMainForm.TComputerOffTypeString.Create(
  const AName: string; const ANameWithAccelerator: string);
begin
  Name := AName;
  NameWithAccelerator := ANameWithAccelerator;
end;

{ Initialization }

procedure TFormMainForm.InitComputerOffTypesHash;
begin
  FComputerOffTypesHash := TDictionary<Integer, TComputerOffTypeString>.Create;

  with FComputerOffTypesHash do
  begin
    TryAdd(cSleep,     TComputerOffTypeString.Create('Sleep',     'Sl&eep'));
    TryAdd(cHibernate, TComputerOffTypeString.Create('Hibernate', '&Hibernate'));
    TryAdd(cShutdown,  TComputerOffTypeString.Create('Shutdown',  'Shu&tdown'));
  end;
end;

{ Persitent Storage }

procedure TFormMainForm.LoadIniFile;
var
  LIniFile: TIniFile;
begin
  { The ItemIndex default value is 0 (from designer) if this fails }
  LIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    FormOptionsDialog.ComputerOffType.ItemIndex :=
      LIniFile.ReadInteger('Main', 'Type', cSleep);
  finally
    LIniFile.Free;
  end;

  PrepareComputerOffType;
  UpdateLabelComputerOff;
  UpdateButtonComputerOff;
end;

procedure TFormMainForm.SaveIniFile;
var
  LIniFile: TIniFile;
begin
  { Don't update (save) ini file if invoked with -private argument }
  if HasPrivateCmd then
    Exit;

  LIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    LIniFile.WriteInteger('Main', 'Type', FComputerOffType);
  finally
    LIniFile.Free;
  end;
end;

{ Startup }

procedure TFormMainForm.Startup;
begin
  TimerCommon.Interval := 0;

  if HasPrivateCmd then
    StartupPrivate
  else
    StartupNormal;
end;

procedure TFormMainForm.StartupNormal;
begin
  LoadIniFile;
  Options.Click;
end;

procedure TFormMainForm.StartupPrivate;
var
  LTimeoutAtOriginal: TTimeoutAtForPrivateCmd;
begin
  { Always set to Sleep in 2 hours }
  with FormOptionsDialog do
  begin
    ComputerOffType.ItemIndex := cSleep;
    LTimeoutAtOriginal.Hour := Hour.Value;
    LTimeoutAtOriginal.Minute := Minute.Value;
    Hour.Value := cTimeoutAtForPrivateCmd.Hour;
    Minute.Value := cTimeoutAtForPrivateCmd.Minute;
  end;

  StartCountDown;

  { Restore, so the value will be set only during the startup with -private argument }
  FormOptionsDialog.Hour.Value := LTimeoutAtOriginal.Hour;
  FormOptionsDialog.Minute.Value := LTimeoutAtOriginal.Minute;
end;

{ CountDown }

procedure TFormMainForm.HandleMissedTimeouts;
var
  LNow: TDateTime;
begin
  LNow := Now;

  { Secured quit, eg. when PC was suspended at night and resumed the next day to avoid
    accidentally calling ComputerOff function. }
  if LNow > FComputerOffTimeout4h then
    QuitApplication

  { Secured ComputerOff, eg. if the process was paused or suspended (edge case) }
  else if LNow > FComputerOffTimeout1s then
    ComputerOff;
end;

{ OptionsClick }

function TFormMainForm.IsZeroCountDownTime: Boolean;
begin
  { Nothing to do, no zero time }
  with FormOptionsDialog do
    if (Hour.Value <> 0) or (Minute.Value <> 0) or (Second.Value <> 0) then
      Exit(False);

  PrepareComputerOffType;
  Result := True;
end;

{ UI CountDown related }

procedure TFormMainForm.RestartCountDown;
begin
  { Putting this here to avoid a weird logic in the ShowAbortComputerOffModal }
  HideComputerOff(False);

  ResetCountDown;
  StartCountDown;
end;

procedure TFormMainForm.ResetCountDown;
begin
  Stop.Enabled := False;

  CountDownBar.Enabled := False;
  CountDownBar.Position := 0;
  TimerCountDown.Enabled := False;
  FCountDownTime.SetTime(0, 0, 0, 0);

  UpdateLabelCountDown;
  PrepareComputerOffType;

  Options.Enabled := True;
end;

procedure TFormMainForm.StartCountDown;
begin
  PepareAllCountDownControls;

  Stop.Enabled := True;
  Options.Enabled := False;
  CountDownBar.Enabled := True;
  TimerCountDown.Enabled := True;
end;

procedure TFormMainForm.UpdateLabelComputerOff;
begin
  LabelComputerOff.Caption := 'Time to ' + FComputerOffTypeString.Name;
end;

procedure TFormMainForm.UpdateButtonComputerOff;
begin
  ButtonComputerOff.Caption := FComputerOffTypeString.NameWithAccelerator;
end;

procedure TFormMainForm.PepareAllCountDownControls;
begin
  PepareCountDown;
  PrepareCountDownBar;
  PepareComputerOffTimeouts;

  UpdateLabelCountDown;
end;

procedure TFormMainForm.PepareCountDown;
begin
  with FormOptionsDialog do
    FCountDownTime.SetTime(Hour.Value, Minute.Value, Second.Value, 0);
end;

procedure TFormMainForm.PrepareCountDownBar;
begin
  CountDownBar.Max := ComputeCountDownBarMax;
  CountDownBar.Position := ComputeCountDownBarPosition;
end;

procedure TFormMainForm.PepareComputerOffTimeouts;
begin
  { Store when to call the ComputerOff (used for edge cases and corrections}
  FComputerOffTimeout := Now + FCountDownTime;

  { Secured quit }
  FComputerOffTimeout4h := FComputerOffTimeout.IncHour(cComputerOffTimeout4h);
  { Secured ComputerOff }
  FComputerOffTimeout1s := FComputerOffTimeout.IncSecond(cComputerOffTimeout1s);
  { Secured Abort modal }
  with cShowAbortModalTreshold do
    FComputerOffTimeout_SecuredAbortModal :=
      FComputerOffTimeout.IncSecond(((Minute * 60) + Second) * -1); // Negative to decrease
end;

procedure TFormMainForm.UpdateLabelCountDown;
begin
  LabelCountDown.Caption := TimeToStr(FCountDownTime.GetTime);
end;

function TFormMainForm.ComputeCountDownBarMax;
begin
  Result := ComputeCountDownBarPosition;
end;

function TFormMainForm.ComputeCountDownBarPosition;
begin
  Result := (FCountDownTime.Hour * 60 * 60) + (FCountDownTime.Minute * 60) +
    (FCountDownTime.Second);
end;

function TFormMainForm.UpdateCountDownBarPosition;
begin
  Result := ComputeCountDownBarPosition;
end;

{ ComputerOff }

procedure TFormMainForm.PreComputerOff;
var
  LIsMin0: Boolean;
begin
  { Nothing to do }
  if FCountDownTime.Hour <> 0 then
    Exit;

  LIsMin0 := FCountDownTime.Minute = 0;

  { Pause the video 8 seconds before changing the power state }
  if LIsMin0 and (FCountDownTime.Second = cPauseVideoThreshold) then
    PauseVideo

  { Close the Abort ComputerOff modal dialog }
  else if (FAbortModal <> nil) and FAbortModalShown and LIsMin0 and
          (FCountDownTime.Second = cCloseAbortModalTreshold)
  then
    FAbortModal.Close // Sets mrCancel result (don't call FreeAndNil here)

  { Show model dialog that allows to abort off action 2m55s before (5s after LG TV) }
  else if (FCountDownTime.Minute = cShowAbortModalTreshold.Minute) and
          (FCountDownTime.Second = cShowAbortModalTreshold.Second)
  then
    ShowAbortComputerOffModal;
end;

procedure TFormMainForm.PauseVideo;
begin
  { Pause the Skylink/YouTube video if it's in the foreground, it sends
    the ctrl+alt+shift+p keyboard shortcut that is handled by
    the Tampermonkey. }
  ShellExecute(0, nil, PChar(cPauseVideoAhkFilepath), nil, nil, SW_HIDE);
end;

procedure TFormMainForm.ComputerOff;
begin
  { Nothing to do, already in the shutdown process invoked by another condition }
  if FShuttingDown then
    Exit;

  FShuttingDown := True;

  { Nothing to do, SeShutdownPrivilege was not enabled }
  if not EnableSeShutdownPrivilege then
    Exit;

  InvokeComputerOff;
  QuitApplication;
end;

function TFormMainForm.EnableSeShutdownPrivilege: Boolean;
var
  LNewState: TOKEN_PRIVILEGES;
  LTokenHandle: THandle;
  LResult: BOOL;
begin
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, LTokenHandle) then
    Exit(False);

  if not LookupPrivilegeValue(nil, SE_SHUTDOWN_NAME, LNewState.Privileges[0].Luid) then
    Exit(False);

  LNewState.PrivilegeCount := 1;
  LNewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

  LResult := AdjustTokenPrivileges(
    LTokenHandle, False, LNewState, SizeOf(LNewState), nil, nil);

  Result := LResult and (GetLastError = ERROR_SUCCESS);
  CloseHandle(LTokenHandle);
end;

procedure TFormMainForm.InvokeComputerOff;
const
  LSHTDN_REASON_MAJOR_OTHER  = DWORD($00000000);
  LSHTDN_REASON_MINOR_OTHER  = DWORD($00000000);
  LSHTDN_REASON_FLAG_PLANNED = DWORD($80000000);
  LcSHTDNPlanned =
    LSHTDN_REASON_MAJOR_OTHER or LSHTDN_REASON_MINOR_OTHER or LSHTDN_REASON_FLAG_PLANNED;
begin
  case FComputerOffType of
{$ifdef DEBUG}
    cSleep:     Winapi.Windows.Beep(2300, 120);
    cHibernate: Winapi.Windows.Beep(5300, 120);
    cShutdown:  Winapi.Windows.Beep(8300, 120);
{$ELSE}
    cSleep:     SetSuspendState(False, False, True); // Suspend2Ram
    cHibernate: SetSuspendState(True,  False, True); // Hibernate
    cShutdown:  ExitWindowsEx(EWX_POWEROFF or EWX_FORCE, LcSHTDNPlanned); // Don't use EWX_FORCEIFHUNG
{$ENDIF}
  end;
end;

{ Abort Modal }

procedure TFormMainForm.ShowAbortComputerOffModal;
const
  mbRetryAbortCancel = [mbRetry, mbAbort, mbCancel];
var
  LModalResult: Integer;
begin
  { Remember the current foreground window }
  FhForegroundWindow := GetForegroundWindow;
  ShowComputerOff;

  FAbortModal :=
    CreateMessageDialog(GetAbortModalMessage, mtConfirmation, mbRetryAbortCancel,
      mbRetry, ['&Cancel', '&Quit', '&Restart']);

  with FAbortModal do
  begin
    Caption := 'Choice';
    Position := poScreenCenter;
    OnShow := FormShowCenterMouse;
    LModalResult := ShowModal;
  end;

  FreeAbortModal;
  { Restore the previously recorded foreground window }
  RestorePreviousWindow;

  case LModalResult of
    mrRetry: RestartCountDown;
    mrAbort: QuitApplication;
  else
    HideComputerOff(False);
  end;
end;

function TFormMainForm.GetAbortModalMessage: string;
var
  LAbortTime: string;
begin
  with cShowAbortModalTreshold do
    LAbortTime := EncodeTime(0, Minute, Second, 0).Format('n"m"s"s"'); // Time format eg.: 2m55s

  Result := Format(cAbortModalMessage, [FComputerOffTypeString.Name, LAbortTime]);
end;

procedure TFormMainForm.FreeAbortModal;
begin
  FAbortModalShown := False;
  FreeAndNil(FAbortModal);
end;

{ Show/Hide/Quit }

procedure TFormMainForm.ShowComputerOff(ARememberForegroundWindow: Boolean);
begin
  { Remember the current foreground window }
  if ARememberForegroundWindow then
    FhForegroundWindow := GetForegroundWindow
  else
    FhForegroundWindow := 0;

  TrayIconMain.Visible := False;
  Show;
  WindowState := wsNormal;
  Application.ProcessMessages;
  Application.BringToFront;
end;

procedure TFormMainForm.HideComputerOff(ARestorePreviousWindow: Boolean);
begin
  Hide;
  WindowState := wsMinimized;
  TrayIconMain.Visible := True;

  { Restore the previously recorded foreground window }
  if ARestorePreviousWindow then
    RestorePreviousWindow;
end;

procedure TFormMainForm.FocusAndCenterMouseOnActivate;
begin
  { CanFocus checks are inside the CenterMouse }
  if TimerCountDown.Enabled then
    CenterMouse(Stop)
  else
    CenterMouse(Options);
end;

procedure TFormMainForm.RestorePreviousWindow;
begin
  { Restore the previously recorded foreground window }
  if FhForegroundWindow <> 0 then
    SetForegroundWindow(FhForegroundWindow);
end;

procedure TFormMainForm.QuitApplication;
begin
  Close;

  TimerCountDown.Enabled := False;
  TimerCommon.Enabled := False;
end;

{ Windows Messages actions }

procedure TFormMainForm.ShowComputerOffFor2s;
begin
  { Remember the current foreground window and cursor position }
  FhForegroundWindow := GetForegroundWindow;
  GetCursorPos(FCursorPosition);

  ShowComputerOff;

  { Hide after 2s }
  TimerCommon.Interval := cHideComputerOffAfter2s;
  TimerCommon.Enabled := True;
end;

procedure TFormMainForm.HideComputerOffAfter2s;
begin
  TimerCommon.Enabled := False;

  { Don't hide if the countdown is not running or any modal is open }
  if not TimerCountDown.Enabled or (Application.ModalLevel > 0) then
    Exit;

  HideComputerOff;

  { Restore cursor position }
  with FCursorPosition do
    SetCursorPos(X, Y);
end;

procedure TFormMainForm.HandleApmResume;
begin
  SynchronizeCountDownTimer;
  QuitOrShowAbortModal;
end;

procedure TFormMainForm.SynchronizeCountDownTimer;
begin
  { Nothing to do, the countdown counter was stopped before suspending }
  if not TimerCountDown.Enabled then
    Exit;

  { I don't need to save the suspend time, subtracting the actual countdown value
    from Now will always be correct. }
  FCountDownTime := Now - FComputerOffTimeout;
  UpdateCountDownBarPosition;
end;

procedure TFormMainForm.QuitOrShowAbortModal;
var
  LNow: TDateTime;
begin
  LNow := Now;

  { Exact time when the ComputerOff function should be invoked has been missed/passed }
  if LNow >= FComputerOffTimeout then
    QuitApplication

  { Exact time was not missed, there is still a time to show the Abort modal }
  else if LNow > FComputerOffTimeout_SecuredAbortModal then
    ShowAbortComputerOffModal;
end;

{ TForm Events }

procedure TFormMainForm.FormShowCenterMouse(Sender: TObject);
begin
  FAbortModalShown := True;
  CenterMouse((Sender as TForm).ActiveControl, False);
end;

end.

