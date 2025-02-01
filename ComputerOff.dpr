program ComputerOff;

{$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  System.SysUtils,
  UnitConstants     in 'src\UnitConstants.pas',
  UnitCommon        in 'src\UnitCommon.pas',
  UnitAbout         in 'src\UnitAbout.pas',         // FormAbout
  UnitMainForm      in 'src\UnitMainForm.pas',      // FormMainForm
  UnitOptionsDialog in 'src\UnitOptionsDialog.pas'; // FormOptionsDialog

{$R *.res}

const
  ApplicationTitle = 'ComputerOff';
  MxOneInstance    = 'Global\' + ApplicationTitle + 'App';
  MsgShowMainForm  = 'MSG_CoShowMainForm';
  EcAlreadyRunning = 1;

var
  hMxOneInstance: THandle;

{ Parse ComputerOff command-line arguments }
procedure ParseCommandLine;
begin
  if FindCmdLineSwitch('private') then
    HasPrivateCmd := True;
end;

{ Only one instance can be running }
procedure OneInstance;
begin
  if GetLastError <> ERROR_ALREADY_EXISTS then
    Exit;

  { Restore from the tray icon }
  PostMessage(HWND_BROADCAST, RmShowMainForm, MsgId_Show, 0);
  ExitProcess(EcAlreadyRunning);
end;

{ Show minimized to tray icon (hidden) with the -private argument }
procedure ShowMinized;
begin
  if not HasPrivateCmd then
    Exit;

  Application.ShowMainForm := False;
  Winapi.Windows.Beep(3300, 120);
end;

begin
{$IFDEF DEBUG}
{$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

  ParseCommandLine;
  RmShowMainForm := RegisterWindowMessage(MsgShowMainForm);

  hMxOneInstance := CreateMutex(nil, False, PChar(MxOneInstance));
  try
    { Only one instance can be running }
    OneInstance;

    TStyleManager.TrySetStyle('Windows10 SlateGray');

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'ComputerOff';
    Application.CreateForm(TFormMainForm, FormMainForm);
    Application.CreateForm(TFormOptionsDialog, FormOptionsDialog);
    Application.CreateForm(TFormAbout, FormAbout);

    { Show minimized to tray icon (hidden) with the -private argument }
    ShowMinized;
    Application.Run;
  finally
    CloseHandle(hMxOneInstance);
  end;
end.
