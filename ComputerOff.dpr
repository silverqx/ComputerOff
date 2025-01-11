program ComputerOff;

{$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

{$SetPEFlags 1}

uses
  Vcl.Forms,
  Winapi.Windows,
  UnitAbout in 'UnitAbout.pas' {FormAbout},
  UnitMainForm in 'UnitMainForm.pas' {FormMainForm},
  UnitOptionsDialog in 'UnitOptionsDialog.pas' {FormOptionsDialog},
  Vcl.Themes,
  Vcl.Styles,
  System.SysUtils;

{$R *.res}

const
  Mg_App_Title = 'ComputerOff';
  Mg_Mx_One_Instance = 'Global\' + Mg_App_Title + 'App';
  MSG_CoShow = 'MSG_CoShow';
  Ec_Already_Running = 1;

var
  hMutex: THandle;

begin
{$IFDEF DEBUG}
{$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}

  if FindCmdLineSwitch('private') then
    HasPrivateCmd := True;

  RM_CoMain := RegisterWindowMessage(MSG_CoShow);

  hMutex := CreateMutex(nil, False, PChar(Mg_Mx_One_Instance));
  try
    { Only one instance can be running. }
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      { Restore from the tray icon }
      PostMessage(HWND_BROADCAST, RM_CoMain, IdM_Show, 0);
      ExitProcess(Ec_Already_Running);
    end;

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
//    TStyleManager.TrySetStyle('Windows10 Dark');
    TStyleManager.TrySetStyle('Windows10 SlateGray');
    Application.Title := Mg_App_Title;
    Application.CreateForm(TFormMainForm, FormMainForm);
    { Show Minimized }
    if HasPrivateCmd then
    begin
      FormMainForm.WindowState := wsMinimized;
      FormMainForm.Show;
      Winapi.Windows.Beep(3300, 120);
    end;
    Application.CreateForm(TFormOptionsDialog, FormOptionsDialog);
    Application.CreateForm(TFormAbout, FormAbout);
    Application.Run;
  finally
    CloseHandle(hMutex);
  end;
end.
