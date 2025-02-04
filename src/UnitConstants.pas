﻿unit UnitConstants;

interface

uses System.SysUtils;

type
  { PreComputerOff }
  TShowRestartAbortModalThreshold = record
    Minute: Word;
    Second: Word;
  end;

  { StartupPrivate }
  TTimeoutAtForPrivateCmd = record
    Hour: Word;
    Minute: Word;
  end;

const
  { StartupPrivate }
  cTimeoutAtForPrivateCmd: TTimeoutAtForPrivateCmd = (Hour: 2; Minute: 0);

  { TimerCommonTimer }
  cStartupOneShot         = Cardinal(1);
  cHideComputerOffAfter2s = Cardinal(2000);

  { GetRestartAbortModalMessage }
  cRestartAbortModalMessage = 'Restart or Abort ''%s'' that will occur in %s?';

  { ComputerOffType.ItemIndex -> FComputerOffType -> FComputerOffTypeString }
  cSleep     = Integer(0);
  cHibernate = Integer(1);
  cShutdown  = Integer(2);

  { PrepareComputerOffTimeouts }
  { Secured quit }
  cComputerOffTimeout4h = Int64(4);
  { Secured ComputerOff }
  cComputerOffTimeout1s = Int64(1);

  { PreComputerOff }
  cPauseVideoThreshold              = Word(8);
  cCloseRestartAbortModalThreshold  = Word(10);
  cShowRestartAbortModalThreshold:
    TShowRestartAbortModalThreshold = (Minute: 2; Second: 55);

  { PauseVideo }
  cPauseVideoAhkFilepath = 'E:\autohotkey\os-global\Src\ComputerOff\PauseVideo.ahk';

  { Others }
var
  ZeroDateTime: TDateTime = 0;

implementation

end.
