unit UnitOptionsDialog;

interface

uses
  Winapi.Windows, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin;

type
  TFormOptionsDialog = class(TForm)
    PanellMain: TPanel;
    Cancel: TButton;
    ComputerOffType: TComboBox;
    Hour: TSpinEdit;
    LabelColon1: TLabel;
    LabelColon2: TLabel;
    LabelComputerOffType: TLabel;
    LabelTimeout: TLabel;
    Minute: TSpinEdit;
    OK: TButton;
    Second: TSpinEdit;
    TimerComboBoxDropDown: TTimer;

    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);

    procedure FormMouseUp(
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelUp(
      Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(
      Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);

    procedure HourChange(Sender: TObject);
    procedure MinuteChange(Sender: TObject);
    procedure SecondChange(Sender: TObject);

    procedure ComputerOffTypeChange(Sender: TObject);

    procedure TimerComboBoxDropDownTimer(Sender: TObject);
    procedure ComputerOffTypeDropDown(Sender: TObject);
    procedure ComputerOffTypeCloseUp(Sender: TObject);

  strict private
    FComputerOffTypeOpened: Boolean;
    function FindSpinEdit(const AMousePosition: TPoint): TSpinEdit;
  end;

var
  FormOptionsDialog: TFormOptionsDialog;

implementation

uses UnitCommon, UnitMainForm;

{$R *.dfm}

{ published }

procedure TFormOptionsDialog.FormShow(Sender: TObject);
begin
  FocusControl(Minute);

  // Center the mouse cursor in the given control
  CenterMouse(OK, False);
end;

procedure TFormOptionsDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // TComboBox uses Enter and Esc during opened drop-down list
  if (ActiveControl = ComputerOffType) and FComputerOffTypeOpened then
    Exit;

  // Enter
  if Key = Char(VK_RETURN) then
    ModalResult := mrOk

  // Esc
  else if Key = Char(VK_ESCAPE) then
    ModalResult := mrCancel;
end;

procedure TFormOptionsDialog.FormMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LIsLeftButton: Boolean;
begin
  LIsLeftButton := Button = mbLeft;

  // Don't handle left mouse clicks on TButton-s
  if LIsLeftButton and (Sender is TButton) then
    Exit;

  // Focus the next/previous control on right/left mouse clicks
  if LIsLeftButton or (Button = mbMiddle) then
    SelectNext(ActiveControl, False, True)

  else if Button = mbRight then
    SelectNext(ActiveControl, True, True);
end;

procedure TFormOptionsDialog.FormMouseWheelDown(
  Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  LSpinEdit: TSpinEdit;
begin
  LSpinEdit := FindSpinEdit(MousePos);

  // Nothing to do
  if LSpinEdit = nil then
    Exit;

  with LSpinEdit do
    Value := Value - Increment;

  Handled := True;
end;

procedure TFormOptionsDialog.FormMouseWheelUp(
  Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  LSpinEdit: TSpinEdit;
begin
  LSpinEdit := FindSpinEdit(MousePos);

  // Nothing to do
  if LSpinEdit = nil then
    Exit;

  with LSpinEdit do
    Value := Value + Increment;

  Handled := True;
end;

procedure TFormOptionsDialog.HourChange(Sender: TObject);
begin
  with Hour do
    if Value < 0 then
      Value := 0 // Don't user 23 here, 0 disables looping over

    else if Value > 23 then
      Value := 23; // Don't user 0 here, 23 disables looping over
end;

procedure TFormOptionsDialog.MinuteChange(Sender: TObject);
begin
  with Minute do
    if Value < 0 then
    begin
      if Hour.Value > 0 then
      begin
        Value := 30;
        with Hour do
          Value := Value - Increment;
      end
      else
        Value := 0; // Don't user 30 here, 0 disables looping over
    end

    // Stop incrementing at 23:30:xx
    else if (Hour.Value = 23) and (Value > 59) then
      Value := 30

    else if Value > 59 then
    begin
      Value := 0;
      with Hour do
        Value := Value + Increment;
    end;
end;

procedure TFormOptionsDialog.SecondChange(Sender: TObject);
begin
  with Second do
    if Value < 0 then
    begin
      Value := 30; // Don't user 0 here, 30 allows looping over
      with Minute do
        Value := Value - Increment;
    end

    else if Value > 59 then
    begin
      Value := 0;
      with Minute do
        Value := Value + Increment;
    end;
end;

procedure TFormOptionsDialog.ComputerOffTypeChange(Sender: TObject);
begin
  // Update it immediately
  FormMainForm.PrepareComputerOffType;
end;

procedure TFormOptionsDialog.TimerComboBoxDropDownTimer(Sender: TObject);
begin
  TimerComboBoxDropDown.Enabled := False;
  FComputerOffTypeOpened := False;
end;

procedure TFormOptionsDialog.ComputerOffTypeDropDown(Sender: TObject);
begin
  FComputerOffTypeOpened := True;
end;

procedure TFormOptionsDialog.ComputerOffTypeCloseUp(Sender: TObject);
begin
  // Timer is needed to keep/extend the open dropdown state long enough
  TimerComboBoxDropDown.Enabled := True;
end;

{ private }

function TFormOptionsDialog.FindSpinEdit(const AMousePosition: TPoint): TSpinEdit;
var
  LWindow: HWND;
  LWinControl: TWinControl;
begin
  Result := nil;

  // Try to find a control under the cursor
  LWindow := WindowFromPoint(AMousePosition);
  if LWindow = 0 then
    Exit;

  LWinControl := FindControl(LWindow);
  if LWinControl = nil then
    Exit;

  // TSpinEdit found
  if LWinControl is TSpinEdit then
    Exit(LWinControl as TSpinEdit);

  if LWinControl.Parent is TSpinEdit then
    Exit(LWinControl.Parent as TSpinEdit);

  // If TSpinEdit has a focus take it
  if ActiveControl is TSpinEdit then
    Exit(ActiveControl as TSpinEdit);
end;

end.
