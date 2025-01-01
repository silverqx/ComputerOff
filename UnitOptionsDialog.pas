unit UnitOptionsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls;

type
  TFormOptionsDialog = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    OK: TButton;
    Cancel: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    function FindSpinEdit(const AMousePos: TPoint): TSpinEdit;
  public
    { Public declarations }
  end;

var
  FormOptionsDialog: TFormOptionsDialog;

implementation

uses System.Types, UnitMainForm;

{$R *.dfm}

procedure TFormOptionsDialog.FormCreate(Sender: TObject);
begin
  ScaleBy(3, 2);
end;

procedure TFormOptionsDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    ModalResult := mrOk;
    Key := #0;
  end;

  if Key = #27 then
    ModalResult := mrCancel;  
end;

procedure TFormOptionsDialog.FormShow(Sender: TObject);
var
  pt: TPoint;
begin
  SetFocus;
  FocusControl(SpinEdit2);
  SpinEdit1.Value := 1;
  SpinEdit2.Value := 0;
  SpinEdit3.Value := 0;

  // Center Mouse Cursor in OK button
  pt := Point(OK.Left + Round(OK.Width / 2), OK.Top + Round(OK.Height / 2));
  pt := ClientToScreen(pt);
  SetCursorPos(pt.X, pt.Y);
end;

procedure TFormOptionsDialog.SpinEdit1Change(Sender: TObject);
begin
  if SpinEdit1.Value = -1 then
    SpinEdit1.Value := 0;
  if SpinEdit1.Value = 24 then
    SpinEdit1.Value := 0;
end;

procedure TFormOptionsDialog.SpinEdit2Change(Sender: TObject);
begin
  if SpinEdit2.Value < 0 then
  begin
    if SpinEdit1.Value > 0 then
    begin
      SpinEdit2.Value := 30;
      SpinEdit1.Value := SpinEdit1.Value - SpinEdit1.Increment;
    end
    else
      SpinEdit2.Value := 0;
  end;
  if SpinEdit2.Value > 59 then
  begin
    SpinEdit2.Value := 0;
    SpinEdit1.Value := SpinEdit1.Value + SpinEdit1.Increment;
  end;
end;

procedure TFormOptionsDialog.SpinEdit3Change(Sender: TObject);
begin
  if SpinEdit3.Value < 0 then
    SpinEdit3.Value := 30;
  if SpinEdit3.Value > 59 then
    SpinEdit3.Value := 0;
end;

procedure TFormOptionsDialog.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbRight then
  begin
    // https://stackoverflow.com/questions/6773400/focus-next-control-on-enter-in-overridden-keyup
    Perform(CM_DIALOGKEY, VK_TAB, 0);
  end;
end;

procedure TFormOptionsDialog.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  LSpinEdit: TSpinEdit;
begin
  LSpinEdit := FindSpinEdit(MousePos);
  if LSpinEdit = nil then
    Exit;

  LSpinEdit.Value := LSpinEdit.Value - LSpinEdit.Increment;
  Handled := True;
end;

procedure TFormOptionsDialog.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint;
  var Handled: Boolean);
var
  LSpinEdit: TSpinEdit;
begin
  LSpinEdit := FindSpinEdit(MousePos);
  if LSpinEdit = nil then
    Exit;

  LSpinEdit.Value := LSpinEdit.Value + LSpinEdit.Increment;
  Handled := True;
end;

function TFormOptionsDialog.FindSpinEdit(const AMousePos: TPoint): TSpinEdit;
var
  LWindow: HWND;
  LWinControl: TWinControl;
begin
  Result := nil;

  LWindow := WindowFromPoint(AMousePos);
  if LWindow = 0 then
    Exit;

  LWinControl := FindControl(LWindow);
  if LWinControl = nil then
    Exit;

  if LWinControl is TSpinEdit then
    Exit(LWinControl as TSpinEdit);

  if LWinControl.Parent is TSpinEdit then
    Exit(LWinControl.Parent as TSpinEdit);

  if ActiveControl is TSpinEdit then
    Exit(ActiveControl as TSpinEdit);
end;

end.
