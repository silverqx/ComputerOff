unit UnitAbout;

interface

uses
  Winapi.Windows, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TFormAbout = class(TForm)
    PanelMain: TPanel;
    Author: TLabel;
    EmailLink: TLinkLabel;
    OK: TButton;
    ProductName: TLabel;
    ProgramIcon: TImage;
    Version: TLabel;

    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);

    procedure EmailLinkLinkClick(
      Sender: TObject; const Link: string; LinkType: TSysLinkType);
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

uses Winapi.ShellAPI, UnitCommon;

{ published }

procedure TFormAbout.FormShow(Sender: TObject);
begin
  CenterMouse(OK);
end;

procedure TFormAbout.FormKeyPress(Sender: TObject; var Key: Char);
begin
  { Esc }
  if Key = Char(VK_ESCAPE) then
    ModalResult := mrOk;
end;

procedure TFormAbout.EmailLinkLinkClick(
  Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

end.

