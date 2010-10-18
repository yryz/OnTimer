unit frmAbout_u;

interface

uses
  Windows, Messages, Forms, ShellAPI, Controls, StdCtrls, Classes, ExtCtrls,
  Graphics;

type
  TfrmAbout = class(TForm)
    pnl1: TPanel;
    InfoLabelL3: TLabel;
    InfoLabelUrl: TLabel;
    InfoLabelL1: TLabel;
    InfoLabelZZ: TLabel;
    lbl1: TLabel;
    img1: TImage;
    lblVer: TLabel;
    procedure InfoLabelUrlClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  public
  end;

var
  frmAbout          : TfrmAbout;

implementation

{$R *.dfm}

procedure TfrmAbout.InfoLabelUrlClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(InfoLabelUrl.Caption + '?f=OnTimer ' + lblVer.Caption), nil, nil, SW_SHOW);
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.

