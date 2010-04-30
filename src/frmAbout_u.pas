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
    InfoLabelQQ: TLabel;
    InfoLabelL2: TLabel;
    InfoLabelL1: TLabel;
    InfoLabelZZ: TLabel;
    lbl1: TLabel;
    img1: TImage;
    lblVer: TLabel;
    procedure InfoLabelUrlClick(Sender: TObject);
    procedure InfoLabelQQClick(Sender: TObject);
  private

  public

  end;

resourcestring
  Tencent           = 'tencent://message?uin=';

var
  frmAbout          : TfrmAbout;

implementation

{$R *.dfm}

procedure TfrmAbout.InfoLabelUrlClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(InfoLabelUrl.Caption + '?form=OnTimer ' + lblVer.Caption), nil, nil, SW_SHOW);
end;

procedure TfrmAbout.InfoLabelQQClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(Tencent + InfoLabelQQ.Caption), nil, nil, SW_SHOW);
end;

end.
