unit AboutUtils;

interface

uses
  Windows, Messages, Forms, ShellAPI, Controls, StdCtrls, Classes, ExtCtrls;

type
  TFAbout = class(TForm)
    pnl1: TPanel;
    InfoLabelL3: TLabel;
    InfoLabelUrl: TLabel;
    InfoLabelQQ: TLabel;
    InfoLabelL2: TLabel;
    InfoLabelL1: TLabel;
    InfoLabelZZ: TLabel;
    procedure InfoLabelUrlClick(Sender: TObject);
    procedure InfoLabelQQClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

resourcestring
  Tencent = 'tencent://message?uin=';

var
  FAbout: TFAbout;

implementation

{$R *.dfm}

procedure TFAbout.InfoLabelUrlClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(InfoLabelUrl.Caption), nil, nil, SW_SHOW);
end;

procedure TFAbout.InfoLabelQQClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(Tencent + InfoLabelQQ.Caption), nil, nil, SW_SHOW);
end;

end.

