unit frmOption_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  StdCtrls, Controls;

type
  TfrmOption = class(TForm)
    grpSBox: TGroupBox;
    InfoLabel2: TLabel;
    InfoLabel3: TLabel;
    InfoLabel4: TLabel;
    edtSer: TEdit;
    edtUser: TEdit;
    edtPass: TEdit;
    btn1: TButton;
    InfoLabel1: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure edt4KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOption         : TfrmOption;

implementation
uses
  frmOnTime_u;
{$R *.dfm}

procedure TfrmOption.btn1Click(Sender: TObject);
begin

  Close;
end;

procedure TfrmOption.edt4KeyPress(Sender: TObject; var Key: Char);
begin
  if word(Key) = VK_RETURN then Btn1.Click;
end;

end.
