unit SetUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  StdCtrls, Controls;

type
  TFSet = class(TForm)
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
  FSet: TFSet;

implementation
uses
  uPublic, FOnTime;
{$R *.dfm}

procedure TFSet.btn1Click(Sender: TObject);
begin
  SetData.smtpserver := edtSer.Text;
  SetData.smtpuser := edtUser.Text;

  if not (edtPass.Text = '¹þ¹þ!') then  SetData.smtppass := edtPass.Text;
  Close;
end;

procedure TFSet.edt4KeyPress(Sender: TObject; var Key: Char);
begin
  if word(Key) = VK_RETURN then Btn1.Click;
end;

end.

