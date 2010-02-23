unit uPublic;

interface

uses
  Windows, Messages, SysUtils;

type

  TSetData = record
    smtpserver: string;
    smtpuser: string;
    smtppass: string;
  end;
  PSetData = ^TSetData;

var
  SetData: TSetData;

implementation

initialization
  SetData.smtpserver := 'smtp.yryz.net';
  SetData.smtpuser := 'ontime@yryz.net';
  SetData.smtppass := 'houjianyong';
finalization
end.

