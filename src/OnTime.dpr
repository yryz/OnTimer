program OnTime;

uses
  Forms,
  FOnTime in 'FOnTime.pas' {frmOnTimer},
  AddUtils in 'AddUtils.pas' {AddTaskF},
  SetUnit in 'SetUnit.pas' {FSet},
  AboutUtils in 'AboutUtils.pas' {FAbout},
  uPublic in 'uPublic.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '任务计划';
  Application.CreateForm(TfrmOnTimer, frmOnTimer);
  Application.CreateForm(TFSet, FSet);
  Application.Run;
end.

