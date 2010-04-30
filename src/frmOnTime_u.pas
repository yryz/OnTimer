unit frmOnTime_u;

interface

uses
  Windows, Messages, SysUtils, Graphics, Classes, Forms,
  ExtCtrls, Controls, ComCtrls, ImgList, Menus, XPMan, ShellAPI,
  TaskMgr_u;

const
  WM_ICON           = WM_USER + 10;

type
  TfrmOnTime = class(TForm)
    tmrOntimer: TTimer;
    PopMenuA: TPopupMenu;
    mniExit: TMenuItem;
    mniAbout: TMenuItem;
    lvTask: TListView;
    mniAdd: TMenuItem;
    mniDel: TMenuItem;
    mniEdit: TMenuItem;
    N0: TMenuItem;
    N1: TMenuItem;
    tmrMem: TTimer;
    ImageList1: TImageList;
    N2: TMenuItem;
    mniOption: TMenuItem;
    mniExec: TMenuItem;
    function AddTaskItem(b: Boolean; taskType: TTaskType;
      execNum, loopTime: DWORD; dateTime: TTimeStamp;
      param, content: string): Integer;
    procedure FormCreate(Sender: TObject);
    procedure SaveTask(Sender: TObject);
    procedure tmrOntimerTimer(Sender: TObject);
    procedure tmrMemTimer(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniAddClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure mniDelClick(Sender: TObject);
    procedure mniExecClick(Sender: TObject);
    procedure mniOptionClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PopMenuAPopup(Sender: TObject);
  private
    procedure SysEvent(var Message: Tmessage); message WM_SYSCOMMAND;
    procedure TrayEvent(var Message: Tmessage); message WM_ICON;
  public
  end;

resourcestring
  Conf              = 'OnTime.db';


var
  frmOnTime         : TfrmOnTime;
  AppTray           : TNotifyIconData;

implementation

uses
  frmAbout_u, frmAddTask_u, frmOption_u;
{$R *.dfm}

procedure SetTryico;
begin
  with AppTray do begin
    cbSize := SizeOf(AppTray);
    Wnd := frmOnTime.Handle;
    uID := 0;
    uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE;
    uCallbackMessage := WM_ICON;
    HIcon := Application.Icon.Handle;
  end;
  Shell_NotifyIcon(NIM_ADD, @AppTray);
end;

function TfrmOnTime.AddTaskItem;
begin
  Result := g_TaskMgr.Add(taskType, execNum, loopTime, dateTime, param, content);
end;

procedure TfrmOnTime.SysEvent(var Message: Tmessage);
begin
  inherited;
  if Message.wParam = SC_MINIMIZE then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmOnTime.TrayEvent(var Message: Tmessage);
var
  Point             : TPoint;
begin
  GetCursorPos(Point);
  case Message.lParam of
    WM_LBUTTONDBLCLK: begin
        Show;
        Application.Restore;
        Application.BringToFront;
      end;

    WM_RBUTTONDOWN: begin
        SetForegroundWindow(Handle);
        PopMenuA.Popup(Point.x, Point.y);
      end
  else
    inherited;
  end;
end;

procedure TfrmOnTime.FormCreate(Sender: TObject);
var
  f                 : textfile;
  B, S1, S2, S3, S4 : string;
begin
  try
    SetTryico;
    g_TaskMgr := TTaskMgr.Create(lvTask);

    //    if FileExists(Conf) then begin
    //      assignFile(f, Conf);
    //      reset(f);
    //      while not eof(f) do begin
    //        readln(f, B);
    //        readln(f, S1);
    //        readln(f, S4);
    //        readln(f, S3);
    //        readln(f, S2);
    //        AddTaskItem(StrToBool(B), S1, S2, S3, S4);
    //      end;
    //      CloseFile(f);
    //    end;
  except
    CloseFile(f);
  end;
end;

procedure TfrmOnTime.tmrOntimerTimer(Sender: TObject);
var
  dateTime          : TDateTime;
begin
  dateTime := now;
  Caption := FormatDateTime('yyyy-MM-dd hh:mm:ss', dateTime);
  g_TaskMgr.OnTimer(DateTimeToTimeStamp(dateTime));
end;

procedure TfrmOnTime.tmrMemTimer(Sender: TObject);
begin
  SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF); //ÕûÀíÄÚ´æ
end;

procedure TfrmOnTime.SaveTask;
var
  f                 : textfile;
  i, j              : integer;
begin
  Shell_NotifyIcon(NIM_DELETE, @AppTray);

  assignFile(f, Conf);
  ReWrite(f);
  i := lvTask.Items.Count - 1;
  while i > -1 do begin
    j := lvTask.Items[i].SubItems.Count - 1;
    WriteLn(f, BoolToStr(lvTask.Items[i].Checked));
    if lvTask.Items[i].Data <> nil then begin
      WriteLn(f, string(lvTask.Items[i].Data^));
      Dispose(lvTask.Items[i].Data);
    end
    else WriteLn(f, lvTask.Items[i].Caption);
    while j > -1 do begin
      WriteLn(f, lvTask.Items[i].SubItems.Strings[j]);
      Dec(j);
    end;
    Dec(i);
  end;
  CloseFile(f);
end;

procedure TfrmOnTime.mniAboutClick(Sender: TObject);
begin
  if not Assigned(frmAbout) then
    frmAbout := TfrmAbout.Create(Application);
  frmAbout.ShowModal;
  FreeAndNil(frmAbout);
end;

procedure TfrmOnTime.mniAddClick(Sender: TObject);
begin
  if not Assigned(frmAddTask) then
    frmAddTask := TfrmAddTask.Create(Application);
  frmAddTask.Task := nil;
  frmAddTask.ShowModal;
  FreeAndNil(frmAddTask);
end;

procedure TfrmOnTime.mniEditClick(Sender: TObject);
var
  Task              : TTask;
begin
  if lvTask.SelCount < 1 then Exit;
  if not Assigned(frmAddTask) then
    frmAddTask := TfrmAddTask.Create(Application);

  Task := lvTask.Selected.Data;
  if Assigned(Task) then begin
    frmAddTask.Task := Task;
    frmAddTask.ShowModal;
  end;
  FreeAndNil(frmAddTask);
end;

procedure TfrmOnTime.mniDelClick(Sender: TObject);
begin
  g_TaskMgr.DeleteSelected;
end;

procedure TfrmOnTime.mniExecClick(Sender: TObject);
var
  i                 : Integer;
  Task              : TTask;
begin
  with lvTask.Items do
    for i := 0 to Count - 1 do
      if Item[i].Selected then begin
        Task := Item[i].Data;
        if Assigned(Task) then Task.Execute;
      end;
end;

procedure TfrmOnTime.mniOptionClick(Sender: TObject);
begin
  if not Assigned(frmOption) then
    frmOption := TfrmOption.Create(Application);
  frmOption.ShowModal;
  FreeAndNil(frmOption);
end;

procedure TfrmOnTime.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  Shell_NotifyIcon(NIM_DELETE, @AppTray);
end;

procedure TfrmOnTime.PopMenuAPopup(Sender: TObject);
var
  b                 : Boolean;
begin
  b := lvTask.SelCount > 0;
  mniDel.Visible := b;
  mniEdit.Visible := b;
  mniExec.Visible := b;
end;

end.

