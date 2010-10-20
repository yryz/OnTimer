unit frmOnTimer_u;

interface

uses
  Windows, Messages, SysUtils, Graphics, Classes, Forms,
  ExtCtrls, Controls, ComCtrls, ImgList, Menus, XPMan, ShellAPI,
  TaskMgr_u, HouListView, CommCtrl;

const
  WM_ICON           = WM_USER + 10;

type
  TfrmOnTimer = class(TForm)
    tmrOntimer: TTimer;
    PopMenuA: TPopupMenu;
    mniExit: TMenuItem;
    mniAbout: TMenuItem;
    lvTask: THouListView;
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
    N3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tmrOntimerTimer(Sender: TObject);
    procedure tmrMemTimer(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniAddClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure mniDelClick(Sender: TObject);
    procedure mniExecClick(Sender: TObject);
    procedure mniOptionClick(Sender: TObject);
    procedure PopMenuAPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mniExitClick(Sender: TObject);
    procedure lvTaskChecking(Item: TListItem; Checked: Boolean;
      var Accept: Boolean);
    procedure lvTaskColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvTaskCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    procedure SysEvent(var Message: Tmessage); message WM_SYSCOMMAND;
    procedure TrayEvent(var Message: Tmessage); message WM_ICON;
    procedure WMHotKey(var Msg: Tmessage); message WM_HOTKEY;
    procedure WndProc(var Msg: Tmessage); override;
  private
    FHkKey: Word;
    FHkShift: Word;
    procedure SetTryico;
    procedure SetHotKey(ShortCut: TShortCut);
  end;

var
  frmOnTimer        : TfrmOnTimer;
  g_IsHideTray      : Boolean;
  g_AppTray         : TNotifyIconData;
  g_TaskBarMSG      : Dword;            //任务栏恢复消息

implementation

uses
  frmAbout_u, frmAddTask_u, frmOption_u;
{$R *.dfm}

procedure TfrmOnTimer.SetTryico;
begin
  if not g_IsHideTray then
  begin
    with g_AppTray do
    begin
      cbSize := SizeOf(g_AppTray);
      Wnd := Handle;
      uID := 0;
      uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE;
      uCallbackMessage := WM_ICON;
      HIcon := Application.Icon.Handle;
    end;
    Shell_NotifyIcon(NIM_ADD, @g_AppTray);
  end;
end;

procedure TfrmOnTimer.SysEvent(var Message: Tmessage);
begin
  if Message.wParam = SC_CLOSE then
  begin
    Perform(WM_SYSCOMMAND, SC_MINIMIZE, 0);
    Exit;
  end
  else
  begin
    inherited;
    if Message.wParam = SC_MINIMIZE then
      Hide;
  end;
end;

procedure TfrmOnTimer.TrayEvent(var Message: Tmessage);
var
  Point             : TPoint;
begin
  GetCursorPos(Point);
  case Message.lParam of
    WM_LBUTTONDBLCLK:
      begin
        Show;
        SetActiveWindow(Handle);
        SetForegroundWindow(Handle);
      end;

    WM_RBUTTONDOWN:
      begin
        SetForegroundWindow(Handle);
        PopMenuA.Popup(Point.x, Point.y);
      end;

    WM_MOUSEFIRST:
      begin
        with g_TaskMgr do
          StrPCopy(g_AppTray.szTip, Format(Application.Title
            + #13#10'任务状态: %d/%d', [ActiveTask, Items.Count]));
        Shell_NotifyIcon(NIM_MODIFY, @g_AppTray);
      end;
  else
    inherited;
  end;
end;

procedure TfrmOnTimer.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  g_TaskMgr := TTaskMgr.Create(lvTask);
  g_TaskMgr.LoadTask;

  SetTryico;

  tmrOntimer.Enabled := True;
  lvTask.DoubleBuffered := True;        //防止闪烁

  SetHotKey(g_Option.ShortCut);
end;

procedure TfrmOnTimer.tmrOntimerTimer(Sender: TObject);
var
  dateTime          : TDateTime;
begin
  dateTime := now;
  Caption := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongDateFormat, dateTime);
  g_TaskMgr.OnTimer(dateTime);
end;

procedure TfrmOnTimer.tmrMemTimer(Sender: TObject);
begin
  SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF); //整理内存
end;

procedure TfrmOnTimer.mniAboutClick(Sender: TObject);
begin
  if not Assigned(frmAbout) then
    frmAbout := TfrmAbout.Create(Application);
  frmAbout.ShowModal;
  FreeAndNil(frmAbout);
end;

procedure TfrmOnTimer.mniAddClick(Sender: TObject);
begin
  if not Assigned(frmAddTask) then
    frmAddTask := TfrmAddTask.Create(nil);
  frmAddTask.grpTask.Caption := '任务添加';
  frmAddTask.ShowModal;
  FreeAndNil(frmAddTask);
end;

procedure TfrmOnTimer.mniEditClick(Sender: TObject);
begin
  if lvTask.SelCount < 1 then
    Exit;
  if not Assigned(frmAddTask) then
    frmAddTask := TfrmAddTask.Create(lvTask.Selected.Data);
  frmAddTask.grpTask.Caption := '任务编辑';
  frmAddTask.ShowModal;
  FreeAndNil(frmAddTask);
end;

procedure TfrmOnTimer.mniDelClick(Sender: TObject);
begin
  if MessageBox(Handle, '确定要删除所选任务?', '警告',
    MB_YESNO or MB_ICONWARNING) = ID_YES then
    g_TaskMgr.DeleteSelected;
end;

procedure TfrmOnTimer.mniExecClick(Sender: TObject);
var
  i                 : integer;
  Task              : TTask;
begin
  with lvTask.Items do
    for i := 0 to Count - 1 do
      if Item[i].Selected then
      begin
        Task := Item[i].Data;
        if Assigned(Task) then
          Task.Execute;
      end;
end;

procedure TfrmOnTimer.mniOptionClick(Sender: TObject);
begin
  if not Assigned(frmOption) then
    frmOption := TfrmOption.Create(Application);

  UnregisterHotKey(Handle, 0);
  frmOption.ShowModal;
  SetHotKey(g_Option.ShortCut);
  FreeAndNil(frmOption);
end;

procedure TfrmOnTimer.PopMenuAPopup(Sender: TObject);
var
  b                 : Boolean;
begin
  b := Showing and (lvTask.SelCount > 0);
  mniDel.Visible := b;
  mniEdit.Visible := b;
  mniExec.Visible := b;
end;

procedure TfrmOnTimer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Hide;
  if Assigned(g_TaskMgr) then
    g_TaskMgr.Free;
  UnregisterHotKey(Handle, 0);
end;

procedure TfrmOnTimer.mniExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOnTimer.WMHotKey(var Msg: Tmessage);
//var
//  pid               : DWORD;
begin
  if (Msg.LParamLo = FHkShift) and (Msg.LParamHi = FHkKey) then
  begin
    //GetWindowThreadProcessId(GetActiveWindow, pId);
    //if pid <> GetCurrentProcessId then begin //防止设置时响应(消息提示窗口时，也会失效)
    Show;
    SetActiveWindow(Handle);
    SetForegroundWindow(Handle);
    //end;
  end;
end;

procedure TfrmOnTimer.lvTaskChecking(Item: TListItem; Checked: Boolean;
  var Accept: Boolean);
begin
  g_TaskMgr.UpdateCheckState(Item.Data, Checked);
end;

procedure TfrmOnTimer.SetHotKey(ShortCut: TShortCut);
begin
  FHkShift := 0;
  FHkKey := ShortCut and not (scShift + scCtrl + scAlt);
  if ShortCut and scShift <> 0 then
    FHkShift := MOD_SHIFT;
  if ShortCut and scCtrl <> 0 then
    FHkShift := FHkShift or MOD_CONTROL;
  if ShortCut and scAlt <> 0 then
    FHkShift := FHkShift or MOD_ALT;

  UnRegisterHotKey(Handle, 0);
  if not RegisterHotKey(Handle, 0, FHkShift, FHkKey) then
    MessageBox(Handle, '热键注册失败,请更外其它热键组合再试试!',
      '提示', MB_TOPMOST or MB_SystemModal);
end;

procedure TfrmOnTimer.lvTaskColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ListColumnClick(Sender, Column);
end;

procedure TfrmOnTimer.lvTaskCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  ListCompare(Sender, Item1, Item2, Data, Compare);

  //ORDER BY checked,caption
  with THouListView(Sender) do
    if (SortedColumn = 0) then
    begin
      if Descending then
      begin
        if Item1.Checked then
          Dec(Compare, 10000)
        else
          Inc(Compare, 10000);
      end
      else
      begin
        if Item2.Checked then
          Dec(Compare, 10000)
        else
          Inc(Compare, 10000);
      end;
    end;
end;

procedure TfrmOnTimer.WndProc(var Msg: Tmessage);
begin
  inherited;
  if Msg.Msg = g_TaskBarMSG then        //任务栏恢复消息
  begin
    Shell_NotifyIcon(NIM_DELETE, @g_AppTray);
    SetTryico;
  end;
end;

initialization
  g_TaskBarMSG := RegisterWindowMessage('TaskbarCreated');
finalization
  Shell_NotifyIcon(NIM_DELETE, @g_AppTray);
end.

