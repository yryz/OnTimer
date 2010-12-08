unit frmOnTimer_u;

interface

uses
  Windows, Messages, SysUtils, Graphics, Classes, Forms,
  ExtCtrls, Controls, ComCtrls, ImgList, Menus, XPMan, ShellAPI,
  TaskMgr_u, HouListView, CommCtrl, StdCtrls, Dialogs;

const
  WM_ICON           = WM_USER + 10;

type
  TfrmOnTimer = class(TForm)
    tmrOntimer: TTimer;
    pmTask: TPopupMenu;
    mniExit: TMenuItem;
    mniAbout: TMenuItem;
    lvTask: THouListView;
    mniAdd: TMenuItem;
    mniDel: TMenuItem;
    mniEdit: TMenuItem;
    N0: TMenuItem;
    N1: TMenuItem;
    tmrMem: TTimer;
    il1: TImageList;
    N2: TMenuItem;
    mniOption: TMenuItem;
    mniExec: TMenuItem;
    N3: TMenuItem;
    pmClass: TPopupMenu;
    mniAddClass: TMenuItem;
    mniDelClass: TMenuItem;
    pnl1: TPanel;
    tvClass: TTreeView;
    edtSearch: TEdit;
    spl1: TSplitter;
    mniEditClass: TMenuItem;
    il2: TImageList;
    N4: TMenuItem;
    mniPause: TMenuItem;
    mniMove: TMenuItem;
    mniMoveTop: TMenuItem;
    mniMoveUp: TMenuItem;
    mniMoveDown: TMenuItem;
    mniMoveBottom: TMenuItem;
    mniN5: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tmrOntimerTimer(Sender: TObject);
    procedure tmrMemTimer(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniAddClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure mniDelClick(Sender: TObject);
    procedure mniExecClick(Sender: TObject);
    procedure mniOptionClick(Sender: TObject);
    procedure pmTaskPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mniExitClick(Sender: TObject);
    procedure lvTaskChecking(Item: TListItem; Checked: Boolean;
      var Accept: Boolean);
    procedure lvTaskColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvTaskCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvTaskClick(Sender: TObject);

    procedure tvClassChange(Sender: TObject; Node: TTreeNode);
    procedure edtSearchChange(Sender: TObject);
    procedure tvClassContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure mniAddClassClick(Sender: TObject);
    procedure mniDelClassClick(Sender: TObject);
    procedure tvClassEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure tvClassEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure mniEditClassClick(Sender: TObject);
    procedure mniPauseClick(Sender: TObject);

    procedure TaskUpdateIndex(Item, Item2: TListItem);
    procedure lvTaskDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvTaskDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure mniMoveClick(Sender: TObject);
    procedure tvClassDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvClassDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure edtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure lvTaskCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    procedure ActiveWindows;
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
        ActiveWindows;
      end;

    WM_RBUTTONDOWN:
      begin
        SetForegroundWindow(Handle);
        pmTask.Popup(Point.x, Point.y);
      end;

    WM_MOUSEFIRST:
      begin
        with g_TaskMgr do
          StrPCopy(g_AppTray.szTip, Format(Application.Title
            + #13#10'任务状态: %d/%d', [ActiveTask, List.Count]));
        Shell_NotifyIcon(NIM_MODIFY, @g_AppTray);
      end;
  else
    inherited;
  end;
end;

procedure TfrmOnTimer.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  Caption := Application.Title;
  g_TaskMgr := TTaskMgr.Create(tvClass, lvTask);
  g_TaskMgr.LoadDB;

  SetTryico;

  edtSearch.Align := alTop;
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

procedure TfrmOnTimer.pmTaskPopup(Sender: TObject);
var
  b                 : Boolean;
begin
  b := Showing and (lvTask.SelCount > 0);
  mniDel.Visible := b;
  mniEdit.Visible := b;
  mniExec.Visible := b;
  mniMove.Visible := b;
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
begin
  if (Msg.LParamLo = FHkShift) and (Msg.LParamHi = FHkKey) then
  begin
    ActiveWindows;
  end;
end;

procedure TfrmOnTimer.ActiveWindows;
var
  frm               : TForm;
begin
  //防止设置时响应(消息提示窗口时，也会失效)
  if Assigned(frmAddTask) then
    frm := frmAddTask
  else if Assigned(frmAbout) then
    frm := frmAbout
  else if Assigned(frmOption) then
    frm := frmOption
  else
  begin
    frm := Self;
    Show;
  end;

  SetActiveWindow(frm.Handle);
  SetForegroundWindow(frm.Handle);
end;

procedure TfrmOnTimer.lvTaskChecking(Item: TListItem; Checked: Boolean;
  var Accept: Boolean);
begin
  TTask(Item.Data).UpdateActive(Checked);
end;

procedure TfrmOnTimer.SetHotKey(ShortCut: TShortCut);
begin
  UnRegisterHotKey(Handle, 0);

  FHkShift := 0;
  FHkKey := ShortCut and not (scShift + scCtrl + scAlt);

  if (FHkKey = 0) then
    Exit;

  if ShortCut and scShift <> 0 then
    FHkShift := MOD_SHIFT;
  if ShortCut and scCtrl <> 0 then
    FHkShift := FHkShift or MOD_CONTROL;
  if ShortCut and scAlt <> 0 then
    FHkShift := FHkShift or MOD_ALT;

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

procedure TfrmOnTimer.lvTaskClick(Sender: TObject);
var
  n                 : Integer;
begin
  n := TListView(Sender).SelCount;
  if n > 0 then
  begin
    if n > 1 then
      TListView(Sender).DragMode := dmManual
    else
      TListView(Sender).DragMode := dmAutomatic;
  end
  else
    tvClass.SetFocus;
end;

procedure TfrmOnTimer.tvClassChange(Sender: TObject; Node: TTreeNode);
var
  s                 : string;
begin
  if Node = nil then
    Exit;

  if Sender = nil then
    s := edtSearch.Text
  else
    s := '';

  if (Node.Parent = nil) then
    case Node.Index of
      0:                                //活动
        g_TaskMgr.LoadUI(tcActive, 0, s);

      1:                                //所有
        g_TaskMgr.LoadUI(tcAll, 0, s);

      4:                                //未分类
        g_TaskMgr.LoadUI(tcNoneClass, 0, s);
    end
  else
    case Node.Parent.Index of
      2:                                //按类型
        g_TaskMgr.LoadUI(tcByType, Integer(Node.Data), s);

      3:                                //按分类
        g_TaskMgr.LoadUI(tcByClass, Integer(Node.Data), s);
    end;
end;

procedure TfrmOnTimer.edtSearchChange(Sender: TObject);
begin
  tvClassChange(nil, tvClass.Selected);
end;

procedure TfrmOnTimer.tvClassContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  b, b2             : Boolean;
  Node, ClassNode   : TTreeNode;
begin
  Node := TTreeView(Sender).GetNodeAt(MousePos.X, MousePos.Y);
  b := Node <> nil;
  if b then
  begin
    Node.Selected := True;
    ClassNode := g_TaskMgr.Classes.ClassNode[tcByClass];
    b := Node = ClassNode;
    b2 := Node.Parent = ClassNode;
  end
  else
    b2 := False;

  mniAddClass.Visible := b;
  mniDelClass.Visible := b2;
  mniEditClass.Visible := b2;
end;

procedure TfrmOnTimer.mniAddClassClick(Sender: TObject);
begin
  g_TaskMgr.Classes.New(InputBox('添加分类', '输入分类名:          ', ''));
end;

procedure TfrmOnTimer.mniDelClassClick(Sender: TObject);
var
  Node              : TTreeNode;
begin
  Node := tvClass.Selected;
  if Assigned(Node)
    and (MessageBox(Handle, '确定要删除分类？(此分类下的数据将转移到“未分类”)',
    '提示', MB_ICONQUESTION or MB_YESNO) = ID_YES) then
  begin
    g_TaskMgr.Classes.Delete(Integer(Node.Data));
    Node.Delete;
  end;
end;

procedure TfrmOnTimer.tvClassEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  if (Node.Parent = g_TaskMgr.Classes.ClassNode[tcByClass])
    and (Node.Text <> S) then
    g_TaskMgr.Classes.Update(False, Integer(Node.Data), Node.Index, S);
end;

procedure TfrmOnTimer.tvClassEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := Node.Parent = g_TaskMgr.Classes.ClassNode[tcByClass];
end;

procedure TfrmOnTimer.mniEditClassClick(Sender: TObject);
begin
  tvClass.Selected.EditText;
end;

procedure TfrmOnTimer.mniPauseClick(Sender: TObject);
begin
  if mniPause.Checked then
  begin
    tmrOntimer.Enabled := False;
    Caption := Caption + ' 暂停!';
  end
  else
    tmrOntimer.Enabled := True;
end;

procedure TfrmOnTimer.lvTaskDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Sender = Source) and (TListView(Sender).GetItemAt(X, Y) <> nil);
end;

procedure TfrmOnTimer.lvTaskDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item, Item2       : TListItem;
begin
  Item := TListView(Source).Selected;
  Item2 := TListView(Sender).GetItemAt(X, Y);
  if Assigned(Item2) then
  begin
    TaskUpdateIndex(Item, Item2);
  end;
end;

procedure TfrmOnTimer.mniMoveClick(Sender: TObject);
var
  Item, Item2       : TListItem;
begin
  Item := lvTask.Selected;
  if Item = nil then
    Exit;

  if Sender = mniMoveTop then
  begin
    if Item.Index = 0 then
      Exit;

    Item2 := lvTask.Items[0];
  end
  else if Sender = mniMoveUp then
  begin
    if Item.Index = 0 then
      Exit;

    Item2 := lvTask.Items[Item.Index - 1];
  end
  else if Sender = mniMoveDown then
  begin
    if Item.Index = lvTask.Items.Count - 1 then
      Exit;

    Item2 := lvTask.Items[Item.Index + 1];
  end
  else if Sender = mniMoveBottom then
  begin
    if Item.Index = lvTask.Items.Count - 1 then
      Exit;

    Item2 := lvTask.Items[lvTask.Items.Count - 1];
  end
  else
    Assert(False);

  TaskUpdateIndex(Item, Item2);
end;

procedure TfrmOnTimer.TaskUpdateIndex(Item, Item2: TListItem);
var
  Task, Task2       : TTask;
begin
  Task := Item.Data;
  Task2 := Item2.Data;
  Task.UpdateIndex(Task2);

  tvClassChange(tvClass, tvClass.Selected);
  if Assigned(Task.ItemUI) then
  begin
    Task.ItemUI.Focused := True;
    Task.ItemUI.Selected := True;
    Task.ItemUI.MakeVisible(True);
  end;
end;

procedure TfrmOnTimer.tvClassDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node              : TTreeNode;
begin
  Node := tvClass.GetNodeAt(X, Y);
  Accept := (Node <> nil)
    and (Node.Parent = g_TaskMgr.Classes.ClassNode[tcByClass]) {User Class}
  and (TTask(lvTask.Selected.Data).CId <> Integer(Node.Data)) {Self ?};
end;

procedure TfrmOnTimer.tvClassDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Node              : TTreeNode;
  Item              : TListItem;
begin
  if Source = lvTask then
  begin
    Node := tvClass.GetNodeAt(X, Y);
    Item := lvTask.Selected;
    if (Node <> nil) and (Item <> nil) then
    begin
      TTask(Item.Data).UpdateCId(Integer(Node.Data));
      Caption := '归类到: ' + Node.Text;
    end;
  end;
end;

procedure TfrmOnTimer.edtSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    edtSearchChange(nil);
end;

procedure TfrmOnTimer.lvTaskCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Item.Index mod 2 <> 0 then
    Sender.Canvas.Brush.Color := $00FFFEFA
  else
    Sender.Canvas.Brush.Color := clWindow;
end;

initialization
  g_TaskBarMSG := RegisterWindowMessage('TaskbarCreated');
finalization
  Shell_NotifyIcon(NIM_DELETE, @g_AppTray);
end.

