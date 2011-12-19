unit TaskMgr_u;

interface
uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls, ComCtrls,
  ShellAPI, SQLite3, SQLiteTable3, HouListView, DateUtils,
  DB_u, WinHttp;

type
  TTaskType = (
    ttExec,
    ttParamExec,
    ttDownExec,
    ttKillProcess,
    ttCmdExec,
    ttWakeUp,
    ttMsgTip,
    ttSendEmail,
    ttSendKey,
    ttShutdownSys,
    ttRebootSys,
    ttLogoutSys,
    ttLockSys,
    ttSuspendSys);

  //任务分类类型
  TTaskClass = (tcActive,               //活动
    tcAll,                              //所有
    tcByType,                           //按类型
    tcByClass,                          //按分类
    tcNoneClass,                        //未分类
    tcStat                              //计数
    );
const
  TASK_TYPE_STR     : array[TTaskType] of string =
    ('普通运行', '参数运行', '下载运行', '结束进程', '执行DOS', '网络唤醒',
    '消息提示', '发送邮件', '模拟按键', '关闭系统', '重启系统', '注销登陆',
    '锁定系统', '系统待机');

  TASK_CLASS_STR    : array[TTaskClass] of string =
    ('活动任务', '所有任务', '任务类型', '任务分类', '未分类', '0');
  TASK_CLASS_IMG    : array[TTaskClass] of Integer =
    (0, 1, 2, 3, 4, 5);

  HIDE_PARAM_HEAD   = '-h';

const
  DATETIME_FORMAT_SETTINGS: TFormatSettings = (
    DateSeparator: '-';
    TimeSeparator: ':';
    ShortDateFormat: 'yyyy-MM-dd';
    LongDateFormat: 'yyyy-MM-dd hh:mm:ss';
    ShortTimeFormat: 'hh:mm';
    LongTimeFormat: 'hh:mm:ss');

type
  TTimeType = (tmDateTime, tmTime, tmLoop, tmMonthly);
  TWeekOfDay = (wdNone, wdSun, wdMon, wdTue, wdWed, wdThu, wdFri, wdSat);
  PWeekSet = ^TWeekSet;
  TWeekSet = set of TWeekOfDay;

  PTimeRec = ^TTimeRec;
  TTimeRec = record                     //要与TTimeStamp结构相同！
    TimeOrLoop: DWORD;
    DateOrWeek: DWORD;
  end;
  PTimeStamp = ^TTimeStamp;

type
  TTask = class;
  TTaskMgr = class;
  TTaskClasses = class;

  PTask = ^TTask;
  TTask = class(TObject)                //TListView中添加此Item
  private
    FNew: Boolean;                      //新数据(DB无记录)
    FOwner: TTaskMgr;
    FItemUI: TListItem;

    FActive: Boolean;
    FId: Integer;                       //任务ID
    FCId: Integer;                      //分类ID
    FExecNum: Integer;                  //可执行次数
    FTaskType: TTaskType;
    FTimeType: TTimeType;               //记时类型
    FWeekStr: string;
    FTimeRec: TTimeRec;                 //设定日期、时间、星期、倒计时
    FParam: string;
    FContent: string;
    FTmpExecNum: Boolean;               //临时执行计数

    procedure SetExecNum(const Value: Integer);
  protected
    FLoopTime: DWORD;                   //倒计时计数
    FNrExec: Integer;                   //已执行次数
    function DecLoop: Integer;          //调用一次，减一秒
    function IncExec: Integer;
    procedure UpdateNrExecUI;
  public
    constructor Create(AOwner: TTaskMgr; bNew: Boolean);
    destructor Destroy; override;

    function ShowCmd(): Integer;
    procedure Execute;

    function CheckTime(dateTime: TDateTime; onlyCheck: Boolean): Integer; //< 0 无效; 0 已到; > 0 未到

    function IsWeek(): Boolean;
    procedure AssignUI(Item: TListItem); //绑定UI
    procedure ResetLoop();

    //DB
    function Update(): Integer;         //更新DB
    function Delete(OnlySelected: Boolean): Integer;

    procedure UpdateCId(const Value: Integer);
    procedure UpdateActive(const Value: Boolean);
    procedure UpdateIndex(const Value: Integer); overload;
    procedure UpdateIndex(const DstTask: TTask); overload;

    property Id: Integer read FId write FId;
    property CId: Integer read FCId write FCId;
    property Active: Boolean read FActive write FActive;
    property ExecNum: Integer read FExecNum write SetExecNum;
    property TaskType: TTaskType read FTaskType write FTaskType;
    property TimeType: TTimeType read FTimeType write FTimeType;
    property TimeRec: TTimeRec read FTimeRec write FTimeRec;
    property Param: string read FParam write FParam;
    property Content: string read FContent write FContent;
    property TmpExecNum: Boolean read FTmpExecNum write FTmpExecNum;

    property ItemUI: TListItem read FItemUI;
  end;

  { 自定义分类 }
  TClassNode = array[TTaskClass] of TTreeNode;
  TTaskClasses = class(TObject)
  private
    FTv: TTreeView;
    FOwner: TTaskMgr;
    FClassNode: TClassNode;
  protected
    procedure LoadClass;
  public
    constructor Create(AOwner: TTaskMgr; tvClass: TTreeView);
    destructor Destroy; override;

    function New(Name: string): TTreeNode;
    function Update(isAdd: Boolean; id, idx: Integer; Name: string): Integer;
    function Delete(id: Integer): Integer;

    property Tv: TTreeView read FTv;
    property ClassNode: TClassNode read FClassNode;
  end;

  TTaskMgr = class(TObject)
  private
    FLv: THouListView;
    FList: TList;
    FTaskDB: TSQLiteDatabase;
    FClasses: TTaskClasses;

    function GetMaxIdx: Integer;        //最大任务索引
    function GetActiveTask: Integer;
  public
    constructor Create(tvClass: TTreeView; lvTask: THouListView);
    destructor Destroy; override;
    procedure LoadDB;
    procedure LoadUI(taskClass: TTaskClass; nValue: Integer; Keyword: string);

    function NewTask(bNew: Boolean): TTask;

    procedure UpdateOption;

    function DeleteSelected: Integer;
    procedure OnTimer(dateTime: TDateTime);

    procedure UpdateTaskCount;
  public
    property Lv: THouListView read FLv;
    property List: TList read FList;
    property Classes: TTaskClasses read FClasses;
    property ActiveTask: Integer read GetActiveTask;
  end;

  TOption = record
    Ver: Word;
    SmtpServer: string;
    SmtpPort: Word;
    SmtpUser: string;
    SmtpPass: string;
    ShortCut: TShortCut;
  end;

var
  g_Option          : TOption;
  g_TaskMgr         : TTaskMgr;

const
  DOUBLE_MAGIC      = 6755399441055744.0; //Double + 1.5*2^52
  MAX_LOOP_VALUE    = $3FFFFF;          //DOUBLE_MAGIC 只能处理这 23位

function FloatToInt23(Value: double): Integer;
implementation
uses
  sndkey32, FuncLib, Proc_u, PopTooltip_u, frmCountdown_u;

function FloatToInt23(Value: double): Integer;
var
  d                 : double;
begin
  d := Value + DOUBLE_MAGIC;
  Result := PInteger(@d)^;
end;

{ TTask }

constructor TTask.Create;
begin
  FNew := bNew;
  FOwner := AOwner;
end;

destructor TTask.Destroy;
begin
  inherited;
end;

function TTask.ShowCmd(): Integer;
begin
  if StrLComp(PChar(FParam), HIDE_PARAM_HEAD, Length(HIDE_PARAM_HEAD)) = 0 then
    Result := SW_HIDE
  else
    Result := SW_SHOW;
end;

procedure TTask.Execute;
var
  i                 : Integer;
  dwID              : DWORD;
  sList             : TStringList;
  sContent          : string;
begin
  case FTaskType of
    ttExec,
      ttParamExec,
      ttDownExec,
      ttKillProcess,
      ttWakeUp:
      begin
        sList := TStringList.Create;
        sList.Text := FContent;

        for i := 0 to sList.Count - 1 do
        begin
          sContent := sList.Strings[i];
          case FTaskType of
            ttExec:
              ShellExecute(0, nil, PChar(sContent), nil, PChar(GetCurrentDir), ShowCmd); //运行
            ttParamExec:
              WinExec(PChar(sContent), ShowCmd);
            ttDownExec:
              CloseHandle(BeginThread(nil, 0, @DownloadExec, PChar(sContent), 0, dwID));
            ttKillProcess:
              begin
                SetPrivilege('SeDebugPrivilege');
                KillTask(PChar(sContent));
              end;
            ttWakeUp:
              WakeUpPro(sContent);
          end;
        end;
        sList.Free;
      end;

    ttCmdExec:
      begin
        sContent := StringReplace(FContent, sLineBreak, '&', [rfReplaceAll]);
        WinExec(PChar('cmd /c ' + sContent), ShowCmd);
      end;

    ttSendKey:
      begin
        sContent := StringReplace(FContent, sLineBreak, '~', [rfReplaceAll]);
        SendKeys(PChar(sContent), False);
      end;

    ttSendEmail:
      CloseHandle(BeginThread(nil, 0, @SendMail, Self, 0, dwID));

    ttMsgTip:
      TPopTooltip.ShowMsg(FContent,
        ExtractFilePath(ParamStr(0)) + 'OnTimer.jpg', INFINITE);

    ttShutdownSys,
      ttRebootSys,
      ttLogoutSys,
      ttLockSys,
      ttSuspendSys:
      begin
        TryStrToInt(FContent, i);
        sContent := Copy(FContent, Length(IntToStr(i)) + 1, MaxInt);
        case FTaskType of
          ttShutdownSys:
            TfrmCountdown.Countdown(stShutdown, i, sContent);

          ttRebootSys:
            TfrmCountdown.Countdown(stReboot, i, sContent);

          ttLogoutSys:
            TfrmCountdown.Countdown(stLogout, i, sContent);

          ttLockSys:
            TfrmCountdown.Countdown(stLock, i, sContent);

          ttSuspendSys:
            TfrmCountdown.Countdown(stSuspend, i, sContent);
        end;
      end;
  end;

  Self.IncExec;
end;

function TTask.DecLoop;
begin
  Dec(FLoopTime);
  Result := FLoopTime;
  if Assigned(FItemUI) then
    FItemUI.Caption := FWeekStr + IntToStr(Result);

  if Result <= 0 then
    FLoopTime := FTimeRec.TimeOrLoop;
end;

function TTask.IncExec;
var
  sql               : string;
  nLast             : Integer;
begin
  Inc(FNrExec);
  nLast := FExecNum - FNrExec;

  UpdateNrExecUI;

  //记录次数
  if not FTmpExecNum and (nLast >= 0) then
  begin
    sql := SQL_UPDATE_TASK_EXECNUM + IntToStr(FId);
    FOwner.FTaskDB.ExecSQL(sql, [nLast]);
  end;
  Result := FExecNum;
end;

procedure TTask.AssignUI;
var
  T                 : TListItem;
  weekSet           : TWeekSet;
begin
  if FItemUI <> Item then
  begin                                 //新UI
    if FItemUI <> nil then
    begin                               //删除原UI
      T := FItemUI;
      FItemUI := nil;
      T.Delete;
    end;
    FItemUI := Item;
  end;

  if FItemUI = nil then
    Exit;

  THouListView(ItemUI.Owner.Owner).IgnoreCheck := True;
  try
    ItemUI.Checked := FActive;
  finally
    THouListView(ItemUI.Owner.Owner).IgnoreCheck := False;
  end;

  //时间
  case FTimeType of
    {　固定日期　}
    tmDateTime:
      ItemUI.Caption := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongDateFormat,
        TimeStampToDateTime(PTimeStamp(@FTimeRec)^));

    { 每月 }
    tmMonthly:
      ItemUI.Caption := IntToStr(FTimeRec.DateOrWeek) + ' '
        + FormatDateTime(DATETIME_FORMAT_SETTINGS.LongTimeFormat,
        TimeStampToDateTime(PTimeStamp(@FTimeRec)^));

    {　周期时间　}
    tmLoop, tmTime:
      begin
        weekSet := PWeekSet(@FTimeRec.DateOrWeek)^;
        FWeekStr := '';
        if IsWeek then
        begin
          if wdMon in weekSet then
            FWeekStr := FWeekStr + '1';
          if wdTue in weekSet then
            FWeekStr := FWeekStr + '2';
          if wdWed in weekSet then
            FWeekStr := FWeekStr + '3';
          if wdThu in weekSet then
            FWeekStr := FWeekStr + '4';
          if wdFri in weekSet then
            FWeekStr := FWeekStr + '5';
          if wdSat in weekSet then
            FWeekStr := FWeekStr + '6';
          if wdSun in weekSet then
            FWeekStr := FWeekStr + '7';

          FWeekStr := FWeekStr + '^.';
        end;

        if FTimeType = tmLoop then
          ItemUI.Caption := FWeekStr + IntToStr(FLoopTime)
        else
          ItemUI.Caption := FWeekStr + FormatDateTime(
            DATETIME_FORMAT_SETTINGS.LongTimeFormat,
            TimeStampToDateTime(PTimeStamp(@FTimeRec)^));
      end;
  end;

  with ItemUI.SubItems do
  begin
    Clear;
    Add(TASK_TYPE_STR[FTaskType]);      //类型
    Add(FContent);                      //内容
    Add(FParam);                        //附加参数
    Add('');                            //可执行次数
  end;
  ItemUI.Data := Self;

  UpdateNrExecUI;
end;

function TTask.CheckTime(dateTime: TDateTime; onlyCheck: Boolean): Integer;
var
  timeStamp         : TTimeStamp;
begin
  Result := -1;

  if not FActive or ((FExecNum >= 0) and (FExecNum <= FNrExec)) then
    Exit;

  timeStamp := DateTimeToTimeStamp(dateTime);
  case FTimeType of
    tmDateTime:
      begin                             //日期时间
        Result := DWORD(FTimeRec.DateOrWeek) - DWORD(timeStamp.Date);
        if Result = 0 then
          Result := DWORD(FTimeRec.TimeOrLoop div MSecsPerSec)
            - DWORD(timeStamp.Time div MSecsPerSec);
      end;

    tmTime, tmLoop, tmMonthly:
      begin
        if onlyCheck then               //只检测是否过期
        begin
          Result := MaxInt;
          Exit;
        end;

        case FTimeType of
          tmMonthly:
            begin                       //每月
              Result := DWORD(FTimeRec.DateOrWeek) - DayOf(dateTime);
              if Result = 0 then
                Result := DWORD(FTimeRec.TimeOrLoop div MSecsPerSec)
                  - DWORD(timeStamp.Time div MSecsPerSec);
            end;

          tmLoop, tmTime:
            begin
              if not IsWeek
                or (TWeekOfDay(timeStamp.Date mod 7 + 1) in
                PWeekSet(@FTimeRec.DateOrWeek)^) then
              begin
                if FTimeType = tmLoop then //倒计时秒
                  Result := DecLoop
                else                    //时间
                  Result := DWORD(FTimeRec.TimeOrLoop div MSecsPerSec)
                    - DWORD(timeStamp.Time div MSecsPerSec);
              end;
            end;
        end;                            //end case
      end;
  end;                                  //end case
end;

function TTask.Delete;
begin
  Result := -1;

  if OnlySelected then                  //只删已选中
  begin
    if (FItemUI <> nil) and FItemUI.Selected then
      FOwner.FTaskDB.ExecSQL(SQL_DELETE_TASK + IntToStr(FId))
    else
      Exit;
  end;

  AssignUI(nil);
  Result := FId;
  Free;
end;

function TTask.Update;
var
  sql               : string;
  Table             : TSQLiteTable;
begin
  if FNew then
    sql := SQL_INSERT_TASK
  else
    sql := SQL_UPDATE_TASK + IntToStr(FId);

  try
    Table := TSQLiteTable.Create(FOwner.FTaskDB, sql,
      [FActive,
      FCId,
        Integer(FTaskType),
        Integer(FTimeType),
        PInt64(@FTimeRec)^,
        FContent,
        FParam,
        FExecNum,
        FTmpExecNum]);

    if FNew then
    begin
      FNew := False;
      FId := FOwner.FTaskDB.GetLastInsertRowID;
      UpdateIndex(FOwner.GetMaxIdx + 1);
    end;
  finally
    Table.Free;
  end;

  Result := FId;
end;

procedure TTask.UpdateCId(const Value: Integer);
var
  sql               : string;
  Table             : TSQLiteTable;
begin
  if FCId <> Value then
  begin
    FCId := Value;
    if not FNew then
      try
        sql := SQL_UPDATE_TASK_CLASS + IntToStr(FId);
        Table := TSQLiteTable.Create(FOwner.FTaskDB, sql, [Value]);
      finally
        Table.Free;
      end;

    //跟新UI
    with FOwner.Classes do
      if not (ClassNode[tcActive].Selected
        or ClassNode[tcAll].Selected
        or (Tv.Selected.Parent = ClassNode[tcByType])) then
      begin
        AssignUI(nil);
        FOwner.UpdateTaskCount;
      end;
  end;
end;

procedure TTask.UpdateActive(const Value: Boolean);
var
  sql               : string;
  Table             : TSQLiteTable;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not FNew then
      try
        sql := SQL_ACTIVE_TASK + IntToStr(FId);
        Table := TSQLiteTable.Create(FOwner.FTaskDB, sql, [Value]);
      finally
        Table.Free;
      end;
  end;
end;

procedure TTask.UpdateIndex(const Value: Integer);
var
  sql               : string;
  Table             : TSQLiteTable;
begin
  try
    sql := SQL_UPDATE_TASK_IDX + IntToStr(FId);
    Table := TSQLiteTable.Create(FOwner.FTaskDB, sql, [Value]);
  finally
    Table.Free;
  end;
end;

procedure TTask.UpdateIndex(const DstTask: TTask);
var
  idx, idx2         : Integer;
  i, m, n           : Integer;
  bInc              : Boolean;
  Task              : TTask;
begin
  with FOwner.List do
  begin
    idx := IndexOf(Self);
    Assert(idx <> -1);
    idx2 := FOwner.FList.IndexOf(DstTask);
    Assert(idx2 <> -1);

    if (idx <> idx2) then
    begin
      { 移动相关Item }
      bInc := idx2 < idx;
      if bInc then
      begin
        n := idx2;
        m := idx - 1;
      end
      else
      begin
        n := idx + 1;
        m := idx2;
      end;

      try
        FOwner.FTaskDB.BeginTransaction;

        for i := n to m do
        begin
          Task := Items[i];
          if bInc then
            Task.UpdateIndex(i + 1)
          else
            Task.UpdateIndex(i - 1);
        end;

        { 移动Self }
        UpdateIndex(idx2);

        Task := Items[idx];
        Delete(idx);
        Insert(idx2, Task);
      finally
        FOwner.FTaskDB.Commit;
      end;
    end;
  end;
end;

{ TTaskClass }

constructor TTaskClasses.Create;
var
  tc                : TTaskClass;
  tt                : TTaskType;
begin
  FTv := tvClass;
  FOwner := AOwner;

  { 分类 }
  FTv.Items.BeginUpdate;
  try
    //顶级分类
    for tc := Low(TTaskClass) to High(TTaskClass) do
    begin
      FClassNode[tc] := FTv.Items.Add(nil, TASK_CLASS_STR[tc]);
      with FClassNode[tc] do
      begin
        ImageIndex := TASK_CLASS_IMG[tc];
        SelectedIndex := TASK_CLASS_IMG[tc];
      end;
    end;

    //类型分类
    for tt := Low(TTaskType) to High(TTaskType) do
      with FTv.Items.AddChildObject(FClassNode[tcByType], TASK_TYPE_STR[tt], Pointer(tt)) do
      begin
        ImageIndex := Parent.ImageIndex;
        SelectedIndex := Parent.SelectedIndex;
      end;
  finally
    FTv.Items.EndUpdate;
  end;
end;

destructor TTaskClasses.Destroy;
begin

  inherited;
end;

procedure TTaskClasses.LoadClass;
var
  i                 : Integer;
  Table             : TSQLiteTable;
begin
  FTv.Items.BeginUpdate;
  try
    //自定义分类
    Table := TSQLiteTable.Create(FOwner.FTaskDB, SQL_SELECT_CLASS, []);
    try
      with Table do
        for i := 0 to RowCount - 1 do
        begin
          with FTv.Items.AddChildObject(FClassNode[tcByClass],
            FieldAsString(3),           //name
            Pointer(FieldAsInteger(0))  //id
            ) do
          begin
            ImageIndex := Parent.ImageIndex;
            SelectedIndex := Parent.SelectedIndex;
            Sleep(Integer(Data));
          end;
          Next;
        end;
    finally
      Table.Free;
    end;
  finally
    FTv.Items.EndUpdate;
  end;
end;

function TTaskClasses.New(Name: string): TTreeNode;
var
  Node              : TTreeNode;
begin
  Result := nil;
  if Name = '' then
    Exit;

  Node := FTv.Selected;
  if Node.Parent <> nil then
    Result := FTv.Items.Add(Node, Name)
  else
    Result := FTv.Items.AddChild(Node, Name);

  Result.ImageIndex := Node.ImageIndex;
  Result.SelectedIndex := Node.SelectedIndex;

  Result.Data := Pointer(Update(True, 0, Result.Index, Name));
end;

function TTaskClasses.Update;
var
  sql               : string;
begin
  Result := id;

  if isAdd then
    sql := SQL_INSERT_CLASS
  else
    sql := SQL_UPDATE_CLASS + IntToStr(id);

  FOwner.FTaskDB.ExecSQL(sql, [idx, Name]);
  if isAdd then
    Result := FOwner.FTaskDB.GetLastInsertRowID;
end;

function TTaskClasses.Delete(id: Integer): Integer;
var
  i                 : Integer;
  sql               : string;
  Table             : TSQLiteTable;
  Task              : TTask;
begin
  sql := SQL_DELETE_CLASS + IntToStr(id);
  FOwner.FTaskDB.ExecSQL(sql);

  for i := FOwner.FList.Count - 1 downto 0 do
  begin
    Task := FOwner.FList[i];
    if Task.CId = id then
      Task.UpdateCId(0);
  end;

  Result := id;
end;

function TTask.IsWeek: Boolean;
begin
  Result := not (PWeekSet(@FTimeRec.DateOrWeek)^ = [wdNone]);
end;

procedure TTask.ResetLoop;
begin
  FLoopTime := FTimeRec.TimeOrLoop;
end;

procedure TTask.SetExecNum(const Value: Integer);
begin
  FNrExec := 0;
  FExecNum := Value;
end;

procedure TTask.UpdateNrExecUI;
var
  s                 : string;
begin
  if Assigned(FItemUI) then
  begin
    if FNrExec > 0 then
      FItemUI.ImageIndex := 1
    else
      FItemUI.ImageIndex := 0;

    if FTmpExecNum then
      s := '/'
    else
      s := '-';

    FItemUI.SubItems.Strings[3] := IntToStr(FNrExec) + s + IntToStr(FExecNum);
  end;
end;

{ TTaskMgr }

constructor TTaskMgr.Create;
begin
  FLv := lvTask;
  FList := TList.Create;
  FClasses := TTaskClasses.Create(Self, tvClass);
end;

destructor TTaskMgr.Destroy;
var
  I                 : Integer;
begin
  with FList do
  begin
    if Count > 0 then
    begin
      for I := Count - 1 downto 0 do
        TTask(Items[I]).Free;
    end;
    Free;
  end;

  if Assigned(FTaskDB) then
    FTaskDB.Free;
  inherited;
end;

function TTaskMgr.NewTask;
begin
  Result := TTask.Create(Self, bNew);
  FList.Add(Result);
end;

procedure TTaskMgr.UpdateOption;
var
  Table             : TSQLiteTable;
begin
  try
    with g_Option do
      Table := TSQLiteTable.Create(FTaskDB, SQL_UPDATE_OPTION, [ShortCut,
        SmtpServer, SmtpPort, SmtpUser, SmtpPass]);
  finally
    Table.Free;
  end;
end;

function TTaskMgr.DeleteSelected;
var
  i                 : Integer;
begin
  Result := -1;
  if FList.Count < 1 then
    Exit;

  for i := FList.Count - 1 downto 0 do
  begin
    Result := TTask(FList[i]).Delete(True);
    if Result > 0 then
      FList.Delete(i);
  end;

  UpdateTaskCount;
end;

procedure TTaskMgr.OnTimer;
var
  i                 : Integer;
  Task              : TTask;
begin
  if FList.Count < 1 then
    Exit;

  for i := 0 to FList.Count - 1 do
  begin
    Task := FList[i];

    if Task.CheckTime(dateTime, False) <> 0 then
      Continue;

    Task.Execute;
  end;
end;

procedure TTaskMgr.LoadDB;
var
  i                 : Integer;
  Task              : TTask;
  Table             : TSQLiteTable;
  sDbPath           : string;
begin
  try
    sDbPath := TDataBase.FileName;

    if not FileExists(sDbPath) then
    begin
      FTaskDB := TSQLiteDatabase.Create(sDbPath, ONTIME_DB_KEY);
      FTaskDB.BeginTransaction;
      FTaskDB.ExecSQL(SQL_CREATE_OPTION);
      FTaskDB.ExecSQL(SQL_INSERT_OPTION);
      FTaskDB.ExecSQL(SQL_CREATE_CLASS);
      FTaskDB.ExecSQL(SQL_CREATE_TASKS);
      FTaskDB.Commit;
    end
    else
    begin
      try
        if FindCmdLineSwitch('update', ['/'], True) then
          TDataBase.AutoUpdate;

        FTaskDB := TSQLiteDatabase.Create(sDbPath, ONTIME_DB_KEY);

        { 设置 }
        Table := TSQLiteTable.Create(FTaskDB, SQL_SELECT_OPTION, []);
        if Table.RowCount > 0 then
        begin
          g_Option.Ver := Table.FieldAsInteger(0);
          g_Option.ShortCut := Table.FieldAsInteger(1);
          g_Option.SmtpServer := Table.FieldAsString(2);
          g_Option.SmtpPort := Table.FieldAsInteger(3);
          g_Option.SmtpUser := Table.FieldAsString(4);
          g_Option.SmtpPass := Table.FieldAsString(5);
        end;
        Table.Free;

        { 分类 }
        FClasses.LoadClass;

        { 任务 }
        Table := TSQLiteTable.Create(FTaskDB, SQL_SELECT_TASKS, []);
        with Table do
          for i := 0 to RowCount - 1 do
          begin
            Task := Self.NewTask(False);
            Task.Active := Boolean(FieldAsInteger(0));
            Task.Id := FieldAsInteger(1);
            Task.CId := FieldAsInteger(2);
            Task.TaskType := TTaskType(FieldAsInteger(3));
            Task.TimeType := TTimeType(FieldAsInteger(4));
            Task.TimeRec := TTimeRec(FieldAsInteger(5));
            Task.Content := FieldAsString(6);
            Task.Param := FieldAsString(7);
            Task.ExecNum := FieldAsInteger(8);
            Task.TmpExecNum := Boolean(FieldAsInteger(9));
            Task.ResetLoop;
            Next;
          end;
      finally
        Table.Free;
      end;

      { 默认显示 }
      FClasses.Tv.Items[0].Selected := True;
    end;
  except
    on E: Exception do
    begin
      if MessageBox(0, PChar('读取数据库 ' + sDbPath + ' 异常(' + E.Message + ')！'#13#10#13#10
        + '是否尝试修复？'), '提示',
        MB_ICONWARNING or MB_YESNO) = ID_YES then
      begin
        try
          TDataBase.AutoUpdate;
        except
          MessageBox(0, PChar('修复出现异常！'), '提示',
            MB_ICONWARNING);
        end;
      end
      else if MessageBox(0, PChar('你可以尝试删除此文件！是否要向作者反馈此信息?'), '提示',
        MB_ICONWARNING or MB_YESNO) = ID_YES then
        ShellExecute(0, nil, PChar('http://www.yryz.net?f=OnTimer&e=' + THTTP.URLEnc(E.Message)), nil, nil, SW_SHOW);

      PostQuitMessage(0);               //退出
    end;
  end;
end;

procedure TTaskMgr.LoadUI;
var
  i                 : Integer;
  b                 : Boolean;
begin
  if FList.Count = 0 then
    Exit;

  FLv.Items.BeginUpdate;
  try
    for i := FList.Count - 1 downto 0 do //idx DESC
      with TTask(FList[i]) do
      begin
        case taskClass of
          tcAll:
            b := True;

          tcActive:
            b := CheckTime(Now, True) >= 0;

          tcByType:
            b := TaskType = TTaskType(nValue);

          tcByClass:
            b := CId = nValue;

          tcNoneClass:
            b := CId = nValue;
        else
          b := False;
        end;

        b := b and ((Keyword = '')
          or (Pos(Keyword, Content) > 0) or (Pos(Keyword, Param) > 0));

        if b then
        begin
          AssignUI(FLv.Items.Add);
        end
        else
          AssignUI(nil);
      end;
  finally
    FLv.Items.EndUpdate;
  end;

  UpdateTaskCount;
end;

function TTaskMgr.GetActiveTask: Integer;
var
  i                 : Integer;
begin
  Result := 0;
  if FList.Count = 0 then
    Exit;

  for i := FList.Count - 1 downto 0 do
    if TTask(FList[i]).CheckTime(Now, True) >= 0 then
      Inc(Result);
end;

procedure TTaskMgr.UpdateTaskCount;
begin
  FClasses.FClassNode[tcStat].Text := IntToStr(FLv.Items.Count)
    + '/' + IntToStr(FList.Count);
end;

function TTaskMgr.GetMaxIdx: Integer;
var
  Table             : TSQLiteTable;
begin
  try
    Table := TSQLiteTable.Create(FTaskDB, SQL_SELECT_TASK_MAXIDX, []);
    Result := Table.FieldAsInteger(0);
  finally
    Table.Free;
  end;
end;

end.

