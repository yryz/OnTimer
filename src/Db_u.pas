unit DB_u;

interface
uses
  Windows, SysUtils, Classes, SQLite3, SQLiteTable3;

const
  ONTIME_DB_VER     = '1';              //int
  ONTIME_DB_KEY     = '';

  { OPTION SQL }
  SQL_CREATE_OPTION = 'CREATE TABLE option(ver INTEGER,'
    + 'shortcut INTEGER,'
    + 'smtpserver TEXT,'
    + 'smtpport INTEGER,'
    + 'smtpuser TEXT,'
    + 'smtppass TEXT)';

  SQL_SELECT_OPTION = 'SELECT * FROM option';

  SQL_INSERT_OPTION = 'INSERT INTO option(ver, smtpserver, smtpport, smtpuser,'
    + 'smtppass) VALUES(' + ONTIME_DB_VER + ', "smtp.126.com", 25, "ontimer", "")';

  SQL_UPDATE_OPTION = 'UPDATE option SET shortcut=?, smtpserver=?, smtpport=?,'
    + 'smtpuser=?, smtppass=?';

  SQL_UPDATE_VER    = 'UPDATE option SET ver=' + ONTIME_DB_VER;

  { CLASS SQL }
  SQL_CREATE_CLASS  = 'CREATE TABLE class('
    + 'id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,'
    + 'cid INTEGER,'
    + 'idx INTEGER,'
    + 'name TEXT)';

  SQL_SELECT_CLASS  = 'SELECT * FROM class ORDER BY idx ASC';

  SQL_INSERT_CLASS  = 'INSERT INTO class(idx, name) VALUES(?,?)';

  SQL_UPDATE_CLASS  = 'UPDATE class SET idx=?, name=? WHERE id=';

  SQL_DELETE_CLASS  = 'DELETE FROM class WHERE id=';

  { TASK SQL }
  SQL_CREATE_TASKS  = 'CREATE TABLE tasks(active INTEGER,'
    + 'id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,'
    + 'cid INTEGER,'
    + 'tasktype INTEGER,'
    + 'timetype INTEGER,'
    + 'time INTEGER,'
    + 'content TEXT,'
    + 'param TEXT,'
    + 'execnum INTEGER,'
    + 'tmpexecnum INTEGER,'
    + 'idx INTEGER)';                   //索引

  SQL_SELECT_TASKS  = 'SELECT * FROM tasks ORDER BY idx ASC';

  SQL_INSERT_TASK   = 'INSERT INTO tasks(active, cid, tasktype, timetype,'
    + 'time, content, param, execnum, tmpexecnum) VALUES(?,?,?,?,?,?,?,?,?)';

  SQL_UPDATE_TASK   = 'UPDATE tasks SET active=?, cid=?, tasktype=?,'
    + 'timetype=?, time=?, content=?, param=?, execnum=?, tmpexecnum=?'
    + ' WHERE id=';

  SQL_UPDATE_TASK_IDX = 'UPDATE tasks SET idx=? WHERE id=';
  SQL_UPDATE_TASK_CLASS = 'UPDATE tasks SET cid=? WHERE id=';
  SQL_UPDATE_TASK_EXECNUM = 'UPDATE tasks SET execnum=? WHERE id=';

  SQL_ACTIVE_TASK   = 'UPDATE tasks SET active=? WHERE id=';
  SQL_DELETE_TASK   = 'DELETE FROM tasks WHERE id=';

  SQL_SELECT_TASK_MAXIDX = 'SELECT MAX(idx) FROM tasks';

type
  TDataBase = class
    class function FileName: string;
    class procedure AutoUpdate;
    class procedure SQLite_Key_Err_Fix; //< 1.3a
  end;

implementation

{ TDataBase }

class procedure TDataBase.AutoUpdate();
var
  i                 : Integer;
  DB                : TSQLiteDatabase;
  Table, Table2     : TSQLiteTable;

  ver               : Integer;
begin
  ver := -2;
  SQLite_Key_Err_Fix;

  DB := TSQLiteDatabase.Create(FileName, ONTIME_DB_KEY);
  DB.BeginTransaction;
  try
    Table := TSQLiteTable.Create(DB, 'SELECT ver FROM option', []);
    ver := Table.FieldAsInteger(0);
  except
    ver := -1;                          //ver < 1.2e
  end;
  Table.Free;

  case ver of
    -1, 0:                              //v1.2d - v1.2h
      begin
        if ver = -1 then
        begin                           // v1.2d to v1.2e
          DB.ExecSQL('DROP TABLE option');
          DB.ExecSQL(SQL_CREATE_OPTION);
          DB.ExecSQL(SQL_INSERT_OPTION);
        end;

        { to v1.3a }
        DB.ExecSQL(SQL_CREATE_CLASS);
        DB.ExecSQL(SQL_CREATE_TASKS);

        //tasks
        Table := TSQLiteTable.Create(DB, 'SELECT checked,tasktype,timetype,time,'
          + 'content,param,execnum FROM tasklist', []);
        with Table do
          for i := 0 to RowCount - 1 do
          begin
            Table2 := TSQLiteTable.Create(DB, SQL_INSERT_TASK, [
              Boolean(FieldAsInteger(0)), //active
                0,                      //cid
                FieldAsInteger(1),      //tasktype
                FieldAsInteger(2),      //timetype
                FieldAsInteger(3),      //time
                FieldAsString(4),       //content
                FieldAsString(5),       //param,
                FieldAsInteger(6),      //execnum
                True                    //tmpexecnum
                ]);
            Table2.Free;
            Next;
          end;

        DB.ExecSQL('DROP TABLE tasklist');
        DB.ExecSQL(SQL_UPDATE_VER);

        MessageBox(0, '数据库升级成功!', '提示', 0);
      end;
  end;

  DB.Commit;
  DB.Free;
end;

class function TDataBase.FileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'OnTimer.db';
end;

class procedure TDataBase.SQLite_Key_Err_Fix;
const
  NIL_DB_KEY        = '';
var
  db                : Pointer;
  Msg               : PAnsiChar;
begin
  Msg := nil;
  if SQLite3_Open(PAnsiChar(UTF8Encode(FileName)), db) <> SQLITE_OK then
    if db <> nil then
    begin
      Msg := Sqlite3_ErrMsg(db);
      raise ESqliteException.CreateFmt('Failed to open database "%s" : %s',
        [FileName, Msg]);
    end
    else
      raise ESqliteException.CreateFmt('Failed to open database "%s" : unknown error',
        [FileName]);

  SQLite3_key(db, PAnsiChar(NIL_DB_KEY), SizeOf(PAnsiChar(NIL_DB_KEY))); //之前的BUG

  SQLite3_Rekey(db, PAnsiChar(ONTIME_DB_KEY), Length(ONTIME_DB_KEY)); //重设

  SQLite3_Close(db);

  if Assigned(Msg) then
    SQLite3_Free(Msg);
end;

end.

