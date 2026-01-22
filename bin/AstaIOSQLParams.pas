{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10319: AstaIOSQLParams.pas 
{
{   Rev 1.0    4/10/2003 6:32:18 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:16 PM  Steve    Version: 1.505
}
unit AstaIOSQLParams;

{$I AstaIO.inc}

interface
uses classes, db;

type
  TAstaIOSQLParamQuery = class(TParams)
  private
    FSQL: TStrings;
    FRowsAffected:Integer;
  protected
    procedure QueryChanged(Sender: TObject);
    Procedure SetRowsAffected(Value:Integer);
  public
    property RowsAffected:Integer read FRowsAffected write SetRowsAffected;
    property SQL: TStrings read FSQL write FSQL;
    constructor Create;
    destructor Destroy; override;
  end;

  TAstaIOSQLQueryList = class(TList)
  private
    function Getquery(Index: Integer): TAstaIOSQLParamQuery;
    procedure SetQuery(Index: Integer; Value: TAstaIOSQLParamQuery);
  public
    function AsString:AnsiString;
    property QueryItems[Index: Integer]: TAstaIOSQLParamQuery read GetQuery write SetQuery; default;
    Constructor CreateFromString(Source:AnsiString);
    destructor Destroy; override;
    procedure AddQuery(Value: TAstaIOSQLParamQuery); overload;
    procedure AddQuery(SQL: string; const Values: array of Variant); overload;
    procedure AddQuery(SQL: string; const Values: array of const); overload;
    procedure AddQuery(SQL: string; const Params:TParams); overload;
  end;

implementation
uses SysUtils,AstaIOParamList;

Procedure TAstaIOSQLParamQuery.SetRowsAffected(Value:Integer);
Var
 AParam:TParam;
begin
 if Value>=0 then begin
   Clear;
   AParam := TParam.Create(Self, ptInput);
   AParam.Name := 'RowsAffected';
   AParam.AsInteger:=Value;
  end; 
end;

constructor TAstaIOSQLParamQuery.Create;
begin
  inherited create;
  FSQL := TStringList.Create;
  TStringList(FSQL).OnChange := QueryChanged;
  FRowsAffected:=-1;
end;

destructor TAstaIOSQLParamQuery.Destroy;
begin
  FSQL.Free;
  inherited Destroy;
end;

procedure TAstaIOSQLParamQuery.QueryChanged(Sender: TObject);
var
  i: Integer;
begin
  ParseSQL(FSQL.Text, True);
  for i := 0 to Count - 1 do
    items[i].ParamType := DB.ptInput;
end;

function TAstaIOSQLQueryList.Getquery(Index: Integer): TAstaIOSQLParamQuery;
begin
  result := TAstaIOSQLParamQuery(items[Index]);
end;

procedure TAstaIOSQLQueryList.SetQuery(Index: Integer; Value: TAstaIOSQLParamQuery);
begin
  items[Index] := Value;
end;

destructor TAstaIOSQLQueryList.Destroy;
var
  i: Integer;
begin
  for i := 0 to count-1 do
    TAstaIOSQLParamQuery(items[i]).Free;
  inherited;
end;

procedure TAstaIOSQLQueryList.AddQuery(Value: TAstaIOSQLParamQuery);
begin
  Add(Value);
end;

function TAstaIOSQLQueryList.AsString:AnsiString;
var
 p:TAstaParamList;
 i:Integer;
begin
 p:=TAstaParamList.Create;
 try
   for i:=0 to count-1 do
    p.AddSQLParam(QueryItems[i]);
  result:=P.AsTokenizedString(False);
  finally
  p.free;
 end;
end;

Constructor TAstaIOSQLQueryList.CreateFromString(Source:AnsiString);
var
 p,temp:TAstaParamList;
 i:Integer;
 q:TAstaIOSQLParamQuery;
begin
 inherited Create;
 p:=TAstaParamList.CreateFromTokenizedString(Source);
 try
   for i:=0 to P.count-1 do begin
    q:=TAstaIOSQLParamQuery.Create;
    q.SQL.Text:=P[i].Name;
    temp:=TAstaParamList.CreateFromTokenizedString(P[i].AsAnsiString);
    try
     temp.AssignParamValues(Q,[]);
     Add(q);
    finally
     temp.free;
    end;
   end;
  finally
  p.free;
 end;
end;

procedure TAstaIOSQLQueryList.AddQuery(SQL: string; const Values: array of Variant);
var
  q: TAstaIOSQLParamQuery;
  i: Integer;
begin
  Q := TAstaIOSQLParamQuery.Create;
  Q.SQL.Text := SQL;
 if q.count <> high(Values) then begin
    q.free;
    raise Exception.create(SQL + ' has a different Paramcount than then Params passed in');
  end;
  for i := 0 to Q.Count - 1 do
    if i <= high(Values) then
      Q[i].Value := Values[i];
  AddQuery(Q);
end;

procedure TAstaIOSQLQueryList.AddQuery(SQL: string; const Params:TParams);
var
q: TAstaIOSQLParamQuery;
begin
 Q:=TAstaIOSQLParamQuery.Create;
 q.sql.text:=SQL;
 if q.count <> Params.Count then begin
    q.free;
    raise Exception.create(SQL + ' has a different Paramcount than then Params passed in');
 end;
 q.assignvalues(params);
 AddQuery(Q);
end;

procedure TAstaIOSQLQueryList.AddQuery(SQL: string; const Values: array of const);
var
  i: Integer;
  q: TAstaIOSQLParamQuery;
begin
  q := TAstaIOSQLParamQuery.Create;
  q.sql.text := SQL;
  if q.count <> high(Values) then begin
    q.free;
    raise Exception.create(SQL + ' has a different Paramcount than then Params passed in');
  end;
  for i := low(Values) to High(Values) do
    with Values[I] do
      case VType of
        vtInteger: Q[i].AsInteger := VInteger;
        vtBoolean: q[i].AsBoolean := VBoolean;
        vtExtended: q[i].AsFloat := VExtended^;
        vtString: q[i].AsString := VString^;
        vtAnsiString: q[i].AsString := string(VAnsiString);
      end;

  AddQuery(Q);
end;



end.

