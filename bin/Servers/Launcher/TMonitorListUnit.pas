unit TMonitorListUnit;

interface

uses
   Windows, SysUtils, Classes, IniFiles, TMonitorThreadUnit;

type

   PProcessNode = ^ProcessNode;
   ProcessNode = Record
      NOrder   : Integer;
      Section  : String;
      Process  : TMonitorThread;
      FullPath : String;
      Arguments: String;
   end;

    function CreateList(ProcessIniFileName: String): TList;
    function DestroyList(var ProcessList: TList): Boolean;
    procedure MonitorList(var ProcessList: TList);

var
    IniFileName: String;

implementation

//------------------------------------------------------------------------------
function CreateList(ProcessIniFileName: String): TList;
begin
   IniFileName := ProcessIniFileName;
   Result := TList.Create;
end;
//------------------------------------------------------------------------------
function DestroyList(var ProcessList: TList): Boolean;
var
   Index: Integer;
begin
   Result := false;
   for Index:=ProcessList.Count-1 downto 0 do begin
      if (ProcessList.Items[Index] <> nil) then begin
         if (PProcessNode(ProcessList.Items[Index]).Process <> nil) then begin
            PProcessNode(ProcessList.Items[Index]).Process.TerminateProcess;
            Sleep(1);
            PProcessNode(ProcessList.Items[Index]).Process.Free;
            Result := true;
         end else begin
            Result := false;
         end;
         Dispose(ProcessList.Items[Index]);
      end;
   end;
   ProcessList.Free;
end;

//------------------------------------------------------------------------------
procedure MonitorList(var ProcessList: TList);
var
   IniFile: TMemIniFile;
   i, j: Integer;
   Sections: TStringList;
   ExeName, Parameters: String;
   NewPNode: PProcessNode;
   ExistInList: Boolean;
begin
   IniFile := TMemIniFile.Create(IniFileName);
   try

      Sections := TStringList.Create;
      try
         // Leo las secciones.
         IniFile.ReadSections(Sections);

         // Borro los items de la lista que no están en el ini.
         for i:=ProcessList.Count-1 downto 0 do begin
            if (Sections.IndexOf(PProcessNode(ProcessList.Items[i]).Section) = -1) then begin
               // Debo borralo!
               PProcessNode(ProcessList.Items[i]).Process.TerminateProcess;
               sleep(1);
               PProcessNode(ProcessList.Items[i]).Process.Free;
               Dispose(ProcessList.Items[i]);
               ProcessList.Delete(i);
            end;
         end;

         // Recorro Items del ini, y modifico o agrego en la lista.
         for i:=0 to Sections.Count-1 do begin

            ExeName := IniFile.ReadString(Sections.Strings[i], 'ExeName', '');
            Parameters := IniFile.ReadString(Sections.Strings[i], 'Parameters', '');

            if (not FileExists(ExeName)) then begin
               // El ExeName es invalido sigo con el siguiente.
               Continue;
            end;

            ExistInList := false;
            // Lo busco en la lista.
            for j:=0 to ProcessList.Count-1 do begin
               if (Sections.Strings[i] = PProcessNode(ProcessList.Items[j]).Section) then begin
                  if ((ExeName <> PProcessNode(ProcessList.Items[j]).FullPath)
                     or (Parameters <> PProcessNode(ProcessList.Items[j]).Arguments)) then begin
                     // Cambió!
                     PProcessNode(ProcessList.Items[i]).FullPath := ExeName;
                     PProcessNode(ProcessList.Items[i]).Arguments := Parameters;
                     PProcessNode(ProcessList.Items[i]).Process.TerminateProcess;
                     Sleep(1);
                     PProcessNode(ProcessList.Items[i]).Process.StartProcess(ExeName, Parameters);
                     Sleep(1);
                  end;
                  ExistInList := true;
                  Break;
               end;
            end;

            if (not ExistInList) then begin
               // Son nuevos los agrego.
               New(NewPNode);
               NewPNode.NOrder := i;
               NewPNode.Section := Sections.Strings[i];
               NewPNode.FullPath := ExeName;
               NewPNode.Arguments := Parameters;
               NewPNode.Process := TMonitorThread.Create(false);
               sleep(1);
               if (NewPNode.Process <> nil) then begin
                  ProcessList.Add(NewPNode);
                  NewPNode.Process.StartProcess(ExeName,Parameters);
               end;
            end;
         end;
      finally
         Sections.Free;
      end;
   finally
      IniFile.Free;
   end;
end;
//------------------------------------------------------------------------------
end.
