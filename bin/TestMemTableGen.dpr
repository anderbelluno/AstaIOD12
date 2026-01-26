program TestMemTableGen;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  DB,
  AstaIOCustomDataSet; // Using the unit we modified

var
  DataSet: TAstaIODataSet; // Or TAstaIOCustomDataSet if TAstaIODataSet is not available directly (it is in AstaIOCustomDataSet.pas usually? No, let's check)
  I: Integer;
  V_Data: Double;
begin
  try
    Writeln('Creating DataSet...');
    DataSet := TAstaIODataSet.Create(nil);
    try
      // Define Fields
      with DataSet.FieldDefs.AddFieldDef do begin
        Name := 'FieldName';
        DataType := ftString;
        Size := 40;
      end;
      with DataSet.FieldDefs.AddFieldDef do begin
        Name := 'FieldValue';
        DataType := ftInteger;
      end;
      with DataSet.FieldDefs.AddFieldDef do begin
        Name := 'FieldData';
        DataType := ftDateTime;
      end;
      with DataSet.FieldDefs.AddFieldDef do begin
        Name := 'FieldData2';
        DataType := ftDateTime;
      end;

      Writeln('Creating DataSet (Active=True)...');
      DataSet.CreateDataSet;
      DataSet.Active := True;

      Writeln('Populating DataSet...');
      V_Data := Now - 30;
      for I := 0 to 9 do
      begin
        DataSet.Append;
        DataSet.FieldByName('FieldName').AsString := 'F' + IntToStr(I);
        DataSet.FieldByName('FieldValue').AsInteger := I;
        DataSet.FieldByName('FieldData').AsDateTime := V_Data;
        DataSet.FieldByName('FieldData2').AsDateTime := V_Data + 1;
        DataSet.Post;
        V_Data := V_Data + (0.00001157407407 * 60);
      end;
      
      Writeln('Record Count: ', DataSet.RecordCount);

      Writeln('Saving to file Test.dat...');
      DataSet.SaveToFile('Test.dat');
      
      Writeln('Clearing DataSet...');
      DataSet.Close;
      DataSet.Active := False;
      DataSet.CreateDataSet; // Re-create empty
      
      Writeln('Loading from file Test.dat...');
      DataSet.LoadFromFile('Test.dat');
      
      Writeln('Loaded Record Count: ', DataSet.RecordCount);
      
      DataSet.First;
      while not DataSet.Eof do
      begin
        Writeln('Record: ', 
          DataSet.FieldByName('FieldName').AsString, ' | ',
          DataSet.FieldByName('FieldValue').AsInteger, ' | ',
          DateTimeToStr(DataSet.FieldByName('FieldData').AsDateTime)
        );
        DataSet.Next;
      end;

    finally
      DataSet.Free;
    end;
    Writeln('Done.');
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
