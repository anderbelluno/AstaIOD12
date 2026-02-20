{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10287: AstaIOReg.pas 
{
{   Rev 1.0    4/10/2003 6:32:00 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:06 PM  Steve    Version: 1.505
}
unit AstaIOReg;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses
  Classes,
  DB,
  SysUtils,
  Dialogs,
  AstaIOCompEdt,
  AstaIOWinBase,AstaIOServiceUtils,
  AstaIOServerDatasetUtils,
  AstaIOPropEdt,
  AstaIOClientRemoteDataSet,
  AstaIOClientWire,
  AstaIOCustomDataSet,
  AstaIODataBasePlugin,
  AstaIODataSetTransport,
  AstaIOProvider,
  AstaIOMetaData,
  {$IFNDEF NOGUI} // sm - 5/10/2003
  AstaIOMetadataListView,
  AstaIOMetadataTreeView,
  AstaIOMetadataTablesView,
  AstaIOStatusBar,
  {$ENDIF}
  AstaIOExecServerMethod,
  AstaIOServerMethod,
  AstaIOSQLParser,
  AstaIOIBInfo,
  AstaIODBInfo,
  AstaIOIProvider,
  AstaIOServerWire,
  AstaIOClientIProvider,
  AstaIOSQLGenerator,
  AstaIONativeClientWire,
  AstaIONativeClientMsg,
  AstaIOSocketServer,
  AstaIOUpdateSQL,
  AstaIOLowCore,
  AstaIOWebClientWire,
  AstaIOPdaServerPlugin,
  AstaIOSQLDataSet,
  AstaIOCloneDataSet,
  AstaIODataSetProvider,
  AstaIOClientDataSet,
  AstaIOAutoUpgrade,
  {$ifdef AstaIOPdaCompile}
  AstaIOPdaDataListProducer,
  AstaIOPdaUtils,
  AstaIOPdaBase,
  {$endif}
  AstaIOHttpDownload,
  AstaIOUIUtils
  ,AstaIODatagramServer,
  AstaIODatagramClient
  //,AstaIOClientWireConnector
  ,AstaIOStringServerWire,
   AstaIOStringClientWire
  ;

procedure Register;

implementation

procedure Register;
begin
//  Registercomponents('AstaIO Abstract',[TAstaIOServerWire,TAstaIOClientWire]);

  RegisterComponents('AstaIO Server',
    [TAstaIOIBInfo,
     TAstaIODBInfo,
     TAstaIOSQLParser,
     TAstaIOSQLGenerator,
     TAstaIOAutoUpgrade]);

   RegisterComponents('AstaIO Server', [TAstaIOPdaServerWirePlugin,TAstaIODatagramServerWire]);
   RegisterComponents('AstaIO Server', [TAstaIOSocketServerWire,TAstaIOIProvider,
   TAstaIOProvider,TAstaIODatabasePlugin,TAstaIOMetaData,
   TAstaIOServerMethodResultSet,
   TAstaIOServerMethodExec]);
  RegisterComponents('AstaIO Client', [TAstaIOStringClientWire]);
  RegisterComponents('AstaIO Server', [TAstaIOStringServerWire]);
  RegisterComponents('AstaIO Client', [TAstaIOClientLeanWire,TAstaIOWebClientWire,TAstaIONativeClientWire,TAstaIODatagramClientWire]);

  RegisterComponents('AstaIO Client',
    [TAstaIODataSet,
      TAstaIOUpdateSQL,
      TAstaIOAuditDataSet,
      TAstaIOClientIProvider,
      TAstaIOClientQuery,
      TAstaIOClientTable,
      TAstaIOClientStoredProc,
      TAstaIOMetaDataDataSet,
      TAstaIOProviderDataSet,
      TAstaIOExecServerMethod,
      TAstaIOSQLDataSet,
      TAstaIOCloneDataSet,
      TAstaIODataSetProvider,
      TAstaIOClientDataSet,
      TAstaIOServerMethodDataSet]);

  {$IFNDEF NOGUI} // sm - 5/10/2003
  RegisterComponents('AstaIO Client',
    [TAstaIOMetadataListView,
    TAstaIOMetadataTreeView,
    TAstaIOMetadataTablesView,
    TAstaIOStatusBar
    ]);
  {$ENDIF}

  AstaIOPropEdt.Register;
  AstaIOCompEdt.Register;
end;

end.



