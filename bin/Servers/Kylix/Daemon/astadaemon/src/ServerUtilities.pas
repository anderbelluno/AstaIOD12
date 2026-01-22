{***************************************************************}
{                                                               }
{
      Utilities used by the server.


{  Copyright (c) 2002 Caelestium software Inc.                        }
{                                                               }
{     All rights reserved.                                      }
{                                                               }
{***************************************************************}
unit ServerUtilities;

interface

  function GetIniFileName: string;
  function GetLogFileName: string;

implementation

uses
  SysUtils;

const
  DefaultConfigFileName = 'astaioserver.conf';
  DefaultLogFileName = 'astaioserver.log';

function GetIniFileName: string;
begin
  Result := GetEnvironmentVariable('SERVER_CFG_FILE');

  if (Result = '') and (GetEnvironmentVariable('HOME') <> '') then
    Result := GetEnvironmentVariable('HOME') + PathDelim + DefaultConfigFileName;

  if Result = '' then
    Result := GetCurrentDir + PathDelim + DefaultConfigFileName;
end;

function GetLogFileName: string;
begin
  Result := GetEnvironmentVariable('SERVER_LOG_FILE');

  if (Result = '') and (GetEnvironmentVariable('HOME') <> '') then
    Result := GetEnvironmentVariable('HOME') + PathDelim + DefaultLogFileName;

  if Result = '' then
    Result := GetCurrentDir + PathDelim + DefaultLogFileName;
end;

end.
