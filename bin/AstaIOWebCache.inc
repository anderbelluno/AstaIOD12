{$define debug}
  {$ifdef Debug}
   {$D+,L+}
  {$endif}
{.$define Demo}
{$DEFINE nestedDataSet}
{$define SQLDataSetOnly}
{$ifndef SQLDataSetOnly}
  {.$DEFINE AstaIOXML}
  {$define AstaBladeRunner}
  {.$define USAndCanada}
{$endif}
{$Define AstaDES}
{$ifndef Linux}
 {$Define Windows}
 {$Define MsWindows}
{$endif}
{$ifdef USAndCanada}
 {$Define AstaAES}
 {$Define AstaRSA}
{$endif}
{.$DEFINE LINUX}
{.$DEFINE LINUX_THREAD}
{.$DEFINE FIELDANDSTREAM}
{.$DEFINE USEHASHLIST}
{.$Define NoClientParamLists}
{$define UseClientInterface}
{$IFDEF WINDOWS}
 {.$define AstaPluginIntf}
{$ENDIF}
{$ifdef mswindows}
 {.$define AstaIOSmartWait}
{$endif}
{$ifdef Linux}    
   {$DEFINE ExceptionsBlocked}
{$ENDIF}
{$define AstaLog}
{$define WaitForDataChange}

// astaioWinBase problem with exceptions
{.$define PoolThreads}
{$define NoThreads}
{.$define AstaBCD}

{$ifdef ver140}
 {$WARN UNIT_PLATFORM OFF}
 {$WARN SYMBOL_PLATFORM OFF}
{$endif}

// uncomment the following row to disable showing
// Login dialog in TAstaCustomMsgClientWire and descendants
// note: it's done to avoid server components to be dependent of QT under Linux
{.$define NoLoginDialog}

{$define AddSignature}
{$ifdef mswindows}
  {$define WindowsMessageEvents}
{$else}
  {$define LinuxMessageQueue}
{$endif}

{.$define PALMUPDATER_INCLUDED}
{$define ActiveStatusBar}

