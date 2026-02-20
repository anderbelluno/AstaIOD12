{.$define debug}
  {$ifdef Debug}
  {$D+,L+}
  {$endif}
{.$define Demo}
{.$Define ServerMethodDatabase}
{$define WinSockSocketInit}
{$ifndef Linux}
  {$define ServerSegmentSends}
{$endif}  
{.$define WideStrChange}

{$define Delphi6AndUp}
{$define Delphi2009AndUp}
{$define Delphi12AndUp}
//calls CoInitialize in AstaIOLowCore
{$define LowCoreCOM}
{$define AstaIOPdaCompile}
{$DEFINE nestedDataSet}
{.$define SQLDataSetOnly}
{$ifndef SQLDataSetOnly}
  {$DEFINE AstaIOXML} //* enabled jn 10/13/2011
  {$ifndef Linux}  
   {.$define AstaBladeRunner}
  {$endif}
  {.$define USAndCanada}
  {$define RsaRequestAuthentication}
{$endif}
{$ifdef USAndCanada}
 {$Define AstaAES}
 {.$Define AstaRSA}
{$endif}
{$ifdef AstaAES} //* jn 08/22/2004
 // Just define one AstaAESKey at time
 {$Define AstaAESKey128}
 {.$Define AstaAESKey192}
 {.$Define AstaAESKey256}
 // Both Client and server have to be compiled
{$endif} //* jn 08/22/2004
{$ifndef AstaAES}
 {$Define AstaDES}
{$endif}
{$ifndef Linux}
 {$Define Windows}
 {$Define MsWindows}
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
 {$define AstaIOSmartWait}
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


// uncomment the following row to disable showing
// Login dialog in TAstaCustomMsgClientWire and descendants
// note: it's done to avoid server components to be dependent of QT under Linux
{.$define NoLoginDialog}
{.$DEFINE NOGUI}

{$define AddSignature}
{$ifdef mswindows}
  {$define WindowsMessageEvents}
{$else}
  {$define LinuxMessageQueue}
{$endif}

{.$define PALMUPDATER_INCLUDED}
{$define ActiveStatusBar}

