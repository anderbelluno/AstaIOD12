// (c) T & R (WA), Donetsk-2001
unit  UUX;

interface

type
	tArr3 = array [0..2] of byte;
	tArr4 = array [0..3] of byte;

	procedure   Encode3 ( const src : PChar;
															cnt : integer;
												var   dst : tArr4);
  function    Decode4 ( const src : PChar;
                        var   dst : tArr3)         : integer;

implementation

var
  gTblDecode : array [0..127] of byte =
  (
   $3D, $40, $40, $40, $40, $40, $40, $40, $40, $40,
   $40, $40, $40, $40, $40, $40, $40, $40, $40, $40,
   $40, $40, $40, $40, $40, $40, $40, $40, $40, $40,
   $40, $40, $40, $40, $40, $40, $40, $40, $40, $40,
   $40, $40, $40, $3E, $40, $40, $40, $3F, $34, $35,
   $36, $37, $38, $39, $3A, $3B, $3C, $3D, $40, $40,
   $40, $40, $40, $40, $40, $00, $01, $02, $03, $04,
   $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
   $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18,
   $19, $40, $40, $40, $40, $40, $40, $1A, $1B, $1C,
   $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26,
	 $27, $28, $29, $2A, $2B, $2C, $2D, $2E, $2F, $30,
	 $31, $32, $33, $40, $40, $40, $40, $40
	);

	gTblEncode : string [128] =
		'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

	procedure
Encode3 ( const src : PChar; cnt : integer; var dst : tArr4);
		function
	Encode  ( v : integer)  : byte;
		begin
			Result := byte (gTblEncode [(v and $3F) + 1]);
		end;
	begin
		dst [0] := Encode (ord (src [0]) shr 2);
    dst [1] := Encode ((ord (src [0]) shl 4) or (ord (src [1]) shr 4));

    if ( cnt < 2 )
			then  dst [2] := byte ('=')
			else  dst [2] := Encode ((ord (src [1]) shl 2) or (ord (src [2]) shr 6));

		if ( cnt < 3 )
			then  dst [3] := byte ('=')
			else  dst [3] := Encode (ord (src [2]));
  end;

  function
Decode4 ( const src : PChar; var   dst : tArr3)         : integer;
  begin
    Result := 1;
    dst [0] := byte (   (gTblDecode [ord (src [0])] shl 2)
                     or (gTblDecode [ord (src [1])] shr 4));

    if ( src [2] <> '=' )
    then begin
      inc (Result);
      dst [1] := byte (   (gTblDecode [ord (src [1])] shl 4)
                       or (gTblDecode [ord (src [2])] shr 2));
    end;

    if ( src [3] <> '=' )
    then begin
      inc (Result);
			dst [2] := byte (   (gTblDecode [ord (src [2])] shl 6)
                       or (gTblDecode [ord (src [3])])      );
    end;
  end;

end.
