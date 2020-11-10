{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Floating point numbers <-> HexString conversion routines

  ©František Milt 2018-10-21

  Version 1.5.5

  Dependencies:
    AuxTypes - github.com/ncs-sniper/Lib.AuxTypes

===============================================================================}
unit FloatHex;

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IFDEF ENDIAN_BIG}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{$IF SizeOf(Extended) = 8}
  {$DEFINE Extended64}
{$ELSEIF SizeOf(Extended) = 10}
  {$UNDEF Extended64}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported platform, type extended must be 8 or 10 bytes.'}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes;

type
  // library-specific exceptions
  EFHException = class(Exception);

  EFHInvalidFlag = class(EFHException);

{===============================================================================
    Auxiliary routines
===============================================================================}

const
  // X87 control word exception masks
  X87CW_EMASK_InvalidOP = UInt16($0001);
  X87CW_EMASK_Denormal  = UInt16($0002);
  X87CW_EMASK_DivByZero = UInt16($0004);
  X87CW_EMASK_Overflow  = UInt16($0008);
  X87CW_EMASK_Underflow = UInt16($0010);
  X87CW_EMASK_Precision = UInt16($0020);

  X87CW_InfinityControl = UInt16($1000);

  X87CW_Precision = UInt16($0300);
  X87CW_Rounding  = UInt16($0C00);

Function EmulatedX87ControlWord: Boolean;{$IFDEF CanInline} inline; {$ENDIF}

Function GetX87ControlWord: UInt16; {$IFNDEF PurePascal}register; assembler;{$ENDIF}
procedure SetX87ControlWord(NewValue: UInt16); {$IFNDEF PurePascal}register; assembler;{$ENDIF}

type
  TX87PrecisionMode = (pcSingle,pcReserved,pcDouble,pcExtended);
  
  TX87RoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TX87Flag = (flMaskInvalidOp,flMaskDenormal,flMaskDivByZero,flMaskOverflow,
              flMaskUnderflow,flMaskPrecision,flInfinityControl);

  TX87Flags = set of TX87Flag;

Function GetX87PrecisionMode: TX87PrecisionMode;
Function SetX87PrecisionMode(NewValue: TX87PrecisionMode): TX87PrecisionMode;

Function GetX87RoundingMode: TX87RoundingMode;
Function SetX87RoundingMode(NewValue: TX87RoundingMode): TX87RoundingMode;

Function GetX87Flag(Flag: TX87Flag): Boolean;
Function SetX87Flag(Flag: TX87Flag; NewValue: Boolean): Boolean;

Function GetX87Flags: TX87Flags;
procedure SetX87Flags(NewValue: TX87Flags);

//------------------------------------------------------------------------------

procedure ConvertFloat64ToFloat80(DoublePtr,ExtendedPtr: Pointer); {$IFNDEF PurePascal}register; assembler;{$ENDIF}
procedure ConvertFloat80ToFloat64(ExtendedPtr,DoublePtr: Pointer); {$IFNDEF PurePascal}register; assembler;{$ENDIF}

{===============================================================================
    Conversion routines
===============================================================================}
{-------------------------------------------------------------------------------
    Type Float16
-------------------------------------------------------------------------------}

Function Float16ToHex(Value: Float16): String;
Function HexToFloat16(const HexString: String): Float16;
Function TryHexToFloat16(const HexString: String; out Value: Float16): Boolean;
Function HexToFloat16Def(const HexString: String; const DefaultValue: Float16): Float16;

{-------------------------------------------------------------------------------
    Type Half
-------------------------------------------------------------------------------}

Function HalfToHex(Value: Half): String;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToHalf(const HexString: String): Half;{$IFDEF CanInline} inline; {$ENDIF}
Function TryHexToHalf(const HexString: String; out Value: Half): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToHalfDef(const HexString: String; const DefaultValue: Half): Half;{$IFDEF CanInline} inline; {$ENDIF}

{-------------------------------------------------------------------------------
    Type Float32
-------------------------------------------------------------------------------}

Function Float32ToHex(Value: Float32): String;
Function HexToFloat32(const HexString: String): Float32;
Function TryHexToFloat32(const HexString: String; out Value: Float32): Boolean;
Function HexToFloat32Def(const HexString: String; const DefaultValue: Float32): Float32;

{-------------------------------------------------------------------------------
    Type Single
-------------------------------------------------------------------------------}

Function SingleToHex(Value: Single): String;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToSingle(const HexString: String): Single;{$IFDEF CanInline} inline; {$ENDIF}
Function TryHexToSingle(const HexString: String; out Value: Single): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToSingleDef(const HexString: String; const DefaultValue: Single): Single;{$IFDEF CanInline} inline; {$ENDIF}

{-------------------------------------------------------------------------------
    Type Float64
-------------------------------------------------------------------------------}

Function Float64ToHex(Value: Float64): String;
Function HexToFloat64(const HexString: String): Float64;
Function TryHexToFloat64(const HexString: String; out Value: Float64): Boolean;
Function HexToFloat64Def(const HexString: String; const DefaultValue: Float64): Float64;

{-------------------------------------------------------------------------------
    Type Double
-------------------------------------------------------------------------------}

Function DoubleToHex(Value: Double): String;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToDouble(const HexString: String): Double;{$IFDEF CanInline} inline; {$ENDIF}
Function TryHexToDouble(const HexString: String; out Value: Double): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToDoubleDef(const HexString: String; const DefaultValue: Double): Double;{$IFDEF CanInline} inline; {$ENDIF}

{-------------------------------------------------------------------------------
    Type Float80
-------------------------------------------------------------------------------}

Function Float80ToHex(Value: Float80): String;
Function HexToFloat80(const HexString: String): Float80;
Function TryHexToFloat80(const HexString: String; out Value: Float80): Boolean;
Function HexToFloat80Def(const HexString: String; const DefaultValue: Float80): Float80;

{-------------------------------------------------------------------------------
    Type Extended
-------------------------------------------------------------------------------}

Function ExtendedToHex(Value: Extended): String;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToExtended(const HexString: String): Extended;{$IFDEF CanInline} inline; {$ENDIF}
Function TryHexToExtended(const HexString: String; out Value: Extended): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToExtendedDef(const HexString: String; const DefaultValue: Extended): Extended;{$IFDEF CanInline} inline; {$ENDIF}

{-------------------------------------------------------------------------------
    Default float type
-------------------------------------------------------------------------------}

Function FloatToHex(Value: Double): String;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToFloat(const HexString: String): Double;{$IFDEF CanInline} inline; {$ENDIF}
Function TryHexToFloat(const HexString: String; out Value: Double): Boolean;{$IFDEF CanInline} inline; {$ENDIF}
Function HexToFloatDef(const HexString: String; const DefaultValue: Double): Double;{$IFDEF CanInline} inline; {$ENDIF}

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}}   // Conversion between ordinals and pointers is not portable
  {$PUSH}{$WARN 2005 OFF}             // Comment level $1 found
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30000)}
    {$DEFINE W5092:={$WARN 5092 OFF}} // Variable "$1" of a managed type does not seem to be initialized
  {$ELSE}
    {$DEFINE W5092:=}
  {$IFEND}
  {$POP}
{$ENDIF}

{===============================================================================
    Internal constants and types
===============================================================================}

{$IFDEF PurePascal}
const
  CW_EInvalidOP = UInt16($0001);  // invalid operation exception mask
  CW_EOverflow  = UInt16($0008);  // overflow exception mask
  CW_EUnderflow = UInt16($0010);  // underflow exception mask
{$IF not Declared(Get8087CW)}
  CW_Default    = UInt16($1372);  // default FPU control word
{$IFEND}
{$ENDIF}

{===============================================================================
    Auxiliary routines
===============================================================================}

{$IFDEF PurePascal}
var
{
  denormal, underflow and precision exceptions are masked, precision set to
  extended, rounding set to nearest and infinity control bit set
}
  Pas_X87CW: UInt16 = $1372;
{$ENDIF}

//------------------------------------------------------------------------------

Function EmulatedX87ControlWord: Boolean;
begin
{$IFDEF PurePascal}
Result := True;
{$ELSE}
Result := False;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetX87ControlWord: UInt16; {$IFNDEF PurePascal}register; assembler;
var
  Temp: UInt16;
asm
    FSTCW   word ptr [Temp]
    MOV     AX, word ptr [Temp]
end;
{$ELSE}
begin
Result := Pas_X87CW;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure SetX87ControlWord(NewValue: UInt16); {$IFNDEF PurePascal}register; assembler;
var
  Temp: UInt16;
asm
    MOV     word ptr [Temp], NewValue
    FLDCW   word ptr [Temp]
end;
{$ELSE}
begin
Pas_X87CW := NewValue;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87PrecisionMode: TX87PrecisionMode;
begin
case (GetX87ControlWord and X87CW_Precision) shr 8 of
  0:  Result := pcSingle;
  2:  Result := pcDouble;
  3:  Result := pcExtended;
else
  Result := pcReserved;
end;
end;

//------------------------------------------------------------------------------

Function SetX87PrecisionMode(NewValue: TX87PrecisionMode): TX87PrecisionMode;
var
  Num:  UInt16;
begin
Result := GetX87PrecisionMode;
case NewValue of
  pcSingle:   Num := 0;
  pcDouble:   Num := 2;
  pcExtended: Num := 3;
else
  Num := 1;
end;
SetX87ControlWord((GetX87ControlWord and not X87CW_Precision) or (Num shl 8));
end;

//------------------------------------------------------------------------------

Function GetX87RoundingMode: TX87RoundingMode;
begin
case (GetX87ControlWord and X87CW_Rounding) shr 10 of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function SetX87RoundingMode(NewValue: TX87RoundingMode): TX87RoundingMode;
var
  Num:  UInt16;
begin
Result := GetX87RoundingMode;
case NewValue of
  rmDown:     Num := 1;
  rmUp:       Num := 2;
  rmTruncate: Num := 3;
else
  Num := 0;
end;
SetX87ControlWord((GetX87ControlWord and not X87CW_Rounding) or (Num shl 10));
end;

//------------------------------------------------------------------------------

Function GetX87Flag(Flag: TX87Flag): Boolean;
begin
case Flag of
  flMaskInvalidOp:    Result := (GetX87ControlWord and X87CW_EMASK_InvalidOP) <> 0;
  flMaskDenormal:     Result := (GetX87ControlWord and X87CW_EMASK_Denormal) <> 0;
  flMaskDivByZero:    Result := (GetX87ControlWord and X87CW_EMASK_DivByZero) <> 0;
  flMaskOverflow:     Result := (GetX87ControlWord and X87CW_EMASK_Overflow) <> 0;
  flMaskUnderflow:    Result := (GetX87ControlWord and X87CW_EMASK_Underflow) <> 0;
  flMaskPrecision:    Result := (GetX87ControlWord and X87CW_EMASK_Precision) <> 0;
  flInfinityControl:  Result := (GetX87ControlWord and X87CW_InfinityControl) <> 0;
else
  raise EFHInvalidFlag.CreateFmt('GetX87Flag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87Flag(Flag: TX87Flag; NewValue: Boolean): Boolean;

  procedure SetFlag(FlagMask: UInt32);
  begin
    If NewValue then
      SetX87ControlWord(GetX87ControlWord or FlagMask)
    else
      SetX87ControlWord(GetX87ControlWord and not FlagMask);
  end;
  
begin
Result := GetX87Flag(Flag);
case Flag of
  flMaskInvalidOp:    SetFlag(X87CW_EMASK_InvalidOP);
  flMaskDenormal:     SetFlag(X87CW_EMASK_Denormal);
  flMaskDivByZero:    SetFlag(X87CW_EMASK_DivByZero);
  flMaskOverflow:     SetFlag(X87CW_EMASK_Overflow);
  flMaskUnderflow:    SetFlag(X87CW_EMASK_Underflow);
  flMaskPrecision:    SetFlag(X87CW_EMASK_Precision);
  flInfinityControl:  SetFlag(X87CW_InfinityControl);
else
  raise EFHInvalidFlag.CreateFmt('SetX87Flag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function GetX87Flags: TX87Flags;
var
  CW: UInt16;
  i:  TX87Flag;
begin
Result := [];
CW := GetX87ControlWord;
For i := Low(TX87Flag) to High(TX87Flag) do
  case i of
    flMaskInvalidOp:    If (CW and X87CW_EMASK_InvalidOP) <> 0 then Include(Result,i);
    flMaskDenormal:     If (CW and X87CW_EMASK_Denormal) <> 0 then Include(Result,i);
    flMaskDivByZero:    If (CW and X87CW_EMASK_DivByZero) <> 0 then Include(Result,i);
    flMaskOverflow:     If (CW and X87CW_EMASK_Overflow) <> 0 then Include(Result,i);
    flMaskUnderflow:    If (CW and X87CW_EMASK_Underflow) <> 0 then Include(Result,i);
    flMaskPrecision:    If (CW and X87CW_EMASK_Precision) <> 0 then Include(Result,i);
    flInfinityControl:  If (CW and X87CW_InfinityControl) <> 0 then Include(Result,i);
  else
    raise EFHInvalidFlag.CreateFmt('GetX87Flags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

procedure SetX87Flags(NewValue: TX87Flags);
var
  CW: UInt16;

  procedure SetFlag(FlagMask: UInt32; NewState: Boolean);
  begin
    If NewState then
      CW := CW or FlagMask
    else
      CW := CW and not FlagMask;
  end;

begin
CW := GetX87ControlWord;
SetFlag(X87CW_EMASK_InvalidOP,flMaskInvalidOp in NewValue);
SetFlag(X87CW_EMASK_Denormal,flMaskDenormal in NewValue);
SetFlag(X87CW_EMASK_DivByZero,flMaskDivByZero in NewValue);
SetFlag(X87CW_EMASK_Overflow,flMaskOverflow in NewValue);
SetFlag(X87CW_EMASK_Underflow,flMaskUnderflow in NewValue);
SetFlag(X87CW_EMASK_Precision,flMaskPrecision in NewValue);
SetFlag(X87CW_InfinityControl,flInfinityControl in NewValue);
SetX87ControlWord(CW);
end;

//==============================================================================

procedure ConvertFloat64ToFloat80(DoublePtr, ExtendedPtr: Pointer); {$IFNDEF PurePascal}register; assembler;
asm
    FLD     qword ptr [DoublePtr]
    FSTP    tbyte ptr [ExtendedPtr]
    FWAIT
end;
{$ELSE}
var
  ControlWord:  UInt16;
  Sign:         UInt64;
  Exponent:     Int32;
  Mantissa:     UInt64;
  MantissaTZC:  Integer;

  procedure BuildExtendedResult(Upper: UInt16; Lower: UInt64);
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    PUInt16(PtrUInt(ExtendedPtr) + 8)^ := Upper;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    UInt64(ExtendedPtr^) := Lower;
  end;

  Function HighZeroCount(Value: UInt64): Integer;
  begin
    If Value <> 0 then
      begin
        Result := 0;
        while (Value and UInt64($8000000000000000)) = 0  do
          begin
            Value := UInt64(Value shl 1);
            Inc(Result);
          end;
      end
    else Result := 64;
  end;

begin
{$IF Declared(Get8087CW)}
ControlWord := Get8087CW;
{$ELSE}
ControlWord := CW_Default;
{$IFEND}
Sign := UInt64(DoublePtr^) and UInt64($8000000000000000);
Exponent := Int32(UInt64(DoublePtr^) shr 52) and $7FF;
Mantissa := UInt64(DoublePtr^) and UInt64($000FFFFFFFFFFFFF);
case Exponent of
        // zero or subnormal
  0:    If Mantissa <> 0 then
          begin
            // subnormals, normalizing
            MantissaTZC := HighZeroCount(Mantissa);
            BuildExtendedResult(UInt16(Sign shr 48) or UInt16(Exponent - MantissaTZC + 15372),
                                UInt64(Mantissa shl MantissaTZC));
          end
        // return signed zero
        else BuildExtendedResult(UInt16(Sign shr 48),0);

        // infinity or NaN
  $7FF: If Mantissa <> 0 then
          begin
            If (Mantissa and UInt64($0008000000000000)) = 0 then
              begin
                // signaled NaN
                If (ControlWord and CW_EInvalidOP) <> 0 then
                  // quiet signed NaN with mantissa
                  BuildExtendedResult(UInt16(Sign shr 48) or $7FFF,
                    UInt64(Mantissa shl 11) or UInt64($C000000000000000))
                else
                  // signaling NaN
                  raise EInvalidOp.Create('Invalid floating point operation')
              end
            // quiet signed NaN with mantissa
            else BuildExtendedResult(UInt16(Sign shr 48) or $7FFF,
                   UInt64(Mantissa shl 11) or UInt64($8000000000000000));
          end  
        // signed infinity
        else BuildExtendedResult(UInt16(Sign shr 48) or $7FFF,UInt64($8000000000000000));

else
  // normal number
  BuildExtendedResult(UInt16(Sign shr 48) or UInt16(Exponent + 15360),
    UInt64(Mantissa shl 11) or UInt64($8000000000000000));
end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ConvertFloat80ToFloat64(ExtendedPtr, DoublePtr: Pointer); register; {$IFNDEF PurePascal}assembler;
asm
  FLD   tbyte ptr [ExtendedPtr]
  FSTP  qword ptr [DoublePtr]
  FWAIT
end;
{$ELSE PurePascal}
const
  Infinity = UInt64($7FF0000000000000);
  NaN      = UInt64($7FF8000000000000);
var
  ControlWord:  UInt16;
  RoundMode:    Integer;
  Sign:         UInt64;
  Exponent:     Int32;
  Mantissa:     UInt64;

  Function ShiftMantissa(Value: UInt64; Shift: Byte): UInt64;
  var
    ShiftedOut: UInt64;
    Distance:   UInt64;

    Function FirstIsSmaller(A,B: UInt64): Boolean;
    begin
      If Int64Rec(A).Hi = Int64Rec(B).Hi then
        Result := Int64Rec(A).Lo < Int64Rec(B).Lo
      else
        Result := Int64Rec(A).Hi < Int64Rec(B).Hi;
    end;

  begin
    If (Shift > 0) and (Shift <= 64) then
      begin
        If Shift = 64 then Result := 0
          else Result := Value shr Shift;
        ShiftedOut := Value and (UInt64($FFFFFFFFFFFFFFFF) shr (64 - Shift));
        case RoundMode of
              // nearest
          0:  If ShiftedOut <> 0 then
                begin
                  If Shift >= 64 then Distance := UInt64(-Int64(ShiftedOut))
                    else Distance := UInt64((UInt64(1) shl Shift) - ShiftedOut);
                  If FirstIsSmaller(Distance,ShiftedOut) or
                     ((Distance = ShiftedOut) and ((Result and 1) <> 0)) then
                    Inc(Result);
                end;
              // down
          1:  If (Sign <> 0) and (ShiftedOut <> 0) then
                Inc(Result);
              // up
          2:  If (Sign = 0) and (ShiftedOut <> 0) then
                Inc(Result);
        else
          {truncate}  // nothing to do
        end;
      end
    else Result := Value;
  end;

begin
{$IF Declared(Get8087CW)}
ControlWord := Get8087CW;
{$ELSE}
ControlWord := CW_Default;
{$IFEND}
RoundMode := (ControlWord shr 10) and 3;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Sign := UInt64(PUInt8(PtrUInt(ExtendedPtr) + 9)^ and $80) shl 56;
Exponent := Int32(PUInt16(PtrUInt(ExtendedPtr) + 8)^) and $7FFF;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
Mantissa := (UInt64(ExtendedPtr^) and UInt64($7FFFFFFFFFFFFFFF));
If ((UInt64(ExtendedPtr^) and UInt64($8000000000000000)) = 0) and ((Exponent > 0) and (Exponent < $7FFF)) then
  begin
    // unnormal number
    If (ControlWord and CW_EInvalidOP) <> 0 then
      // return negative SNaN (don't ask me, ask Intel)
      UInt64(DoublePtr^) := UInt64(NaN or UInt64($8000000000000000))
    else
      // invalid operand
      raise EInvalidOp.Create('Invalid floating point operation');
  end
else
  case Exponent of
            // zero or denormal (denormal cannot be represented as double)
    0:      If Mantissa <> 0 then
              begin
                // denormal
                If (ControlWord and CW_EUnderflow) <> 0 then
                  begin
                    If ((RoundMode = 1{down}) and (Sign <> 0)) or
                       ((RoundMode = 2{up}) and (Sign = 0)) then
                      // convert to smallest representable number
                      UInt64(DoublePtr^) := Sign or 1
                    else
                      // convert to signed zero
                      UInt64(DoublePtr^) := Sign;
                  end
                // signal underflow
                else raise EUnderflow.Create('Floating point underflow');
              end
            // return signed zero
            else UInt64(DoublePtr^) := Sign;

            // exponent is too small to be represented in double even as subnormal
    1..
    $3BCB:  If (ControlWord and CW_EUnderflow) <> 0 then
              begin
                If ((RoundMode = 1{down}) and (Sign <> 0)) or
                   ((RoundMode = 2{up}) and (Sign = 0)) then
                  // convert to smallest representable number
                  UInt64(DoublePtr^) := Sign or 1
                else
                  // convert to signed zero
                  UInt64(DoublePtr^) := Sign;
              end
            // signal underflow
            else raise EUnderflow.Create('Floating point underflow');

            // subnormal values (resulting exponent in double is 0)
    $3BCC..
    $3C00:  If (ControlWord and CW_EUnderflow) <> 0 then
              UInt64(DoublePtr^) := Sign or ShiftMantissa((Mantissa or
                UInt64($8000000000000000)),$3C0C - Exponent)
            else
              // signal underflow
              raise EUnderflow.Create('Floating point underflow');

            // exponent is too large to be represented in double (resulting
            // exponent would be larger than $7FE)
    $43FF..
    $7FFE:  If (ControlWord and CW_EOverflow) <> 0 then
              begin
                If (RoundMode = 3{trunc}) or
                   ((RoundMode = 1{down}) and (Sign = 0)) or
                   ((RoundMode = 2{up}) and (Sign <> 0)) then
                  // convert to largest representable number
                  UInt64(DoublePtr^) := Sign or UInt64($7FEFFFFFFFFFFFFF)
                else
                  // convert to signed infinity
                  UInt64(DoublePtr^) := Sign or Infinity
              end
            // signal overflow
            else raise EOverflow.Create('Floating point overflow');

            // special cases (INF, NaN, ...)
    $7FFF:  case UInt64(ExtendedPtr^) shr 62 of
                  // pseudo INF, pseudo NaN (treated as invalid operand)
              0,
              1:  If (ControlWord and CW_EInvalidOP) <> 0 then
                    // return negative SNaN
                    UInt64(DoublePtr^) := UInt64(NaN or UInt64($8000000000000000))
                  else
                     // invalid operand
                    raise EInvalidOp.Create('Invalid floating point operation');

                  // infinity or SNaN
              2:  If (UInt64(ExtendedPtr^) and UInt64($3FFFFFFFFFFFFFFF)) <> 0 then
                      begin
                        // signaled NaN
                        If (ControlWord and CW_EInvalidOP) <> 0 then
                          // return quiet signed NaN with truncated mantissa
                          UInt64(DoublePtr^) := Sign or NaN or (Mantissa shr 11)
                        else
                          // signaling NaN
                          raise EInvalidOp.Create('Invalid floating point operation');
                      end
                  // signed infinity
                  else UInt64(DoublePtr^) := Sign or Infinity;

                  // quiet signed NaN with truncated mantissa
              3:  UInt64(DoublePtr^) := Sign or NaN or (Mantissa shr 11);
            else
              // unknown case, return positive NaN
              UInt64(DoublePtr^) := NaN;
            end;
  else
    // representable numbers, normalized value
    Exponent := Exponent - 15360; // 15360 = $3FFF - $3FF
    // mantissa shift correction
    Mantissa := ShiftMantissa(Mantissa,11);
    If (Mantissa and UInt64($0010000000000000)) <> 0 then
      Inc(Exponent);
    UInt64(DoublePtr^) := Sign or (UInt64(Exponent and $7FF) shl 52) or
                          (Mantissa and UInt64($000FFFFFFFFFFFFF));
  end;
end;
{$ENDIF PurePascal}

{===============================================================================
    Internal routines
===============================================================================}

Function RectifyHexString(const Str: String; RequiredLength: Integer): String;

  Function StartsWithHexMark: Boolean;
  begin
    If Length(Str) > 0 then
      Result := Str[1] = '$'
    else
      Result := False;
  end;

begin
If not StartsWithHexMark then
  Result := '$' + Str
else
  Result := Str;
Inc(RequiredLength);
If Length(Result) <> RequiredLength then
  begin
    If Length(Result) < RequiredLength then
      Result := Result + StringOfChar('0',RequiredLength - Length(Result))
    else
      Result := Copy(Result,1,RequiredLength);
  end;
end;

{===============================================================================
    Conversion routines
===============================================================================}
{-------------------------------------------------------------------------------
    Type Float16
-------------------------------------------------------------------------------}

Function Float16ToHex(Value: Float16): String;
var
  Overlay:  UInt16 absolute Value;
begin
Result := IntToHex(Overlay,4);
end;

//------------------------------------------------------------------------------

Function HexToFloat16(const HexString: String): Float16;
var
  Overlay:  UInt16 absolute Result;
begin
Overlay := UInt16(StrToInt(RectifyHexString(HexString,4)));
end;

//------------------------------------------------------------------------------

Function TryHexToFloat16(const HexString: String; out Value: Float16): Boolean;
begin
try
  Value := HexToFloat16(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat16Def(const HexString: String; const DefaultValue: Float16): Float16;
begin
If not TryHexToFloat16(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Type Half
-------------------------------------------------------------------------------}

Function HalfToHex(Value: Half): String;
begin
Result := Float16ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToHalf(const HexString: String): Half;
begin
Result := HexToFloat16(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToHalf(const HexString: String; out Value: Half): Boolean;
begin
Result := TryHexToFloat16(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToHalfDef(const HexString: String; const DefaultValue: Half): Half;
begin
Result := HexToFloat16Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Type Float32
-------------------------------------------------------------------------------}

Function Float32ToHex(Value: Float32): String;
var
  Overlay:  UInt32 absolute Value;
begin
Result := IntToHex(Overlay,8);
end;
 
//------------------------------------------------------------------------------

Function HexToFloat32(const HexString: String): Float32;
var
  Overlay:  UInt32 absolute Result;
begin
Overlay := UInt32(StrToInt(RectifyHexString(HexString,8)));
end;
 
//------------------------------------------------------------------------------

Function TryHexToFloat32(const HexString: String; out Value: Float32): Boolean;
begin
try
  Value := HexToFloat32(HexString);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function HexToFloat32Def(const HexString: String; const DefaultValue: Float32): Float32;
begin
If not TryHexToFloat32(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Type Single
-------------------------------------------------------------------------------}

Function SingleToHex(Value: Single): String;
begin
Result := Float32ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToSingle(const HexString: String): Single;
begin
Result := HexToFloat32(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToSingle(const HexString: String; out Value: Single): Boolean;
begin
Result := TryHexToFloat32(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToSingleDef(const HexString: String; const DefaultValue: Single): Single;
begin
Result := HexToFloat32Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Type Float64
-------------------------------------------------------------------------------}

Function Float64ToHex(Value: Float64): String;
var
  Overlay:  UInt64 absolute Value;
begin
Result := IntToHex(Overlay,16);
end;

//------------------------------------------------------------------------------

Function HexToFloat64(const HexString: String): Float64;
var
  Overlay:  Int64 absolute Result;
begin
Overlay := StrToInt64(RectifyHexString(HexString,16));
end;

//------------------------------------------------------------------------------

Function TryHexToFloat64(const HexString: String; out Value: Float64): Boolean;
begin
try
  Value := HexToFloat64(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat64Def(const HexString: String; const DefaultValue: Float64): Float64;
begin
If not TryHexToFloat64(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Type Double
-------------------------------------------------------------------------------}

Function DoubleToHex(Value: Double): String;
begin
Result := Float64ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToDouble(const HexString: String): Double;
begin
Result := HexToFloat64(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToDouble(const HexString: String; out Value: Double): Boolean;
begin
Result := TryHexToFloat64(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToDoubleDef(const HexString: String; const DefaultValue: Double): Double;
begin
Result := HexToFloat64Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Type Float80
-------------------------------------------------------------------------------}

type
  // overlay used when working with 10-byte extended precision float
  TFloat80Overlay = packed record
    Part_64:  Int64;
    Part_16:  UInt16;
  end;

//------------------------------------------------------------------------------

Function Float80ToHex(Value: Float80): String;
var
  Overlay:  TFloat80Overlay {$IFNDEF Extended64}absolute Value{$ENDIF};
begin
{$IFDEF Extended64}
ConvertFloat64ToFloat80(@Value,@Overlay);
{$ENDIF}
Result := IntToHex(Overlay.Part_16,4) + IntToHex(Overlay.Part_64,16);
end;

//------------------------------------------------------------------------------

Function HexToFloat80(const HexString: String): Float80;
var
  Temp:     String;
  Overlay:  TFloat80Overlay {$IFNDEF Extended64}absolute Result{$ENDIF};
begin
Temp := RectifyHexString(HexString,20);
Overlay.Part_16 := UInt16(StrToInt(Copy(HexString,1,5)));
Overlay.Part_64 := StrToInt64('$' + Copy(HexString,6,16));
{$IFDEF Extended64}
ConvertFloat80ToFloat64(@Overlay,@Result);
{$ENDIF}
end;
 
//------------------------------------------------------------------------------

Function TryHexToFloat80(const HexString: String; out Value: Float80): Boolean;
begin
try
  Value := HexToFloat80(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat80Def(const HexString: String; const DefaultValue: Float80): Float80;
begin
If not TryHexToFloat80(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Type Extended
-------------------------------------------------------------------------------}

Function ExtendedToHex(Value: Extended): String;
begin
{$IFDEF Extended64}
Result := Float64ToHex(Value);
{$ELSE}
Result := Float80ToHex(Value);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function HexToExtended(const HexString: String): Extended;
begin
{$IFDEF Extended64}
Result := HexToFloat64(HexString);
{$ELSE}
Result := HexToFloat80(HexString);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TryHexToExtended(const HexString: String; out Value: Extended): Boolean;
begin
{$IFDEF Extended64}
Result := TryHexToFloat64(HexString,Value);
{$ELSE}
Result := TryHexToFloat80(HexString,Value);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function HexToExtendedDef(const HexString: String; const DefaultValue: Extended): Extended;
begin
{$IFDEF Extended64}
Result := HexToFloat64Def(HexString,DefaultValue);
{$ELSE}
Result := HexToFloat80Def(HexString,DefaultValue);
{$ENDIF}
end;

{-------------------------------------------------------------------------------
    Default float type
-------------------------------------------------------------------------------}

Function FloatToHex(Value: Double): String;
begin
Result := DoubleToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToFloat(const HexString: String): Double;
begin
Result := HexToDouble(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToFloat(const HexString: String; out Value: Double): Boolean;
begin
Result := TryHexToDouble(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToFloatDef(const HexString: String; const DefaultValue: Double): Double;
begin
Result := HexToDoubleDef(HexString,DefaultValue);
end;

end.
