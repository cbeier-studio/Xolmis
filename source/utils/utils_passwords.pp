{ Xolmis Password helpers

  Copyright (C) 2023 Christian Beier <hello@christianbeier.studio>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General
  Public License as published by the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with this program.  If not,
  see <https://www.gnu.org/licenses/>.
}

unit utils_passwords;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function HashPasswordArgon2id(const APassword: String): String;
function VerifyPasswordArgon2id(const APassword, AStoredHash: String): Boolean;

implementation

uses
  HlpArrayUtils, HlpArgon2TypeAndVersion, HlpConverters, HlpHashFactory,
  HlpHashLibTypes, HlpIHashInfo, HlpPBKDF_Argon2NotBuildInAdapter;

const
  ARGON2_PREFIX = 'argon2id';
  ARGON2_VERSION = 19;
  ARGON2_MEMORY_KB = 65536;
  ARGON2_ITERATIONS = 3;
  ARGON2_PARALLELISM = 1;
  ARGON2_SALT_SIZE = 16;
  ARGON2_HASH_SIZE = 32;

function CreateSalt(ASize: Integer): THashLibByteArray;
var
  G: TGUID;
  Offset, CopyLen, Remaining: Integer;
begin
  SetLength(Result, ASize);
  Offset := 0;

  while Offset < ASize do
  begin
    if CreateGUID(G) <> 0 then
      raise Exception.Create('Unable to generate secure salt');

    Remaining := ASize - Offset;
    CopyLen := SizeOf(TGUID);
    if CopyLen > Remaining then
      CopyLen := Remaining;

    Move(G, Result[Offset], CopyLen);
    Inc(Offset, CopyLen);
  end;
end;

function BuildArgon2id(const APassword, ASalt: THashLibByteArray;
  AIterations, AMemoryKB, AParallelism, AHashSize: Integer): THashLibByteArray;
var
  Params: IArgon2Parameters;
  Kdf: IPBKDF_Argon2;
begin
  Params := TArgon2idParametersBuilder.Builder
    .WithSalt(ASalt)
    .WithIterations(AIterations)
    .WithMemoryAsKB(AMemoryKB)
    .WithParallelism(AParallelism)
    .WithVersion(TArgon2Version.Version13)
    .Build;

  Kdf := TKDF.TPBKDF_Argon2.CreatePBKDF_Argon2(APassword, Params);
  try
    Result := Kdf.GetBytes(AHashSize);
  finally
    Kdf.Clear;
  end;
end;

function ParseHash(const AStoredHash: String; out AIterations, AMemoryKB,
  AParallelism: Integer; out ASalt, AHash: THashLibByteArray): Boolean;
var
  Parts: TStringList;
  ParamItems: TStringList;
  I: Integer;
  SepPos: Integer;
  Key, Value: String;
begin
  Result := False;
  ASalt := nil;
  AHash := nil;
  AIterations := 0;
  AMemoryKB := 0;
  AParallelism := 0;

  Parts := TStringList.Create;
  ParamItems := TStringList.Create;
  try
    ExtractStrings(['$'], [], PChar(AStoredHash), Parts);
    if Parts.Count <> 5 then
      Exit;
    if not SameText(Parts[0], ARGON2_PREFIX) then
      Exit;
    if not SameText(Parts[1], 'v=19') then
      Exit;

    ExtractStrings([','], [], PChar(Parts[2]), ParamItems);
    if ParamItems.Count <> 3 then
      Exit;

    for I := 0 to ParamItems.Count - 1 do
    begin
      SepPos := Pos('=', ParamItems[I]);
      if SepPos <= 1 then
        Exit;
      Key := Trim(Copy(ParamItems[I], 1, SepPos - 1));
      Value := Trim(Copy(ParamItems[I], SepPos + 1, MaxInt));
      if SameText(Key, 'm') then
      begin
        if not TryStrToInt(Value, AMemoryKB) then
          Exit;
      end
      else if SameText(Key, 't') then
      begin
        if not TryStrToInt(Value, AIterations) then
          Exit;
      end
      else if SameText(Key, 'p') then
      begin
        if not TryStrToInt(Value, AParallelism) then
          Exit;
      end
      else
        Exit;
    end;

    ASalt := TConverters.ConvertHexStringToBytes(Parts[3]);
    AHash := TConverters.ConvertHexStringToBytes(Parts[4]);
    if (Length(ASalt) = 0) or (Length(AHash) = 0) then
      Exit;

    Result := True;
  finally
    ParamItems.Free;
    Parts.Free;
  end;
end;

function HashPasswordArgon2id(const APassword: String): String;
var
  PasswordBytes, SaltBytes, HashBytes: THashLibByteArray;
begin
  PasswordBytes := TConverters.ConvertStringToBytes(APassword, TEncoding.UTF8);
  SaltBytes := CreateSalt(ARGON2_SALT_SIZE);
  HashBytes := BuildArgon2id(PasswordBytes, SaltBytes, ARGON2_ITERATIONS,
    ARGON2_MEMORY_KB, ARGON2_PARALLELISM, ARGON2_HASH_SIZE);

  Result := Format('%s$v=%d$m=%d,t=%d,p=%d$%s$%s', [
    ARGON2_PREFIX,
    ARGON2_VERSION,
    ARGON2_MEMORY_KB,
    ARGON2_ITERATIONS,
    ARGON2_PARALLELISM,
    TConverters.ConvertBytesToHexString(SaltBytes, False),
    TConverters.ConvertBytesToHexString(HashBytes, False)
  ]);
end;

function VerifyPasswordArgon2id(const APassword, AStoredHash: String): Boolean;
var
  Iterations, MemoryKB, Parallelism: Integer;
  PasswordBytes, SaltBytes, ExpectedHash, CandidateHash: THashLibByteArray;
begin
  Result := False;
  if not ParseHash(AStoredHash, Iterations, MemoryKB, Parallelism,
    SaltBytes, ExpectedHash) then
    Exit;

  PasswordBytes := TConverters.ConvertStringToBytes(APassword, TEncoding.UTF8);
  CandidateHash := BuildArgon2id(PasswordBytes, SaltBytes, Iterations,
    MemoryKB, Parallelism, Length(ExpectedHash));

  Result := TArrayUtils.ConstantTimeAreEqual(ExpectedHash, CandidateHash);
end;

end.