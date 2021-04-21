{
 /***************************************************************************
                                websocket.pas
                                -------------

                   Initial Revision : Wed Apr 20 CST 2021

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Web Component Library (WCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit websocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TOnMessage = procedure(Sender: TObject; Data: string) of object;

  { TCustomWebSocketClient }

  TCustomWebSocketClient = class(TComponent)
  private
    FOnClose: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnMessage: TOnMessage;
    FOnOpen: TNotifyEvent;
    FUrl: string;
  public
    property Url: string read FUrl write FUrl;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
  end;

implementation


end.
