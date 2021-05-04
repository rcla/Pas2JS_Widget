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

  TOnMessage = procedure(aSender: TObject; aData: String) of object;

  { TCustomWebSocketClient }

  TCustomWebSocketClient = class(TComponent)
  private
    fOnClose: TNotifyEvent;
    fOnError: TNotifyEvent;
    fOnMessage: TOnMessage;
    fOnOpen: TNotifyEvent;
    fUrl: String;
  public
    property Url: String read fUrl write fUrl;
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
    property OnError: TNotifyEvent read fOnError write fOnError;
    property OnMessage: TOnMessage read fOnMessage write fOnMessage;
    property OnOpen: TNotifyEvent read fOnOpen write fOnOpen;
  end;

implementation

end.
