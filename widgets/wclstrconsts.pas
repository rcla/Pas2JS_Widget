{
 /***************************************************************************
                               wclstrconsts.pas
                               ----------------

                   Initial Revision : Mon Jan 13 CST 2020

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Web Component Library (WCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WCLStrConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  rsFormResourceSNotFoundForResourcelessFormsCreateNew = 'Form resource %s '
    +'not found. For resourceless forms CreateNew constructor must be used.';
  rsFormStreamingError = 'Form streaming "%s" error: %s';
  rsFileButtonNoFileSelected = 'No file selected';
  rsResourceNotFound = 'Resource not found: %s';
  rsErrUncaughtException = 'Uncaught exception of type %s: ' + LineEnding + LineEnding + '%s';
  rsErrUncaughtObject = 'Uncaught exception of type %s.';
  rsNoTimers = 'No more timers available.';

  rsFixedColsTooBig = 'Too many fixed columns.';
  rsFixedRowsTooBig = 'Too many fixed rows.';

implementation

end.

