&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: Wordle.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: James Bowen

  Created: 04/01/2024

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

using Progress.Lang.*.
using System.*.
using System.IO.*.
using System.IO.Compression.*.
using System.Text.*.
using Progress.Json.JsonParser. 
using Progress.Json.ObjectModel.ObjectModelParser. 
using Progress.Json.ObjectModel.JsonConstruct. 
using Progress.Json.ObjectModel.JsonObject.
using Progress.Json.ObjectModel.JsonArray.

create widget-pool.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

define variable hnWordCell       as widget-handle no-undo extent 30.
define variable hnKey            as widget-handle no-undo extent 28.

define variable WordleWordExtent as character     no-undo extent 5 initial ["H","A","P","P","Y"].
define variable WordleWord       as character     no-undo initial "OPNEDG".
define variable CurrentRow       as integer       no-undo.
define variable NumberOfWords    as integer       no-undo.
define variable CurrentWordCell  as integer       no-undo.

define temp-table ttWordleWords no-undo
    field WordleID as integer
    field Word     as character 
    index idxWordleWord is Primary Unique
    Word.

define temp-table ttButtonKeyStatus no-undo
    field ButtonHandle as widget-handle
    field FlagHandle   as widget-handle.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 bntNewWordle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetCompactedWordleList C-Win 
FUNCTION GetCompactedWordleList returns longchar private
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RandomNumber C-Win 
FUNCTION RandomNumber returns integer private
    ( input upperLimit as integer ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Unzip C-Win 
FUNCTION Unzip returns character private ( input str as longchar ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bntNewWordle 
    LABEL "New Wordle" 
    SIZE 24 BY 2.38
    FONT 1.

DEFINE RECTANGLE RECT-4
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 27 BY 3.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    bntNewWordle AT ROW 1.95 COL 6 WIDGET-ID 4 NO-TAB-STOP 
    RECT-4 AT ROW 1.48 COL 5 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 141.2 BY 31.76
    FONT 20 WIDGET-ID 100.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 40 ROW 1.71
    SIZE 69.2 BY 19.71
    FONT 20 WIDGET-ID 200.

DEFINE FRAME FRAME-B
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 3 ROW 22.43
    SIZE 137.2 BY 10 WIDGET-ID 300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "ABL Wordle"
        HEIGHT             = 31.76
        WIDTH              = 141.2
        MAX-HEIGHT         = 39.19
        MAX-WIDTH          = 221.4
        VIRTUAL-HEIGHT     = 39.19
        VIRTUAL-WIDTH      = 221.4
        MAX-BUTTON         = no
        RESIZE             = yes
        SCROLL-BARS        = no
        STATUS-AREA        = no
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = yes
        THREE-D            = yes
        MESSAGE-AREA       = yes
        SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN 
    FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE
    FRAME FRAME-B:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-B
/* Query rebuild information for FRAME FRAME-B
     _Query            is NOT OPENED
*/  /* FRAME FRAME-B */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* ABL Wordle */
    or endkey of {&WINDOW-NAME} anywhere 
    do:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        if this-procedure:persistent then return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON window-close OF C-Win /* ABL Wordle */
    do:
        /* This event will close the window and terminate the procedure.  */
        apply "CLOSE":U to this-procedure.
        return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntNewWordle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntNewWordle C-Win
ON choose OF bntNewWordle IN FRAME DEFAULT-FRAME /* New Wordle */
    do:
        run NewGame in this-procedure.  
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
    run disable_UI.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
    on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
    run enable_UI.
    
    run Initialise  in this-procedure.
    
    run GetWordleWords in this-procedure.
    
    
    
    run NewGame in this-procedure. 
    
    
  
    if not this-procedure:persistent then
        wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckWordCell C-Win 
PROCEDURE CheckWordCell PRIVATE :
    /*------------------------------------------------------------------------------
                                                 Purpose:
                                                 Notes:
                                                ------------------------------------------------------------------------------*/
    define variable inWordCellIndex as integer no-undo.

    do inWordCellIndex = 1 to 30:
        
        if hnWordCell[inWordCellindex]:read-only then
            next.
        
        //Correct cell match
        if trim(hnWordCell[inWordCellindex]:screen-value) eq hnWordCell[inWordCellindex]:private-data then
        do:
            assign
                hnWordCell[inWordCellindex]:bgcolor   = 18
                hnWordCell[inWordCellindex]:fgcolor   = 15
                hnWordCell[inWordCellindex]:read-only = true
                .
                
            run FlagKeyButtonCorrectSpot in this-procedure (input hnWordCell[inWordCellindex]:private-data).                 
        end.                
        else
        do: 
            
            if index(WordleWord, trim(hnWordCell[inWordCellindex]:screen-value)) gt 0 then
            do:
                assign
                    hnWordCell[inWordCellindex]:bgcolor   = 17
                    hnWordCell[inWordCellindex]:fgcolor   = 15
                    hnWordCell[inWordCellindex]:read-only = true
                    .
                    
                run FlagKeyButtonWrongSpot in this-procedure (input trim(hnWordCell[inWordCellindex]:screen-value)). 
            end.
            else
            do:
                
                assign
                    hnWordCell[inWordCellindex]:bgcolor   = 19
                    hnWordCell[inWordCellindex]:fgcolor   = 15
                    hnWordCell[inWordCellindex]:read-only = true
                    .
                
                run FlagKeyButtonNotFound in this-procedure (input trim(hnWordCell[inWordCellindex]:screen-value)).
            end.
        end.         

    end.

    return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearGrid C-Win 
PROCEDURE ClearGrid PRIVATE :
    /*------------------------------------------------------------------------------
                                         Purpose:
                                         Notes:
                                        ------------------------------------------------------------------------------*/

    define variable inWordCellIndex as integer       no-undo.
    define variable KeyFlag         as widget-handle no-undo.
    define variable inKeyIndex      as integer       no-undo.

    do inWordCellIndex = 1 to 30:
        
        if valid-handle(hnWordCell[inWordCellindex]) then
            delete object hnWordCell[inWordCellindex]. 

    end.
    
    for each ttButtonKeyStatus:
        
        assign
            KeyFlag = ttButtonKeyStatus.FlagHandle.
        
        if valid-object(KeyFlag) then
            delete object KeyFlag.
            
        delete ttButtonKeyStatus.    
        
    end.
    
    do inKeyIndex = 1 to 28:
        
        if valid-object(hnKey[inKeyIndex]) then
            delete object hnKey[inKeyIndex]. 
        
    end.

    return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DrawGrid C-Win 
PROCEDURE DrawGrid PRIVATE :
    /*------------------------------------------------------------------------------
                                                         Purpose:
                                                         Notes:
                                                        ------------------------------------------------------------------------------*/
    define variable inRows          as integer no-undo.
    define variable inColumns       as integer no-undo.
    
    //define variable hnWordCell as widget-handle no-undo extent 30.
    
    define variable inCell          as integer no-undo initial 62. // 30 x 30 pixcel Cell
    define variable inpadding       as integer no-undo initial 6.
    
    define variable inWordCellIndex as integer no-undo.
    
    do inRows = 0 to 5:
        
        do inColumns = 0 to 4:
            
            inWordCellIndex = inWordCellIndex + 1.
            
            create fill-in hnWordCell[inWordCellIndex] 
                assign
                frame     = frame Frame-A:HANDLE
                x    = (inColumns * inCell) + ((inColumns + 1) * inpadding)
                y    = (inRows * inCell) + ((inRows + 1) * inpadding)
                width-pixels  = inCell
                height-pixels  = inCell
                sensitive = true
                visible   = true
                read-only = true
                bgcolor = 0
                fgcolor = 15
                font = 20
                format = "XX"
                private-data = WordleWordExtent[inColumns + 1] // Store the correct words
                //screen-value = string(inWordCellIndex)
                triggers:
                    on value-changed persistent run WordCellUpdated in THIS-PROCEDURE (input inWordCellIndex ).
                    on backspace persistent run WordCellBackspace in THIS-PROCEDURE (input inWordCellIndex ).
                    on return persistent run GuessWord in THIS-PROCEDURE (input inWordCellIndex ).
                    on tab  persistent run WordCellTab in THIS-PROCEDURE (input inWordCellIndex ).
                    on ENTRY persistent run WordCellFocus in THIS-PROCEDURE (input inWordCellIndex ).
                    
                end triggers.

            
        end.
    end.        

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DrawKeyboard C-Win 
PROCEDURE DrawKeyboard PRIVATE :
    /*------------------------------------------------------------------------------
                                 Purpose:
                                 Notes:
                                ------------------------------------------------------------------------------*/
    define variable KeyboardRows     as character no-undo extent 3 initial ["QWERTYUIOP","ASDFGHJKL",">ZXCVBNM<"].

    define variable KeyboardRow      as character no-undo.
    
    define variable inRows           as integer   no-undo.
    define variable chKeyLabel       as character no-undo.
    define variable inKey            as integer   no-undo.
    define variable inpadding        as integer   no-undo initial 6.
    define variable inKeySize        as integer   no-undo initial 62.
    define variable inKeySizeOveride as integer   no-undo initial 0.
    define variable inKeyIndex       as integer   no-undo.
    define variable inOffset         as integer   no-undo.
    define variable inFont           as integer   no-undo initial 20.
    
    
    
    do inRows = 0 to 2:
        
        assign        
            KeyboardRow = KeyboardRows[inRows + 1]
            inOffset    = 0 
            inOffset    = 31 
            when inRows eq 1.
            
        
        do inKey = 0 to length(KeyboardRow) - 1:
            
            inKeyIndex = inKeyIndex + 1.
            
            chKeyLabel = substring(KeyboardRow, inKey + 1, 1 ).
            
            case chKeyLabel:
                when ">" then 
                    assign
                        chKeyLabel       = "Enter"
                        inFont           = ?
                        inKeySizeOveride = 31 .
                when "<" then
                    assign
                        chKeyLabel       = "«"
                        inKeySizeOveride = 31 .
                otherwise
                assign
                    inFont           = 20
                    inKeySize        = 62
                    inKeySizeOveride = 0.
            end case.
            
            create button hnKey[inKeyIndex] 
                assign
                frame     = frame Frame-B:HANDLE
                x    = (inKey * inKeySize) + ((inKey + 1) * inpadding) + inOffset 
                y    = (inRows * inKeySize) + ((inRows + 1) * inpadding)
                width-pixels  = inKeySize  
                height-pixels  = inKeySize
                //Flat-button = true
                sensitive = true
                visible   = true
                font = inFont
                label = chKeyLabel
                private-data = chKeyLabel // Store the correct words
                    //screen-value = string(inWordCellIndex)
                triggers:
                    on choose persistent run KeyPress in THIS-PROCEDURE (input inKeyIndex ).
                end triggers.
        
        End.
    end.

    finally:

    end finally.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableRow C-Win 
PROCEDURE EnableRow PRIVATE :
    /*------------------------------------------------------------------------------
                                                     Purpose:
                                                     Notes:
                                                    ------------------------------------------------------------------------------*/

    define input parameter inEnableRow as integer no-undo.
    
    define variable inFirstColum as integer no-undo.
    define variable inLastColum  as integer no-undo.
    define variable widgetIndex  as integer no-undo.
    
    if not (inEnableRow ge 1 and inEnableRow le 6) then
        return no-apply. 

        // Zero index position. 
    assign
        CurrentRow   = inEnableRow
        inEnableRow  = inEnableRow - 1
        inFirstColum = 1 + (inEnableRow * 5)
        inLastColum  = inFirstColum + 4.
    
    do widgetIndex = 1 to 30:
        
        if widgetIndex ge inFirstColum and widgetIndex le inLastColum then
            hnWordCell[widgetIndex]:read-only = false.
        else     
            hnWordCell[widgetIndex]:read-only = true.
                
    end.
    
    apply "entry" to hnWordCell[inFirstColum].
    

    
    //message "inFirstColum" inFirstColum "inLastColum" inLastColum.
    
    return no-apply.    

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     ENABLE the User Interface
      Parameters:  <none>
      Notes:       Here we display/view/enable the widgets in the
                   user-interface.  In addition, OPEN all queries
                   associated with each FRAME and BROWSE.
                   These statements here are based on the "Other 
                   Settings" section of the widget Property Sheets.
    ------------------------------------------------------------------------------*/
    ENABLE RECT-4 bntNewWordle 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    VIEW FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW FRAME FRAME-B IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlagKeyButtonCorrectSpot C-Win 
PROCEDURE FlagKeyButtonCorrectSpot PRIVATE :
    /*------------------------------------------------------------------------------
                         Purpose:
                         Notes:
                        ------------------------------------------------------------------------------*/
    define input parameter ButtonKeyValue as character. 
    
    define variable FlagKey          as widget-handle no-undo.
    
    define variable inButtonKeyIndex as integer       no-undo.

    do inButtonKeyIndex = 1 to 28:
        
        if hnKey[inButtonKeyIndex]:private-data eq ButtonKeyValue then
        do:
            Find ttButtonKeyStatus 
                where ttButtonKeyStatus.ButtonHandle eq hnKey[inButtonKeyIndex]
                no-error.
                
            if not available ttButtonKeyStatus then
            do:
                create rectangle FlagKey.    
                
                // Make the button smaller
                Assign    
                    hnKey[inButtonKeyIndex]:X             = hnKey[inButtonKeyIndex]:X + 2
                    hnKey[inButtonKeyIndex]:Y             = hnKey[inButtonKeyIndex]:Y + 2
                    hnKey[inButtonKeyIndex]:Width-pixels  = hnKey[inButtonKeyIndex]:Width-pixels - 4 
                    hnKey[inButtonKeyIndex]:Height-pixels = hnKey[inButtonKeyIndex]:Height-pixels - 4
                    .    
                
                Assign
                    FlagKey:frame         = frame Frame-B:HANDLE
                    FlagKey:x             = hnKey[inButtonKeyIndex]:x - 3
                    FlagKey:y             = hnKey[inButtonKeyIndex]:y - 3
                    FlagKey:Width-pixels  = hnKey[inButtonKeyIndex]:Width-pixels + 6
                    FlagKey:Height-pixels = hnKey[inButtonKeyIndex]:Height-pixels + 6
                    FlagKey:sensitive     = true
                    FlagKey:visible       = true
                    FlagKey:rounded       = true
                    FlagKey:GRAPHIC-EDGE  = true
                    FlagKey:edge-pixels   = 2.
                    
                create ttButtonKeyStatus.
                                
                assign
                    ttButtonKeyStatus.ButtonHandle = hnKey[inButtonKeyIndex]
                    ttButtonKeyStatus.FlagHandle   = FlagKey.
                    
            end.   
            else
                assign
                    FlagKey = ttButtonKeyStatus.FlagHandle.
            
            assign
                FlagKey:bgcolor = 18
                FlagKey:fgcolor = 18.
            
        end.
        else
            next.
    end.
    
    
    Return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlagKeyButtonNotFound C-Win 
PROCEDURE FlagKeyButtonNotFound PRIVATE :
    /*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/
    define input parameter ButtonKeyValue as character. 
    
    define variable FlagKey          as widget-handle no-undo.
    
    define variable inButtonKeyIndex as integer       no-undo.

    do inButtonKeyIndex = 1 to 28:
        
        if hnKey[inButtonKeyIndex]:private-data eq ButtonKeyValue then
        do:
            Find ttButtonKeyStatus 
                where ttButtonKeyStatus.ButtonHandle eq hnKey[inButtonKeyIndex]
                no-error.
                
            if not available ttButtonKeyStatus then
            do:
                // Make the button smaller
                Assign    
                    hnKey[inButtonKeyIndex]:X             = hnKey[inButtonKeyIndex]:X + 2
                    hnKey[inButtonKeyIndex]:Y             = hnKey[inButtonKeyIndex]:Y + 2
                    hnKey[inButtonKeyIndex]:Width-pixels  = hnKey[inButtonKeyIndex]:Width-pixels - 4 
                    hnKey[inButtonKeyIndex]:Height-pixels = hnKey[inButtonKeyIndex]:Height-pixels - 4
                    .
                
                create rectangle FlagKey.    
                
                Assign
                    FlagKey:frame         = frame Frame-B:HANDLE
                    FlagKey:x             = hnKey[inButtonKeyIndex]:x - 3
                    FlagKey:y             = hnKey[inButtonKeyIndex]:y - 3
                    FlagKey:Width-pixels  = hnKey[inButtonKeyIndex]:Width-pixels + 6
                    FlagKey:Height-pixels = hnKey[inButtonKeyIndex]:Height-pixels + 6
                    FlagKey:sensitive     = true
                    FlagKey:visible       = true
                    FlagKey:rounded       = true
                    FlagKey:GRAPHIC-EDGE  = true
                    FlagKey:edge-pixels   = 2
                    FlagKey:bgcolor       = 19
                    FlagKey:fgcolor       = 19.  //19=Grey
                
                
                        
                    
                create ttButtonKeyStatus.
                                
                assign
                    ttButtonKeyStatus.ButtonHandle = hnKey[inButtonKeyIndex]
                    ttButtonKeyStatus.FlagHandle   = FlagKey.
                    
                                   
                    
            end.   
        end.
        else
            next.
    end.
    
    
    Return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlagKeyButtonWrongSpot C-Win 
PROCEDURE FlagKeyButtonWrongSpot PRIVATE :
    /*------------------------------------------------------------------------------
                         Purpose:
                         Notes:
                        ------------------------------------------------------------------------------*/
    define input parameter ButtonKeyValue as character. 
    
    define variable FlagKey          as widget-handle no-undo.
    
    define variable inButtonKeyIndex as integer       no-undo.

    do inButtonKeyIndex = 1 to 28:
        
        if hnKey[inButtonKeyIndex]:private-data eq ButtonKeyValue then
        do:
            Find ttButtonKeyStatus 
                where ttButtonKeyStatus.ButtonHandle eq hnKey[inButtonKeyIndex]
                no-error.
                
            if not available ttButtonKeyStatus then
            do:
                create rectangle FlagKey.    
                
                 // Make the button smaller
                Assign    
                    hnKey[inButtonKeyIndex]:X             = hnKey[inButtonKeyIndex]:X + 2
                    hnKey[inButtonKeyIndex]:Y             = hnKey[inButtonKeyIndex]:Y + 2
                    hnKey[inButtonKeyIndex]:Width-pixels  = hnKey[inButtonKeyIndex]:Width-pixels - 4 
                    hnKey[inButtonKeyIndex]:Height-pixels = hnKey[inButtonKeyIndex]:Height-pixels - 4
                    . 
                
                Assign
                    FlagKey:frame         = frame Frame-B:HANDLE
                    FlagKey:x             = hnKey[inButtonKeyIndex]:x - 3
                    FlagKey:y             = hnKey[inButtonKeyIndex]:y - 3
                    FlagKey:Width-pixels  = hnKey[inButtonKeyIndex]:Width-pixels + 6
                    FlagKey:Height-pixels = hnKey[inButtonKeyIndex]:Height-pixels + 6
                    FlagKey:sensitive     = true
                    FlagKey:visible       = true
                    FlagKey:rounded       = true
                    FlagKey:GRAPHIC-EDGE  = true
                    FlagKey:edge-pixels   = 2
                    FlagKey:bgcolor       = 17
                    FlagKey:fgcolor       = 17.
                    
                create ttButtonKeyStatus.
                                
                assign
                    ttButtonKeyStatus.ButtonHandle = hnKey[inButtonKeyIndex]
                    ttButtonKeyStatus.FlagHandle   = FlagKey.
                    
            end.   
        end.
        else
            next.
    end.
    
    
    Return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetWordleWords C-Win 
PROCEDURE GetWordleWords PRIVATE :
    /*------------------------------------------------------------------------------
                                     Purpose:
                                     Notes:
                                    ------------------------------------------------------------------------------*/

    define variable CompressedData   as longchar no-undo.
    define variable UncompressedData as longchar no-undo.
    define variable myLongchar       as longchar no-undo.
    define variable myParser         as class    ObjectModelParser no-undo.
    define variable Request          as class    JsonConstruct     no-undo.
    define variable wordleWords      as class    JsonArray         no-undo.
    define variable randomIndex      as integer  no-undo.
    define variable WordleID         as integer  no-undo.


    CompressedData = GetCompactedWordleList().
    
    FIX-CODEPAGE (UncompressedData) = 'utf-8'.

    UncompressedData = ABLCompress:Unzip(CompressedData).

    myParser = new ObjectModelParser().
    wordleWords = cast(myParser:Parse(UncompressedData), JsonArray).
    
    assign
        NumberOfWords = wordleWords:Length. 
    
    do WordleID = 1 to NumberOfWords:
        create ttWordleWords.
        
        assign
            ttWordleWords.WordleID = WordleID
            ttWordleWords.Word     = CAPS(wordleWords:GetCharacter(WordleID)).
    end.

    finally:
        
        if valid-object(myParser) then 
            delete object myParser. 
            
        if valid-object(wordleWords) then
            delete object wordleWords.             

    end finally.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GuessWord C-Win 
PROCEDURE GuessWord PRIVATE :
    /*------------------------------------------------------------------------------
                                                 Purpose:
                                                 Notes:
                                                ------------------------------------------------------------------------------*/
    define input parameter WordCellIndex as integer no-undo.
    
    define variable GuessedWordDelimited as character no-undo.
    define variable GuessedWord          as character no-undo.
    define variable GuessedWordIndex     as integer   no-undo.
    
    define variable FirstCell            as integer   no-undo.
    define variable LastCell             as integer   no-undo.
    
    if hnWordCell[WordCellIndex]:read-only then
        return no-apply.
        
    assign            
        LastCell  = CurrentRow * 5  
        FirstCell = LastCell - 4.
    
    do GuessedWordIndex = FirstCell to LastCell: 
        
        assign
            GuessedWordDelimited = GuessedWordDelimited + hnWordCell[GuessedWordIndex]:screen-value.
    end.
        
    assign
        GuessedWordDelimited = trim(GuessedWordDelimited)
        GuessedWord          = replace(GuessedWordDelimited, ' ', '').
    
    if length(GuessedWord) ne 5 then
    do:
        message substitute("Not enough letters: &1", quoter(GuessedWord)) 
            view-as alert-box warning title C-WIN:Title.
            
        return no-apply.
    end. 
    
    // Validate that the world exists in the Wordle Dictionary 
    find first ttWordleWords
        where ttWordleWords.Word eq GuessedWord
        no-error.
        
    if not available ttWordleWords then
    do:
        //bell.
        message substitute("Not vaild: &1", quoter(GuessedWord) ).
        return  no-apply.     
    end.        
        
    //Match the letters. 
    
    run CheckWordCell in this-procedure.
    
    //Check for Win of game
    if GuessedWord EQ WordleWord then
    Do:
        
        run LockWordCells in this-procedure.
        
        message "CORRECT!"
            view-as alert-box information title C-WIN:Title.
        
    End. 
    else
    do:
        
        //If not on the last row then move on to the next row.       
        if CurrentRow ne 6 then
            run EnableRow in this-procedure (input CurrentRow + 1).
        else
        do:
            run LockWordCells in this-procedure.
            
            message substitute("Answer: &1", quoter(WordleWord))
                view-as alert-box information Title "Game Over".
                
        end.
    end.
    
    return no-apply.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initialise C-Win 
PROCEDURE Initialise PRIVATE :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

//Positiona the Child Frame to the centre of the window
    frame FRAME-A:x = (C-Win:width-pixels - frame FRAME-A:width-pixels) / 2.
    frame FRAME-B:x = (C-Win:width-pixels - frame FRAME-B:width-pixels) / 2.
    
    define variable CustFont   as character no-undo.
    define variable CustColour as character no-undo.
    
    //Set the required colours
    PUT-KEY-VALUE Section "Colors" Key "color17" value "181,159,59".
    PUT-KEY-VALUE Section "Colors" Key "color18" value "83,128,78".
    PUT-KEY-VALUE Section "Colors" Key "color19" value "58,58,60".
    
    //Set the required font(s)
    PUT-KEY-VALUE Section "fonts" Key "font20" value "Arial, size=36 bold".
     
    RETURN.  
    
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KeyPress C-Win 
PROCEDURE KeyPress PRIVATE :
    /*------------------------------------------------------------------------------
                         Purpose:
                         Notes:
                        ------------------------------------------------------------------------------*/
    define input parameter KeyIndex as integer no-undo.
    
    case hnKey[KeyIndex]:private-data:
        when "Enter" then   
            //run GuessWord(input CurrentWordCell).
            apply "return" to hnWordCell[CurrentWordCell].  
        when "«" then
            apply "backspace" to hnWordCell[CurrentWordCell].
        otherwise 
        do:
            hnWordCell[CurrentWordCell]:screen-value = hnKey[KeyIndex]:private-data.
    
            run WordCellUpdated in this-procedure (input CurrentWordCell).        
        end.
    end case. 
    return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LockWordCells C-Win 
PROCEDURE LockWordCells PRIVATE :
    /*------------------------------------------------------------------------------
                                         Purpose:
                                         Notes:
                                        ------------------------------------------------------------------------------*/
    define variable inWordCellIndex as integer no-undo.

    do inWordCellIndex = 1 to 30:
        
        if hnWordCell[inWordCellindex]:read-only then
            next.
        else    
            hnWordCell[inWordCellindex]:read-only = true.

    end.

    return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewGame C-Win 
PROCEDURE NewGame PRIVATE :
    /*------------------------------------------------------------------------------
                                     Purpose:
                                     Notes:
                                    ------------------------------------------------------------------------------*/
    
    define variable randomIndex as integer no-undo.
    
    
    
    
    run ClearGrid in this-procedure.
    
    run SelectRandomWord in this-procedure.
    
    randomIndex = RandomNumber(input NumberOfWords).

    FOR FIRST ttWordleWords
        where ttWordleWords.WordleID eq randomIndex:        
    
        ASSIGN
            WordleWord          = ttWordleWords.Word
            WordleWordExtent[1] = substring(WordleWord, 1, 1)
            WordleWordExtent[2] = substring(WordleWord, 2, 1)
            WordleWordExtent[3] = substring(WordleWord, 3, 1)
            WordleWordExtent[4] = substring(WordleWord, 4, 1)
            WordleWordExtent[5] = substring(WordleWord, 5, 1)
            .
            
    END.
    
    // Draw a grid 5 x 6
    run DrawGrid in this-procedure.
    
    run DrawKeyboard in this-procedure.
    
    //Enable the first row
    run EnableRow in this-procedure (input 1).

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRandomWord C-Win 
PROCEDURE SelectRandomWord PRIVATE :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
   
    define buffer ttWordleWords for temp-table ttWordleWords. 
   
    FOR FIRST ttWordleWords
        where ttWordleWords.WordleID eq RandomNumber(input NumberOfWords):        
    
        ASSIGN
            WordleWord          = ttWordleWords.Word
            WordleWordExtent[1] = substring(WordleWord, 1, 1)
            WordleWordExtent[2] = substring(WordleWord, 2, 1)
            WordleWordExtent[3] = substring(WordleWord, 3, 1)
            WordleWordExtent[4] = substring(WordleWord, 4, 1)
            WordleWordExtent[5] = substring(WordleWord, 5, 1)
            .
            
        message WordleWord.
            
    END.
    
    return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WordCellBackspace C-Win 
PROCEDURE WordCellBackspace PRIVATE :
    /*------------------------------------------------------------------------------
                                                 Purpose:
                                                 Notes:
                                                ------------------------------------------------------------------------------*/
    define input parameter WordCellIndex as integer no-undo.
    
    define variable inFirstColum as integer no-undo.
    define variable inLastColum  as integer no-undo.
 
    if hnWordCell[WordCellIndex]:read-only then
        return no-apply. 
 
    if hnWordCell[WordCellIndex]:screen-value eq "" and not WordCellIndex mod 5 eq 1 then
    do:
        apply "BACK-TAB" to hnWordCell[WordCellIndex].
        return no-apply.   
    end.
 
    hnWordCell[WordCellIndex]:screen-value = "".
    
    //We are on the first word cell. Can't goback
    if WordCellIndex mod 5 eq 1 then
        return no-apply.
    
    return no-apply.   

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WordCellFocus C-Win 
PROCEDURE WordCellFocus PRIVATE :
    /*------------------------------------------------------------------------------
                         Purpose:
                         Notes:
                        ------------------------------------------------------------------------------*/
    define input parameter WordCellIndex as integer no-undo.

    assign
        CurrentWordCell = WordCellIndex.
        
    message CurrentWordCell. 
        
    return.        

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WordCellTab C-Win 
PROCEDURE WordCellTab PRIVATE :
    /*------------------------------------------------------------------------------
                                     Purpose:
                                     Notes:
                                    ------------------------------------------------------------------------------*/
    define input parameter WordCellIndex as integer no-undo.

    if trim( hnWordCell[WordCellIndex]:screen-value ) eq "" then
        return no-apply.  

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WordCellUpdated C-Win 
PROCEDURE WordCellUpdated PRIVATE :
    /*------------------------------------------------------------------------------
                                                 Purpose:
                                                 Notes:
                                                ------------------------------------------------------------------------------*/
    define input parameter WordCellIndex as integer no-undo.
    
    if hnWordCell[WordCellIndex]:read-only then
        return no-apply. 
    
    if not (caps(hnWordCell[WordCellIndex]:screen-value) ge "A" and CAPS(hnWordCell[WordCellIndex]:screen-value) le "Z") then
    do:
        hnWordCell[WordCellIndex]:screen-value = "".
        return no-apply.
    end.        
        
    hnWordCell[WordCellIndex]:screen-value = " " + CAPS(hnWordCell[WordCellIndex]:screen-value).
    
    // If we are not on the 5th Cell, then apply a TAB. 
    if not (WordCellIndex mod 5 eq 0) and trim(hnWordCell[WordCellIndex]:screen-value) ne "" then 
        apply "TAB" to hnWordCell[WordCellIndex]. 

    return.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetCompactedWordleList C-Win 
FUNCTION GetCompactedWordleList returns longchar private
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result as longchar no-undo.
        
    result = "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".
        

    return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RandomNumber C-Win 
FUNCTION RandomNumber returns integer private
    ( input upperLimit as integer ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable randomNumber as class Random no-undo.
        
    randomNumber = new Random().
        
    return randomNumber:Next(0, upperLimit + 1).

    finally:
        
        if valid-object(randomNumber) then
            delete object randomNumber.

    end finally.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Unzip C-Win 
FUNCTION Unzip returns character private ( input str as longchar ):
    define variable msi        as class    MemoryStream no-undo.  
    define variable mso        as class    MemoryStream no-undo.
    define variable gZipStream as class    GZipStream   no-undo. 
    define variable bytes      as "Byte[]" no-undo.
        //Convert the Base64 into byte Array. 
    bytes = Convert:FromBase64String(str).
        
    define variable result as longchar no-undo.

    msi = new MemoryStream(bytes).
    mso = new MemoryStream().
    
    gZipStream = new GZipStream(msi, CompressionMode:Decompress).
    gZipStream:CopyTo(mso).
    gZipStream:Close(). 
        

    return Encoding:UTF8:GetString( mso:ToArray() ).

    finally:
            
        if valid-object ( gZipStream) then delete object gZipStream.
        if valid-object ( mso) then delete object mso.
        if valid-object ( msi) then delete object msi.
        if valid-object ( bytes) then delete object bytes.

    end finally.
end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

