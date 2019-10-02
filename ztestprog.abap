*&---------------------------------------------------------------------*
*& Report  ZPPOEEVER3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ztestprog.


TABLES : AFRU ,CRHD,CRTX ,AFKO ,AFVV.
DATA: IT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV,
      WA_FIELDCAT   TYPE SLIS_FIELDCAT_ALV,
       wa_layout   TYPE slis_layout_alv.

data: lo_help type REF TO zmhelper .
create OBJECT lo_help.



DATA: BEGIN OF IT_OUT OCCURS 0,
  ARBPL TYPE ARBPL ,
  KTEXT TYPE KTEXT,
  AUFNR  TYPE AUFNR  ,
  BUDAT  TYPE BUCHDATUM  ,
  AUFPL  like AFKO-AUFPL ,
  VGW02 like AFVV-VGW02,
   VGW04 like AFVV-VGW04,
   BMSCH like AFVV-BMSCH,
   PERF  like AFRU-LMNGA ,
   TO_DT  type  AFRU-ISM06 ,
   AVAL  like AFRU-ISM06 ,
   QTY  like AFRU-LMNGA,
   OEE like AFRU-LMNGA,
    ISM06  like  AFRU-ISM06 ,
   ISM02  like  AFRU-ISM02  ,
   LMNGA  like  AFRU-LMNGA ,
   XMNGA  like  AFRU-XMNGA ,
  PLNBEZ TYPE MATNR.
DATA: END OF IT_OUT.

data: IT_OUT_TEMP like TABLE OF IT_OUT WITH HEADER LINE.
data: IT_TAP like TABLE OF IT_OUT WITH HEADER LINE.

DATA: BEGIN OF IT_DATES OCCURS 0,
  BUDAT like  AFRU-BUDAT ,
  AUFNR  TYPE AUFNR  ,
    ARBPL TYPE ARBPL .
DATA: END OF IT_DATES.

DATA: BEGIN OF IT_primary OCCURS 0,
   RUECK  like  AFRU-RUECK ,
   RMZHL  like  AFRU-RMZHL .
DATA: END OF IT_primary.

data : myPERF like AFRU-LMNGA.
data : myTO_DT  like AFRU-ISM06.
data : myAVAL like AFRU-ISM06.
data : myQTY like AFRU-LMNGA.
data : myOEE  like AFRU-LMNGA.

DATA: BEGIN OF IT_Order OCCURS 0,
  AUFNR  TYPE AUFNR  ,
   APLZL  like  AFRU-APLZL  ,

   ISM02  like  AFRU-ISM02  ,
   LMNGA  like  AFRU-LMNGA ,
   XMNGA  like  AFRU-XMNGA ,
   BUDAT like  AFRU-BUDAT   .
DATA: END OF IT_Order.

DATA: BEGIN OF IT_OUT_Workcenter OCCURS 0,
  OBJID TYPE OBJID.
DATA: END OF IT_OUT_Workcenter .

SELECTION-SCREEN : BEGIN OF BLOCK blck WITH FRAME TITLE text-100.
SELECT-OPTIONS : p_workc FOR  CRHD-ARBPL .
SELECT-OPTIONS: p_date FOR   AFRU-BUDAT  OBLIGATORY .
SELECTION-SCREEN END OF BLOCK blck.


PERFORM GET_DATA.
 PERFORM DISPLAY_DATA.

FORM GET_DATA.
select  OBJID  INTO CORRESPONDING FIELDS OF TABLE IT_OUT_Workcenter from CRHD where ARBPL in p_workc and ( VGWTS = 'ZEG1' or VGWTS = 'ZEG2' or VGWTS = 'ZEG3' )  .
  loop at IT_OUT_Workcenter .

   LOOP at IT_OUT .
    DELETE IT_OUT INDEX sy-tabix.
    DELETE IT_OUT_TEMP INDEX sy-tabix.
     EndLoop.

*     LOOP at IT_OUT_TEMP .
*           DELETE IT_OUT_TEMP INDEX sy-tabix.
*     EndLoop.


    "work center name
    CALL METHOD LO_HELP->GET_WORKCENTERBYID
      EXPORTING
        P_WORKCENTERID = IT_OUT_Workcenter-OBJID
      IMPORTING
        o_ARBPL  = IT_OUT-ARBPL.
    "work center description
      CALL METHOD LO_HELP->GET_WORKCENTERDESC
      EXPORTING
        P_WORKCENTERID = IT_OUT_Workcenter-OBJID
      IMPORTING
        O_KTEXT_UP = IT_OUT-KTEXT.

*     select  RUECK  RMZHL INTO CORRESPONDING FIELDS OF TABLE IT_PRIMARY from AFRU  where ARBID = IT_OUT_Workcenter-OBJID and  STOKZ = '' and STZHL = ''  AND
*        BUDAT in P_DATE  .

select  RUECK  RMZHL INTO CORRESPONDING FIELDS OF TABLE IT_PRIMARY from AFRU 
  FOR ALL ENTRIES IN  IT_OUT_Workcenter
   where ARBID = IT_OUT_Workcenter-OBJID and  STOKZ = '' and STZHL = ''  AND
        BUDAT in P_DATE  .

       LOOP at IT_PRIMARY .
             Clear IT_OUT-AUFPL  .
             Clear IT_OUT-VGW02  .
             Clear IT_OUT-VGW04  .
             Clear IT_OUT-BMSCH  .
             Clear IT_OUT-AUFNR   .
             Clear IT_OUT-BUDAT    .
             Clear  IT_OUT-PLNBEZ  .
             Clear  IT_OUT-ISM06 .
             Clear  IT_OUT-PERF   .
             Clear  IT_OUT-TO_DT  .
             Clear  IT_OUT-AVAL   .
             Clear  IT_OUT-QTY  .
             Clear  IT_OUT-OEE   .
       select  AUFNR BUDAT APLZL   sum( ISM02 ) as ISM02 sum( LMNGA ) as LMNGA  sum( XMNGA ) as XMNGA INTO CORRESPONDING FIELDS OF TABLE IT_ORDER from AFRU  where
       RMZHL = IT_PRIMARY-RMZHL and
       RUECK = IT_PRIMARY-RUECK
        GROUP BY   AUFNR BUDAT APLZL  .
          loop at IT_ORDER .
            it_out-AUFNR = IT_ORDER-AUFNR.
            it_out-BUDAT = IT_ORDER-BUDAT.
            it_out-ISM02 = IT_ORDER-ISM02.
            it_out-LMNGA = IT_ORDER-LMNGA.
            it_out-XMNGA = IT_ORDER-XMNGA.
       CALL METHOD LO_HELP->GET_MATFROMAFKOBYORDER
      EXPORTING
        P_ORDERID = IT_ORDER-AUFNR
      IMPORTING
        O_MAT_NO = IT_OUT-PLNBEZ
         O_AUFPL = IT_OUT-AUFPL.

        CALL METHOD LO_HELP->GET_DATAFROMAFVV
      EXPORTING
        p_AUFPL = IT_OUT-AUFPL
        p_APLZL = IT_ORDER-APLZL
      IMPORTING
        o_VGW02 = IT_OUT-VGW02
         o_VGW04 = IT_OUT-VGW04
        o_BMSCH  = IT_OUT-BMSCH .

         CALL METHOD LO_HELP->GET_SUMISM062
      EXPORTING
        p_RUECK = IT_PRIMARY-RUECK
        P_RMZHL = IT_PRIMARY-RMZHL
      IMPORTING
        O_SUM_ISM06 = IT_OUT-ISM06.


SHIFT IT_OUT-PLNBEZ LEFT DELETING LEADING '0'.
SHIFT  it_out-AUFNR  LEFT DELETING LEADING '0'.
ENDLOOP.
APPEND it_out .
    ENDLOOP .
         if  lines( it_out ) > 0  .
            LOOP at IT_DATES .
    DELETE IT_DATES INDEX sy-tabix.
     EndLoop.
           loop at  it_out .
             IT_DATES-BUDAT = it_out-BUDAT.
             IT_DATES-AUFNR = it_out-AUFNR.
             IT_DATES-ARBPL = it_out-ARBPL.
             APPEND IT_DATES.
             ENDLOOP.
             SORT IT_DATES BY BUDAT AUFNR ARBPL.
             DELETE ADJACENT DUPLICATES FROM  IT_DATES COMPARING BUDAT AUFNR ARBPL.

          loop at IT_DATES .
             Clear IT_OUT-AUFPL  .
             Clear IT_OUT-VGW02  .
             Clear IT_OUT-VGW04  .
             Clear IT_OUT-BMSCH  .
             Clear IT_OUT-AUFNR   .
             Clear IT_OUT-BUDAT    .
             Clear IT_OUT-PLNBEZ  .
             Clear IT_OUT-ISM06 .
             Clear IT_OUT-ISM02 .
             Clear IT_OUT-LMNGA .
             Clear IT_OUT-XMNGA .
             Clear  IT_OUT-PERF   .
             Clear  IT_OUT-TO_DT  .
             Clear  IT_OUT-AVAL   .
             Clear  IT_OUT-QTY  .
             Clear  IT_OUT-OEE   .
        loop at  it_out .
       if it_out-BUDAT = IT_DATES-BUDAT and it_out-AUFNR = IT_DATES-AUFNR and it_out-ARBPL = IT_DATES-ARBPL .
         IT_OUT_TEMP-ARBPL =  it_out-ARBPL .
         IT_OUT_TEMP-KTEXT =  it_out-KTEXT .
         IT_OUT_TEMP-AUFNR =  it_out-AUFNR .
         IT_OUT_TEMP-BUDAT  =  it_out-BUDAT .
         IT_OUT_TEMP-PLNBEZ =  it_out-PLNBEZ.
         IT_OUT_TEMP-BMSCH  =  IT_OUT-BMSCH  .
         IT_OUT_TEMP-VGW02  =  IT_OUT-VGW02  .
         IT_OUT_TEMP-VGW04  =  IT_OUT-VGW04 .
         IT_OUT_TEMP-ISM02 = IT_OUT_TEMP-ISM02  + IT_OUT-ISM02 .
         IT_OUT_TEMP-ISM06 = IT_OUT_TEMP-ISM06 + IT_OUT-ISM06.
         IT_OUT_TEMP-LMNGA = IT_OUT_TEMP-LMNGA + IT_OUT-LMNGA .
         IT_OUT_TEMP-XMNGA = IT_OUT_TEMP-XMNGA + IT_OUT-XMNGA.
       ENDIF.
       ENDLOOP .
       APPEND IT_OUT_TEMP .
        Clear  IT_OUT_TEMP-ARBPL  .
        Clear  IT_OUT_TEMP-KTEXT  .
        Clear  IT_OUT_TEMP-AUFNR   .
        Clear  IT_OUT_TEMP-BUDAT    .
        Clear  IT_OUT_TEMP-PLNBEZ  .
        Clear  IT_OUT_TEMP-PERF   .
        Clear  IT_OUT_TEMP-TO_DT  .
        Clear  IT_OUT_TEMP-AVAL   .
        Clear  IT_OUT_TEMP-QTY  .
        Clear  IT_OUT_TEMP-OEE   .
        Clear IT_OUT_TEMP-ISM02 .
        Clear IT_OUT_TEMP-ISM06 .
        Clear IT_OUT_TEMP-LMNGA .
        Clear IT_OUT_TEMP-XMNGA .
        Clear   IT_OUT_TEMP-BMSCH .
        Clear  IT_OUT_TEMP-VGW02   .
         Clear IT_OUT_TEMP-VGW04  .
       ENDLOOP .





            loop at IT_OUT_TEMP .

         IT_TAP-ARBPL =  IT_OUT_TEMP-ARBPL .
         IT_TAP-KTEXT =  IT_OUT_TEMP-KTEXT .
         IT_TAP-AUFNR =  IT_OUT_TEMP-AUFNR .
         IT_TAP-BUDAT  =  IT_OUT_TEMP-BUDAT .
         IT_TAP-PLNBEZ =  IT_OUT_TEMP-PLNBEZ.

         clear myPERF .
         clear myTO_DT .
         clear myAVAL .
         clear myQTY .
         clear myOEE  .

           if IT_OUT_TEMP-ISM02 <> 0  and  IT_OUT_TEMP-BMSCH <> 0 .
        myPERF = ( ( ( ( IT_OUT_TEMP-LMNGA + IT_OUT_TEMP-XMNGA ) * IT_OUT_TEMP-VGW02 ) / IT_OUT_TEMP-BMSCH ) / IT_OUT_TEMP-ISM02 )  * 100 .
        else .
         myPERF = 0 .
          ENDIF.

         if ( IT_OUT_TEMP-LMNGA + IT_OUT_TEMP-XMNGA ) <> 0 .
        myQTY = ( IT_OUT_TEMP-LMNGA / ( IT_OUT_TEMP-LMNGA + IT_OUT_TEMP-XMNGA ) ) * 100 .
          else .
          myQTY = 0 .
          ENDIF.



        if ( IT_OUT_TEMP-ISM02 + IT_OUT_TEMP-ISM06 ) <> 0 .
        myAVAL = ( IT_OUT_TEMP-ISM02 / ( IT_OUT_TEMP-ISM02 + IT_OUT_TEMP-ISM06 ) )  * 100.
       else .
          myAVAL  = 0 .
         ENDIF.

        myTO_DT = IT_OUT_TEMP-ISM06 .

          if ( IT_OUT_TEMP-ISM02 + IT_OUT_TEMP-ISM06 )  <> 0 and IT_OUT_TEMP-BMSCH <> 0 .
        myOEE = ( ( ( IT_OUT_TEMP-LMNGA * IT_OUT_TEMP-VGW02 ) / IT_OUT_TEMP-BMSCH ) / ( IT_OUT_TEMP-ISM02 + IT_OUT_TEMP-ISM06 ) ) * 100 .
          else .
         myOEE  = 0 .
         ENDIF.

       IT_TAP-PERF = myPERF .
       IT_TAP-QTY =   myQTY .
       IT_TAP-TO_DT =  myTO_DT .
       IT_TAP-OEE =    myOEE .
       IT_TAP-AVAL =  myAVAL .

              APPEND IT_TAP.

   Clear  IT_OUT_TEMP-ARBPL  .
        Clear  IT_OUT_TEMP-KTEXT  .
        Clear  IT_OUT_TEMP-AUFNR   .
        Clear  IT_OUT_TEMP-BUDAT    .
        Clear  IT_OUT_TEMP-PLNBEZ  .
        Clear  IT_OUT_TEMP-PERF   .
        Clear  IT_OUT_TEMP-TO_DT  .
        Clear  IT_OUT_TEMP-AVAL   .
        Clear  IT_OUT_TEMP-QTY  .
        Clear  IT_OUT_TEMP-OEE   .
        Clear IT_OUT_TEMP-ISM02 .
        Clear IT_OUT_TEMP-ISM06 .
        Clear IT_OUT_TEMP-LMNGA .
        Clear IT_OUT_TEMP-XMNGA .
        Clear   IT_OUT_TEMP-BMSCH .
        Clear  IT_OUT_TEMP-VGW02   .
         Clear IT_OUT_TEMP-VGW04  .

        Clear  IT_TAP-ARBPL  .
        Clear  IT_TAP-KTEXT  .
        Clear  IT_TAP-AUFNR   .
        Clear  IT_TAP-BUDAT    .
        Clear  IT_TAP-PLNBEZ  .
        Clear  IT_TAP-PERF   .
        Clear  IT_TAP-TO_DT  .
        Clear  IT_TAP-AVAL   .
        Clear  IT_TAP-QTY  .
        Clear  IT_TAP-OEE   .
        Clear IT_TAP-ISM02 .
        Clear IT_TAP-ISM06 .
        Clear IT_TAP-LMNGA .
        Clear IT_TAP-XMNGA .
             endloop.

       ENDIF.

 ENDLOOP.


ENDFORM.

FORM DISPLAY_DATA.
WA_FIELDCAT-FIELDNAME  = 'ARBPL'.
WA_FIELDCAT-SELTEXT_M  = 'Workcenter'.
WA_FIELDCAT-SELTEXT_L  = 'Workcenter'.
WA_FIELDCAT-SELTEXT_S  = 'Workcenter'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'KTEXT'.
WA_FIELDCAT-SELTEXT_M  = 'Work center Desc'.
WA_FIELDCAT-SELTEXT_L  = 'Work center Description'.
WA_FIELDCAT-SELTEXT_S  = 'WC Desc'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'BUDAT'.
WA_FIELDCAT-SELTEXT_M  = 'Posting Date'.
WA_FIELDCAT-SELTEXT_L  = 'Posting Date'.
WA_FIELDCAT-SELTEXT_S  = 'Posting Date'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'AUFNR'.
WA_FIELDCAT-SELTEXT_M  = 'Order'.
WA_FIELDCAT-SELTEXT_L  = 'Order'.
WA_FIELDCAT-SELTEXT_S  = 'Order'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.


WA_FIELDCAT-FIELDNAME  = 'PLNBEZ'.
WA_FIELDCAT-SELTEXT_M  = 'Material'.
WA_FIELDCAT-SELTEXT_L  = 'Material'.
WA_FIELDCAT-SELTEXT_S  = 'Material'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'PERF'.
WA_FIELDCAT-SELTEXT_M  = 'Performance %'.
WA_FIELDCAT-SELTEXT_L  = 'Performance %'.
WA_FIELDCAT-SELTEXT_S  = 'Performance %'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'TO_DT'.
WA_FIELDCAT-SELTEXT_M  = 'Total DT'.
WA_FIELDCAT-SELTEXT_L  = 'Total DT'.
WA_FIELDCAT-SELTEXT_S  = 'Total DT'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'AVAL'.
WA_FIELDCAT-SELTEXT_M  = 'Availability %'.
WA_FIELDCAT-SELTEXT_L  = 'Availability %'.
WA_FIELDCAT-SELTEXT_S  = 'Availability %'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'QTY'.
WA_FIELDCAT-SELTEXT_M  = 'Quality %'.
WA_FIELDCAT-SELTEXT_L  = 'Quality %'.
WA_FIELDCAT-SELTEXT_S  = 'Quality %'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.

WA_FIELDCAT-FIELDNAME  = 'OEE'.
WA_FIELDCAT-SELTEXT_M  = 'OEE %'.
WA_FIELDCAT-SELTEXT_L  = 'OEE %'.
WA_FIELDCAT-SELTEXT_S  = 'OEE %'.
APPEND WA_FIELDCAT TO IT_FIELDCAT.


CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM         = SY-REPID
      IT_FIELDCAT                = IT_FIELDCAT
      is_layout                         = wa_layout
      I_SAVE                     = 'X'
*      IT_SORT                    = IT_SORT_DATA
    TABLES
      t_outtab                   = IT_TAP.
ENDFORM.