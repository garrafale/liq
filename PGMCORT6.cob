       CBL OPTIMIZE(FULL)
       PROCESS NOSEQ
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMCORT6.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.  DECIMAL-POINT    IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
001800         SELECT TABLAF     ASSIGN        TO TABLAF
001900                           ORGANIZATION  IS INDEXED
002000                           ACCESS MODE   IS RANDOM
002100                           RECORD  KEY   IS TBF-CLAVE
002200                           FILE STATUS   IS FS-TABLAF.
002300
               SELECT  MOVMMCLA  ASSIGN       TO MOVCLA
                                 FILE STATUS  IS FS-MCLA.

               SELECT  TABLAS    ASSIGN       TO TABLAS                       99
                                 ORGANIZATION IS INDEXED                      99
                                 ACCESS       IS DYNAMIC                      99
                                 RECORD  KEY  IS T026-CLAVE                   99
                                 FILE STATUS  IS FS-TAB.                      99

               SELECT  MAEESTVS  ASSIGN       TO MAEESTVS                     99
                                 ORGANIZATION IS INDEXED                      99
                                 ACCESS       IS RANDOM                       99
                                 RECORD  KEY  IS ESTVS-CLAVE                  99
                                 FILE STATUS  IS FS-MAEEST.                   99

               SELECT  MAECOM    ASSIGN       TO MAECOM                       99
                                 ORGANIZATION IS INDEXED                      99
                                 ACCESS       IS RANDOM                       99
                                 RECORD  KEY  IS COM-CLAVE                    99
                                 FILE STATUS  IS FS-MAECOM.                   99

310100         SELECT  CONTDGI   ASSIGN       TO CONTDGI                      99
                                 FILE STATUS  IS FS-CONTDGI.                  99

               SELECT  SALIDA    ASSIGN       TO SALIDA
                                 FILE STATUS  IS FS-SAL.


221120         SELECT  CONTESL   ASSIGN       TO CONTESL                      99
                                 FILE STATUS  IS FS-CONTESL.

004700         SELECT  SALIDA6   ASSIGN       TO SALIDA6                      99
004800                           FILE STATUS  IS FS-SALIDA6.
004900
005000         SELECT  MOVSALT7  ASSIGN       TO MOVSALT7                     99
005100                           FILE STATUS  IS FS-MOVSALT7.


       DATA DIVISION.
       FILE SECTION.
       FD  MOVMMCLA
           LABEL RECORD        IS STANDARD
           BLOCK CONTAINS       0 RECORDS.
           COPY   MOVMES.
           COPY   MOV650X REPLACING == :MM: == BY == ==.
           EJECT

       FD  TABLAS                                                             99
           LABEL RECORD        IS STANDARD                                    99
           BLOCK CONTAINS       0 RECORDS.                                    99
           COPY   D532026.                                                    99
           COPY   D532027.                                                    99
           COPY   D532042.                                                    99
           COPY   D532045.                                                    99
           COPY   D532048.                                                    99
DIC/23     COPY   D532050A.                                                   99
           COPY   D532061.                                                    99
           COPY   D532067.                                                    99
           COPY   D532068.                                                    99
CRDCUO     COPY   D532074.                                                    99
           COPY   D532083.                                                    99
           COPY   D532092.                                                    99
190504     COPY   D532136.                                                    99
FAS174     COPY   D532226.                                                    99
           EJECT                                                              99

OPTIMO FD  MAEESTVS                                                           99
           LABEL RECORD        IS STANDARD                                    99
           BLOCK CONTAINS       0 RECORDS.                                    99
       01  REG-ESTVS.                                                         99
           03  ESTVS-CLAVE.                                                   99
               05  ESTVS-CODSIS   PIC XXX.                                    99
               05  ESTVS-CODTAR   PIC XXX.                                    99
               05  ESTVS-CODADM   PIC XXX.                                    99
               05  ESTVS-NUMEST   PIC X(10).                                  99
20         03  EST-CODBCO                        PIC 9(3).
               88   EST-BCO-PRIVADA              VALUE 332
                                                       410 411
                                                       413 414
                                                       415 416 417 420
                                               430 452 454 469 480 490
                                               458 459 491 498 499 512
                                                       456
JPP01                                                  462 463 532
                                                       590 591.
23         03  EST-CODCASA                       PIC 9(3).
           03  ESTVS-NUMCOM       PIC 9(10).
36         03  EST-MOTIVO-BAJA                   PIC X.
               88  EST-BAJA-BANCO                      VALUE 'B'.
               88  EST-BAJA-CAPTURA                    VALUE 'C'.
               88  EST-BAJA-FALTA-DOCUM                VALUE 'D'.
               88  EST-BAJA-SEGURIDAD                  VALUE 'V'.
               88  EST-BAJA-GERENCIA-ESTABLEC          VALUE 'E'.
               88  EST-BAJA-FILTRO-DATOS-CTA           VALUE 'K'.
               88  EST-BAJA-ROBO-DATOS-FRAUDE          VALUE 'L'.
               88  EST-BAJA-LAVADO-DINERO              VALUE 'M'.
               88  EST-BAJA-CCARGO-EXCEDE-1            VALUE 'O'.
               88  EST-BAJA-CPRA-FRAUDE-8              VALUE 'P'.
               88  EST-BAJA-RESERV-USO-FUTURO          VALUE 'Q'.
               88  EST-BAJA-DNO-FRAUDE                 VALUE 'R'.
               88  EST-BAJA-AUDITA-FRAUDE              VALUE 'S'.
               88  EST-BAJA-BANCARROTA                 VALUE 'T'.
               88  EST-BAJA-NO-RESPETA-ADQMST          VALUE 'U'.
               88  EST-BAJA-ACTIVIDAD-FRAUDE           VALUE 'W'.
               88  EST-BAJA-NO-SEGURIDAD-PCI           VALUE 'X'.
               88  EST-BAJA-TRANS-ILEGALES             VALUE 'Y'.
               88  EST-BAJA-OTRO-COMER-PRSONA          VALUE 'Z'.
37         03  EST-DTO-CUOTAS                    PIC X.
               88  EST-CUOTAS-JUMBO                    VALUE 'A'.
               88  EST-CUOTAS-DUTY                     VALUE 'B'.
38         03  EST-LST-CUOTAS-LIQ                PIC X.
               88  EST-CUOTAS-IBERIA                   VALUE '1'.
39         03  EST-CONV-ESP                      PIC X.
               88  EST-CONV-ESP-GTIA-ACEPT              VALUE 'G'.
               88  EST-CONV-ESP-CONTRAC-TODO            VALUE 'T'.
               88  EST-CONV-ESP-NO-CONTRACARGA          VALUE 'N'.
               88  EST-CONV-ESP-CONTRAC-83              VALUE 'P'.
               88  EST-CONV-ESP-CONTRAC-R01SEG          VALUE 'S'.
               88  EST-CONV-ESP-CONTRAC-R-A             VALUE 'A'.
               88  EST-CONV-ESP-CONTRAC-R-R             VALUE 'R'.
               88  EST-CONV-ESP-NO-CCO-41               VALUE 'D'.
               88  EST-CONV-EST-NORMAL                  VALUE ' '.
40         03  EST-PLAN-CUOTAS                   PIC X.
               88  EST-PLAN-STANDARD                    VALUE  '0'.
MD-020         88  EST-PLAN-EN-1-PAGO            VALUE '1' '2' '3' '4'.
               88  EST-PLAN-ACE-TOPE-RUBRO       VALUE '1'.
               88  EST-PLAN-ACE-HASTA18          VALUE '2'.
               88  EST-PLAN-ACE-HASTA24          VALUE '3'.
               88  EST-PLAN-ACE-0CUOTAS          VALUE '4'.
41         03  EST-TIPO-PRESENTACION             PIC X.
               88  EST-PRES-MANUAL                      VALUE '0'.
               88  EST-PRES-OFFLINE                     VALUE '1'.
               88  EST-PRES-ONLINE                      VALUE '2'.
               88  EST-PRES-ON-OFF-LINE                 VALUE '3'.
               88  EST-PRES-REMOTO                      VALUE '4'.
               88  EST-PRES-REM-ONLINE                  VALUE '5'.
               88  EST-PRES-OFF-DEBAUT                  VALUE '6'.
               88  EST-PRES-ON-DEBAUT                   VALUE '7'.
42         03  EST-ADMIN                         PIC 999.
45         03  EST-DOLAR                         PIC X.
           03  ESTVS-DENEST       PIC X(30).
76         03  EST-TIPOAUT                       PIC X.
               88  EST-TIPOAUT-OK                  VALUES '0' THRU '9'.
               88  EST-TIPOAUT-NORMAL              VALUE  '0'.
               88  EST-TIPOAUT-VTASTEL             VALUE  '1'.
               88  EST-TIPOAUT-VTA-CORREO-MAIL     VALUE  '2'.
               88  EST-TIPOAUT-S-M-S               VALUE  '3'.
               88  EST-TIPOAUT-SOSPECHOSO          VALUE  '4' '3'.
               88  EST-TIPOAUT-ACTIV-RIESGO        VALUE  '4' '3'.
               88  EST-TIPOAUT-ART-HOGAR           VALUE  '5'.
               88  EST-TIPOAUT-SELF-SERVICE        VALUE  '5'.
               88  EST-TIPOAUT-VIGILADO            VALUE  '6'.
               88  EST-TIPOAUT-PAGOTEL-SERV        VALUE  '7'.
               88  EST-TIPOAUT-PAGOELEC-SERV       VALUE  '8'.
               88  EST-TIPOAUT-SERVICIO-PAGO-VISA  VALUE  '9'.
77         03  EST-LST-DTOCUP                    PIC X.
               88  EST-NO-LST-DTOCUP                        VALUE '0'.
               88  EST-SI-LST-DTOCUP                        VALUE '1'.
77         03  EST-MCA-LIQ-ANTICIPADA REDEFINES EST-LST-DTOCUP
                                                 PIC X.
               88  EST-LQA-NO-ADHERIDO               VALUE '0' '1' ' '.
               88  EST-LQA-INHABILITADO              VALUE '2'.
               88  EST-LQA-INHABIL-SEGURIDAD         VALUE '2'.
               88  EST-LQA-SI-ADHERIDO               VALUE '4'.
               88  EST-LQA-INHABIL-X-ALTA            VALUE '6'.
78         03  EST-DEBITO-AUTOMATICO             PIC X.
               88  EST-NO-ES-DB-AUTOMATICO                  VALUE '0'.
               88  EST-ES-DB-AUTOMATICO                     VALUE '1'.
               88  EST-ES-DB-AUT-INSTITUCIONAL              VALUE '2'.
79         03  EST-ACEP-PLAN-BCO                 PIC X.
               88  EST-NO-ACEP-PLAN-BCO                     VALUE '0'.
               88  EST-SI-ACEP-PLAN-BCO                     VALUE '9'.
80         03  EST-CALLE                         PIC X(30).
110        03  EST-PUERTA                        PIC 9(5).
115        03  EST-PISO                          PIC XX.
117        03  EST-LOD                           PIC X(3).
120        03  EST-NUMLOD                        PIC X(4).
124        03  EST-CODPOST.
124            05  EST-POST-ALFA1                PIC X(1).
125            05  EST-POST-ALFA2                PIC X(3).
128            05  EST-POST-COD                  PIC X(4).
132        03  EST-NUMEST-POST                   PIC  9(8)  COMP.
136        03  EST-LOC                           PIC X(30).
166        03  EST-MCA-PAGO                      PIC X.
               88  EST-NO-RETENER-PAGO                      VALUE '0'.
               88  EST-RETENER-PAGO                         VALUE '1'.
166        03  EST-MCA-SEGURIDAD  REDEFINES
               EST-MCA-PAGO                      PIC X.
               88  EST-SG-ANTECED-FRAUDE                    VALUE '2'.
167        03  EST-DIAS-PAGO                     PIC 99.
169        03  EST-TELEF                         PIC X(13).
182        03  EST-CODGEO.
182            05  EST-CODPROV2                  PIC X.
183            05  EST-CODLOC                    PIC 9(4).
187        03  EST-NUMEST-GEO                    PIC  9(8)  COMP.
191        03  EST-PORCDTO-ALT2                  PIC 999V99 COMP-3.
194        03  EST-PORCDTO-ALT3                  PIC 999V99 COMP-3.
197        03  EST-BENEF                         PIC X(25).
222        03  EST-PAGO-ARP.
222            05  EST-ARP-FORPAGO               PIC XX.
224            05  EST-ARP-CASACTA               PIC XXX.
227            05  EST-ARP-TIPCTA                PIC XX.
229            05  EST-ARP-CTABCO                PIC 9(15)  COMP-3.
222        03  EST-PAGO-PAT   REDEFINES EST-PAGO-ARP.
222            05  EST-PAT-FORPAGO               PIC XX.
224            05  EST-PAT-CASACTA               PIC XXX.
227            05  EST-PAT-TIPCTA                PIC XX.
229            05  EST-PAT-CTABCO                PIC 9(15)  COMP-3.
237        03  EST-PAGO-USD.
237            05  EST-USD-FORPAGO               PIC XX.
239            05  EST-USD-CASACTA               PIC XXX.
242            05  EST-USD-TIPCTA                PIC XX.
244            05  EST-USD-CTABCO                PIC 9(15)  COMP-3.
252        03  EST-PERSONA                       PIC X(20).
272        03  EST-CONSOLIDADA-1                 PIC X.
273        03  EST-HABIL-PLAN                    PIC X.
               88  EST-HABIL-PLAN-CD                        VALUE '1'.
274        03  EST-ID-MADRE                      PIC X.
               88  EST-MADRE-COTO                           VALUE 'C'.
               88  EST-MADRE-NORTE                          VALUE 'N'.
275        03  EST-AUT-FULL-BANDA                PIC X(01).
               88  EST-NO-APLICA             VALUE 'N' ' ' LOW-VALUE.
               88  EST-SOLO-BANDA-U-OPER     VALUE 'S'.
               88  EST-SOLO-BANDA-U-OPEREX   VALUE 'F'.
               88  EST-SOLO-BANDA-U-OPERNAC  VALUE 'R'.
               88  EST-SOLO-BANDA            VALUE 'D'.
               88  EST-BANDA-MANUAL-OPEREX   VALUE 'E'.
               88  EST-BANDA-MANUAL-IDENTIF  VALUE 'I'.
               88  EST-ECOMMERCE-VBV         VALUE 'V'.
276        03  EST-CONSOLIDADA-ETAPA             PIC X(01).
277        03  EST-RUBVISA                       PIC 9(8).
277        03  FILLER  REDEFINES  EST-RUBVISA.
277            05  FILLER                        PIC X(4).
281            05  EST-RUBRO.
281                07  EST-RUBRO-NUM             PIC 9(4).
285        03  EST-NUMEST-RUB                    PIC  9(8)  COMP.
289        03  EST-SHOPPING                      PIC X.
               88  EST-SERV-AGUA                           VALUE 'A'.
               88  EST-PATIO-BULLRICH                      VALUE 'B'.
               88  EST-SHOPPING-CABALLITO                  VALUE 'C'.
               88  EST-SHOPPING-ADROGUE                    VALUE 'D'.
               88  EST-ESSO-PURCHASING                     VALUE 'E'.
               88  EST-GALERIAS-PACIFICO                   VALUE 'F'.
               88  EST-SERV-GAS                            VALUE 'G'.
               88  EST-SHOPPING-LINIERS                    VALUE 'H'.
               88  EST-AGENTES-PAGO                        VALUE 'I'.
               88  EST-SOLO-COMPRAS                        VALUE 'J'.
               88  EST-SERV-LUZ                            VALUE 'L'.
               88  EST-MAYORISTA-MINORISTA                 VALUE 'M'.
               88  EST-LOMAS-CENTER                        VALUE 'N'.
               88  EST-PURCHASING                          VALUE 'P'.
               88  EST-LA-RURAL                            VALUE 'R'.
               88  EST-SOLEIL                              VALUE 'S'.
               88  EST-SERV-TELEFONO                       VALUE 'T'.
               88  EST-UNICENTER                           VALUE 'U'.
               88  EST-SHOP-GENERAL                        VALUE 'X'.
               88  EST-NO-ES-SHOPPING                      VALUE ' '.
290        03  EST-COBRANDING.
290            05  EST-COBRANDING-TIPO           PIC X.
                   88  EST-NO-ES-COBRANDING                VALUE ' '.
                   88  EST-COBRANDING-UNICENTER            VALUE 'U'.
291            05  EST-COBRANDING-GRUPO          PIC X.
                   88  EST-NO-ES-COBRANDING-GRUPO          VALUE ' '.
292        03  EST-CONTROL-CVV2                  PIC X(1).
               88  EST-NO-CONTROLA-CVV2   VALUE '*'.
               88  EST-TX-MANUAL-CON-CVV2 VALUE 'V'.
293        03  EST-PORCDTO-DEBITO                PIC S9(3)V99 COMP-3.
296        03  EST-MCA-MOTO                      PIC X.
               88  EST-NO-ES-MOTO                      VALUE ' '.
               88  EST-ES-MOTO                         VALUE '1' '2'
                                                             '3' '4'.
               88  EST-ES-E-COMMERCE                   VALUE '5' '6'
                                                             '7' '8'.
297        03  EST-SUC-BAPRO                     PIC S9(5) COMP-3.
300        03  EST-NRO-PROCESO                   PIC S9(7) COMP-3.
304        03  EST-FORPAGANT                     PIC X(4).
308        03  EST-LIMVTA                        PIC 99.
310        03  EST-PORCDTO                       PIC 9(3)V99.
315        03  EST-FALTA.
315            05  EST-DDALTA                    PIC 99.
317            05  EST-MMALTA                    PIC 99.
319            05  EST-AAALTA                    PIC 99.
321        03  EST-CODEQ                         PIC X(3).
               88  EST-EQ-ECADAT                       VALUE 'ARD'.
               88  EST-EQ-ARIGITAL                     VALUE 'RGT'.
               88  EST-EQ-VISA-ARG                     VALUE 'XLJ'.
324        03  EST-AFF-GROUP                     PIC 9(4).
328        03  EST-ESTADO                        PIC X(4).
332        03  EST-FBAJA.
332            05  EST-DDBAJA                    PIC 99.
334            05  EST-MMBAJA                    PIC 99.
336            05  EST-AABAJA                    PIC 99.
338        03  EST-CLASE-BOLETIN                 PIC X.
               88  EST-BOLE-GENERAL                    VALUE ' '.
               88  EST-BOLE-ESPECIAL                   VALUE 'E' 'I'.
               88  EST-BOLE-ESPECIAL-DIFERIDO          VALUE 'I'.
               88  EST-BOLE-BMR                        VALUE 'M'.
               88  EST-BOLE-REGIONAL                   VALUE 'R'.
339        03  EST-CICLO                         PIC 99.
341        03  EST-CANTCUO-HABIL                 PIC 99.
343        03  EST-MCA-TP                        PIC X.
               88  EST-VALIDA-TP-NO                    VALUE '0'.
JC9202         88  EST-VALIDA-TP-ACA                   VALUE '1'.
JB0012         88  EST-SHELL-SELF-SERVICE              VALUE '2'.
AM9208         88  EST-V-I-P                           VALUE '3'.
AM9208         88  EST-IBERO-ASISTENCIA                VALUE '4'.
JC9309         88  EST-VALIDA-TP-DUTY                  VALUE '5'.
JC9512         88  EST-CARREFOUR                       VALUE '6'.
MS9102         88  EST-VENTA-DIRECTA                   VALUE '7'.
JB0025         88  EST-MCDONALDS                       VALUE '8'.
344        03  EST-FORPAGO                       PIC X(4).
348        03  EST-ULTFPR.
348            05  EST-DDULTFPR                  PIC 99.
350            05  EST-MMULTFPR                  PIC 99.
352            05  EST-AAULTFPR                  PIC 99.
354        03  EST-ULTIMP                        PIC 9(13)V99.
369        03  EST-IMPUESTOS                     OCCURS 4.
               05  EST-CODIMP.
                   07  EST-PROVIMP               PIC X.
                   07  EST-IMPUESTO              PIC XX.
IND(4)         05  EST-DATOS-SEGMENTA-CUO  REDEFINES EST-CODIMP.
426                07  EST-TIPCARTA-CUOTAS       PIC X.
427                07  EST-TIPCARTA-PLAZOS       PIC X.
428                07  EST-TIPCARTA-TERM-BANELCO PIC X.
               05  EST-DATOS-INGBRU.
372                07  EST-NROIB.
372                    09  EST-NUMINSC               PIC 9(10).
                       09  EST-NUMINSF REDEFINES EST-NUMINSC PIC X(10).
382                    09  EST-FILLER-JURIS          PIC X(04).
372                07  EST-NUMIB REDEFINES EST-NROIB PIC 9(14).
386                07  EST-MCA-PERCEPCION        PIC X(1).
                       88  EST-RET-PERCEPCION        VALUE '0'.
                       88  EST-NO-RET-PERCEPCION     VALUE '1'.
387                07  EST-TIPCONT               PIC X(1).
               05  EST-DATOS-CUIT  REDEFINES EST-DATOS-INGBRU.
391                07  EST-C-U-I-T               PIC X(13).
391                07  FILLER      REDEFINES     EST-C-U-I-T.
391                    09  EST-CUIT-TIPDOC       PIC XX.
393                    09  EST-CUIT-GUION1       PIC X.
                       88  EST-CUIT-G1               VALUE '-'.
394                    09  FILLER                PIC X(8).
402                    09  EST-CUIT-GUION2       PIC X.
                       88  EST-CUIT-G2               VALUE '-'.
403                    09  EST-CUIT-DV           PIC X.
404                07  EST-ULT3DG                PIC 999   COMP-3.
406                07  EST-TIPCUIT               PIC X(1).
               05  EST-DATOS-GANANCIAS REDEFINES EST-DATOS-INGBRU.
410                07  EST-CUIT                  PIC X(13).
423                07  EST-PLANCUO-ANT03         PIC X.
424                07  FILLER                    PIC X.
425                07  EST-TCGCIAS               PIC X(1).
IND(4)         05  FILLER              REDEFINES EST-DATOS-INGBRU.
429                07  EST-FLIQANT.
429                    09  EST-AALIQANT          PIC 99.
431                    09  EST-MMLIQANT          PIC 99.
433                    09  EST-DDLIQANT          PIC 99.
435                07  EST-MODIF-041199.
435                    09  EST-DTO-041199        PIC 999V99.
440                    09  EST-PZO-041199        PIC 99.
442                07  EST-MCA-TOPE-DTODEB       PIC X.
443                07  EST-PLANVISA-03           PIC X.
444                07  EST-NO-TECNIF-NO-USAR     PIC X.
445        03  EST-MODALIDAD                     PIC X.
446        03  EST-TELEF-DDN                     PIC X(5).
451        03  EST-ID-CAPTURA                    PIC X.
               88  EST-TIENE-TERM-CAPTURA                   VALUE 'C'.
452        03  EST-NUMEST-ORIGEN                 PIC X(10).
462        03  EST-MCA-LEY25063                  PIC X.
               88  EST-EXENTO-L25063                        VALUE 'E'.
462        03  EST-MCA-LAPOS  REDEFINES  EST-MCA-LEY25063   PIC X.
               88  EST-LAPOS-WEB-OTROS                      VALUE 'O'.
               88  EST-LAPOS-WEB-PROPIOS                    VALUE 'P'.
               88  EST-LAPOS-CELULAR                        VALUE 'C'.
463        03  EST-MARCA-AGRO                    PIC X.
               88  EST-ES-AGRO                              VALUE 'A'.
               88  EST-TARJETA-REGALO                       VALUE 'R'.
               88  EST-SOLO-CARGA-POR-POS-TRR               VALUE 'B'.
463            88  EST-PHONECENTER                          VALUE 'T'.
463            88  EST-TRAVEL-ASSIST                        VALUE 'S'.
463            88  EST-DISTRIBUTION                         VALUE 'D'.
463        03  EST-ID-BONOS   REDEFINES  EST-MARCA-AGRO
                                                 PIC X.
               88  EST-OPERA-PATACON                        VALUE 'P'.
               88  EST-OPERA-CASHBACK                       VALUE 'C'.
463        03  EST-MARCA-AFIP REDEFINES  EST-MARCA-AGRO
                                                 PIC X.
               88  EST-AFIP-ADUANAS                         VALUE 'E'.
463        03  EST-MARCA-ADQCLI REDEFINES  EST-MARCA-AGRO
                                                 PIC X.
               88  EST-ADQUI-CLIENTES                       VALUE 'Q'.
               88  EST-ADQUI-CLIENTES-HSBC                  VALUE 'H'.
               88  EST-ADQUI-CLIENTES-HSBC-II               VALUE 'I'.
               88  EST-ADQUI-CLIENTES-HIPO                  VALUE 'J'.
               88  EST-ADQUI-CLIENTES-HIPO-II               VALUE 'K'.
               88  EST-ADQUI-CLIENTES-COMAFI-II             VALUE 'Y'.
               88  EST-ADQUI-CLIENTES-COMAFI                VALUE 'Z'.
               88  EST-ADQUI-CLIENTES-BBVA                  VALUE 'F'.
               88  EST-ADQUI-CLIENTES-PROA                  VALUE 'O'.
               88  EST-ADQUI-CLIENTES-PROA-1                VALUE 'M'.
               88  EST-ADQUI-CLIENTES-PROA-2                VALUE 'N'.
               88  EST-ADQUI-CLIENTES-PROA-3                VALUE 'W'.
               88  EST-ADQUI-CLIENTES-STAND                 VALUE 'G'.
               88  EST-ADQUI-CLIENTES-STAND-1               VALUE 'L'.
               88  EST-POSICION-463-LIBRES       VALUE 'U' 'V' 'X'.
464        03  EST-CODPROV1                      PIC X(1).
465        03  EST-CODCAR                        PIC 99.
467        03  EST-CANTEQ                        PIC 99.
469        03  EST-FINSTAL.
469            05  EST-DDINSTAL                  PIC 99.
471            05  EST-MMINSTAL                  PIC 99.
473            05  EST-AAINSTAL                  PIC 99.
475        03  EST-FFACT.
475            05  EST-DDFACT                    PIC 99.
477            05  EST-MMFACT                    PIC 99.
479            05  EST-AAFACT                    PIC 99.
481        03  EST-FUFACT.
481            05  EST-DDUFACT                   PIC 99.
483            05  EST-MMUFACT                   PIC 99.
485            05  EST-AAUFACT                   PIC 99.
487        03  EST-FBAJA-EQUIP.
487            05  EST-DDBAJA-EQUIP              PIC 99.
489            05  EST-MMBAJA-EQUIP              PIC 99.
491            05  EST-AABAJA-EQUIP              PIC 99.
487        03  EST-FALTA-PATACON   REDEFINES EST-FBAJA-EQUIP.
487            05  EST-DDALTA-PATACON            PIC 99.
489            05  EST-MMALTA-PATACON            PIC 99.
491            05  EST-AAALTA-PATACON            PIC 99.
493        03  EST-IMPPEND                       PIC S9(9)V99  COMP-3.
499        03  EST-MODEQ.
499            05  EST-BOL-MAXREC                PIC X(1).
500        03  EST-MARCA-GRP                     PIC X(1).
               88  EST-GRUPO-ABIERTO                   VALUE ' '.
               88  EST-GRUPO-CERRADO                   VALUE 'C'.
501        03  EST-MCA-BUSINESS                  PIC X.
               88  EST-ES-BUSINESS                     VALUE '1'.
502        03  EST-NUMCOM-MADRE                  PIC X(10).
512        03  EST-LIMVTA-ANT                    PIC X(02).
514        03  EST-MCA-TRANSFERENCIA             PIC X.
               88  EST-TPP-NO-HABILITADO               VALUE ' '.
               88  EST-TPP-B                           VALUE '1'.
               88  EST-TPP-BT                          VALUE '2'.
               88  EST-TPP-T                           VALUE '3'.
               88  EST-TPP-T-B                         VALUE '4'.
               88  EST-TPP-T-BT                        VALUE '5'.
               88  EST-TPP-INHABILITADO                VALUE '6'.
               88  EST-TPP-BAJA                        VALUE '7'.

               88  EST-TPP-TRANSFIEREN                 VALUE '3' '4'
                                                             '5'.
               88  EST-TPP-BENEFICIARIO                VALUE '1' '2'
                                                             '4' '5'.
               88  EST-TPP-TRANSF-BENEF                VALUE '4' '5'.
515        03  EST-MCA-TRANSACCIONES             PIC X.
               88  EST-NO-TRANSACCIONA                 VALUE 'N'.
516        03  EST-TOPE-VTA                      PIC 9(07).
523        03  EST-FBAJA-TPP.
523            05  EST-AABAJA-TPP                PIC 99.
525            05  EST-MMBAJA-TPP                PIC 99.
527            05  EST-DDBAJA-TPP                PIC 99.
529        03  EST-ALTA-PENDIENTE                PIC X(01).
               88  EST-ALTA-INMEDIATA                  VALUE 'S'.
               88  EST-ALTA-APROBADA                   VALUE 'A'.
               88  EST-ALTA-NORMAL                     VALUE ' '.
530        03  EST-ID-CARGO                      PIC XX.
               88  EST-CARGO-SIST-PROPIOS              VALUE '02'.
532        03  EST-BONIF-PROMO-DESTINAT          PIC 9(02)V9.
535        03  EST-MK-ERES                       PIC X(01).
536        03  EST-PERMITE-BAJA                  PIC X(01).
               88  EST-SI-BAJA                         VALUE ' '.
               88  EST-NO-BAJA                         VALUE 'N'.
537        03  EST-VTA-PEI-GALICIA-RURAL         PIC X(01).
               88  EST-SI-VTA-PEI-GALICIA-RURAL        VALUE 'S'.
538        03  EST-PROMOCION-CUOTAS              PIC X(01).
               88  EST-SI-PROMOCION-CUOTAS             VALUE ' '.
               88  EST-NO-PROMOCION-CUOTAS             VALUE 'N'.
539        03  EST-BIN-PAGADOR-CERRADO.
539            05  EST-MARCA-BC                  PIC X(02).
                   88  EST-ES-BIN-CERRADO              VALUE 'BC'.
541            05  EST-PORC-CRD-BC               PIC 9(03)V999.
547            05  EST-PORC-DEB-BC               PIC 9(03)V999.
553            05  EST-FALTA-BC.
553                07  EST-FALTA-BC-AA           PIC 9(04).
557                07  EST-FALTA-BC-MM           PIC 9(02).
559                07  EST-FALTA-BC-DD           PIC 9(02).
561        03  EST-PORC-BONIF-CARGO-TR           PIC 9(03)V999.
567        03  EST-FORZAR-PEX                    PIC X(01).
               88  EST-FORZAR-PEX-SI                   VALUE 'S'.
               88  EST-FORZAR-PEX-NO                   VALUE ' ' 'N'.
568        03  EST-MARCA-CARTAS                  PIC X(03).
571        03  EST-SEGMENTO-IVR                  PIC X(01).
572        03  EST-COD-TARIFA-CARGO              PIC X(02).
               88  EST-NO-LIQUIDA-CARGO                VALUE '00'.
               88  EST-TARIFA-GRAL                     VALUE '01'.
574        03  EST-MCA-MICROP                    PIC X(01).
575        03  EST-RECATEG-CUIT                  PIC X(01).
576        03  EST-AFIP-DEVOL-IVA                PIC X(2).
               88  EST-DEVUELVE-IVA                    VALUE '00'.
               88  EST-EXCEP-MONOT                     VALUE '09'.
               88  EST-EXCEP-CUIT                      VALUE '07'.
               88  EST-EXCEP-RUBRO                     VALUE '03'.
               88  EST-EXCEP-CODPOS                    VALUE '08'.
578        03  EST-ACUERDO-COMERCIAL             PIC X(01).
               88 EST-CON-ACUERDO-COM                  VALUE 'A'.
               88 EST-CON-ACUERDO-COM-0306             VALUE 'B'.
               88 EST-CON-ACUERDO-COM-0312             VALUE 'C'.
               88 EST-CON-ACUERDO-COM-BRID             VALUE 'D'.
               88 EST-CON-ACUERDO-COM-TURISMO          VALUE 'E'.
               88 EST-CON-ACUERDO-COM-CARR-NOR         VALUE 'F'.
               88 EST-CON-ACUERDO-COM-FARMACITY        VALUE 'G'.
               88 EST-CON-ACUERDO-COM-COTO             VALUE 'H'.
               88 EST-CON-ACUERDO-COM-DIADTO           VALUE 'I'.
               88 EST-CON-ACUERDO-COM-DISCO-CAP        VALUE 'J'.
               88 EST-CON-ACUERDO-COM-LA-GALLEGA       VALUE 'K'.
               88 EST-CON-ACUERDO-COM-GARBACOMPU       VALUE 'L'.
               88 EST-CON-ACUERDO-COM-WALLMART         VALUE 'M'.
               88 EST-CON-ACUERDO-COM-PROCAMPO         VALUE 'N'.
               88 EST-CON-ACUERDO-COM-GAS              VALUE 'O'.
               88 EST-CON-ACUERDO-COM-PASALA           VALUE 'P'.
               88 EST-CON-ACUERDO-DISCO-VEA            VALUE 'Q'.
               88 EST-SIN-ACUERDO-COM                  VALUE ' '.
579        03  EST-FEC-EMISION.
579            05  EST-DD-EMISION                PIC 99.
581            05  EST-MM-EMISION                PIC 99.
583            05  EST-AA-EMISION                PIC 99.
585        03  EST-AXA-NO-DEPURAR                PIC X(01).
586        03  EST-ALIC-DBLQE-X-TERCEROS         PIC 99V999.
586        03  EST-ALIC-DBLQE-CARGOS       REDEFINES
               EST-ALIC-DBLQE-X-TERCEROS         PIC 99V999.
591        03  EST-BK-MCA-LIQ-ANTICIPADA         PIC X(01).
592        03  EST-MSG-LEY-TARJETA               PIC X(01).
593        03  EST-BK-PORCDTO                    PIC 9(3)V99.
598        03  EST-BK-PORCDTO-DEBITO             PIC 9(3)V99.
603        03  EST-MCA-SEGURIDAD-FOLLETO         PIC X(01).
               88  EST-SG-ENVIO-FOLLETO                     VALUE 'S'.
604        03  EST-MCA-SEGURIDAD-LIQ-ANT         PIC X(01).
               88  EST-SG-NO-HABILITA-LIQ-ANT               VALUE 'N'.
               88  EST-SG-HABILITA-LIQ-ANT                  VALUE 'S'.
605        03  EST-IBRUTOS                       PIC X(01).
               88  EST-REG-SIMPLIF-IBCF                     VALUE 'S'.
606        03  EST-IMPUESTO-DB-CR                PIC X(01).
               88  EST-APLICA-IMP-DB-CR                     VALUE 'S'.
607        03  EST-DIAS-PAGO-TAR-REGALO          PIC 9(03).
610        03  EST-PORCDTO-TAR-REGALO            PIC 9(03)V99.
615        03  EST-MCA-PACTADO                   PIC X(01).
               88  EST-ES-PACTADO                         VALUE 'P'.
616        03  EST-CUIT-CLAVE                    PIC X(13).
629        03  EST-NUMEST-CUIT                   PIC  9(8)  COMP.
633        03  EST-MCA-GNC                       PIC X(01).
               88  EST-ES-GNC                             VALUE 'G'.
634        03  EST-INHABIL-AUT-X-SEG           PIC X(01).
634            88 EST-INH-AUTORIZAR              VALUE 'S'.
634            88 EST-INH-AUT-COMERCIOS          VALUE 'C'.
634            88 EST-INH-AUT-BANCO              VALUE 'B'.
634            88 EST-HABILITADO                 VALUE ' ' LOW-VALUE.
635        03  EST-TRASLADA-LEYENDA-MENS         PIC X(01).
635            88 EST-TRASLADA-LEYENDA               VALUE 'S'.
636        03  EST-ALICUOTA-PBSAS.
636            05  EST-ALI-PBSAS                 PIC 9V99.
639        03  EST-MCA-PERCEP-ANT                PIC X(1).
640        03  EST-MCA-3CC                       PIC X(01).
               88  EST-ES-3CC                             VALUE 'C'.
641        03  EST-FULTMOD.
641            05  EST-DDULTMOD                  PIC 9(2).
643            05  EST-MMULTMOD                  PIC 9(2).
645            05  EST-AAULTMOD                  PIC 9(2).
647        03  EST-HORA-ULTMOD.
647            05  EST-HHULTMOD                  PIC 9(2).
649            05  EST-MIULTMOD                  PIC 9(2).
651            05  EST-SSULTMOD                  PIC 9(2).
653        03  EST-PLAN-CUOTAS-ANT               PIC X.
654        03  EST-CANTCUO-HABIL-ANT             PIC 99.
656        03  EST-ID-AFILIACION                 PIC X.
               88  EST-DOBLE-AFILIACION-ORIGEN            VALUE 'O'.
               88  EST-DOBLE-AFILIACION                   VALUE 'P'.
657        03  EST-BK-TIPCONT-IB                 PIC X.
658        03  EST-BK-IBRUTOS-RS                 PIC X.
659        03  EST-FIRMANTES.
659           05 EST-FIRMANTE-1                  PIC 9(8).
667           05 EST-FIRMANTE-2                  PIC 9(8).
675           05 EST-FIRMANTE-3                  PIC 9(8).
683           05 EST-FIRMANTE-4                  PIC 9(8).
691        03  EST-MAIL                          PIC X(40).
731        03  EST-CONTRATO                      PIC X(1).
              88 EST-CONTRATO-PRISMA VALUE 'P'.
              88 EST-CONTRATO-VISA   VALUE 'V'.
              88 EST-CONTRATO-BANCO  VALUE LOW-VALUE ' ' 'B'.
              88 EST-CONTRATO-ADQ    VALUE LOW-VALUE 'A'.
732        03  EST-MVOS                          PIC X(02).
              88 EST-MVOS-DATOS-ADIC         VALUE 'A1'.
              88 EST-MVOS-DATOS-ADIC-CODGEO  VALUE 'A2'.
              88 EST-MVOS-CANALES            VALUE '01' THRU '99'.
734        03  EST-FIRMANTE-5                    PIC 9(8).
742        03  EST-FUNCIONARIO-MODIF             PIC X(8).
750        03  EST-INGRESO-MODIF                 PIC X(1).
               88  EST-INGRESO-M-HOST            VALUE 'H'.
               88  EST-INGRESO-M-WEB             VALUE 'W'.
               88  EST-INGRESO-M-PH              VALUE 'P'.
751        03  EST-ADMIN-ADQ                     PIC 9(3).
754        03  EST-BANDERA                       PIC 9(3).
757        03  EST-IDENTIFICADOR                 PIC X(15).
772        03  EST-MODALIDADES                   PIC X(8).
780        03  EST-CBU                           PIC X(22).
802        03  EST-ID-SOLIC                      PIC X(11).
813        03  EST-COD-AFIP                      PIC 9(6).
819        03  EST-MCA-P2P                       PIC X(1).
820        03  EST-ADHIERE-NORMA-PCI             PIC X(1).
821        03  EST-MCA-CONTACTLESS               PIC X(1).
822        03  EST-ACELERACION-PRISMA.
822            05 EST-ACEL-PRISMA-BCO            PIC X(3).
                  88 EST-ACEL-PRISMA-SI          VALUE '007' THRU '598'.
825            05 EST-ACEL-PRISMA-COD-TASA       PIC X(2).
                  88 EST-ACEL-PRISMA-COD-VALIDO  VALUE '10' THRU '99'.
825            05 EST-ACEL-PRISMA-COD-TNA        REDEFINES
                  EST-ACEL-PRISMA-COD-TASA       PIC 9(2).
827            05 EST-ACEL-PRISMA-TIPO-TRX       PIC X.
                  88  EST-ACEL-PRISMA-1PAGO      VALUE '1'.
                  88  EST-ACEL-PRISMA-PLAN-GOB   VALUE 'G'.
                  88  EST-ACEL-PRISMA-TODA-TX    VALUE 'T'.
                  88  EST-ACEL-PRISMA-NO-BONIF   VALUE 'N'.
                  88  EST-ACEL-PRISMA-CON-ACBA   VALUE 'B'.
                  88  EST-ACEL-PRISMA-SIN-ACBA   VALUE 'X'.
828        03  EST-MCA-AGRUPADOR                 PIC X(1).
829        03  EST-MCA-VPP                       PIC X(1).
830        03  EST-URL                           PIC X(50).
880        03  EST-MATCH                         PIC X(1).
                  88  EST-EST-NUEVO              VALUE 'A'.
                  88  EST-CONSULTA-ENV-ALT       VALUE 'B'.
                  88  EST-EST-BAJO               VALUE 'I'.
                  88  EST-CONSULTA-ENV-BAJ       VALUE 'F'.
881        03  EST-MCA-CAT-BCRA                  PIC X(2).
883        03  EST-ID-MKP-PF                     PIC X(11).
894        03  EST-MARCA-MKP-PF                  PIC X(1).
                  88  EST-PF                     VALUE 'P'.
                  88  EST-MKP                    VALUE 'M'.
895        03  EST-FALTA-AGRUP.
               05  EST-DDALTA-AGRUP              PIC 99.
               05  EST-MMALTA-AGRUP              PIC 99.
               05  EST-AAALTA-AGRUP              PIC 99.
901        03  EST-COD-GOB-PAIS                  PIC X(3).
904        03  EST-CATEG-RIESGOS                 PIC X(2).
906        03  EST-ACEL-PRISMA-PLAZO-PAGO        PIC X(1).
905        03  EST-SEGMENTO-COMER                PIC X(2).
907        03  EST-FILLER-RESTO                  PIC X(421).
1330       03  EST-FUNCIONARIO                   PIC X(8).
1338       03  EST-INGRESO                       PIC X(1).
               88  EST-INGRESO-HOST              VALUE ' ' 'H'.
               88  EST-INGRESO-WEB               VALUE 'W'.
1339       03  EST-ULTIMA-MODIFICA.
1339           05  EST-FECHA                     PIC 9(6).
1345           05  EST-HORA                      PIC X(6).
           EJECT                                                              99

       FD  MAECOM                                                             99
           LABEL RECORD        IS STANDARD                                    99
           BLOCK CONTAINS       0 RECORDS.                                    99
           COPY   MAECOM.                                                     99
           EJECT                                                              99

310100 FD  CONTDGI                                                            99
           RECORD 1500 CHARACTERS                                             99
           BLOCK  0.                                                          99
           COPY   DGI0055.                                                    99
           EJECT                                                              99

       FD  SALIDA
           RECORD 200 CHARACTERS                                              99
           BLOCK  0.                                                          99
       01  LINEA-SALIDA           PIC X(200).


250920 FD  CONTESL                                                            99
           RECORD 2000 CHARACTERS                                             99
           BLOCK  0.                                                          99
      * SE TOCO EL COPY PASANDO EL CONTL-NROIB(9 A X)
           COPY   CONTESZ.                                                    99



070600 FD  SALIDA6
070700     BLOCK 0.
070800 01  REG-SALIDA6    PIC X(1350).
070900
071000 FD  MOVSALT7                                                           02
071100     BLOCK  0.
071200 01  REG-MOVSALT7.
071300     03  FILLER               PIC X(17).
071400     03  MOVSALT7-FPAG.
071500         05  MOVSALT7-DDPAG   PIC 99.
071600         05  MOVSALT7-MMPAG   PIC 99.
071700         05  MOVSALT7-AAPAG   PIC 99.
071800     03  FILLER               PIC X(73).
071900     03  MOVSALT7-FPRES.
072000         05  MOVSALT7-DDPRES  PIC 99.
072100         05  MOVSALT7-MMPRES  PIC 99.
072200         05  MOVSALT7-AAPRES  PIC 99.
072300     03  FILLER               PIC X(31).
072400     03  MOVSALT7-IMPORTE     PIC S9(13)V99.
072500     03  FILLER               PIC X(21).
072600     03  MOVSALT7-DEALER      PIC 9(10) COMP-3.
072700     03  FILLER               PIC X(89).
072800     03  MOVSALT7-FVALOR      PIC 9(7)  COMP-3.
072900     03  FILLER               PIC X(11).
073000     03  MOVSALT7-TERM-CAPTURA    PIC X.
073100     03  FILLER               PIC X.
073200     03  MOVSALT7-MCA-EDC     PIC X(01).
073300     03  FILLER               PIC X(71).
073400     03  MOVSALT7-LIQEST-PENDIENTE  PIC XXX.
073500     03  FILLER               PIC X(42).
073600     03  MOVSALT7-SERGRAB-X   PIC XX.
073700
073800     03  FILLER               PIC X(205).
073900     03  MOVSALT7-IMPORTE-GRAV    PIC S9(11)V99 COMP-3.
074000     03  FILLER               PIC X(38).
074100     EJECT
074200
074300 FD  TABLAF   RECORD 300.
074400 01  REG-TABLAF.
074500     03  TBF-CLAVE.
074600         05  TBF-IDENTAB   PIC  9(03).
074700         05  FILLER        PIC  X(11).
074800     03  FILLER            PIC  X(286).
074900
       WORKING-STORAGE SECTION.
       01  W-IDENTAB           PIC 9(3).

       01  W-EST-ESTADO        PIC X(4).

       COPY MOVMESA.

074400 COPY D532076.

086700 01  W-FECHA-VALOR        PIC 9(8).
086800 01  W-026-EMIS-LIQ-NEGAT   PIC X.
086900 01  W-LINEA-CP             PIC XXX       VALUE SPACES.
087000 01  W-CARGO-TOTAL          PIC S9(15)V99 COMP-3 VALUE +0.

       01  TABLA-BCOS-ACCIONISTAS.                                            99
           03  W026-ELEM   OCCURS 1 TO 500 TIMES                              99
                           DEPENDING ON ODO026
                           ASCENDING   KEY   W026-IDENBCO                     99
                           INDEXED BY  I026.                                  99
               05  W026-IDENBCO          PIC XXX.                             99
               05  W026-DENBCO           PIC X(35).                           99
               05  W026-EMIS             PIC 9.                               99

       01  TABLA-BCOS-PARTICIPANTES.                                          99
110320     03  W027-ELEM   OCCURS 1 TO 50000 TIMES                            99
                           DEPENDING ON ODO027
                           ASCENDING   KEY   W027-IDENCASA                    99
                           INDEXED BY  I027.                                  99
               05  W027-IDENBCO          PIC XXX.                             99
               05  W027-IDENCASA         PIC XXX.                             99
               05  W027-MARCA-IB         PIC XX.                              99
P.BSAS         05  W027-CODPROV          PIC X.                               99
               05  W027-DENCASA          PIC X(35).                           99
               05  W027-CALLE            PIC X(21).                           99
               05  W027-PUERTA           PIC 9(5).                            99
               05  W027-POST-COD         PIC X(4).                            99
               05  W027-RESP-A-RET       PIC X(40).                           99
               05  W027-RESP-CARACTER    PIC X(6).                            99
               05  W027-CUIT             PIC X(13).                           99
                                                                              99
       01  ODO026                        PIC 9(3) VALUE 0.
       01  ODO027                        PIC 9(5) VALUE 0.

       77  BANCO-ANTERIOR           PIC 999 VALUE 0.
       77  CASA-ANTERIOR            PIC 999 VALUE 0.
       77  ESTAB-ANTERIOR           PIC 9(10) VALUE 0.
       77  TOTAL-BANCO              PIC S9(13)V99 VALUE 0.
       77  TOTAL-CASA               PIC S9(13)V99 VALUE 0.
       77  TOTAL-ESTAB              PIC S9(13)V99 VALUE 0.
       77  TOTAL-GENERAL            PIC 9(13)V99  VALUE 0.
       77  CONT-MOVMMCLA            PIC 9(9) VALUE 0.
       77  CONT-MAEEST              PIC 9(9) VALUE 0.
       77  CONT-MAECOM              PIC 9(9) VALUE 0.
       77  CONT-CONTDGI             PIC 9(9) VALUE 0.
       77  CONT-CONTESL             PIC 9(9) VALUE 0.
080000 77  CONT-SALIDA6             PIC 9(9) VALUE 0.
080100 77  CONT-MOVSALT7            PIC 9(9) VALUE 0.

       01  W-BANCO-DATOS.
           03  W-BANCO-NOMBRE           PIC X(40).
           03  W-BANCO-CODIGO           PIC 9(3).

       01  W-CASA-DATOS.
           03  W-CASA-NOMBRE            PIC X(35).
           03  W-CASA-DIRECCION.
               05  W-CASA-CALLE         PIC X(21).
               05  W-CASA-PUERTA        PIC 9(5).
               05  W-CASA-COD-POST      PIC X(4).

       01  W-ESTAB-DATOS.
           03  W-ESTAB-CODIGO           PIC 9(10).
           03  W-ESTAB-NOMBRE           PIC X(40).

       01  W-COMERCIO-DATOS.
           03  W-COMERCIO-CODIGO        PIC 9(10).
           03  W-COMERCIO-RAZ-SOC       PIC X(30).

       01  IMPORTE-FORMATEADO       PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.

       01  FIN-MOVMMCLA             PIC X VALUE 'N'.
           88 EOF-MOVMMCLA          VALUE 'Y'.

       01  FIN-TAB                  PIC X VALUE 'N'.
           88 EOF-TAB               VALUE 'Y'.

       01  FIN-MAEEST               PIC X VALUE 'N'.
           88 EOF-MAEEST            VALUE 'Y'.

       01  W-MAEEST.
           03 W-EST-ADMIN         PIC 9(3).

083700 01  W-FILE-STATUS.
083800     03  FS-MCLA            PIC XX VALUE SPACES.
083900     03  FS-TAB             PIC XX VALUE SPACES.
084000     03  FS-SAL             PIC XX VALUE SPACES.
084100     03  FS-CONTESL         PIC XX VALUE SPACES.
084200     03  FS-CONTDGI         PIC XX VALUE SPACES.
084300     03  FS-MAEEST          PIC XX VALUE SPACES.
084400     03  FS-MAECOM          PIC XX VALUE SPACES.
084500     03  FS-SALIDA6         PIC XX VALUE SPACES.
084600     03  FS-MOVSALT7        PIC XX VALUE SPACES.
084700     03  FS-TABLAF          PIC XX VALUE SPACES.
084800

       01  W-IVA-TODOS.
           05 W-IVA-1 PIC S99V99 VALUE 21.
           05 W-IVA-2 PIC S99V99 VALUE 10,5.
           05 W-IVA-3 PIC S99V99 VALUE 3.

       01  W-IVA-CARGO-EST.
           05 W-IVA-1-CARGO-EST PIC S9(11)V99 COMP-3 VALUE 0.
           05 W-IVA-2-CARGO-EST PIC S9(16)V99 VALUE 0.
           05 W-IVA-3-CARGO-EST PIC S9(16)V99 VALUE 0.

       01  W-MOV-CARGO          PIC S9(16)V99 VALUE 0.
       01  W-MOV-IMPORTE        PIC S9(16)V99 VALUE 0.
       01  W-AUX-MOV-IMPORTE    PIC S9(16)V99 VALUE 0.
       01  W-MOV-BONIF          PIC S9(16)V99 VALUE 0.

       01  W-MOV-PERCEPCION      PIC S9(16)V99 VALUE 0.                       99
       01  W-IMPORTE-TOTAL       PIC S9(16)V99 VALUE 0.
       01  W-TOTAL-PERCEPCIONES  PIC S9(16)V99 VALUE 0.
       01  MINIMO-PERCEP-IVA PIC 9(2)V99   VALUE 60.

       77  W-NRO-LIQ       PIC S9(11) COMP-3  VALUE  900000.

       01  W-EST-FORPAGO.
           88  W-EST-FORPAGO-CUENTA  VALUE '0008' '0009' '0011' '0012'.
           88  W-EST-FORPAGO-TRANSF-BCRIA     VALUE '0008' '0009'.
               05  W-EST-FORPA0      PIC  XX  VALUE ZEROS.
               05  W-EST-FORPAG      PIC  XX  VALUE ZEROS.
                   88  W-EST-FORPAG-CUENTA  VALUE '08' '09' '11' '12'.

280601 01  W-EST-CONSOLIDADA-ETAPA     PIC X   VALUE '0'.                     99
010801     88  W-EST-ETAPA-DISCONTINUA         VALUE '1'                      99
030901                                               '2'                      99
011001                                               '3'                      99
011101                                               '4'.                     99

       01 L-PARAMETRO.
           03  L-LONG              PIC S9(4) COMP.
           03  L-LIQ               PIC X VALUE '1'.
               88  L-LIQ-EST                 VALUE '1' '2' '3'.
               88  L-LIQARP-EST              VALUE '1'.
               88  L-LIQDOL-EST              VALUE '2'.
               88  L-LIQPAT-EST              VALUE '3'.
               88  L-LIQ-SIN-DECIMALES       VALUE '0'.
               88  L-LIQ-CON-DECIMALES       VALUE '1' '2' '3'.
               88  L-LIQ-ARP                 VALUE '1'.
               88  L-LIQ-DOL                 VALUE '2'.
           03  L-DIAS              PIC XX    VALUE '05'.
               88  L-LIQ-3DIAS               VALUE '03'.
               88  L-LIQ-5DIAS               VALUE '05'.
           03  L-MODO              PIC XX    VALUE 'CG'.
               88  L-LIQ-ORDEN-PAGO          VALUE 'OP'.
               88  L-LIQ-NOTA-CREDITO        VALUE 'NC'.
               88  L-LIQ-PAGO-EXPRESO        VALUE 'PE'.
               88  L-LIQ-BANCO               VALUE 'LB'.
               88  L-LIQ-CARGOS              VALUE 'CG'.
               88  L-LIQ-PEX-BCO             VALUE 'PE' 'LB'.
           EJECT

       01   W-IDENTIF-LIQ PIC X(3) VALUE 'EST'.
       01   NRO-ITEM  PIC S9(4) COMP.

       PROCEDURE DIVISION.
       0000-PROCESO.
           PERFORM 0500-ABRIR-ARCHIVOS
           PERFORM 1000-CARGAR-TABLAS

      ******************************************************************
      *           MAINLINE                                             *
      ******************************************************************
           PERFORM 2000-LEER-MOVMMCLA
           PERFORM 2500-INICIALIZACION

           PERFORM UNTIL EOF-MOVMMCLA
             MOVE SPACES TO LINEA-SALIDA
             EVALUATE TRUE
               WHEN MOV-CODBCO NOT = MOV-A-CODBCO
                   PERFORM 3300-CORTE-ESTAB
                   PERFORM 3200-CORTE-CASA
                   PERFORM 3100-CORTE-BANCO
                   PERFORM 5100-IMPRIMIR-BANCO
                   PERFORM 5200-IMPRIMIR-CASA
                   PERFORM 5300-IMPRIMIR-ESTAB

               WHEN MOV-CODCASA NOT = MOV-A-CODCASA
                   PERFORM 3300-CORTE-ESTAB
                   PERFORM 3200-CORTE-CASA
                   PERFORM 5200-IMPRIMIR-CASA
                   PERFORM 5300-IMPRIMIR-ESTAB

               WHEN MOV-NUMEST NOT = MOV-A-NUMEST
                   PERFORM 3300-CORTE-ESTAB
                   PERFORM 5300-IMPRIMIR-ESTAB

               WHEN OTHER CONTINUE
             END-EVALUATE

             PERFORM 3350-CALCULAR-CARGO

             ADD MOV-IMPORTE TO TOTAL-ESTAB
             PERFORM 4000-IMPRIMIR-REGISTRO

      *      INITIALIZE W-IVA-CARGO-EST

             PERFORM 2000-LEER-MOVMMCLA
           END-PERFORM

           PERFORM 5400-GENERAR-ARCHI-X-ESTAB
           PERFORM 5500-GRABAR-ARCHI-X-ESTAB

           PERFORM 6400-IMPRIMIR-TOTALES-FINALES
           PERFORM 0700-CERRAR-ARCHIVOS
           GOBACK.

      ******************************************************************
      *                       FIN MAINLINE                             *
      ******************************************************************

       0500-ABRIR-ARCHIVOS.
095600     OPEN INPUT  MOVMMCLA TABLAS MAEESTVS MAECOM TABLAF
095700          OUTPUT SALIDA CONTESL CONTDGI SALIDA6 MOVSALT7
095800
095900     IF W-FILE-STATUS NOT = '00000000000000000000'
096000         DISPLAY 'ERROR AL ABRIR ARCHIVO ' W-FILE-STATUS
096100         GOBACK
096200     END-IF

           MOVE SPACES TO LINEA-SALIDA.

       0700-CERRAR-ARCHIVOS.
096700     CLOSE MOVMMCLA TABLAS MAEESTVS MAECOM SALIDA TABLAF
096800           CONTESL CONTDGI SALIDA6 MOVSALT7.

       1000-CARGAR-TABLAS.
           INITIALIZE T026-CLAVE
           MOVE '026' TO T026-IDENTAB
           START TABLAS      KEY NOT  LESS    T026-CLAVE.                     99
           READ TABLAS NEXT
                       AT END SET EOF-TAB TO TRUE
           END-READ
           SET I026 TO 1
           PERFORM UNTIL EOF-TAB OR T026-IDENTAB NOT = '026'
             ADD 1 TO ODO026
    1        MOVE  T026-IDENBCO    TO W026-IDENBCO(I026)
    4        MOVE  T026-DENBCO     TO W026-DENBCO(I026)
    96       MOVE  T026-CARGO-EMIS TO W026-EMIS(I026)


             READ TABLAS NEXT
                         AT END SET EOF-TAB TO TRUE
             END-READ
             SET   I026 UP BY 1
           END-PERFORM

           INITIALIZE T027-CLAVE
           MOVE '027' TO T027-IDENTAB
           START TABLAS      KEY NOT  LESS    T027-CLAVE.                     99
           READ TABLAS NEXT
                       AT END SET EOF-TAB TO TRUE
           END-READ
           SET I027 TO 1
           PERFORM UNTIL EOF-TAB OR T027-IDENTAB NOT = '027'
             ADD 1 TO ODO027
             MOVE T027-IDENCASA          TO W027-IDENCASA(I027)
             MOVE T027-DENCASA           TO W027-DENCASA(I027)
             MOVE T027-RESP-A-RET        TO W027-RESP-A-RET(I027)
             MOVE T027-CALLE             TO W027-CALLE(I027)
             MOVE T027-PUERTA            TO W027-PUERTA(I027)
             MOVE T027-POST-COD          TO W027-POST-COD(I027)

             READ TABLAS NEXT
                         AT END SET EOF-TAB TO TRUE
             END-READ
             SET   I027 UP BY 1
           END-PERFORM

016410     MOVE  SPACES          TO  TBF-CLAVE
016500     MOVE  076             TO  TBF-IDENTAB
016600     READ TABLAF END-READ
           DISPLAY '1 -> REG-TABLAF ' REG-TABLAF(21:6)
101240     MOVE  REG-TABLAF TO REG-T076
           DISPLAY '2 -> REG-T076   ' T076-FPRES.




       2000-LEER-MOVMMCLA.
           READ MOVMMCLA
               AT END SET EOF-MOVMMCLA   TO TRUE
               NOT AT END
                   ADD 1 TO CONT-MOVMMCLA
           END-READ.

       2500-INICIALIZACION.
           IF NOT EOF-MOVMMCLA
               MOVE REG-MOVMES  TO REG-MOVMES-ANT
               PERFORM 2600-TRATAR-PRIMER-REG
           END-IF.

       2600-TRATAR-PRIMER-REG.
               PERFORM 4100-LEER-BANCO
               PERFORM 4200-LEER-CASA
               PERFORM 4300-LEER-ESTAB
               PERFORM 4350-LEER-COM
               PERFORM 5100-IMPRIMIR-BANCO
               PERFORM 5200-IMPRIMIR-CASA
               PERFORM 5300-IMPRIMIR-ESTAB
               PERFORM 3350-CALCULAR-CARGO
               PERFORM 5400-GENERAR-ARCHI-X-ESTAB.

       3100-CORTE-BANCO.
           PERFORM 6100-IMPRIMIR-TOTAL-BANCO
           MOVE ZERO           TO TOTAL-BANCO
           PERFORM 4100-LEER-BANCO.

       3200-CORTE-CASA.
           PERFORM 6200-IMPRIMIR-TOTAL-CASA
           ADD TOTAL-CASA      TO TOTAL-BANCO
           MOVE ZERO           TO TOTAL-CASA
           PERFORM 4200-LEER-CASA.

       3300-CORTE-ESTAB.
           PERFORM 3400-TERMINAR-ESTAB
           MOVE REG-MOVMES TO REG-MOVMES-ANT
           PERFORM 4300-LEER-ESTAB
           PERFORM 4350-LEER-COM.

       3350-CALCULAR-CARGO.
             INITIALIZE W-IVA-CARGO-EST

             IF EST-C-U-I-T(2) = LOW-VALUES OR
                EST-C-U-I-T(2) = '00-00000000-0'
               MOVE 'X' TO EST-TIPCUIT(2)
             END-IF

             IF MOV-EDC-EST OR MOV-EDC-SOLO-EST
                MOVE MOV-IMPORTE TO W-MOV-CARGO W-MOV-IMPORTE
             ELSE
                MOVE MOV-IMPORTE TO W-MOV-BONIF
                SUBTRACT W-MOV-BONIF FROM W-MOV-CARGO
                         GIVING W-MOV-IMPORTE
             END-IF

             COMPUTE W-IVA-1-CARGO-EST =
                     W-MOV-IMPORTE * W-IVA-1 / 100

             EVALUATE TRUE
               WHEN EST-TIPCUIT(2) = SPACES
                 MOVE W-MOV-IMPORTE TO W-MOV-PERCEPCION

                 COMPUTE W-IVA-3-CARGO-EST =
                         W-MOV-PERCEPCION * W-IVA-3 / 100

                 IF W-IVA-3-CARGO-EST <= 60
                   MOVE 0 TO W-IVA-3-CARGO-EST
                 END-IF

               WHEN EST-TIPCUIT(2) = 'A'
                 OR EST-TIPCUIT(2) = 'M'
                 OR EST-TIPCUIT(2) = 'S'
                 CONTINUE

      *        SI ES N LO USA COMO IVA Y SI ES X COMO PERCEPCION
               WHEN OTHER
                 ADD W-IVA-1-CARGO-EST TO W-MOV-IMPORTE
                                          GIVING W-AUX-MOV-IMPORTE
                 COMPUTE W-IVA-2-CARGO-EST =
                 W-AUX-MOV-IMPORTE * W-IVA-2 / 100

             END-EVALUATE

      *      POR LO MENOS UN CAMPO VA A VENIR CON 0, POR ESO SE SUMA
             ADD W-IVA-2-CARGO-EST W-IVA-3-CARGO-EST
                 GIVING W-TOTAL-PERCEPCIONES

             ADD W-MOV-IMPORTE W-IVA-1-CARGO-EST
                               W-IVA-2-CARGO-EST
                               W-IVA-3-CARGO-EST
                  GIVING       W-IMPORTE-TOTAL.

       3400-TERMINAR-ESTAB.
           PERFORM 6300-IMPRIMIR-TOTAL-ESTAB
           ADD TOTAL-ESTAB TO TOTAL-CASA
           MOVE ZERO       TO TOTAL-ESTAB
112300     PERFORM 6500-GENERA-MOVSALT7
           PERFORM 5400-GENERAR-ARCHI-X-ESTAB
           PERFORM 5500-GRABAR-ARCHI-X-ESTAB.

       4000-IMPRIMIR-REGISTRO.
           MOVE 'REGISTRO:'        TO LINEA-SALIDA(2:9)
           MOVE CONT-MOVMMCLA      TO LINEA-SALIDA(11:9)
           MOVE MOV-IMPORTE        TO IMPORTE-FORMATEADO
           MOVE IMPORTE-FORMATEADO TO LINEA-SALIDA(21:20)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA.

       4100-LEER-BANCO.
           SEARCH ALL W026-ELEM
             WHEN  W026-IDENBCO(I026) EQUAL MOV-CODBCO
              MOVE  W026-DENBCO(I026)  TO W-BANCO-NOMBRE
           END-SEARCH.

       4200-LEER-CASA.
           SEARCH ALL W027-ELEM
             WHEN W027-IDENCASA(I027) EQUAL MOV-CODCASA
              MOVE W027-DENCASA(I027)     TO  W-CASA-NOMBRE
              MOVE W027-CALLE(I027)       TO  W-CASA-CALLE
              MOVE W027-PUERTA(I027)      TO  W-CASA-PUERTA
              MOVE W027-POST-COD(I027)    TO  W-CASA-COD-POST
           END-SEARCH.

       4300-LEER-ESTAB.
           MOVE '082'      TO ESTVS-CODSIS
           MOVE '001'      TO ESTVS-CODTAR
           MOVE '140'      TO ESTVS-CODADM
           MOVE MOV-NUMEST TO ESTVS-NUMEST

           READ MAEESTVS INVALID KEY
                DISPLAY 'NO SE ENCONTRO ESTVS-CLAVE ' ESTVS-CLAVE
           END-READ

           ADD 1 TO CONT-MAEEST

           MOVE ESTVS-DENEST TO W-ESTAB-NOMBRE
           MOVE ESTVS-NUMCOM TO W-COMERCIO-CODIGO
           MOVE EST-ESTADO TO W-EST-ESTADO.


       4350-LEER-COM.
           MOVE '082'        TO COM-CODSIS
           MOVE '001'        TO COM-CODTAR
           MOVE '140'        TO COM-CODADM
           MOVE ESTVS-NUMCOM TO COM-NUMCOM

           READ MAECOM   INVALID KEY
                DISPLAY 'NO SE ENCONTRO COM-NUMCOM ' COM-NUMCOM
           END-READ

           ADD 1 TO CONT-MAECOM

           MOVE COM-RAZSOC TO W-COMERCIO-RAZ-SOC.

       5100-IMPRIMIR-BANCO.
           MOVE 'BANCO: ' TO LINEA-SALIDA(2:7)
           MOVE MOV-CODBCO   TO LINEA-SALIDA(9:3)
           MOVE W-BANCO-NOMBRE TO LINEA-SALIDA(13:40)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA.

       5200-IMPRIMIR-CASA.
           MOVE 'CASA:  ' TO LINEA-SALIDA(2:7)
           MOVE MOV-CODCASA     TO LINEA-SALIDA(9:3)
           MOVE W-CASA-NOMBRE   TO LINEA-SALIDA(13:40)
           MOVE W-CASA-CALLE    TO LINEA-SALIDA(53:21)
           MOVE W-CASA-PUERTA   TO LINEA-SALIDA(75:5)
           MOVE W-CASA-COD-POST TO LINEA-SALIDA(81:4)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA.

       5300-IMPRIMIR-ESTAB.
           MOVE 'ESTAB: '          TO LINEA-SALIDA(2:7)
           MOVE MOV-NUMEST         TO LINEA-SALIDA(9:10)
           MOVE W-ESTAB-NOMBRE     TO LINEA-SALIDA(20:40)
           MOVE W-COMERCIO-CODIGO  TO LINEA-SALIDA(61:10)
           MOVE W-COMERCIO-RAZ-SOC TO LINEA-SALIDA(72:30)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA
           MOVE ALL '* ' TO LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA.

       5400-GENERAR-ARCHI-X-ESTAB.
           INITIALIZE REG-DGI0055 REG-CONTESL

           EVALUATE TRUE
             WHEN MOV-A-CODBCO = '914' AND EST-CODBCO = '014'
040899            MOVE EST-ADMIN TO W-EST-ADMIN                               99

C.E.P.       WHEN MOV-A-CODBCO NOT = EST-CODBCO AND                           99
C.E.P.            COM-CAMBIO-PAGADORA NOT = ZEROS  AND                        99
C.E.P.            COM-CAMBIO-PAGADORA NOT = SPACES AND                        99
C.E.P.            COM-CAMBIO-PAGADORA NOT = LOW-VALUES                        99
C.E.P.            MOVE COM-ADMIANT TO W-EST-ADMIN                             99

             WHEN OTHER
MESAL1            MOVE MOV-A-CODADM    TO W-EST-ADMIN                         99
           END-EVALUATE

           MOVE EST-FORPAGO TO W-EST-FORPAGO
      *    MOVE EST-CONSOLIDADA-ETAPA TO W-EST-CONSOLIDADA-ETAPA

PATACO     IF    W-EST-FORPAG-CUENTA                                          99
010801*       AND W-EST-ETAPA-DISCONTINUA                                     99
010801        AND (L-LIQ-ORDEN-PAGO                                           99
021008         OR  L-LIQ-PAGO-EXPRESO                                         99
190907         OR  L-LIQ-CARGOS)                                              99
010801        AND  NOT EST-BCO-PRIVADA                                        99
010801        AND  NOT EST-RETENER-PAGO                                       99
010801        AND  NOT EST-ES-AGRO                                            99
010801             MOVE  'D'    TO  CONTL-ID-DIARIA                           99
                                    REG-DGI0055(277:1)
010801     ELSE                                                               99
FFR03              MOVE  'V'    TO  CONTL-ID-DIARIA                           99
                                    REG-DGI0055(277:1)
           END-IF

      *    DISPLAY CONT-CONTESL ' CONT-CONTESL'
      *    DISPLAY W-EST-FORPAGO ' W-EST-FORPAGO'
      *    DISPLAY W-EST-CONSOLIDADA-ETAPA ' W-EST-CONSOLIDADA-ETAPA'
      *    DISPLAY L-PARAMETRO ' L-PARAMETRO'
      *    DISPLAY EST-CODBCO ' EST-CODBCO'
      *    DISPLAY EST-MCA-PAGO ' EST-MCA-PAGO'
      *    DISPLAY EST-MARCA-AGRO ' EST-MARCA-AGRO'
      *    DISPLAY ' '



           MOVE ZEROS TO CONTL-NUM-INGBRU DGI55-NUM-INGBRU
           MOVE MOV-A-NUMEST    TO DGI55-NUMEST     CONTL-NUMEST
           MOVE MOV-A-CASAEST   TO DGI55-CASAEST    CONTL-CASAEST
           MOVE MOV-A-BCOEST    TO DGI55-BCOEST     CONTL-BCOEST
           MOVE ESTVS-NUMCOM    TO DGI55-NUMCOM     CONTL-NUMCOM
           MOVE W-IMPORTE-TOTAL TO DGI55-IMPPAGAR   CONTL-IMPPAGAR
           MOVE MOV-A-COMIS     TO DGI55-PORRET     CONTL-PORRET
           MOVE EST-FORPAGO     TO DGI55-FORPAGO    CONTL-FORPAGO
           MOVE ESTVS-DENEST    TO DGI55-DENOM      CONTL-DENOM
           MOVE EST-ARP-CASACTA TO DGI55-CASACTA    CONTL-CASACTA
           MOVE EST-ARP-TIPCTA  TO DGI55-TIPCTA     CONTL-TIPCTA
           MOVE EST-ARP-CTABCO  TO DGI55-CTABCO     CONTL-CTABCO
           MOVE EST-NROIB(1)    TO DGI55-NROIB      CONTL-NROIB
           MOVE EST-BENEF       TO DGI55-BENEF-O-RAZSOC
                                   CONTL-RAZSOC
           MOVE EST-CODPROV2    TO DGI55-CODPROV1   CONTL-CODPROV1
           MOVE EST-CODIMP(1)   TO DGI55-CODIMP     CONTL-CODIMP
           MOVE EST-TIPCONT(1)  TO DGI55-TIPCONT    CONTL-TIPCONT
           MOVE MOV-A-FPAG      TO DGI55-FPAG       CONTL-FPAG
           MOVE MOV-A-CASAEST   TO DGI55-CASAPRES   CONTL-CASAPRES
           MOVE MOV-A-DIAS-PAGO TO DGI55-DIAS-PAGO  CONTL-DIAS-PAGO
           MOVE EST-C-U-I-T(2)  TO DGI55-C-U-I-T    CONTL-C-U-I-T
           MOVE EST-TIPCUIT(2)  TO DGI55-TIPCUIT    CONTL-TIPCUIT
           MOVE COM-ID-LIQ      TO REG-DGI0055(276:1) CONTL-ID-LIQ-VIP
           MOVE EST-TCGCIAS(3)  TO DGI55-TIPGCIAS   CONTL-TIPGCIAS
           MOVE EST-CODIMP(3)   TO DGI55-CODGCIAS   CONTL-CODGCIAS
           MOVE W-EST-ADMIN     TO DGI55-ADMIN      CONTL-ADMIN
           MOVE MOV-A-PEX-NROSOL  TO DGI55-SOLIC-EXPRESO
                                   CONTL-SOLIC-EXPRESO
                                   CONTL-SOLIC-BCOACEL
           MOVE MOV-A-TASA-PRELIQ TO DGI55-TASA-PAGO-EXPRESO
                                   CONTL-TASA-PAGO-EXPRESO
                                   CONTL-TASA-PAGO-BCOACEL
           MOVE EST-SUC-BAPRO   TO DGI55-SUC-BAPRO
                                   CONTL-SUC-BAPRO
           MOVE W-IDENTIF-LIQ   TO CONTL-IDENTIF-LIQ
                                   DGI55-IDENTIF-LIQ

           MOVE W-TOTAL-PERCEPCIONES TO CONTL-RET-IVA-ESP
                                        DGI55-RET-IVA-ESP

           MOVE W-IVA-1-CARGO-EST    TO CONTL-RET-IVA-DTO-1
                                        DGI55-RET-IVA-DTO-1
                                        CONTL-IVA1-EDC-EST
                                        DGI55-IVA1-EDC-EST

           MOVE L-DIAS               TO CONTL-GRUPO-LIQ
                                        DGI55-GRUPO-LIQ

           MOVE W-MOV-IMPORTE        TO CONTL-CARGO-EDC-EST
                                        DGI55-CARGO-EDC-EST

      *    EN EL PGM ORIG, CONDICION DEL DEALER
           MOVE 'C'                  TO CONTL-TIPO-LIQ
                                        DGI55-DISTINTA-MONEDA
      *    EN EL PGM ORIG, MUEVE 'C' CUANDO ES CARGOS
                                        CONTL-ID-LQDEBITA.

       5500-GRABAR-ARCHI-X-ESTAB.
           ADD 1 TO W-NRO-LIQ
           MOVE W-NRO-LIQ TO DGI55-NROLIQ CONTL-NROLIQ


           WRITE REG-CONTESL
           ADD 1 TO CONT-CONTESL

           WRITE REG-DGI0055
           ADD 1 TO CONT-CONTDGI

           PERFORM 6600-GRABAR-SALIDA6.

       6100-IMPRIMIR-TOTAL-BANCO.
           MOVE 'TOTAL BANCO: '    TO LINEA-SALIDA(2:13)
           MOVE TOTAL-BANCO        TO IMPORTE-FORMATEADO
           MOVE IMPORTE-FORMATEADO TO LINEA-SALIDA(21:20)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA
           ADD TOTAL-BANCO TO TOTAL-GENERAL
           MOVE ALL '=' TO LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA.

       6200-IMPRIMIR-TOTAL-CASA.
           MOVE 'TOTAL CASA: '     TO LINEA-SALIDA(2:12)
           MOVE TOTAL-CASA         TO IMPORTE-FORMATEADO
           MOVE IMPORTE-FORMATEADO TO LINEA-SALIDA(21:20)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA
           MOVE ALL '-'            TO LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA.

       6300-IMPRIMIR-TOTAL-ESTAB.
           MOVE 'TOTAL ESTAB: '    TO LINEA-SALIDA(2:13)
           MOVE TOTAL-ESTAB        TO IMPORTE-FORMATEADO
           MOVE IMPORTE-FORMATEADO TO LINEA-SALIDA(21:20)
           WRITE LINEA-SALIDA
           MOVE SPACES TO LINEA-SALIDA
           MOVE '*** ' TO LINEA-SALIDA(2:4)
           MOVE SPACES TO LINEA-SALIDA.

       6400-IMPRIMIR-TOTALES-FINALES.
           PERFORM 6300-IMPRIMIR-TOTAL-ESTAB

           ADD TOTAL-ESTAB TO TOTAL-CASA
           PERFORM 6200-IMPRIMIR-TOTAL-CASA

           ADD TOTAL-CASA TO TOTAL-BANCO
           PERFORM 6100-IMPRIMIR-TOTAL-BANCO

           DISPLAY 'REGISTROS DEL MOVMMCLA: ' CONT-MOVMMCLA
           DISPLAY 'REGISTROS DEL MAECOM..: ' CONT-MAECOM
           DISPLAY 'REGISTROS DEL MAEEST..: ' CONT-MAEEST
           DISPLAY 'REGISTROS DEL CONTESL.: ' CONT-CONTESL
           DISPLAY 'REGISTROS DEL CONTDGI.: ' CONT-CONTDGI
           DISPLAY 'REGISTROS DEL SALIDA6.: ' CONT-SALIDA6.
           DISPLAY 'REGISTROS DEL MOVSALT7: ' CONT-MOVSALT7.
           DISPLAY 'TOTAL LIQUIDACION.....: ' TOTAL-GENERAL.

132300*6500-GENERA-MOVSALT7.
132400*    MOVE W-IMPORTE-TOTAL TO W-CARGO-TOTAL
132500*    COMPUTE W-CARGO-TOTAL = 0 - W-CARGO-TOTAL
132600*    DISPLAY W-CARGO-TOTAL ' ' MOV-NUMEST
132700*    IF W-CARGO-TOTAL NOT LESS ZEROS OR
132800*     ((W-026-EMIS-LIQ-NEGAT  NOT EQUAL ZERO OR                         22
132900*     (EST-CODBCO = '499' AND EST-AFF-GROUP = '1000'))                  22
133000*       AND EST-ESTADO EQUAL '4000'
133100*       AND EST-CODBCO NOT EQUAL '016'
133200*       AND EST-CODBCO NOT EQUAL '316'
133300*       AND EST-CODBCO NOT EQUAL '150'
133400*       AND EST-CODBCO NOT EQUAL '067'
133500*       AND EST-CODBCO NOT EQUAL '314'
133600*       AND EST-CODBCO NOT EQUAL '322'
133700*       AND EST-CODBCO NOT EQUAL '045'
133800*       AND EST-CODBCO NOT EQUAL '311'
133900*       AND EST-CODBCO NOT EQUAL '332'
134000*       AND EST-CODBCO NOT EQUAL '532'
134100*       AND EST-CODBCO NOT EQUAL '415' )
134200*            CONTINUE
134300*     ELSE
134400*         IF MOV-CODOP = '8015'
134500*            MOVE 'TCP'             TO W-LINEA-CP
134800*         ELSE
134900*            MOVE 'ECP'             TO W-LINEA-CP
135200*         END-IF
135000*         MOVE  REG-MOVMES-ANT   TO REG-MOVSALT7
135100*         PERFORM 6550-GRABA-MOVSALT7
135300*     END-IF.
135400*
135500*6550-GRABA-MOVSALT7.
135600*      MOVE '040924'                TO MOVSALT7-FPAG
135700*      MOVE T076-DDPRES             TO MOVSALT7-DDPRES
135800*      MOVE T076-MMPRES             TO MOVSALT7-MMPRES
135900*      MOVE T076-AAPRES             TO MOVSALT7-AAPRES
136000*      MOVE T076-FPRES              TO MOVSALT7-FVALOR
136100*      MOVE  'LQ'                   TO MOVSALT7-SERGRAB-X
136200*      MOVE  MOVSALT7-IMPORTE       TO MOVSALT7-IMPORTE-GRAV
136300*      WRITE REG-MOVSALT7 END-WRITE
136400*      ADD 1 TO CONT-MOVSALT7.
      *

132300 6500-GENERA-MOVSALT7.
132400     MOVE W-IMPORTE-TOTAL TO W-CARGO-TOTAL
132500     COMPUTE W-CARGO-TOTAL = 0 - W-CARGO-TOTAL
132600*    DISPLAY W-CARGO-TOTAL ' ' MOV-NUMEST
132700     IF W-CARGO-TOTAL NOT LESS ZEROS OR
132800      ((W-026-EMIS-LIQ-NEGAT  NOT EQUAL ZERO OR                         22
132900      (EST-CODBCO = '499' AND EST-AFF-GROUP = '1000'))                  22
133000        AND EST-ESTADO EQUAL '4000'
133100        AND EST-CODBCO NOT EQUAL '016'
133200        AND EST-CODBCO NOT EQUAL '316'
133300        AND EST-CODBCO NOT EQUAL '150'
133400        AND EST-CODBCO NOT EQUAL '067'
133500        AND EST-CODBCO NOT EQUAL '314'
133600        AND EST-CODBCO NOT EQUAL '322'
133700        AND EST-CODBCO NOT EQUAL '045'
133800        AND EST-CODBCO NOT EQUAL '311'
133900        AND EST-CODBCO NOT EQUAL '332'
134000        AND EST-CODBCO NOT EQUAL '532'
134100        AND EST-CODBCO NOT EQUAL '415' )
134200             CONTINUE
134300      ELSE
134400          IF MOV-CODOP = '8015'
134500             MOVE 'TCP'             TO W-LINEA-CP
134600             MOVE  REG-MOVMES-ANT   TO REG-MOVSALT7
                   DISPLAY '3 -> REGMOVSAL7 ' MOVSALT7-FVALOR
134700             PERFORM 6501-GRABA-MOVSALT7
134800          ELSE
134900             MOVE 'ECP'             TO W-LINEA-CP
135000             MOVE  REG-MOVMES-ANT   TO REG-MOVSALT7
                   DISPLAY '4 -> REGMOVSAL7 ' MOVSALT7-FVALOR
135100             PERFORM 6501-GRABA-MOVSALT7
135200          END-IF
135300      END-IF.
135400
135500 6501-GRABA-MOVSALT7.
135600       MOVE '040924'                TO MOVSALT7-FPAG
135700       MOVE T076-DDPRES             TO MOVSALT7-DDPRES
135800       MOVE T076-MMPRES             TO MOVSALT7-MMPRES
135900       MOVE T076-AAPRES             TO MOVSALT7-AAPRES
136000       MOVE T076-FPRES              TO MOVSALT7-FVALOR
             DISPLAY '5 -> MOVSALT7FV ' MOVSALT7-FVALOR
136100       MOVE  'LQ'                   TO MOVSALT7-SERGRAB-X
136200       MOVE  MOVSALT7-IMPORTE       TO MOVSALT7-IMPORTE-GRAV
             DISPLAY '6 -> MOVSALT7FV ' MOVSALT7-FVALOR
136300       WRITE REG-MOVSALT7 END-WRITE
136400       ADD 1 TO CONT-MOVSALT7.

136600 6600-GRABAR-SALIDA6.
136800     MOVE MOV-A-FECHA-VALOR TO W-FECHA-VALOR
136900     IF W-FECHA-VALOR GREATER EST-ULTFPR
137000         MOVE MOV-A-FECHA-VALOR TO EST-ULTFPR
137100     END-IF

137200* MOVER IMPORTE CALCULADO A EST-ULTIMP
137300     MOVE REG-ESTVS  TO REG-SALIDA6
137400     WRITE REG-SALIDA6  END-WRITE
137500     ADD 1 TO CONT-SALIDA6.
