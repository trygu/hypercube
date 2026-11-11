      PROGRAM ITER
C  --------------------------------------------------------------------
C I   PROGRAMM ZUR WAHRUNG DER (SEKUNDAEREN) GEHEIMHALTUNG BEI         I
C I   1 - 7 DIMENSIONALEN DATENBESTAENDEN UEBER MEHRER TABELLEN        I
C  --------------------------------------------------------------------
c*****changes giessing 10.02 a
c I Versionänderung:
c   bei GEWRAN=3 werden Zellen mit imodw=9 wie Primärsperrungen
c   gewichtet.
c*****changes giessing 10.02 e
c-------changes giessing 0902 a
c I Versionsänderungen:
c           -bei Bedarf: stufenweise Reduktion der Quaderquote
c           -mit zusätzlichem Parameter EXACT in TABELLE
c           -EXACT=
c            0: keine Rücksperrungen in 'frozen cells' (QQUOTE wird
c               nicht bis auf Null reduziert). Programmabruch,wenn
c               Pivots nicht gesichert werden können.
c            1: bei Bedarf Rücksperrungen in 'frozen cells'.
c               Programmabruch,wenn
c               Pivots nicht gesichert werden können.  
c            2: keine Rücksperrungen in 'frozen cells' (QQUOTE wird
c               nicht bis auf Null reduziert). Kein Programmabruch,wenn
c               Pivots nicht gesichert werden können.
c-------changes giessing 0902 e         

c-------changes giessing 0801 a
c I Version : GHMITER4
c           -Mit RANDSCHRANKENKONFORMer GEWICHTUNG TYP gewran=2
C-----changes giessing 0801 e
C I VERSION : GHMITER22
C I        -MIT NEUEM SATZAUFBAU, STEUERDATEI
C I        -GEWICHTUNG UND OBERE UND UNTERE SCHAETZWERTFEHLER
C I         GLEICHZEITIG (SCHAETZWERTFEHLERAENDERUNGEN WERDEN AN UNTER-
C I         TABELLEN EINMALIG WEITERGEGEBEN)
C I        -ES WERDEN AUCH FUER SEKUNDAERE SPERRUNGEN IN UNTEREN
C I         AGGREGATIONEN QUOTEN BESTIMMT
C I        -DER UPDATE DER SCHAETZFEHLER WIRD ERST NACH ABARBEITUNG
C I         DER GESAMTEN UNTERTABELLE VORGENOMMEN.
C I         (WURDE EIN FEHLER GEANDERT, IST EINE AENDERUNG DER ANDEREN
C I         FEHLER NICHT MEHR MOEGLICH)
C I        -EINE NEGATIVE GEWICHTUNG IST NICHT MEHR MOEGLICH
C I        -SETZEN VON NEGATIVEN RANDSCHRANKEN
C I        -STEUERUNG DER DOPPELQUADERSICHERUNG (EB MIT EB NICHT MOEGL.)
C I        -BEACHTUNG VON ZWEIER-DOMINANZFAELLEN (SENSITIVITÄT)
C I        -TABELLENBEARBEITUNG (POSITIV - POS./NEG.- NEGATIV)
C I MONITORING-VERSION BEI TABELLE MIT MAX 3 DIMENSIONEN
C  --------------------------------------------------------------------
C I   STAND : 10/2000    VERSION 2.2                                   I
C I                                                                    I
C I                                                                    I
C I   Aenderungen in der PC-Version (09.99)                            I
C I                                                                    I
C I     - Deutschsprachige Sonderzeichen gegen englische Zeichenfolgen I
C I       ausgetauscht.                                                I
C I                                                                    I
C I     - Alle Ausgaben in externe Dateien ausgelagert.                I
C I                                                                    I
C I     - Dateien werden im Unterprogramm OPENFILE mit Namen geoeffnet.I
C I       Ergaenzt um Key-Acsess Zugriff                               I
C I                                                                    I
C I     - Dateien FT09F001, FT10F001, FT12F001 werden im "unformatted" I
C I       Modus als "scratch" Dateien geoeffnet.                       I
C I     - Protokolldateien FT03F001 und FT07F001 umbenannt in PROTO001 I
C I       und PROTO002. Im Fehlerfall wird die Datei PROTO003 erstellt I
C I                                                                    I
C I     - Lese- und Schreiboperationen werden typspezifisch durch-     I
C I       gefuehrt; Typkonvertierungen entfallen.                      I
C I                                                                    I
C I     - Dateien FT01F001 und FT08F001 werden mit freier Formatierung I
C I       bearbeitet, Feldseperator ist ein Leerzeichen. Zeichenketten I
C I       werden in einfache Hochkomma  eingeschlossen.                I
C I                                                                    I
C I     - Das Assemblerprogramm YDYNA entfaellt.                       I
C I     - Unterprogramm HUNK0 umgeschrieben. Das Speichermanagement    I
C I       wurde so veraendert, dass 64 MB im Maximalfall fuer Arrays   I
C I       zur Verfuegung stehen.                                       I
C I     - Die verwendeten Arrays sind typspezifisch; Typ-              I
C I       konvertierungen entfallen.                                   I
C I                                                                    I
C I  PC - Zeitfunktionen gegen die G77 Zeitfunktion CTIME ausgetauscht,I
C I       diese liefert Zeitdaten im UNIX Format.                      I
C I                                                                    I
C I IBM - Zeitfunktionen gegen die VS2 Zeitfunktion DATIM ausgetauscht,I
C I                                                                    I
C I   Anmerkungen                                                      I
C I                                                                    I
C I     - Die Funktion BTEST bleibt erhalten. Sollte ein anderer       I
C I       Compiler als G77 verwendet werden und dieser nicht BTEST     I
C I       unterstuetzen, muss BTEST durch entsprechende logische       I
C I       Funktionen ersetzt werden. Das koennte dann z.B. IAND sein.  I
C I     - CTIME muss bei einem anderen Compiler durch entsprechende    I
C I       Zeitfunktionen ersetzt werden.                               I
C I                                                                    I
C I     - Die INCLUDE Anweisung muss gegebenenfalls durch die Dateien  I
C I       selbst ersetzt werden.                                       I
C I       In der IBM Version wird auf Member in einer Bibliothek       I
C I       zugegriffen, daher Umstellung der Namen der Message-Dateien  I
C I                                                                    I
C I   Aenderungen in der PC-Version (03.00)                            I
C I                                                                    I
C I     - Einfuehrung von Unterbindung der Doppelquaderbildung         I
C I       (notwendig bei Dominanzen ueber 2)                           I
C I     - Einfuehrung der negativen Randschranke (bevorzugte Sperrung  I
C I       in den Rand)                                                 I
C I                                                                    I
C I   Aenderungen in der PC-Version (04.00)                            I
C I                                                                    I
C I     - neuer Satzaufbau                                             I
C I     - geaenderte Steuerdateistruktur                               I
C I     - obere und untere Schaetzfehlergrenzen                        I
C I     - negative Gewichtung nicht mehr moeglich                      I
C I     - Einfuehrung von zweier-Dominanzfaellen                       I
C I     - Aufbereitete Untertabellenausgabe bei Fehler                 I
C I                                                                    I
C I MONITORING-VERSION BEI TABELLE MIT MAX 3 DIMENSIONEN               I
C I     - Ausgabe der fehlerhaften Quader in neuer Version             I
C I     - Angabe von TEST in der Steuerdatei (letzter Eintrag 1.Stelle)I
C I       !! ACHTUNG !! Sehr viel Output bei grossen Tabellen          I
C I                                                                    I
C I   Aenderungen in der PC-Version (05.00)                            I
C I                                                                    I
C I     - STEUERUNG DER DOPPELQUADERSICHERUNG (EB MIT EB NICHT MOEGL.) I
C I     - TABELLENBEARBEITUNG (POSITIV - POS./NEG.- NEGATIV)           I
C I                                                                    I
C I   Aenderungen in der PC-Version (07.00)                            I
C I                                                                    I
C I     - Strukturveraenderungen im Programmablauf                     I
C I       Auslagern des Einlesen der Steuerinformationen in Subroutine I
C I       Auslagern des Einlesen der Tabellenformationen in Subroutine I
C I       Open und Close von Dateien ueber Dateinamen (ALLE)           I
C I     - Moeglichkeit der ueberlappenden Tabellenbearbeitung          I
C I       durch                                                        I
C I       Zusaetzliche Steuerung fuer die ueberlapp. Tabellenverarb.   I
C I       Bildung einer Uberschneidungsdatei (temp.)                   I
C I       Erkennen der zugehoerigen temp. Dateien ueber Dateiname      I
C I       Erkennung der Ueberlappung anhand der beiden Informations-   I
C I       dateien (Kombinationsmoeglichkeiten aller Schluessel in      I
C I       Modul BAULAP und SUCHSS ermitteln)                           I
C I     - Voraussetzung sind nun Sortierbare Schluessel, da fuer eine  I
C I       schnelle Ermittlung der Ueberlappung ein Sort ueber die      I
C I       Schluessel durchgefuehrt wird (CSORT)                        I
C I     - Anschliessend wird die Reihenfolge der verschiedenen TabellenI
C I       anhand der Indices zu den Schluesseln sortiert (NSORT)       I
C I     - Einfuehren zweier neuer Klassierungen von Sperrungen         I
C I       ISPSO1 fuer Sperrungen im Ueberlappungsbereich ohne Doppel-  I
C I          quaderbildung (-8)                                        I
C I       ISPSM1 fuer Sperrungen in der Basisdatei die bereits         I
C I          im Vorlauf erfolgreich gesperrt wurden (-9)               I
C I                                                                    I
C I   Aenderungen in der PC-Version (11.00)                            I
C I                                                                    I
C I     - Aufgrund der Vergleichbarkeit der Werte und der Ueberlappung I
C I       muss eine Klassierung anhand der Spreizung der Gesamttabelle I
C I       vorgenommen werden (alle Einzeltabellen), deshalb Minimal-   I
C I       und Maximalwert von allen Tabellen in der Gesamtsteuerdatei  I
C I       angeben (wird aus den Einzeltabellensteuerungen entfernt)    I
C I     - Sicherheitsueberpruefung der Werte im Ueberlappungsbereich   I
C I       eingefuegt (um falsche Angaben in Pseudo-Dim.-Schluessel     I
C I       zu erkennen                                                  I
C I     - Die Einschaetzung ob es sich um positiv/negative Tabelle     I
C I       handelt, gilt fuer den gesamten Raum (ebenso für sperrbare  I
C I       Nullwerte) daher Steuerung in Gesamtsteuerdatei (TABELLE)    I
C I     - Markierung der verschiedenen Aenderungen IBM zu PC           I
C I       a) Datum/Zeit Ermittlung verschieden(mehrer Module)          I
C I       b) Speicherplatz fuer TAB0 auf IBM geringer(HUNK0)           I
C I       c) File-Name Zuweisung nicht unter IBM (OpenFile)            I
C I       d) INCLUDE PC=Hochkommata  IBM=Klammer                       I
C I                                                                    I
C I   UNTERPROGRAMME :                                                 I
C I   INIT,HUNK0                                                       I
C  --------------------------------------------------------------------
      INTEGER MAXMEM,imxdim,JJ
c*****changes giessing 08.01 a
      Integer mode,print1,exact
c*****changes giessing 08.01 e
      CHARACTER*2 VERSION/'14'/
	character*8 hour

C-IBM-Version
*     CHARACTER*36 time
C  Maximal anzufordender Speicherbereich
      PARAMETER (maxmem=64*1024*1024)
c*****changes giessing 08.01 a
      common/print1/print1
c*****changes giessing 08.01 e
c----changes giessing 09/02-a
      COMMON /EXACT/EXACT
c----changes giessing 09/02-e
C
C-IBM-Version
*     INCLUDE (ITERM)
C-PC-Version
      INCLUDE 'ITERM'
C
C -- Open Dateien 1. PROTOKOLLDATEI
C
      CALL OpenFile ('PROTO001', 3, 'N', 'F', ' ', 0)
      CALL OpenFile ('PROTO002', 7, 'N', 'F', ' ', 0)
C
C -- Open Datei Tabellensteuerung bzw. Speicherplatz-Abruf
C
      CALL OpenFile ('TABELLE ', 9, 'O', 'F', ' ', 0)
C
C -- Info Laufzeit
C
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1005) version,Time
C-PC-VERSION
c     changes e.drumm 15.11.01
c      call time(hour)
c      write(7,1005)version, hour
      WRITE(7,1005) version,CTime(Time())
c     end changes e.drumm
C
C     Ueberpruefen ob iterativer Ablauf
C
c*****changes giessing 02.01 a
c      CALL INIT(JJ,imxdim)
      CALL INIT(JJ,imxdim,mode)
c*****changes giessing 02.01 e
C
      IF (jj.GT.maxmem) THEN
         WRITE(7,1010)
         STOP 0001
      ENDIF
      CALL HUNK0(JJ,imxdim,mode)
      IF (jj.EQ.0) THEN
         WRITE(7,1100)
*        WRITE(7,1100)
         STOP 0002
      ENDIF
      END

c*****changes giessing 08.01 a
c      SUBROUTINE HUNK0 (JJ,imxdim)
      SUBROUTINE HUNK0 (JJ,imxdim,mode)
c*****changes giessing 08.01 e
C  -------------------------------------------------------------------
C I                                                                   I
C I   H U N K 0                                                       I
C I                                                                   I
C I   SUBROUTINE ZUR AUFTEILUNG DES ARBEITSSPEICHERS                  I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   SUBROUTINE HUNK0(JJ,imxdim,mode)                                     I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I   JJ     -LAENGE IN BYTES(I*4)                                    I
C I   imxdim -Max. Dimension (I*4)                                     I
C I                                                                   I
C I   UNTERPROGRAMME :                                                I
C I   STEUER                                                          I
C  -------------------------------------------------------------------
C  Create the data areas with defined length
C  ( Values are calculated for 64 MB of memory)
      INTEGER MaxIDD,MaxIL,MaxNDIM,MaxIMS,MaxIW,MaxLG,MaxISK
      INTEGER MaxLST,MaxIMAX
C- NEU 12.11.01
*     PARAMETER  (MaxIDD=200)
C- NEU 28.04.04
*     PARAMETER  (MaxIDD=600)
      PARAMETER  (MaxIDD=1500)
C- NEU 28.04.04
C- NEU 12.11.01
C- NEU 28.04.04
      PARAMETER  (MaxIL=12000)
C- NEU 28.04.04
      PARAMETER  (MaxNDIM=7)
      PARAMETER  (MaxIMS=10)
C- NEU 28.04.04
      PARAMETER  (MaxIW=12000)
C- NEU 28.04.04
C- NEU 12.11.01
*     PARAMETER  (MaxLG=6000)
      PARAMETER  (MaxLG=50000)
C- NEU 12.11.01
      PARAMETER  (MaxISK=100000)
      PARAMETER  (MaxLST=220)
C-IBM-Version
*     PARAMETER  (MaxIMAX=3000000)
C-PC-Version
      PARAMETER  (MaxIMAX=4000000)
C
      INTEGER imxdim,tabanz
C
      INTEGER G_IA(MaxIL,MaxNDIM)
      INTEGER G_IU(MaxIL,MaxNDIM),G_IO(MaxIL,MaxNDIM)
      INTEGER G_OK(MaxIL,MaxNDIM),G_MUS(MaxIMS,MaxNDIM)
      INTEGER G_TABI(MaxLG),G_BS(MaxISK)
      INTEGER G_STACK(MaxLST),G_KEYF(MaxIW,MaxNDIM)
      INTEGER G_IP(MaxIL,MaxNDIM),G_UK(MaxIL,MaxNDIM)
c*****changes giessing 08.01 a
      Integer mode,print1,exact
c*****changes giessing 08.01 e
      CHARACTER*8 G_SS(maxIW,MaxNDIM)
C
      COMPLEX*16 G_TAB(MaxLG),G_TABR(MaxLG)
      COMPLEX*16 G_TAB0(MaxIMAX)
      COMMON /BIG/G_TAB0
c*****changes giessing 08.01 a
      common/print1/print1
c*****changes giessing 08.01 e
c----changes giessing 09/02-a
      COMMON /EXACT/EXACT
c----changes giessing 09/02-e
C
C
C-IBM-Version
*     INCLUDE (HUNK0M)
C-PC-VERSION
      INCLUDE 'HUNK0M'
C
C-----FEST VORGEGEBENE ARBEITSTABELLEN--------------------------------
C
C     IDDD SUMME DER DIMENSIONSLAENGEN DER BASISTABELLE
C     IMS  MAXIMALZAHL DER AGGREGATIONSSTUFEN
C     IW   ZAHL DER SCHLOESSER JE DIMENSION
C     LG   MAXIMALE LAENGE EINER UNTERTABELLE
C     ISK  MAXIMALZAHL SEKUNDAER GESPERRTER FELDER
C     LST  ARBEITSBEREICH FUER DIE SORTIERUNG
C
      IMS=MaxIMS
      IW=MaxIW
      IL=IW
      IDDD=MaxIDD
      LG=MaxLG
      LST=MaxLST
      ISK=MaxISK
C
      JJ1=JJ/4
      IT=7*IL
      IM=7*IMS
      LGG=LG+2*LG*4
      ISS=2*7*IW
      IK=IDDD+6*IT+IM+ISS+LGG+ISK+LST+IW*7
      IMAX=JJ1-IK
      IK=IK*4
      IF (IMAX.LE.0) GO TO 10
C Belegt in Bytes
      IMAX1=IMAX
C Freier Speicherplatz in DoubleComplex
      IMAX=IMAX/4
C
      IF (IMAX.GT.MaxIMAX) THEN
         WRITE (7,4010) MaxIMAX,IMAX
         STOP 1002
      END IF
C
C-----PROTOKOLL-------------------------------------------------------
C
      WRITE (3,4020) JJ,IMAX1
      WRITE (3,4030) IL,IDDD,IMS,IW,LG,ISK,LST
C
C-----AUFRUF DES STEUERPROGRAMMS FUER DEN GESAMTEN ABLAUF (ITERATIV) ---
C
c*****changes giessing 08.01 a
c      CALL STEUER (IDDD, IL, G_IA, G_IP, G_IU, G_IO, G_UK, G_OK,
c     # IMS, G_MUS, IW, LG, G_TABI, G_TAB, G_TABR, ISK, G_BS,
c     #LST, G_STACK, G_KEYF, IMAX, imxdim, G_SS, G_TAB0)
      CALL STEUER (IDDD, IL, G_IA, G_IP, G_IU, G_IO, G_UK, G_OK,
     # IMS, G_MUS, IW, LG, G_TABI, G_TAB, G_TABR, ISK, G_BS,
     #LST, G_STACK, G_KEYF, IMAX, imxdim, G_SS, G_TAB0,mode)
c*****changes giessing 08.01 e
C
      GO TO 30
C
C-----ABBRUCH BEI NICHT AUSREICHENDEM ADRESSRAUM------------------------
C
   10 WRITE (3,4040) JJ,IK
      GO TO 30
C
C Abbruch bei falscher Dimensionsangaben
   20 WRITE(3,4050) imxdim
   30 RETURN
      END


c*****changes giessing 08.01 a
c      SUBROUTINE STEUER (IDDD, IL, IA, IP, IU, IO, UK, OK, IMS,
c     #MUS, IW, LG, TABI, TAB, TABR, ISK, BS, LST, STACK, KEYF,
c     #IMAX, imxdim, SS, TAB0)
      SUBROUTINE STEUER (IDDD, IL, IA, IP, IU, IO, UK, OK, IMS,
     #MUS, IW, LG, TABI, TAB, TABR, ISK, BS, LST, STACK, KEYF,
     #IMAX, imxdim, SS, TAB0,mode)
c*****changes giessing 08.01 e

C  -------------------------------------------------------------------
C I                                                                   I
C I   S T E U E R                                                     I
C I                                                                   I
C I   SUBROUTINE ZUR STEUERUNG DER GEHEIMHALTUNG                      I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   SUBROUTINE STEUER(IDDD,IL,IA,IP,IU,IO,UK,OK,IMS,MUS,            I
C I  * IW,SS,LG,TABI,TAB,ISK,BS,IBS,LST,STACK,KK,KEYF,IMAX,TAB0,NDIM) I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I   IDDD   SUMME DER DIMENSIONSLAENGEN      (I*4)                   I
C I     IL   ANZAHL DER MOEGL. AGGR. U. POS.  (I*4)                   I
C I     IA   AGGREGATIONSTABELLE              (I*4)                   I
C I     IP   POSITIONSTABELLE                 (I*4)                   I
C I     IU   UNTERE TEILMATRIXINDICES         (I*4)                   I
C I     IO   OBERE TEILMATRIXINDICES          (I*4)                   I
C I     UK   UNTERE VERWEISMATRIXINDICES      (I*4)                   I
C I     OK   OBERE  VERWEISMATRIXINDICES      (I*4)                   I
C I    IMS   LAENGE VON MUS                   (I*4)                   I
C I    MUS   ANZAHL DER SCHLUESSEL PRO AGGR.  (I*4)                   I
C I     IW   LAENGE VON SS                    (I*4)                   I
C I     SS   SACHSCHLUESSELTABELLE            (C*8)                   I
C I     LG   LAENGE EINER UNTERTABELLE        (I*4)                   I
C I   TABI   (REL.) UNTERTABELLENINDICES      (I*4)                   I
C I    TAB   WERTE DER UNTERTABELLE           (C*16)                  I
C I          REIHENFOLGE(KLASSIERTER WERT, GEWICHT, HOECHSTER EINZEL- I
C I                      WERT, OBERER -, UNTERER SCHAETZFEHLER)       I
C I    TABR  WERTE DER UNTERTABELLE(PARALLEL) (C*16)                  I
C I          REIHENFOLGE(KL./SONDER-WERTE, GEWICHT, HOECHSTER EINZEL- I
C I                  UPGEDATETE WERT, OBERER -, UNTERER SCHAETZFEHLER)I
C I    ISK   LAENGE DES SORTIERBEREICHS       (I*4)                   I
C I     BS   SORTIERBEREICH                   (I*4)                   I
C I    LST   LAENGE DER ARBEITSTABELLE        (I*4)                   I
C I  STACK   ARBEITSTABELLE FUER SORT         (I*4)                   I
C I   KEYF   SCHLUESSELINDEXTABELLE           (I*4)                   I
C I          FUER IN DER EINGABE GEF. SCHLUESSEL                      I
C I   IMAX   LAENGE DER BASISTABELLE          (C*8)                   I
C I   TAB0   BASISTABELLE                     (C*16)                  I
C I          REIHENFOLGE(KLASSIERTER WERT, GEWICHT, HOECHSTER EINZEL- I
C I                      WERT, OBERER -, UNTERER SCHAETZFEHLER)       I
C I   NDIM   ANZAHL DER DIMENSIONEN           (I*4)                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   LOAD,TABELL,TABIN,ARBEIT,UPDA,RCIN,RCOUT,OPENFILE,INFO          I
C I   BAULAP,SUCHSS                                                   I
C  -------------------------------------------------------------------
      INTEGER*2 erk_tab(0:40,0:750000),int_tab(20,40,3500)
      INTEGER indx(750000)
      INTEGER anz_dim(20),pseu_stell(0:5,0:40,0:20),stell,neu_start
      INTEGER anz_schl(0:40)
      INTEGER ss_stell,imxdim,ndim,tabanz,itab,iiab,ig_dim(7),ue_lapp
      INTEGER ue_sperr,i_sperr,gs_sperr,ueg_sperr
      INTEGER*2 SO(40)
      INTEGER QUA
      INTEGER KWERT
      INTEGER IRTAB(0:127)
      INTEGER RFLAG(7),ABSBET,GEWRAN,SCHAEZ,IIZW,IIZW3,IRIZW2,IRIZW
      INTEGER IDRZW3,IDRZW,idrzw4,irizw4
c*****changes giessing 02.01 a
      INTEGER ISZ
      INTEGER IMAX,IL,IMS,IW,III,NGES
      integer II1
      integer mode
c----changes giessing 09/02-a
      INTEGER EXACT
c----changes giessing 09/02-e
c----changes giessing 03/03-a
      INTEGER NREDU(11)
c----changes giessing 03/03-e
c*****changes giessing 02.01 e
      INTEGER TABI(LG),II(7),LI(7),MS(7),IERR(7),IA(IL,imxdim),
     #IP(IL,imxdim),IO(IL,imxdim),IU(IL,imxdim),MA(7),MP(7),
     #MUS(IMS,imxdim),ISK,BS(ISK),LST,STACK(LST),UK(IL,imxdim),
     #OK(IL,imxdim),KEYF(IW,imxdim)
c*****changes giessing 08.01 a
      Integer print1
c*****changes giessing 08.01 e
C
*     REAL*8 RUZW,ROZW,RUZW2,ROZW2,RUZW3,ROZW3
      REAL*4 RUZW,ROZW,RUZW2,ROZW2,RUZW3,ROZW3,ROZW4,RUZW4
C- Neu 12.11.01
*     REAL*4 QDA(2000,5,135)
      REAL*4 QDA(50000,5,21)
C- Neu 12.11.01
C
      REAL*8 FMIN,MAXF
      REAL*8 POT(7),GEWMAX,GEWMIN,KMAX
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 WGES
      REAL*8 PMAX1,PMIN1,PP
C
      COMPLEX*16 TAB0(IMAX)
      COMPLEX*16 TAB(LG)
      COMPLEX*16 TABR(LG)
C
      LOGICAL DBQUA
C
*      CHARACTER*8 SS(3500,7)
      CHARACTER*8 SS(IW,IMXDIM)
      CHARACTER OUTL*40
      CHARACTER*8 DIMBEZ(20,7),nam_dim(40)
      CHARACTER*8 tab_nam(20),tab_ft04(20),tab_ft09(20),tab_ft10(20)
      CHARACTER*8 tab_ft12(20),tab_out(20),tab_ft15(20),tab_ft60(20)
      CHARACTER*8 pseu_key(0:40,0:20)
C- NEU 28.04.04
*      CHARACTER*8 schl_nam(0:40,0:3500)
      CHARACTER*8 schl_nam(0:40,0:12000)
C- NEU 28.04.04
      CHARACTER*4 test
	character*8 hour
C-IBM-Version
*     CHARACTER*36 time
C
      COMMON /AUFAB/SO
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMMON /ABSBET/ ABSBET,GEWRAN,SCHAEZ,DBQUA
      COMMON /INSTA/POT,GEWMAX,GEWMIN
      COMMON /QUA/QUA,QDA
      COMMON /RAND/ IRTAB
      COMMON /DIMBZ/DIMBEZ
      COMMON /RFLAG/ RFLAG
      COMMON /KWERT/ KWERT
      COMMON /TEST/TEST
c*****changes giessing 08.01 a
      common /print1/print1
c*****changes giessing 08.01 e
      COMMON /SPMERK/ IGEI,ISPP,ISPS,ISPS1,IGEIT,ISPSQ,ISPSQ1,
     #                ISPSO1,ISPSM1
C
C-----COMMON - BEREICHE FUER UNTERPROGRAMME BAULAP und SUCHSS --------
C
      COMMON /DAT/ tab_ft04,tab_nam,tab_out
      COMMON /LAPP/ tabanz
      COMMON /SSLA/ anz_schl,nam_dim,pseu_key
      COMMON /SSNA/ schl_nam
      COMMON /PSEU/ anz_dim,pseu_stell,stell
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab
C
C-----COMMON - BEREICHE FUER UNTERPROGRAMME ARBEIT UND TABELL-----------
C
c----changes giessing 09/02-a
      COMMON /EXACT/EXACT
c----changes giessing 09/02-e
c----changes giessing 03/03-a
      COMMON /NREDU/NREDU
c----changes giessing 03/03-e
      COMMON IGG(7),IMDI(7),IUU(7),IOO(7)
      COMMON /INDEX/ II,LI,MS,MA,MP
C
C-----EQUIVALENCE FUER DIMENSIONSLAENGEN--------------------------------
C
      EQUIVALENCE (LI(1),LI1),(LI(2),LI2),(LI(3),LI3),(LI(4),LI4),(LI(5)
     #,LI5),(LI(6),LI6),(LI(7),LI7)
      EQUIVALENCE (IMDI(1),ID1),(IMDI(2),ID2),(IMDI(3),ID3),(IMDI(4),
     #ID4),(IMDI(5),ID5),(IMDI(6),ID6),(IMDI(7),ID7)
C
C-IBM-Version
*     INCLUDE (STEUERM)
C-PC-VERSION
      INCLUDE 'STEUERM'
C
C-IBM-Version
*     PP=10.0E+70
C-PC-VERSION

c      write(0,*) 'steuer,mode',mode


      PP=10.0**25
      KWERT=100001
      neu_start=0
      ue_lapp=0
      ss_stell=0
      iiab=0
      do 984 i=1,40
         SO(i)=0
  984 continue

c----changes giessing 03/03-a
      do 988 i=1,11
988       nredu(i)=0
c----changes giessing 03/03-e
C
C -- OPEN DATEIEN
C
c*****changes giessing 08.01 a
c----OEFFNEN DER DATEI MIT DER FUER DIE BERECHNUNG DER SPREIZUNG BEI DER GEWICHTUNG
C----BENOETIGTEN VERTEILUNGSINFORMATION UND ZUSÄTZLICHER MONITORING-DATEI
c----ZUR KONTROLLE DER GEWICHTUNG Ft14f001
      if ((mode.eq.2).or.(mode.eq.4)) then
       CALL OpenFile ('FT18F001',18, 'O', 'F', ' ', 0)
       CALL OpenFile ('FT14F001',14, 'N', 'F', ' ', 0)
       if (print1.eq.4)
     #  CALL OpenFile ('FT17F001',17, 'N', 'F', ' ', 0)
      endif
c      write(0,*) 'mode','print1',mode,print1
c*****changes giessing 08.01 e
      IF (tabanz.EQ.1) then
C - Einzeltabelle (Standard)
         if(tab_nam(1).eq.'        ')  tab_nam(1) ='EINGABE '
         if(tab_out(1).eq.'        ')  tab_out(1) ='AUSGABE '
         if(tab_ft04(1).eq.'        ')  tab_ft04(1)='STEUER  '
         tab_ft09(1)='FT09FILE'
         tab_ft10(1)='FT10FILE'
         tab_ft12(1)='FT12FILE'
         ndim=imxdim
      ENDIF
C  Vorlesen aller Steuerinformationen, Aufbau Ueberlappungserkennung
      IF (tabanz.NE.1) THEN
         DO 17 iiab=1,tabanz
            CALL OpenFile (tab_ft04(iiab), 4, 'O', 'F', ' ', 0)
c            write(0,*) 'vor call info, tab',iiab
            CALL INFO(IW,MUS,IMS,NDIM,MS,imxdim,ig_dim,iiab,ss)
            IF(iiab.LT.10) THEN
             WRITE(tab_ft15(iiab),'(A7,I1)') 'FT15FI0',iiab
            ELSE
             WRITE(tab_ft15(iiab),'(A6,I2)') 'FT15FI',iiab
            ENDIF
c            write(0,*) tab_ft15(iiab)
            CALL OpenFile (tab_ft15(iiab), 79+iiab, 'S', 'U', ' ', 0)
c            write(0,*) 'nach openfile ft15, tab',iiab
            WRITE(79+iiab)
     #      MINW,MAXW,FMIN,MAXF,OMINW,GEWMAX,GEWMIN,NZ,
     #      QQUOTE,ABSBET,GEWRAN,SCHAEZ,XMINW,NULW,
     #      DQUOTE,DBQUA,NDIM,NGES,WGES,RFLAG,POT,IW,MUS,IMS,MS,
     #      IGG(7),IMDI(7),IUU(7),IOO(7),
*     #      ((ss(j,i),i=1,7),j=1,3500),
     #      ((ss(j,i),i=1,7),j=1,IW),
     #      TEST
c            write(0,*) 'vor baulap, tab',iiab
            CALL BAULAP(ndim,iiab,ss_stell,ig_dim,ss)
c            write(0,*) 'nach baulap, tab',iiab
   17    CONTINUE
C
C Sortierung der Schluessel nach einlesen der Steuerdateien
C
         LSTN=LST/2
         CALL CSORTS(ss_stell,LSTN,STACK)
C
C Erstellung der Ueberlappungstabelle
C
         CALL SUCHSS(ss_stell)
C - Ueberschneidungsdatei (Temporaere Dateien einrichten)
         CALL OpenFile ('SCHNEIDt', 13, 'S', 'U', 'D', 24)
      ENDIF
C
C Iteration ueber alle Tabellen zur Erstellung der Arbeitsdateien
C
      DO 85 itab=1,tabanz
c      write(0,*) '85 itab Schleife'
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1006) tab_nam(itab),Time
C-PC-VERSION
c     e.drumm 15.11.01
      WRITE(7,1006) tab_nam(itab),CTime(Time())
c		call time(hour)
c      write(7,1006) tab_nam(itab),hour
c     end changes e.drumm 15.11.01
C - Ueberlappende Tabellen (Temporaere Dateien einrichten)
      IF (tabanz.NE.1) THEN
       IF(itab.LT.10) then
         WRITE(tab_ft09(itab),'(A7,I1)') 'FT09FI0',itab
         WRITE(tab_ft10(itab),'(A7,I1)') 'FT10FI0',itab
         WRITE(tab_ft12(itab),'(A7,I1)') 'FT12FI0',itab
         WRITE(tab_ft60(itab),'(A7,I1)') 'FT60FI0',itab
       ELSE
         WRITE(tab_ft09(itab),'(A6,I2)') 'FT09FI',itab
         WRITE(tab_ft10(itab),'(A6,I2)') 'FT10FI',itab
         WRITE(tab_ft12(itab),'(A7,I1)') 'FT12FI',itab
         WRITE(tab_ft60(itab),'(A6,I2)') 'FT60FI',itab
       ENDIF
       CALL OpenFile (tab_ft04(itab), 4, 'O', 'F', ' ', 0)
       CALL OpenFile (tab_ft09(itab), 19+itab, 'S', 'U', 'D', 24)
      ELSE
C
C -- STEUERDATEI EINLESEN (Einzeltabelle)
C
       CALL OpenFile (tab_ft04(itab), 4, 'O', 'F', ' ', 0)
       CALL INFO(IW,MUS,IMS,NDIM,MS,imxdim,ig_dim,iiab,ss)
       CALL OpenFile (tab_ft09(itab), 19+itab, 'S', 'U', ' ', 0)
      ENDIF
      CALL OpenFile (tab_nam(itab), 1, 'O', 'F', ' ', 0)
      CALL OpenFile (tab_ft10(itab), 39+itab, 'S', 'U', ' ', 0)
      CALL OpenFile (tab_ft12(itab), 59+itab, 'S', 'U', 'D', 13028)
C
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1007) Time
C-PC-VERSION
c     changes e.drumm 15.11.01
      WRITE(7,1007) CTime(Time())
c		call time(hour)
c      write(7,1007) hour
c     end changes e.drumm
C
C- Neu 12.11.01
         DO 5 M=1,50000
C- Neu 12.11.01
           DO 4 J=1,5
C- Neu 12.11.01
             DO 3 I=1,21
C- Neu 12.11.01
                QDA(M,J,I)=0.0
    3        CONTINUE
    4      CONTINUE
    5    CONTINUE
C
         KMAX=100000.0*(1.0+(2.2*((2.0**NDIM-1.0)**2)))
C-- Parameterdatei rueckladen
         IF(tabanz.ne.1) THEN
            REWIND 79+itab
            READ(79+itab) MINW,MAXW,FMIN,MAXF,OMINW,GEWMAX,GEWMIN,NZ,
     #       QQUOTE,ABSBET,GEWRAN,SCHAEZ,XMINW,NULW,DQUOTE,DBQUA,NDIM,
     #       NGES,WGES,RFLAG,POT,IW,MUS,IMS,MS,
     #       IGG(7),IMDI(7),IUU(7),IOO(7),
*     #       ((ss(j,i),i=1,7),j=1,3500),TEST
     #       ((ss(j,i),i=1,7),j=1,IW),TEST
C- Sort-Reihenfolge aufstellen um schneller einlesen zu koennen
            n=0
            do 18 j = 1, ndim
                 pseu_stell(5,j,itab)=pseu_stell(1,j,itab)
   18       continue
            do 19 j = ndim+1,ss_stell
               n=n+1
               IF (n.GT.anz_dim(itab)) then
c                write(0,*) 'itab,anz_dim(itab),n,ss_stell',
c     #                      itab,anz_dim(itab),n,ss_stell
                 STOP 7777
               endif
               pseu_stell(5,j,itab)=pseu_stell(2,n,itab)
   19    continue
         ENDIF
C### NEU 25.10.01 INIT ALLER VARIABLEN FUER TABELL UND FETCH #######
         DO 555 ITF=1,IMXDIM
           DO 554 IFT=1,IL
              IA(IFT,ITF)=0
              IP(IFT,ITF)=0
              IO(IFT,ITF)=0
              IU(IFT,ITF)=0
  554      CONTINUE
  555    CONTINUE
C### NEU 25.10.01 INIT ALLER VARIABLEN FUER TABELL UND FETCH ########
C -- Initialisieren von IERR
         DO 10 I=1,7
            IERR(I)=0
   10    CONTINUE
C
         IJK=0
         DO 20 IIIIII=0,127
   20       IRTAB(IIIIII)=0

*        WRITE (3,4010)
C
C-----LADEN DER DATEI MIT DEN RELATIVEN SATZNUMMERN UND ERSTELLUNG------
C-----DER INDEXTABELLE SOWIE AUFBAU DER BASISTABELLE--------------------
C
C
c*****changes giessing 08.01 a

c------BEI EINZELTABELLLEN: LESEN DER SORTIERTEN SCHLÜSSELDATEI FT15 UND DER DATEI MIT
c------DEN VARIABLENSCHLÜSSELN FT16, EINTEILEN DER TABELLE IN
c------TEILABSCHNITTE (ABSCHN) ENTSPRECHEND DER BESETZTEN DIMENSIONEN
c------(DIMENSIONEN, DIE MIT 'INSGESAMT' BELEGT SIND, GELTEN DABEI ALS
C-------NICHTBESETZT), EINLESEN VON TEILGLIEDERUNGEN UND TABELLENTEIL-
c------BEZOGENEN MODIFIKATIONSWÜNSCHEN, AUSGABE DER ERMITTELTEN PARAMETER
C------'ANZMODW' (SONST DATEI FT18) ALS MATRIX
c       if(mode.eq.1) then
C         if(itab.eq.1)
C-------Achtung-Achtung-Achtung- 'mode=1' ist in dieser Version noch nicht realisiert!!!
c          CALL sequences(IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS,
c     #    IW,SS, III,NGES, WGES, ISZ,KEYF,itab, imxdim, lst, stack,
c     #    ss_stell,ierr,anzmodw)
c----------Bei Einzeltabellen Aufruf des Unterprogramms Load aus dem Unterprogramm keys von sequences
c----------überspringen von load in Steuer-------
c          if(tabanz.eq.1) goto 55
c         else
c---------Aufruf des Unterprogramms Load aus dem Unterprogramm keys
c---------überspringen von load in Steuer-------
c          call keys(IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS, IW,
c     #    SS, III,NGES, WGES, ISZ,KEYF,itab, imxdim, lst, stack,
c     #    ss_stell,ierr,anzmodw)
c          goto 55
c         endif
c      nextcall=.false.

C
C-----LADEN DER DATEI MIT DEN RELATIVEN SATZNUMMERN UND ERSTELLUNG------
C-----DER INDEXTABELLE SOWIE AUFBAU DER BASISTABELLE--------------------
C
        CALL LOAD (IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS, IW,
     #SS, III, NGES, WGES, ISZ, KEYF,itab, imxdim, lst, stack,
     #ss_stell,ierr,MODE)

c         endif
c
c        CALL LOAD (IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS, IW,
c     #  SS, III, NGES, WGES, ISZ, KEYF, itab, imxdim, lst, stack,
c     #  ss_stell,ierr)
   55  continue
c*****changes giessing 08.01 e
C
C-----PROTOKOLL DES LADEZUSTANDES UND DER INDEXTABELLE------------------
C
         WRITE (3,4090) NGES,IMAX
         WRITE (3,4100)
         DO 70 I=1,NDIM
            WRITE (3,4110) I,I,IERR(I),I,II(I),I,LI(I)
            IV=II(I)
            WRITE (3,4120)

            DO 60 IS=1,IV
   60          WRITE (3,4130) IA(IS,I),IP(IS,I),IU(IS,I),IO(IS,I),
     #          (SS(KEYF(J,I),I),J=UK(IS,I),OK(IS,I))
 4130 FORMAT (1X,4(I7,3X),20X,5(A8,2X)/(61X,5(A8,2X)))
   70 CONTINUE
C--- SCHAETZFEHLERMINIMUM FUER NULLFAELLE FESTLEGEN
         IF (SCHAEZ.EQ.0) FMIN=MINW
         FMIN=FMIN/1.1
c*****changes giessing 02.01 a
c-------SCHAEZ=2 => NUR SCHAEZFEHLEREINTRAEGE IN LOAD ABER SONST WIE SCHAEZ=0
      if(schaez.eq.2) schaez=0
c*****changes giessing 02.01 e
C
C--- UEBERPRUEFUNG DES ABSTANDSMASSES MIT DEN DIMENSIONSLAENGEN
C
         IF(GEWMAX.LE.0.0) GEWMAX=1.0
         IF(GEWMIN.LE.0.0) GEWMIN=1.0
         DO 75 I=1,NDIM
            PMAX1=LOG(PP/(GEWMAX*KMAX))/LOG(REAL(LI(I)))
            PMIN1=-LOG(PP*GEWMIN)/LOG(REAL(LI(I)))
            WRITE (3,4135) I,PMAX1,PMIN1
            IF(POT(I).GT.PMAX1) THEN
               WRITE (3,4136) POT(I),I,PMAX1
               POT(I)=PMAX1
            ENDIF
            IF(POT(I).LT.PMIN1) THEN
               WRITE (3,4137) POT(I),I,PMIN1
               POT(I)=PMIN1
            ENDIF
   75    CONTINUE
C
C-----ERSTELLUNG DER (REL.) INDICES ALLER UNTERTABELLEN AUF EXTERNER----
C-----DATEI-------------------------------------------------------------
C
         DO 80 I=1,7
            II(I)=MAX0(II(I),1)
            LI(I)=MAX0(LI(I),1)
   80       MS(I)=MAX0(MS(I)-1,1)
         CALL TABELL (LG, TABI, IL, NDIM, IA, IP, IU, IO, IDDD,
     #    NZ, ISZ, itab)
C
C-----PROTOKOLL DER UNTERTABELLEN--------------------------------------
C
         WRITE (3,4140) NZ,NGES,WGES
C
         IF (tabanz.ne.1) THEN
           Rewind 79+itab
           WRITE(79+itab) MINW,MAXW,FMIN,MAXF,OMINW,GEWMAX,GEWMIN,NZ,
     #     QQUOTE,ABSBET,GEWRAN,SCHAEZ,XMINW,NULW,DQUOTE,
     #     DBQUA,NDIM,NGES,WGES,RFLAG,POT,TWERT,IDDD,LI,II,MS,MUS,ISZ,
     #     TEST
           CLOSE (UNIT=59+itab)
           CALL OpenFile (tab_ft60(itab), 59+itab, 'S', 'U', ' ', 0)
C-- Sperrverzeichnisdatei einrichten
           WRITE(59+itab) ijk,(BS(iji),iji=1,ijk),IRTAB
         ENDIF
C
C -- Ende erstellen Arbeitsdateien
C
C-IBM-Version
*        CALL CTime(time)
*        WRITE(7,1008) tab_nam(itab),Time
C-PC-VERSION
         WRITE(7,1008) tab_nam(itab),CTime(Time())
c     changes e.drumm 15.11.01
c		call time(hour)
c      write(7,1008) tab_nam(itab),hour
c     end changes e.drumm
   85 CONTINUE
C
C--  Iteration der Geheimhaltung ueberlappender Tabellen
C
      loop=1
   87 CONTINUE
      ueg_sperr=0
      do 600 itab=1,tabanz
C-IBM-Version
*       CALL CTime(time)
*       WRITE(7,1005) loop,tab_nam(itab),Time
C-PC-VERSION
        WRITE(7,1005) loop,tab_nam(itab),CTime(Time())
c     changes e.drumm 15.11.01
c		call time(hour)
c      write(7,1005) loop,tab_nam(itab),hour
c     end changes e.drumm
C
        ue_sperr=0
        gs_sperr=0
        i_sperr=0
C
C
C-----SETZEN DER SPERRIDENTIFIKATOREN (KOMPENSATION VON QUADER----------
C-----KONFIGURATIONEN MIT MINIMALER SUMME IM RAND-----------------------
C
        IGEI=-1
        ISPP=-2
        ISPS=-3
        ISPS1=-4
C----- TEMPORAERE VERMERKE
        IGEIT=-5
        ISPSQ=-6
        ISPSQ1=-7
C----- VERMERKE FUER UEBERLAPPENDE SEKUNDAERE SPERRUNGEN OHNE UEBERPR.
C----- innerhalb der ueberlappung bzw basisdatei
        ISPSO1=-8
        ISPSM1=-9
C
C-----RUECKLADEN DER STEUERPARAMETER/BASISTABELLE (ERMOEGLICHT ITERAT.)
C     hier: Ueberlappende Tabellen
        IF (tabanz.NE.1) THEN
         REWIND  79+itab
C-- Parameterdatei rueckladen
         READ(79+itab) MINW,MAXW,FMIN,MAXF,OMINW,GEWMAX,GEWMIN,NZ,
     #   QQUOTE,ABSBET,GEWRAN,SCHAEZ,XMINW,NULW,DQUOTE,DBQUA,
     #   NDIM,NGES,WGES,RFLAG,POT,TWERT,IDDD,LI,II,MS,MUS,ISZ,TEST
         REWIND  59+itab
         READ(59+itab) ijk,(BS(iji),iji=1,ijk),IRTAB
C-- Quaderquote ab 3. Durchlauf auf Null setzen
         IF (loop.GT.2) QQUOTE=0.0
         DO 90 IJ=1,NGES
C-- Basisdatei rueckladen
           READ (UNIT=19+itab,REC=IJ) IIZW,IRRZW,IDRZW,ROZW,RUZW,ue_lapp
C-- Einzelberichtspfl. ab 2. Bearbeitung auf ohne Pruefung
           IF (loop.GT.1) THEN
              IF(iizw/kwert.EQ.IGEI)
     #           iizw=int(iizw+(ispsm1-igei)*kwert)
           ENDIF
C-- Ueberlappungsbereich updaten
           IF (ue_lapp.NE.0) THEN
            IF (iizw.GE.0) THEN
              READ (UNIT=13,REC=ue_lapp)
     #              IIZW2,IRRZW2,IDRZW2,ROZW,RUZW
C- Sperrungen aus Ueberlappungen uebernehmen (ispso1,isps)
              IF (IIZW2/kwert.LE.isPS) THEN
                 iizw=iizw2
C- sek. Sperrungen aus Ueberlapp. in aktuelle Sperrtabelle uebernehmen
C- Basisdatei updaten fuer Altsperrungen
                 IIZW4=IIZW*(-1)+ISPSM1*KWERT
                 WRITE (UNIT=19+itab,REC=IJ)
     #                 IIZW4,IRIZW,IDRZW,ROZW,RUZW,0
                 ijk=ijk+1
                 IF (IJK.GT.ISK) THEN
                    WRITE (7,4180) ISK
                    STOP 2004
                 END IF
                 bs(ijk)=ij
C-- Altsperrungen mitzaehlen
                 i_sperr=i_sperr+1
              ENDIF
            ENDIF
           ENDIF
C- Einzelberichtspflichtige nicht im Ueberlappungsbereich temp markieren
  455      IF (loop.EQ.1.and.ue_lapp.EQ.0.and.iizw/kwert.eq.igei) then
                iizw=int(iizw+(igeit-igei)*kwert)
           ENDIF
           CALL RCIN(IIZW,IRRZW,IDRZW,ROZW,RUZW,TAB0(IJ))
   90    CONTINUE
        ELSE
C
C  Normal-Einzeltabelle einlesen
C
         REWIND 19+itab
         DO 91 IJ=1,NGES
          READ (UNIT=19+itab) IIZW,IRRZW,IDRZW,ROZW,RUZW,ue_lapp
          CALL RCIN(IIZW,IRRZW,IDRZW,ROZW,RUZW,TAB0(IJ))
   91    CONTINUE
        ENDIF
C
        NDIM1=NDIM+1
        IMUL=2**NDIM-1
        RSCHE=REAL(IMUL)*REAL(KWERT-1)*(-1)
        RSCHS=1.1*RSCHE
        RSCHG=2.0*REAL(IMUL)*ABS(RSCHS)
        IF(loop.EQ.1)
     #  WRITE (3,4150) IGEI,
     #               IGEIT,
     #               ISPP,
     #               ISPS,
     #               ISPS1,
     #               ISPSQ,
     #               ISPSQ1,
     #               ISPSO1,
     #               ISPSM1,
     #               RSCHE,
     #               RSCHS,
     #               RSCHG
C
C-----ITERATION DES GEHEIMHALTUNGSVERFAHRENS----------------------------
C
        DO 190 NNNN=1,100
         IGSP=0
         REWIND 39+itab
C
C-----SCHLEIFE UEBER ALLE UNTERTABELLEN---------------------------------
C
         DO 180 I=1,NZ
C
C-----EINLESEN DER INDICES UND UEBERTRAGEN DER WERTE DER UNTERTABELLE---
C-----IN DEN ARBEITSBEREICH---------------------------------------------
C
            READ (39+itab) MA,MP,IMDI,LM,(TABI(IJ),IJ=1,LM)
C
            DO 110 I1=1,LM
               IND=TABI(I1)
               IF (IND) 110,110,100
  100          TAB(I1)=TAB0(IND)
               CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,TAB(I1))
C---temp.EB AB 2. ITERATION zuruecksetzen
               if (nnnn.gt.1.and.iizw/kwert.eq.igeit.and.
     #             tabanz.gt.1) then
                  iizw=int(iizw+(igei-igeit)*kwert)
                  CALL RCIN(IIZW,IRIZW,IDRZW,ROZW,RUZW,TAB(I1))
               END IF
C----    EB AB 2. ITERATION AUF NORMALEN SEK. GEHEIM. SETZTEN
               IF (NNNN.GT.1.AND.IIZW/KWERT.EQ.IGEI) THEN
                  IIZW=IIZW-KWERT
                  CALL RCIN(IIZW,IRIZW,IDRZW,ROZW,RUZW,TAB(I1))
               END IF
C----
  110       CONTINUE
C
C-----BESETZEN DER ARBEITSFELDER--------------------------------------
C
            IF (NDIM1.GT.7) GO TO 130
            DO 120 II0=NDIM1,7
               IUU(II0)=IDDD
               IOO(II0)=IDDD
               IGG(II0)=1
               IMDI(II0)=1
  120       CONTINUE
  130       CONTINUE
C
C-----GEHEIMHALTUNGSVERFAHREN-----------------------------------------
C
            CALL ARBEIT (TAB, TABR, ID1, ID2, ID3, ID4, ID5, ID6, ID7,
     #       NDIM, RSCHS, RSCHE, RSCHG, NNNN, itab)
C
C-----AUSGABE DER SEKUNDAER GESPERRTEN FELDER DER UNTERTABELLE----------
C-----UND ZURUECKSETZEN DER UNTERTABELLE AUF WERT NULL------------------
C-----ABGEAENDERT :-----------------------------------------------------
C-----SORTIERUNG DER SEKUNDAER GESPERRTEN FELDER (REL. SATZNUMMER-------
C-----BEZOGEN AUF BESETZTE FELDER) IM HAUPTSPEICHER --------------------
C-----(TABELLE BS(IJK))-------------------------------------------------
C
            DO 170 IT=1,LM
               IND=TABI(IT)
C- change 18.01.2001 jerskat
               IF(IND.EQ.0) GOTO 170
C- change 18.01.2001 jerskat
               CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,TAB(IT))
               CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,TABR(IT))
C--- HOLEN ORIGINALTABELLE UM SCHAETZWERTE FUER PIVOT ABZUDATEN
               CALL RCOUT(IIZW2,IRIZW,IDRZW,ROZW2,RUZW2,TAB0(IND))
C-- Nur Sperrungen
               IF (IIZW) 140,170,160
C-- Altsperrungen die bereits erfasst wurden (einlesen-updaten)
  140          if (iizw/kwert.eq.isps.and.irizw3.gt.50) GOTO 160
               if (iizw/kwert.eq.ispsq.and.irizw3.gt.50) GOTO 160
C-- Nur Neusperrungen (ISPSQ,ISPSQ1) fuer Einzeltabelle
               IF (IIZW/KWERT-ISPSQ) 150,150,160
  150          IJK=IJK+1
               IF (IJK.GT.ISK) THEN
                  WRITE (7,4180) ISK
                  STOP 2004
               END IF
               BS(IJK)=TABI(IT)
               IGSP=10
C
C-----UEBERTRAGEN DER GESETZTEN SPERRVERMERKE IN DIE BASISTABELLE-------
C-----(WIRD BEI WEITEREN ITERATIONEN BENOETIGT)-------------------------
C
C - SPERRUNGEN SCHLIESSEN EINE GEWICHTUNG AUS/IMAGINAERER TEIL = 1
c*****changes giessing 02.01 a
c - GILT AUCH FUER GEWICHTUNGEN VOM TYP GEWRAN=2
               IF (GEWRAN.ne.0) IRIZW=INT(LOG(1.0/XMINW)*TWERT)+1
c               IF (GEWRAN.EQ.1) IRIZW=INT(LOG(1.0/XMINW)*TWERT)+1
c*****changes giessing 02.01 e
C- Sperrung in Aggr-Wechsel/Ueberlappung sek. Sperrung
               IF (IIZW/KWERT.EQ.ISPSQ1.AND.IDRZW3.GT.0) THEN
                   IIZW4=IIZW2*(-1)+ISPSM1*KWERT
                   IIZW3=IIZW2*(-1)+ISPS*KWERT
                   IIZW2=IIZW2*(-1)+ISPS1*KWERT
C- Sperrung in Aggr-Wechsel/Ueberlappung nicht doppelte Sicherung
               ELSEIF (IIZW/KWERT.EQ.ISPSQ1.AND.IDRZW3.EQ.0) THEN
                   IIZW4=IIZW2*(-1)+ISPSM1*KWERT
                   IIZW3=IIZW2*(-1)+ISPSO1*KWERT
                   IIZW2=IIZW2*(-1)+ISPS1*KWERT
C- Sperrung ohne Aggr-Wechsel/Ueberlappung nicht doppelte Sicherung
               ELSEIF (IIZW/KWERT.EQ.ISPSQ.AND.IDRZW3.EQ.0) THEN
                   IIZW4=IIZW2*(-1)+ISPSM1*KWERT
                   IIZW3=IIZW2*(-1)+ISPSO1*KWERT
                   IIZW2=IIZW2*(-1)+ISPS*KWERT
C- Sperrung ohne Aggr-Wechsel/Ueberlappung sek. Sperrung
               ELSE
                   IIZW4=IIZW2+(-1)+ISPSM1*KWERT
                   IIZW3=IIZW2*(-1)+ISPS*KWERT
                   IIZW2=IIZW2*(-1)+ISPS*KWERT
               ENDIF
C
C-- Updaten der temp. Basisdatei
C-- und uebertragen der Sperrvermerke in die Ueberschneidungsdatei
C
               IF (tabanz.NE.1) THEN
                   READ (UNIT=19+itab,REC=IND)
     #                   IIZW,IRIZW4,IDRZW4,ROZW4,RUZW4,ue_lapp
C- ue_lapp auf 0 setzen, daher keine weitere Pruefung der Ueberlappung
C- nur (ISPSM1) fuer Neusperrungen
                   WRITE (UNIT=19+itab,REC=IND)
     #                   IIZW4,IRIZW,IDRZW,ROZW3,RUZW3,0
                   IF (ue_lapp.NE.0) then
C- in Ueberschneidung mit oben ermittelten Sperrung (IIZW3) uebernehmen
C- nur (ISPS,ISPSO1)
                      WRITE (UNIT=13,REC=ue_lapp)
     #                      IIZW3,IRIZW,IDRZW,ROZW3,RUZW3
C--- Sperrungen in Ueberlappung fuer Einzeltabelle
                      ue_sperr=ue_sperr+1
C--- Sperrungen in Ueberlappung fuer Ueberpruefung ob weiterer Lauf
                      ueg_sperr=ueg_sperr+1
                   ELSE
C--- Normal-Sperrungen fuer Einzeltabelle
                      gs_sperr=gs_sperr+1
                   ENDIF
               ENDIF
  160          CONTINUE
C---
C--- UPDATEN DER OBEREN UND UNTEREN SCHAETZFEHLERGRENZEN BEI SEKUNDAER
C---
C- nur aktuelle Sperrungen uebernehmen (ISPS,ISPS1)
               CALL RCIN(IIZW2,IRIZW,IDRZW,ROZW3,RUZW3,TAB0(IND))
               CALL RCIN(0,IRIZW,IDRZW,ROZW3,RUZW3,TAB(IT))
  170       CONTINUE
  180    CONTINUE
         IF (tabanz.NE.1) THEN
          WRITE (3,4190) NNNN,tab_nam(itab),i_sperr,gs_sperr,ue_sperr
         ENDIF
C
         IF (IGSP.EQ.0) GO TO 200
  190   CONTINUE
C
C-- Sperrtabelle einer Tabelle
C
  200   IF (tabanz.NE.1) then
         REWIND  59+itab
C-- Sperrverzeichnisdatei aktuallisieren
         WRITE(59+itab) ijk,(BS(iji),iji=1,ijk),IRTAB
        ENDIF
C
C  Ende Iterationsschleife ueber Tabellengeheimhaltung
C
  600 CONTINUE
      IF (ueg_sperr.eq.0) THEN
         GOTO 650
      ENDIF
      loop=loop+1
      GOTO 87
  650 CONTINUE
C
C-----UPDATE-VORGANG----------------------------------------------------
C
      IF (tabanz.NE.1) THEN
         DO 700 i=1,tabanz
            REWIND 79+i
C-- Parameterdatei zuruecklesen
            READ(79+i) MINW,MAXW,FMIN,MAXF,OMINW,GEWMAX,GEWMIN,NZ,
     #        QQUOTE,ABSBET,GEWRAN,SCHAEZ,XMINW,NULW,DQUOTE,DBQUA,
     #        NDIM,NGES,WGES,RFLAG,POT,TWERT,IDDD,LI,II,MS,MUS,ISZ,TEST
C-- Sperrverzeichnisdatei zuruecklesen
            REWIND 59+i
            READ(59+i) ijk,(BS(iji),iji=1,ijk),IRTAB
C
C-----RANDSPERRUNG-TABELLE/ Ende der Einzeltabellengeheimhaltung
C
            WRITE (3,4200) tab_nam(i)
C
            DO 710 II1=1,NDIM
               IF (RFLAG(II1).EQ.1) THEN
                   WRITE (3,4220) II1,DIMBEZ(i,ii1)
                ELSEIF (RFLAG(II1).EQ.0) THEN
                      WRITE (3,4240) II1,DIMBEZ(i,ii1)
                    ELSE
                      WRITE (3,4245) II1,DIMBEZ(i,ii1)
               END IF
  710       CONTINUE
C
            WRITE (3,'(1X)')
            DO 730 II1=1,2**NDIM-1
                IF (IRTAB(II1).EQ.0) GO TO 730
                OUTL=' '
               II3=0
               DO 720 II2=0,NDIM-1
                IF (BTEST(II1,II2)) THEN
                 WRITE (OUTL(II3*9+1:II3*9+9),'(I1,A)') II2+1,'. DIM., '
                 II3=II3+1
                END IF
  720          CONTINUE
C
               WRITE (3,4250) IRTAB(II1),OUTL
  730       CONTINUE
C-IBM-Version
*           CALL CTime(time)
*           WRITE(7,1009) tab_nam(itab),Time
C-PC-VERSION
            WRITE(7,1009) tab_nam(itab),CTime(Time())
c     changes e.drumm 15.11.01
c		call time(hour)
c      write(7,1009) tab_nam(itab),hour
c     end changes e.drumm
C
            CALL OpenFile (tab_nam(i), 1, 'O', 'F', ' ', 0)
            CALL OpenFile (tab_out(i), 8, 'N', 'F', ' ', 0)
c*****changes giessing 08.01 a
c         CALL UPDA (NGES, WGES, ISK, BS, LST, STACK, IJK, NDIM)
          CALL UPDA (NGES, WGES, ISK, BS, LST, STACK, IJK, NDIM,mode)
c*****changes giessing 08.01 e
            CLOSE (UNIT=19+i)
            CLOSE (UNIT=39+i)
            CLOSE (UNIT=59+i)
            CLOSE (UNIT=79+i)
  700    CONTINUE
      ELSE
C
C-----RANDSPERRUNG-TABELLE/ Ende der Einzeltabellengeheimhaltung
C
         WRITE (3,4200)
C
         DO 210 II1=1,NDIM
            IF (RFLAG(II1).EQ.1) THEN
               WRITE (3,4220) II1,DIMBEZ(1,II1)
             ELSEIF (RFLAG(II1).EQ.0) THEN
                   WRITE (3,4240) II1,DIMBEZ(1,II1)
                 ELSE
                   WRITE (3,4245) II1,DIMBEZ(1,II1)
            END IF
  210    CONTINUE
C
         WRITE (3,'(1X)')
         DO 230 II1=1,2**NDIM-1
            IF (IRTAB(II1).EQ.0) GO TO 230
            OUTL=' '
            II3=0
            DO 220 II2=0,NDIM-1
               IF (BTEST(II1,II2)) THEN
                WRITE (OUTL(II3*9+1:II3*9+9),'(I1,A)') II2+1,'. DIM., '
                II3=II3+1
               END IF
  220       CONTINUE
C
            WRITE (3,4250) IRTAB(II1),OUTL
  230    CONTINUE


C-IBM-Version
*        CALL CTime(time)
*        WRITE(7,1009) tab_nam(itab),Time
C-PC-VERSION
         WRITE(7,1009) tab_nam(itab),CTime(Time())
c     changes e.drumm 15.11.01
c		call time(hour)
c      write(7,1009) tab_nam(itab),hour
c     end changes e.drumm
C
         CALL OpenFile (tab_out(1), 8, 'N', 'F', ' ', 0)
c*****changes giessing 08.01 a
c         CALL UPDA (NGES, WGES, ISK, BS, LST, STACK, IJK, NDIM)
         CALL UPDA (NGES, WGES, ISK, BS, LST, STACK, IJK, NDIM,mode)
c*****changes giessing 08.01 e
      ENDIF
c----changes giessing 03/03-a

         write(2,*) (NREDU(i),i=1,11)
c----changes giessing 03/03-e
C
C-IBM-Version
*     CALL CTime(time)
*     WRITE (7,4270) Time
C-PC-VERSION
      WRITE(7,4270) CTime(Time())
c     changes e.drumm 15.11.01
c		call time(hour)
c      write(7,4270) hour
c     end changes e.drumm
C
C-----CLOSE DER DATEIEN ------------------------------------------------
C
      CLOSE (UNIT=1)
      CLOSE (UNIT=4)
      CLOSE (UNIT=7)
      CLOSE (UNIT=8)
      CLOSE (UNIT=13)
C
      RETURN
      END
c*****changes giessing 02.01 a
C      SUBROUTINE LOAD (IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS,
C     # IW, SS, III, NGES, WGES, ISZ, KEYF, itab ,imxdim, lst, stack,
C     # ss_stell,ierr)

c------Bei Realisierung der Einbindung von poolac zu verwenden
c------(noch klären: wird Feld 'daten' gebraucht?
c      SUBROUTINE LOAD (IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS, IW,
c      #SS, III, NGES, WGES, ISZ, KEYF,itab, imxdim, lst, stack,
c      #ss_stell,ierr,MODE,nextcall,schl,
c      #wert,gewi,wtart,berpfl,daten,ultimkey,hoew,fehlo,fehlu,imodw)

      SUBROUTINE LOAD (IMAX, NDIM, IL, IA, IP, IU, IO, UK, OK, IMS, MUS,
     # IW, SS, III, NGES, WGES, ISZ, KEYF,itab, imxdim, lst, stack,
     #ss_stell,ierr,MODE)
c      #,nextcall,ultimkey,
c      #schl,wert,gewi,wtart,berpfl,daten,hoew,fehlo,fehlu,imodw)
c*****changes giessing 02.01 E
C  -------------------------------------------------------------------
C I                                                                   I
C I   L O A D                                                         I
C I                                                                   I
C I   SUBROUTINE ZUM LADEN DER ARBEITSDATEIEN UND STEUERUNG DER       I
C I   INDEXPRUEFUNG HINSICHTLICH DER AGGREGATION UND POSITION         I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   SUBROUTINE LOAD(IMAX,NDIM,IL,IA,IP,IU,IO,UK,OK,IMS,MUS,IW,SS,   I
C I  * III,NGES,WGES,ISZ,KEYF)                                        I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I   IMAX LAENGE DER ARBEITSTABELLE (I*4)                            I
C I   NDIM ZAHL DER DIMENSIONEN      (I*4)                            I
C I     IL ZAHL DER POSITIONEN       (I*4)                            I
C I     IA AGGREGATIONSTABELLE       (I*4)                            I
C I     IP POSITIONSTABELLE          (I*4)                            I
C I     IU UNTERE TEILMATRIXINDICES  (I*4)                            I
C I     IO OBERE  TEILMATRIXINDICES  (I*4)                            I
C I    IMS ZAHL DER AGGREGATIONEN    (I*4)                            I
C I    MUS ANZAHL DER SCHLOESSER     (I*4)                            I
C I     IW ZAHL DER SCHLOESSER       (I*4)                            I
C I     SS SCHLOESSERTABELLE         (I*4)                            I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I    III VORGEF. DIMENSIONEN       (I*4)                            I
C I   NGES ANZAHL DER BES. FELDER    (I*4)                            I
C I   WGES WERTESUMME DER BES. FELDER(I*4)                            I
C I    ISZ ZAHL DER EING.-SAETZE     (I*4)                            I
C I                                                                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   AGGPOS,AGTEST                                                   I
C  -------------------------------------------------------------------
      IMPLICIT REAL*8 (W,P)
C
c*****changes giessing 02.01 a
      parameter (maxmodw=7)
c*****changes giessing 02.01 e

      LOGICAL NEG,NRFLAG,NEGTST,FEHLER,DBQUA
c*****changes giessing 02.01 a
      logical nextcall
      logical ultimkey
c*****changes giessing 02.01 e
C
      CHARACTER*4 TEST
C- NEU 28.04.04
*      CHARACTER*8 SCHL(7),SS(3500,7),VSCHL1,VSCHL2,NULL/' '/
      CHARACTER*8 SCHL(7),SS(12000,7),VSCHL1,VSCHL2,NULL/' '/
C- NEU 28.04.04
      CHARACTER*8 tab_nam(20),tab_out(20),tab_ft04(20)
C-IBM-Version
*     CHARACTER*36 time
C
      INTEGER I,IJ
      INTEGER BERPFL,WTART,IMS,IW,IL,NDIM,MA(7),MP(7),MS(7),IERR(7),
     #IA(IL,imxdim),IP(IL,imxdim),IO(IL,imxdim),IU(IL,imxdim),
     #MUS(IMS,imxdim),
     #II(7),LI(7),IMAX,TAB(3257),UK(IL,imxdim),OK(IL,imxdim),
     #K(7),KEYF(IW,imxdim)
      INTEGER*4 SKL3,SKL4,SKL2
      REAL*4 SFEHLO,SFEHLU
      INTEGER*4 KL3,KL4,INLL,KL2
      INTEGER RFLAG(7),ABSBET,GEWRAN,IVV,SCHAEZ
      INTEGER KWERT
      INTEGER lst,stack(lst),lstn,istart,ss_stell
      INTEGER lap_point,stell,yes,itab,neu_start
C- NEU 28.04.04
*      INTEGER*2 erk_tab(0:40,0:750000),int_tab(20,40,3500)
      INTEGER*2 erk_tab(0:40,0:750000),int_tab(20,40,12000)
C- NEU 28.04.04
      INTEGER indx(750000)
      INTEGER tabanz,anz_dim(20),pseu_stell(0:5,0:40,0:20)
      INTEGER anz_schl(0:40)
c*****changes giessing 02.01 a
      integer anzmodw(maxmodw)
      integer mode
      integer isz
      integer imodw,smodw
      integer nges,III
      integer NSZ,IISZ,I0
      integer print1
c*****changes giessing 02.01 e

      CHARACTER*8 pseu_key(0:40,0:20),save_schl(40)
C- NEU 28.04.04
*      CHARACTER*8 nam_dim(40),schl_nam(0:40,0:3500)
      CHARACTER*8 nam_dim(40),schl_nam(0:40,0:12000)
C- NEU 28.04.04
	character*8 hour
C
      REAL*8 HOEW,WHOEW
      REAL*8 FEHLO,FEHLU
      REAL*8 WERT
      REAL*8 OMINW
      REAL*8 GEWI,XGEWI
      REAL*8 POT(7),GEWMAX,GEWMIN
      REAL*8 FMIN,MAXF
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
c*****changes giessing 02.01 a
      real*8 preflow(maxmodw),prefup(maxmodw)
      real*8 wges
c*****changes giessing 02.01 e
C
      COMMON /OSAV/OMINW
      COMMON /INSTA/POT,GEWMAX,GEWMIN
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMMON /INDEX/ II,LI,MS,MA,MP
      COMMON /RFLAG/ RFLAG
      COMMON /KWERT/ KWERT
      COMMON /ABSBET/ ABSBET,GEWRAN,SCHAEZ,DBQUA
      COMMON /LAPP/ tabanz
      COMMON /DAT/ tab_ft04,tab_nam,tab_out
      COMMON /PSEU/ anz_dim,pseu_stell,stell
      COMMON /SSLA/ anz_schl,nam_dim,pseu_key
      COMMON /SSNA/ schl_nam
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab

c*****changes giessing 08.01 a
c - UEBERGABE AUS WEIGHTS
      common /prefer/preflow,prefup
      common /print1/print1
c*****changes giessing 08.01 e
C
C-IBM-Version
*     INCLUDE (LOADM)
C-PC-VERSION
      INCLUDE 'LOADM'

c     changes e.drumm 15.11.01
      nextcall=.false.
	ultimkey=.false.
c     end changes e.drumm 15.11.01

c*****changes giessing 08.01 a
c      write(0,*) 'load,mode',mode

       if((mode.eq.4).or.(mode.eq.2)) then
c------VORBEREITUNG DER RANDSCHRANKENKONFORMEN GEWICHTUNG TYP 2.
C------EINLESEN DER AUS DEN TABELLEN ERMITTELTEN VERTEILUNGSPARAMETER
c------NUR FALLS NICHT GEWRAN EQ 3
        read(18,*) (anzmodw(modw),modw=1,7)
c         write(0,*)'anzmodw(modw),modw=1,7'
c         write(0,*) (anzmodw(modw),modw=1,7)
         smodw=0
         do 23 modw=1,7
23        smodw=smodw+anzmodw(modw)
C------AUFRUF DES UNTERPROGRAMMS WEIGHTS ZUR BERECHNUNG DER
C------PARAMETER Preflow UND Prefup FUER DIE SPREIZUNG BEI DER GEWICHTUNG
         if(smodw.gt.0)
     #     call weights(ndim,anzmodw)

        else
         if(gewran.ge.2) gewran=0
       endif

c*****changes giessing 08.01 e

c*****changes giessing 02.01 a
c ----BEIM EINLESEN UEBER SEQUENCES BZW KEYS (MODE 1)
C-----IST ULTIMKEY ERFUELLT WENN DER LETZTE SATZ VERARBEITET IST

      if((mode.eq.1).and.ultimkey) goto 180

C-----NEXTCALL IST ERFUELLT WENN DER ERSTE SATZ VERARBEITET IST
C-----DANN EINSPRUNG HINTER: 70    READ(1,....)

      if((mode.eq.1).and.(nextcall)) goto 71

c*****changes giessing 02.01 e
C
C-----Tabellespezifischen Sort auf Suchtabelle(erg_tab,Indx)-----------
C-----nur bei ueberlappung aufrufen (Dimensionsreihenfolge)
C
      istart=0
      IF (neu_start.gt.0) then
       istart=1
       do 7557 i=1,lst
          stack(i)=0
 7557  continue
       LSTN=LST/2
C-IBM-Version
*      CALL CTime(time)
*      WRITE(7,1001) itab,Time
C-PC-VERSION
c       WRITE(7,1001) itab,CTime(Time())
c     changes e.drumm 15.11.01
       WRITE(7,1001) itab,CTime(Time())
c      call time(hour)
c      write(7,1001)itab, hour
c     end changes e.drumm 15.11.01
C
       CALL NSORT(ndim,itab,ss_stell,lstn,stack,anz_schl)
C-IBM-Version
*      CALL CTime(time)
*      WRITE(7,1002) itab,Time
C-PC-VERSION
c     changes e.drumm 15.11.01
       WRITE(7,1002) itab,CTime(Time())
c      call time(hour)
c      write(7,1002)itab, hour
c     end changes e.drumm 15.11.01
C
      ENDIF
C
C-----BESETZUNGSTABELLE EROEFFNEN---------------------------------------
C
      NEG=.FALSE.
      ifou=0
C
      GEWMAX=0.0
      GEWMIN=10.0**37
      FMIN=MINW
      MAXF=MAXW
      INLL=0
      IVV=1
      ISZ=1
      IISZ=1
      NSZ=1
      NGES=0
      WGES=0.0
      ISEK=0
      ISEKN=0
      IWN=0
      WSEK=0.0
      NEINZ=0
      WEINZ=0.0
      NPRIM=0
      WPRIM=0.0
      NUEIN=0
      WUEIN=0.0
      NNULL=0
      WNULL=0.0
      NGN=0
      WGN=0.0
      NENULL=0
      NGNULL=0
      WGNULL=0.0
      WLWERT=10.0**37
      WHWERT=0.0
      WBWERT=0.0
      WAWERT=10.0**37
      NRFLAG=.FALSE.
      NEGTST=.TRUE.
      FEHLER=.FALSE.
C
C-----EINGABE DER ZU SPERRENDEN TABELLE (FESTES FORMAT)-----------------
C
c*****changes giessing 02.01 a

C-----EINGABE DER ZU SPERRENDEN TABELLE NUR BEI MODE 0,2,4
C-----SONST VERWENDUNG DER VARIABLENBELEGUNG FUER WTART ETC
C-----AUS UEBERGABE KEYS
c-----BEI MODE 2 u. 4 ZUSÄTZLICH EINLESEN DER GEWICHTSPARAMETER IMODW
       if(mode.eq.0) then
        READ (1,*,END=180) WTART,BERPFL,WERT,GEWI,
     #  HOEW,FEHLO,FEHLU,(SCHL(I),I=1,NDIM)
       else if((mode.eq.2).or.(mode.eq.4)) then
        READ (1,*,END=180) WTART,BERPFL,WERT,GEWI,IMODW,
     #  HOEW,FEHLO,FEHLU,(SCHL(I),I=1,NDIM)
       endif
c       write(0,*) 'WTART,BERPFL,WERT,GEWI,IMODW,
c     #  HOEW,FEHLO,FEHLU,(SCHL(I),I=1,NDIM)'
c       write(0,*) WTART,BERPFL,WERT,GEWI,IMODW,
c     #  HOEW,FEHLO,FEHLU,(SCHL(I),I=1,NDIM)
c*****changes giessing 02.01 e
C
   22 XGEWI=1.0
      WHOEW=HOEW

      IF(GEWRAN.EQ.1) then
         IF(GEWI.LT.0) then
            WRITE (7,4065) GEWI
            STOP 3003
         ENDIF
      ELSE
         GEWI=1.0
c*****changes giessing 02.01 A
c-----GEWICHTUNG TYP2 IST RANDSCHRANKENKONFORM; ALSO
C-----AUFHEBEN DER RANDSCHRANKE NICHT ERFORDERLICH
C         GEWRAN=0
c*****changes giessing 10.02 a
         if((gewran.ne.2).and.(gewran.ne.3)) GEWRAN=0
c*****changes giessing 10.02 e
c*****changes giessing 02.01 e
      ENDIF

      IF ((FEHLO.LT.0.0.OR.FEHLU.LT.0.0).AND.SCHAEZ.EQ.1) THEN
         WRITE (7,4067) FEHLO,FEHLU
         STOP 3004
      ENDIF

      GEWMAX=MAX(GEWMAX,GEWI)
      GEWMIN=MIN(GEWMIN,GEWI)
C-----ZWISCHENSICHERUNG DES ORIGINALWERTES
      WWERT=WERT
C
      XMINW=MINW

C
C----VORBEREITUNG ABSOLUTER BETRAG FUER NUR NEGATIVE TABELLE
C
      IF (ABSBET.EQ.3) THEN
C----ANSONSTEN WIRD DER WERT WIE BEI EINER POS. TABELLE BEHANDELT
         WERT=ABS(WERT)
C----HOECHSTER EINZELWERT FUER DOMINANZ AN ABSOLUTEN BETRAG ANPASSEN
         IF (HOEW.EQ.0.0) THEN
            IF (BERPFL.GE.1) HOEW=XMINW
         ELSE
            HOEW=ABS(HOEW)
         ENDIF
      ENDIF
C
C----VORBEREITUNG ABSOLUTER BETRAG
C
      IF (ABSBET.EQ.2) THEN
         WRITE (3,4080)

         IF (WERT.EQ.0.0) THEN
C---------- NULL-FAELLE BEI NEGATIVEN WERTEN WERDEN ALS WERT BETRACHTET
            IF (BERPFL.GE.1) WERT=XMINW
         ELSE
            IF (WERT.EQ.NULW) WERT=XMINW
            IF (WERT.LT.0.0) NEGTST=.FALSE.
            WERT=ABS(WERT)
         END IF
C--- HOECHSTER EINZELWERT FUER DOMINANZ AN ABSOLUTEN BETRAG ANPASSEN
         IF (HOEW.EQ.0.0) THEN
            IF (BERPFL.GE.1) HOEW=XMINW
         ELSE
            HOEW=ABS(HOEW)
         ENDIF
      END IF
C
C----VORBEREITUNG WERTEVERSCHIEBUNG (NEGATIVE WERTE)
C

      IF (ABSBET.EQ.1) THEN
         WRITE (3,4090)

C---------FUER NULLWERTE WIRD DAS UMSATZSTEUERVERHAELTNIS 1996 ALS
C---------BERECHNUNGSGRUNDLAGE GENUTZT (HIER:10**8)
         IF (ABS(MAXW).GT.ABS(MINW)) THEN
            XMINW=ABS(MAXW)/10**8
         ELSE
            XMINW=ABS(MINW)/10**8
         END IF
C---------VERSCHIEBUNGSWERTE FUER ALLE WERTE
         MAXW=MAXW-MINW+XMINW
         IF (WERT.EQ.NULW) WERT=XMINW
C--------NULLWERTE WERDEN BEI NEGATIVEN WERTEN ALS NORMALE WERTE GESETZT
         IF (BERPFL.GE.1) THEN
            IF (WERT.LT.0.0) NEGTST=.FALSE.
            WERT=WERT-MINW+XMINW
C--- HOECHSTER EINZELWERT FUER DOMINANZ AN VERSCHIEBUNG ANPASSEN
            HOEW=HOEW-MINW+XMINW
            TWERT=REAL(KWERT-2)/LOG(MAXW/XMINW)
         END IF
      ELSE
         TWERT=REAL(KWERT-2)/LOG(MAXW/XMINW)
      END IF
C
C----UEBERPRUEFUNG DES FEHLERBEREICHES
C
      IF (FEHLU.GT.WERT.AND.(ABSBET.NE.2.AND.ABSBET.NE.1)) THEN
         IF(schaez.eq.1) WRITE (3,4094) FEHLU
         FEHLU=WERT
         IF(schaez.eq.1) WRITE (3,4098) FEHLU
      ENDIF
C
C----UEBERPRUEFUNG DES WERTEBEREICHES DA SONST KLASSIERUNGSFEHLER
C
      IF (WERT.GE.MAXW.AND.WERT.NE.NULW) THEN
         WRITE (3,4100) MAXW,WWERT,WERT

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C----UEBERPRUEFUNG DES EINZELWERTBEREICHES DA SONST KLASSIERUNGSFEHLER
C
      IF (HOEW.GT.MAXW) THEN
         WRITE (3,4105) MAXW,WHOEW,HOEW
C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      ENDIF
C
C----UEBERPRUEFUNG DES EINZELWERTBEREICHES DA SONST KLASSIERUNGSFEHLER
C
      IF (GEWI.GT.MAXW) THEN
         WRITE (3,4106) MAXW,GEWI
C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      ENDIF
C- BEI GEWICHTUNG WIRD DIE RANDSCHRANKENWERTE UNTERBUNDEN, DA SONST
C- VERZERRUNGEN IN DEN SUMMEN AUFTRETEN
C
      IF (GEWRAN.EQ.1) NRFLAG=.TRUE.
C
C- UEBERPRUEFUNG DES MINIMALWERTES/NEGATIV DANN UMBERECHNUNG
C
      IF (XMINW.GT.WERT.AND.WERT.NE.0.0) THEN
         WRITE (3,4110) XMINW,WWERT,WERT

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C- UEBERPR. DES MINIMAL HOECHST.-EINZELWERT/NEGATIV DANN UMBERECHNUNG
C
      IF (XMINW.GT.HOEW.AND.HOEW.NE.0.0) THEN
         WRITE (3,4115) XMINW,WHOEW,HOEW

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C- UEBERPR. DES MINIMAL GEWICHTSWERTES DANN UMBERECHNUNG
C
      IF (XMINW.GT.GEWI.AND.GEWI.NE.0.0) THEN
         WRITE (3,4116) XMINW,GEWI

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C---------------------------------------------------------
C 1234 FORMAT(15X,A8,2X,F15.0,F15.0,13X,I5,4X,I10,6A8)
      IF (BERPFL.EQ.1.AND.WTART.LE.127) THEN
         WRITE (7,4120) (SCHL(I),I=1,NDIM)
         STOP 3006
      END IF
C
C-----PRUEFEN VON AGGREGATION UND POSITION UEBER BIS ZU 7 DIMENSIONEN---
C
C-----ANFANGSWERTE------------------------------------------------------
C
      WRITE (3,4130) tab_nam(itab),WTART,BERPFL,WWERT,GEWI,WHOEW,
     #    FEHLO,FEHLU,NDIM,(SCHL(I),I=1,NDIM)

c      write(0,*) 'tab_nam(itab),WTART,BERPFL,WWERT,GEWI,WHOEW,
c     #    FEHLO,FEHLU,NDIM,(SCHL(I),I=1,NDIM)'
c      write(0,*) tab_nam(itab),WTART,BERPFL,WWERT,GEWI,WHOEW,
c     #    FEHLO,FEHLU,NDIM,(SCHL(I),I=1,NDIM)
      VSCHL1=SCHL(1)
      VSCHL2=SCHL(2)

      DO 40 L=1,NDIM
c         write(0,*) '40-schleife',l
         IP(1,L)=1
         II(L)=0
         LI(L)=1
         IERR(L)=0
         K(L)=0
c         write(0,*) 'vor aggpos'
         CALL AGGPOS (MS(L), MUS(1,L), SS(1,L), LI(L), SCHL(L), II(L),
     #    IA(1,L), IP(1,L), IU(1,L), IO(1,L), UK(1,L), OK(1,L), IERR(L),
     #     K(L), KEYF(1,L))
c         IF (IERR(L)) 230,40,230
c*****changes giessing 08.01 a
         IF (IERR(L).ne.0) then
           iii=l
           goto 230
         endif
c*****changes giessing 08.01 e
   40 CONTINUE
c      write(0,*) 'nach aggpos'
      III=1
      IV=1
      GO TO 100
C
C-----AUSGABE DER INDIZES DER BESETZTEN TABELLENFELDER------------------
C
   50 IF (IISZ.LT.3258) GO TO 60
c      write(0,*) 'vor write(60'
      WRITE (UNIT=59+itab,REC=NSZ) (TAB(I0),I0=1,3257)
c      write(0,*) 'write 60 geklappt'

      IISZ=1
      NSZ=NSZ+1
   60 TAB(IISZ)=0

c*****changes giessing 08.01 a

   70 if(mode.eq.0) then
        READ (1,*,END=180) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,
     #  (SCHL(I),I=1,NDIM)
       else if((mode.eq.2).or.(mode.eq.4)) then
        READ (1,*,END=180) WTART,BERPFL,WERT,GEWI,IMODW,HOEW,
     #  FEHLO,FEHLU,(SCHL(I),I=1,NDIM)
       else
C------NAECHSTER SATZ AUS KEYS-------------
         if(mode.eq.1) return
       endif
C------EINSPRUNG AUS KEYS NACH VERARBEITUNG DES ERSTEN SATZES-----
   71  continue
c*****changes giessing 08.01 e
C
   76 XGEWI=1.0
      WHOEW=HOEW
      IF(GEWRAN.EQ.1) then
         IF(GEWI.LT.0) then
            WRITE (7,4065) GEWI
            STOP 3007
         ENDIF
      ELSE
         GEWI=1.0
      ENDIF

      IF ((FEHLO.LT.0.0.OR.FEHLU.LT.0.0).AND.SCHAEZ.EQ.1) THEN
         WRITE (7,4067) FEHLO,FEHLU
         STOP 3008
      ENDIF

      GEWMAX=MAX(GEWMAX,GEWI)
      GEWMIN=MIN(GEWMIN,GEWI)

C-----SICHERUNG ORIGINALWERT
      WWERT=WERT
C
C----VORBEREITUNG ABSOLUTER BETRAG FUER NUR NEGATIVE TABELLE
C
      IF (ABSBET.EQ.3) THEN
C----ANSONSTEN WIRD DER WERT WIE BEI EINER POS. TABELLE BEHANDELT
         WERT=ABS(WERT)
C----HOECHSTER EINZELWERT FUER DOMINANZ AN ABSOLUTEN BETRAG ANPASSEN
         IF (HOEW.EQ.0.0) THEN
            IF (BERPFL.GE.1) HOEW=XMINW
         ELSE
            HOEW=ABS(HOEW)
         ENDIF
      ENDIF
C
C----VORBEREITUNG ABSOLUTER BETRAG
C
      IF (ABSBET.EQ.2) THEN
         IF (WERT.EQ.0.0) THEN
C-----------NULL-FAELLE BEI NEGATIVEN WERTEN WERDEN ALS WERT BETRACHTET
            IF (BERPFL.GE.1) WERT=XMINW
         ELSE
            IF (WERT.EQ.NULW) WERT=XMINW
            IF (WERT.LT.0.0) NEGTST=.FALSE.
            WERT=ABS(WERT)
         END IF
C--- HOECHSTER EINZELWERT FUER DOMINANZ AN ABSOLUTEN BETRAG ANPASSEN
         IF (HOEW.EQ.0.0) THEN
            IF (BERPFL.GE.1) HOEW=XMINW
         ELSE
            HOEW=ABS(HOEW)
         ENDIF
      END IF
C
C----VORBEREITUNG WERTEVERSCHIEBUNG (NEGATIVE WERTE)
C
      IF (ABSBET.EQ.1) THEN
         IF (WERT.EQ.NULW) WERT=XMINW
         IF (BERPFL.GE.1) THEN
            IF (WERT.LT.0.0) NEGTST=.FALSE.
            WERT=WERT-MINW+XMINW
C--- HOECHSTER EINZELWERT FUER DOMINANZ AN VERSCHIEBUNG ANPASSEN
            HOEW=HOEW-MINW+XMINW
         END IF
      END IF
C
C----UEBERPRUEFUNG DES FEHLERBEREICHES
C
      IF (FEHLU.GT.WERT.AND.(ABSBET.NE.2.AND.ABSBET.NE.1)) THEN
         IF(schaez.eq.1) WRITE (3,4094) FEHLU
         FEHLU=WERT
         IF(schaez.eq.1) WRITE (3,4098) FEHLU
      ENDIF
C
C----UEBERPRUEFUNG DES WERTEBEREICHES DA SONST KLASSIERUNGSFEHLER
C
      IF (.NOT.FEHLER.AND.WERT.GE.MAXW.AND.WERT.NE.NULW) THEN
         WRITE (3,4100) MAXW,WWERT,WERT

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C----UEBERPRUEFUNG DES EINZELWERTBEREICHES DA SONST KLASSIERUNGSFEHLER
C
      IF (.NOT.FEHLER.AND.HOEW.GT.MAXW) THEN
         WRITE (3,4105) MAXW,WHOEW,HOEW
C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      ENDIF
C
C----UEBERPRUEFUNG DES EINZELWERTBEREICHES DA SONST KLASSIERUNGSFEHLER
C
      IF (.NOT.FEHLER.AND.GEWI.GT.MAXW) THEN
         WRITE (3,4106) MAXW,GEWI
C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C- BEI GEWICHTUNG WIRD DIE RANDSCHRANKENWERTE UNTERBUNDEN, DA SONST
C- VERZERRUNGEN IN DEN SUMMEN AUFTRETEN
C
      IF (GEWRAN.EQ.1) NRFLAG=.TRUE.
C
C- UEBERPRUEFUNG DES MINIMALWERTES/NEGATIV DANN UMBERECHNUNG
C
      IF (.NOT.FEHLER.AND.XMINW.GT.WERT.AND.WERT.NE.0.0) THEN
         WRITE (3,4110) XMINW,WWERT,WERT

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C- UEBERPR. DES MINIMAL HOECHST.-EINZELWERT/NEGATIV DANN UMBERECHNUNG
C
      IF (.NOT.FEHLER.AND.XMINW.GT.HOEW.AND.HOEW.NE.0.0) THEN
         WRITE (3,4115) XMINW,WHOEW,HOEW

C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
C- UEBERPR. DES MINIMAL GEWICHTUNGSWERT DANN UMBERECHNUNG
C
      IF (.NOT.FEHLER.AND.XMINW.GT.GEWI.AND.GEWI.NE.0.0) THEN
         WRITE (3,4116) XMINW,GEWI
C
C-------IM FEHLERFALL BESTAND DURCHGEHEN
         FEHLER=.TRUE.
      END IF
C
      IF (BERPFL.EQ.1.AND.WTART.LE.127) THEN

         WRITE (3,4160)
         WRITE (7,4120) (SCHL(I),I=1,NDIM)
         STOP 3009
      END IF
      ISZ=ISZ+1
      IISZ=IISZ+1
C
C-----PRUEFUNG DER GUELTIGEN FOLGE VON AGGREGATION UND POSITION---------
C-----IN DER EINGABEDATEI (JE DIMENSION III)----------------------------
C
      IF (III+1.GT.NDIM) GO TO 90
      IF (VSCHL2.EQ.SCHL(III+1)) GO TO 90
      CALL AGTEST (II(III), IA(1,III), IERR(III))
      IF (IERR(III)) 230,80,230
   80 IV=IV*LI(III)
      III=III+1
      VSCHL1=NULL
      IF (III+1.GT.NDIM) GO TO 90
      VSCHL2=SCHL(III+1)
   90 CONTINUE
      IF (VSCHL1.EQ.SCHL(III)) THEN
         IVV=IVV+1
         IF (IVV.GT.IV) THEN
            WRITE (7,4170) VSCHL1
            STOP 3010
         END IF
         GO TO 100
      END IF
      IVV=1
      LI(III)=LI(III)+1
      VSCHL1=SCHL(III)
      CALL AGGPOS (MS(III), MUS(1,III), SS(1,III), LI(III), SCHL(III),
     #II(III), IA(1,III), IP(1,III), IU(1,III), IO(1,III), UK(1,III),
     #OK(1,III), IERR(III), K(III), KEYF(1,III))
      IF (IERR(III)) 230,100,230
C
C-----BILDEN BESETZUNGSZAHL,WERTESUMME----------------------------------
C
  100 CONTINUE
c      write(0,*) 'nach 100'
C----------------
C- TESTAUSGABE DER VERAENDERTEN WERTE/ORIG. WERTE UND SCHLUESSEL
C
C-----ABARBEITUNG UND KLASSIERUNG IM NORMALFALL-------------------------
C-----BEI DER KLASSIERUNG IST DIE NULL-WERT-KLASSIERUNG VON DEN WERT-  -
C-----BEZOGENEN KLASSIERUNG EINDEUTIG GETRENNT (DURCH INTEGER BERECHN.)-
C-GW  DIE GEWICHTUNG FLIESST IN GESPERRTE WERTE NICHT MIT EIN !
C
      IF ((WERT.EQ.0.0).AND.(WTART.LT.128).AND.(BERPFL.LT.1)) THEN
         INLL=INLL+1
         GOTO 50
      ENDIF
C----
C---- RANGEBEREICH ERMITTELN
C----
C-WERT=0.0 U. NULW WIRD AUF MINIM. WERT DER JEWEILIGEN KLASSE GESETZT
      NGES=NGES+1
      IF (WERT.EQ.NULW.OR.WERT.EQ.0.0) THEN
         IWN=IWN+1
      ELSE IF (WWERT.NE.NULW.AND.WWERT.NE.0.0) THEN
C---- ERMITTELN MINWERT UND MAXWERT(ABS UND NEGATIV)
       if (hoew.ne.0.0) then
         WLWERT=MIN(WLWERT,HOEW)
         WAWERT=MIN(WAWERT,ABS(HOEW))
         WHWERT=MAX(WHWERT,HOEW)
         WBWERT=MAX(WBWERT,ABS(HOEW))
       endif
       if (gewi.ne.0.0) then
         WLWERT=MIN(WLWERT,GEWI)
         WAWERT=MIN(WAWERT,ABS(GEWI))
         WHWERT=MAX(WHWERT,GEWI)
         WBWERT=MAX(WBWERT,ABS(GEWI))
       endif
         WLWERT=MIN(WLWERT,WWERT)
         WAWERT=MIN(WAWERT,ABS(WWERT))
         WHWERT=MAX(WHWERT,WWERT)
         WBWERT=MAX(WBWERT,ABS(WWERT))
         WGES=WGES+WERT
      END IF
C---- FEHLERFALL NUR DURCHLESEN UND WERTE FESTSTELLEN
      IF (FEHLER) GO TO 70
      IF (NGES-IMAX) 110,110,240
  110 IF (IISZ.LT.3258) GO TO 115

      WRITE (UNIT=59+itab,REC=NSZ) (TAB(I0),I0=1,3257)

      IISZ=1
      NSZ=NSZ+1
  115 TAB(IISZ)=NGES
C
C-----KLASSIERUNG UND UMSETZUNG PRIMAER GESPERRTER FELDER---------------
C----- KLASSE (-300004 BIS -400002 )--- (NULL-WERT = -400003 ) -------
C
      IF (WTART-127) 160,160,130
C-----ZAEHL SEK. GESP. WERTE BEI PROGRAMMSTART--------------------------
  130 IF (WTART.LT.257) GO TO 140
      ISEK=ISEK+1
      WSEK=WSEK+WERT
      IF (WERT.EQ.NULW.OR.WERT.EQ.0.0) THEN
         ISEKN=ISEKN+1
* Klassierung fuer sekundaere Sperrungen in Eingabe nur ueberpruefen
* ohne Einzelberichtspflichtige zu beruecksichtigen-09.10.2000
* wird vorerst nicht eingesetzt, da Abbruch bei auffinden von sek. Sperr
*        WRITE(7,1199)
*        STOP 1199
         KL2=((KWERT-1)+3*KWERT)*(-1)
*neu?    KL2=((KWERT-1)+8*KWERT)*(-1)
      ELSE
         KL2=(INT(LOG(WERT/XMINW)*TWERT)+3*KWERT+1)*(-1)
*neu?    KL2=(INT(LOG(WERT/XMINW)*TWERT)+8*KWERT+1)*(-1)
      END IF
      GO TO 170
C
C-----WERT NE 0---------------------------------------------------------
C
  140 IF (BERPFL.GT.1) GO TO 150
C
C-----EINZELBERICHTSPFLICHTIG-- KLASSE (-100001 BIS -200000 )-----------
C----- (NULL-WERT = -200001 ) ------------------------------------------
C
      IF (WERT.EQ.NULW.OR.WERT.EQ.0.0) THEN
         NENULL=NENULL+1
         KL2=((KWERT-1)+1*KWERT)*(-1)
      ELSE
         NEINZ=NEINZ+1
         WEINZ=WEINZ+WERT
         KL2=(INT(LOG(WERT/XMINW)*TWERT)+1*KWERT+1)*(-1)
      END IF
      GO TO 170
C
C-----NICHT EINZELBERICHTSPFLICHTIG-------------------------------------
C----- KLASSE (-200002 BIS -300002 )--- (NULL-WERT = -300003 ) -------
C
  150 IF (WERT.EQ.NULW.OR.WERT.EQ.0.0) THEN
         NGNULL=NGNULL+1
         KL2=((KWERT-1)+2*KWERT)*(-1)
      ELSE
         NPRIM=NPRIM+1
         WPRIM=WPRIM+WERT
         KL2=(INT(LOG(WERT/XMINW)*TWERT)+2*KWERT+1)*(-1)
      END IF
      GO TO 170

  160 NGN=NGN+1
      WGN=WGN+WERT
C
C-----KLASSIERUNG AUF HALBWORT------------------------------------------
C----- KLASSE ( 1 BIS 99999 )--- (NULL-WERT = 100000 ) -----------------
C
      IF (WERT.EQ.NULW.OR.WERT.EQ.0.0) THEN
         KL2=KWERT-1
      ELSE
         KL2=INT(LOG(WERT/XMINW)*TWERT)+1
c*****changes giessing 02.01 a
c         IF (GEWRAN.EQ.1) XGEWI=GEWI
         IF (GEWRAN.EQ.1) then
           XGEWI=GEWI


         else
C--------GEWICHTUNG VOM TYP 2 SO DASS DIE SPREIZUNG
C--------DER GEWICHTETEN KLASSIERTEN WERTE ( 1 BIS 99999 ) BETRAEGT
         if (gewran.eq.2) then
          if ((imodw.ge.1).and.(imodw.le.7)) then
            if(xgewi.eq.0.0) xgewi=1.0
             xgewi=xgewi*
     #        (preflow(imodw)+
     #        (prefup(imodw)-preflow(imodw))/
     #        real(kwert-3)*real(kl2-1))/real(kl2)
          endif
c*****changes giessing 10.02 a
C--------GEWICHTUNG VOM TYP 3 SO DASS DER KLASSIERTE GEWICHTSWERT
C--------BEI IMODW UNGL 9 1 BETRÄGT; 2 SONST
         else if(gewran.eq.3) then
          if(imodw.ne.9) then
            xgewi=xminw
          else 
            xgewi=xminw*(exp(1/twert))
          endif
c*****changes giessing 10.02 e
         endif
         endif


            IF (.NOT.FEHLER.AND.
     #          XMINW.GT.xGEWI.AND.xGEWI.NE.0.0) THEN
                WRITE (0,4116) XMINW,xGEWI
C
C------    -IM FEHLERFALL BESTAND DURCHGEHEN
                FEHLER=.TRUE.
            END IF
C----------AUSGABE IN FT14
            if((mode.eq.2).or.(mode.eq.4)) then
             if(print1.eq.4) then
               write(14,*) imodw,' ',wwert,' ',kl2,' ',
     #            real(kl2)*1.0*(EXP(REAL(
     #     (INT(LOG(XGEWI/XMINW)*TWERT)+1)
     #     /TWERT))*MINW),' ',xgewi,' ',
     #     (INT(LOG(XGEWI/XMINW)*TWERT)+1),' ',
     #     (SCHL(I),I=1,NDIM)
             endif
            endif
c*****changes giessing 02.01 e
      END IF
  170 CONTINUE
C------------------------------------
C---WENN KEINE OBERE UND UNTERE SCHAETZWERTGRENZEN VORGEGEBEN
C---DANN OBERE= MAX. WERT  UN D   UNTERE=WERT
C------------------------------------
c*****changes giessing 02.01 a
c      IF (SCHAEZ.EQ.0) THEN
c-----VERWENDUNG DER STANDARDSCHRANKEN MAXW UND WERT AUCH BEI SCHAETZ 2
C-----AUSSER IM FALL IMODW GLEICH NULL (DANN FEHLO=FEHLU=0)
      IF ((SCHAEZ.EQ.0).or.
     #     ((schaez.gt.1).and.(imodw.ne.0))) THEN
c*****changes giessing 02.01 e
         FEHLO=MAXW
         FEHLU=WERT
c*****changes giessing 02.01 A
C      ELSE
      else if((schaez.gt.1).and.(imodw.eq.0)) then
           fehlo=0.0
           fehlu=0.0
      endif

      If ((schaez.eq.1).or.
     #   ((schaez.gt.1).and.(imodw.eq.0)) ) then
c*****changes giessing 02.01 e
         FMIN=MIN(FEHLO,FMIN)
         FMIN=MIN(FEHLU,FMIN)
         MAXF=MAX(FEHLO,MAXF)
         MAXF=MAX(FEHLU,MAXF)
      ENDIF
C------------------------------------
C---SONDERBEHANDLUNG NULLWERTE/ECHTE NULLEN
C------------------------------------
      IF (FEHLU.EQ.0.0) THEN
*        FEHLU=XMINW/1.1
         FEHLU=XMINW/MAXW
         FMIN=FEHLU
      ENDIF
      IF (FEHLO.EQ.0.0) THEN
*        FEHLO=XMINW/1.1
         FEHLO=XMINW/MAXW
         FMIN=FEHLO
      ENDIF
C------------------------------------
C--- KLASSIERUNG GEWICHTUNG (NULLWERT KANN NICHT AUFTRETEN)
      IF (XGEWI.EQ.0.0) XGEWI=1.0
C------------------------------------
      KL3=INT(LOG(XGEWI/XMINW)*TWERT)+1
C------------------------------------
C--- KLASSIERUNG HOECHSTER EINZELWERT FUER DOMINANZ
C------------------------------------
      IF (HOEW.EQ.0.0.OR.DQUOTE.EQ.0.0) THEN
         KL4=0
      ELSE
         KL4=INT(LOG(HOEW/XMINW)*TWERT)+1
      ENDIF
C
C--- Ueberpruefen ob Ueberschneidungen vorliegen nur bei Ueberlappung
C
      IF(tabanz.NE.1) THEN
         DO 20 lap_point=istart,neu_start
            yes=0
            DO 10 i=1,ndim
               IF(schl(i).NE.schl_nam(pseu_stell(5,i,itab),
     #            erk_tab(pseu_stell(5,i,itab),lap_point))) goto 20
                  yes=yes+1
                  save_schl(yes)=schl(i)
   10       CONTINUE
            DO 15 i=ndim+1,anz_dim(itab)+ndim
               IF(pseu_key(i-ndim,itab).NE.schl_nam
     #            (pseu_stell(5,i,itab),
     #            erk_tab(pseu_stell(5,i,itab),lap_point))) goto 20
                  yes=yes+1
                  save_schl(yes)=pseu_key(i-ndim,itab)
   15       CONTINUE
C
C-- Ueberlappungsdatei aufbauen
C
            IF(yes.EQ.(anz_dim(itab)+ndim)) THEN
               ifou=ifou+1
               WRITE (UNIT=19+itab,REC=NGES)
     #                KL2,KL3,KL4,SNGL(FEHLO),
     #                SNGL(FEHLU),indx(lap_point)
C-- Sicherheitsueberpruefung ob Wert mit Ueberlappung uebereinstimmt
               IF(itab.gt.1) THEN
                  READ (UNIT=13,REC=indx(lap_point),ERR=19)
     #                SKL2,SKL3,SKL4,SFEHLO,SFEHLU
                  IF(SKL2.NE.0.and.SKL2.NE.KL2) THEN
                     WRITE(7,4800) tab_nam(itab),wert,
     #                    (save_schl(n),n=1,anz_dim(itab)+ndim)
                     write(7,*) 'skl2, kl2', skl2,kl2  
                     STOP 8891
                  ENDIF
               ENDIF
C-- Sicherheitsueberpruefung ob Wert mit Ueberlappung uebereinstimmt
   19          WRITE (UNIT=13,REC=indx(lap_point))
     #                KL2,KL3,KL4,SNGL(FEHLO),SNGL(FEHLU)
               istart=lap_point+1
               GOTO 70
            ENDIF
   20    CONTINUE
         WRITE (UNIT=19+itab,REC=NGES) KL2,KL3,KL4,SNGL(FEHLO),
     #          SNGL(FEHLU),0
      ELSE
C
         WRITE (UNIT=19+itab) KL2,KL3,KL4,SNGL(FEHLO),SNGL(FEHLU),0
      ENDIF
C
      GO TO 70
C
C------------------------------------
C- KEINE NEGATIVEN WERTE ABER BETRAG ODER VERSCHIEBUNG
C------------------------------------
  180 IF (NEGTST.AND.(MINW.LT.0.OR.ABSBET.EQ.1)) THEN
         WRITE (7,4180)
         STOP 3011
      END IF
C------------------------------------
C-MINIMALWERT BEI NEGATIVEN ZAHLEN AUF XMINW
C------------------------------------
      MINW=XMINW
C------------------------------------

      WRITE (UNIT=59+itab,REC=NSZ) (TAB(I0),I0=1,3257)

      CALL AGTEST (II(III), IA(1,III), IERR(III))
      IF (IERR(III)) 230,190,230
  190 IF (NDIM.EQ.III) GO TO 200
      III=III+1
      GO TO 180
C------------------------------------
C-----ABFRAGE AUF RICHTIGE ANZAHL DER SAETZE
C------------------------------------
  200 ISZ1=1
      DO 210 IIXX=1,NDIM
         ISZ1=ISZ1*LI(IIXX)
  210 CONTINUE
      IF (ISZ.NE.ISZ1) THEN
         WRITE (7,4190) ISZ,ISZ1
         STOP 3012
      END IF

C
C-----STATISTIK---------------------------------------------------------
C
      WRITE (3,4200)

      PRNSEK=DFLOAT(ISEK)*100.0/DFLOAT(NGES)
      PRNEIN=DFLOAT(NEINZ)*100.0/DFLOAT(NGES)
      PRNPRI=DFLOAT(NPRIM)*100.0/DFLOAT(NGES)
      PENULL=DFLOAT(NENULL)*100.0/DFLOAT(NGES)
      PGNULL=DFLOAT(NGNULL)*100.0/DFLOAT(NGES)
      PRNNGN=DFLOAT(NGN)*100.0/DFLOAT(NGES)
      IF (WGES.NE.0.0) THEN
         PRSEK=WSEK*100.0/WGES
         PREIN=WEINZ*100.0/WGES
         PRPRI=WPRIM*100.0/WGES
         PRNGN=WGN*100.0/WGES
      ELSE
         PRSEK=0.0
         PREIN=0.0
         PRPRI=0.0
         PRNGN=0.0
      END IF
      PRNGES=100.0
      PRGES=100.0
      WRITE (3,4210) NGES,PRNGES,
     #               ISEK,PRNSEK,
     #               NEINZ,PRNEIN,
     #               NENULL,PENULL,
     #               NPRIM,PRNPRI,
     #               NGNULL,PGNULL,
     #               NGN,PRNNGN,
     #               INLL,
     #               NGES+INLL,
     #               MINW,MAXW,
     #               WLWERT,WHWERT,
     #               WAWERT,WBWERT,
     #               ISZ

*     WRITE (7,'(I10,A)') ISZ,' ZTF'
*     WRITE (7,'(I10,A)') NGES,' FWGN'
*     WRITE (7,'(I10,A)') IWN,' WN'
*     WRITE (7,'(I10,A)') NEINZ+NPRIM+NENULL+NGNULL,' PGH'
*     WRITE (7,'(I10,A)') NENULL+NGNULL,' PGHN'
*     WRITE (7,'(I10,A)') ISEK,' SGH'
*     WRITE (7,'(I10,A)') ISEKN,' SGHN'
C------------------------------------
C-WARNING   (SPREIZUNG DER KLASSIERUNG 50 PROZENT UNTERSCHIED)
C------------------------------------
      IF((MAXW-MINW)/(WBWERT-WAWERT).GT.50.0) THEN
                 WRITE (3,4135)
          ENDIF
C------------------------------------
C- FEHLER BEI DER ANGABE VON MAXWWERT ODER MINIMALWERT
C------------------------------------
      IF (FEHLER) THEN
         WRITE (7,4220)
         STOP 3013
      END IF
C
C- ZURUECKSETZEN DER RANDFLAGS BEI GEWICHTUNG IM BESTAND
C
      IF (NRFLAG) THEN
         DO 220 I1=1,NDIM
            RFLAG(I1)=0
  220    CONTINUE

         WRITE (3,4230)
      END IF
      IF(tabanz.ne.1)  WRITE (3,4235) ifou,tab_nam(itab)
C
      RETURN
C
C-----ABBRUCH BEI SCHLUESSELFEHLER--------------------------------------
C

  230 WRITE (7,4240) (SCHL(I),I=1,NDIM),IERR(III),II(III),IA(1,III),III
      STOP 3014
C
C-----ABBRUCH BEI ZU KLEINER ARBEITSTABELLE-----------------------------
C
  240 WRITE (7,4250) IMAX
      STOP 3016
      END
c************************************************************************
c*****changes giessing 08.01 a
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine weights(ndim,anzmodw)

      parameter (maxtab=20)
      parameter (maxmodw=7)
c     implicit logical a-z
      integer anzmodw(maxmodw),lll
      integer kwert
      integer mintind,maxtind
      integer ndim
      integer numcells, num1cells
      integer print1
      integer weighcl
      real*8 imul,neunneun
      real*8 rate(maxmodw)
      real*8 omin
      real*8 preflow(maxmodw),prefup(maxmodw)
      real*8 so,sn
      real*8 teta
      real*8 thresh(5)
      logical datarun
      logical int_iter
      logical sec_keys

      common /prefer/preflow,prefup

      common /kwert/kwert
      common /print1/print1
c      common /sec_keys/sec_keys
c*********preference-codes**********************************
c 1: keine
c 2: möglichst nicht sperren, mit Priorität
c 3: möglichst nicht sperren ohne Wertdifferenzierung
c 4: möglichst nicht sperren mit Wertdifferenzierung
c 5: bevorzugt sperren mit Wertdifferenzierung
c 6: bevorzugt sperren ohne Wertdifferenzierung
c 7: bevorzugt sperren, mit Priorität
c
c sonstige:
c 0: darf nicht gesperrt werden (Range auf Null setzten)
c 8: muss gesperrt werden (kl2 wird auf 'Primärsperrung' gesetzt)
c*********************************************************************

c     changes Elke Drumm 01.01
      INCLUDE 'WEIGHTS.MSG'
c     end changes Elke Drumm 01.01
      if(anzmodw(2)*anzmodw(7).gt.0) then
       write(7,1000) anzmodw(2),anzmodw(7)
       stop 8000
      endif
      numcells=0
      num1cells=0

      do 10 weighcl=1,7
       numcells=numcells+anzmodw(weighcl)
       if ((weighcl.eq.1).or.(weighcl.eq.4).or.(weighcl.eq.5))
     # num1cells=num1cells+anzmodw(weighcl)
10    continue
               if(print1.eq.4) write(17,*)
     #          ' numcells,num1cells',numcells,num1cells

      do 20 weighcl=1,7
       if ((weighcl.eq.2).or.(weighcl.eq.7))
     #   rate(weighcl)=real(anzmodw(weighcl))/real(numcells)
       if ((weighcl.eq.1).or.(weighcl.eq.4).or.(weighcl.eq.5))
     #   rate(weighcl)=real(anzmodw(weighcl))/real(num1cells)
           if(print1.eq.4)
     #       write(17,*) ' weighcl,anzmodw,rate',
     #       weighcl,anzmodw(weighcl),rate(weighcl)
20    continue
      imul=real(2**ndim-1)
         neunneun=real(kwert-2)
c*******bevorzugt sperren mit Priorität: ************************
c   [1 ... teta*omin]    bevorzugt(7),
c   [omin...thresh4]     bevorzugt(5),
c   [thresh4...thresh5]  keine(1)
c   [thresh5...neunneun] nicht sperren(4)
c
      if(anzmodw(7).gt.0) then
        so=rate(7)
        teta=(so*neunneun+imul*(1.-2.*so))/
     #       (neunneun*((1.0-so)*(imul-1.)+so)+(1.-2.*so))
        omin=(imul-1.)*neunneun/(imul-teta)
        thresh(1)=teta*omin
        thresh(2)=omin
        thresh(3)=neunneun
c*******möglichst nicht sperren mit Priorität************************
c   [1 ... thresh4]       bevorzugt(5),
c   [thresh4...thresh5]   keine(1),
c   [thresh5...teta*omin] nicht sperren(4)
c   [omin...neunneun]     nicht sperren(2)
c
      else if(anzmodw(2).gt.0) then
        sn=rate(2)
        omin=(neunneun-(1.-sn)*imul+sn)/
     #       ((1.-sn)*imul+sn)
        teta=(1.0+(imul-1.0)/omin)/imul
        thresh(3)=teta*omin
        thresh(1)=omin
        thresh(2)=1.0
c********keine Prioritäten****************************************
c   [1...thresh4]        bevorzugt(5),
c   [thresh4...thresh5]  keine(1)
c   [thresh5...neunneun] nicht sperren(4)
      else
        thresh(2)=1.0
        thresh(3)=neunneun
      endif
c*****************************************************************
        thresh(4)=thresh(2)+(thresh(3)-thresh(2))*rate(5)
        thresh(5)=thresh(4)+(thresh(3)-thresh(2))*rate(1)
c*********************************************************************
      preflow(7)=1.
      prefup(7)=thresh(1)
      preflow(2)=thresh(1)
      prefup(2)=neunneun
      prefup(5)=thresh(4)
      preflow(5)=thresh(2)
      prefup(1)=thresh(5)
      preflow(1)=prefup(5)
      prefup(4)=thresh(3)
      preflow(4)=prefup(1)
      preflow(3)=prefup(4)
      prefup(3)=prefup(4)
      preflow(6)=preflow(5)
      prefup(6)=preflow(5)
c
      do 7777 lll=1,7
        write(17,*) 'preflow: ',preflow(lll)
        write(17,*) 'prefup:  ',prefup(lll)
7777  continue
c*******************************************************************
      return
      end

c************************************************************************
c*****changes giessing 08.01 e


      SUBROUTINE AGGPOS (MG, MU, LOCK, L, KEY, IND, A, P, U, O, UK, OK,
     #ERR, K, KEYF)
C  -------------------------------------------------------------------
C I                                                                   I
C I   A G G P O S                                                     I
C I                                                                   I
C I   SUBROUTINE ZUM AUFBAU DER DER AGGREGATIONS-, POSITIONS-         I
C I   UND TEILMATRIXINDEXTABELLEN (SEGMENTE ZUR STRUKTUR DER          I
C I   BASISDATEI                                                      I
C I                                                                   I
C  -------------------------------------------------------------------
C I   SUBROUTINE AGGPOS(MG,MU,LOCK,L,KEY,IND,A,P,U,O,UK,OK,ERR,K,KEYF)I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I     MG LAENGE DER GRUPPIERUNGS-  (I*4)                            I
C I        TABELLE                                                    I
C I     MU GRUPPIERUNGSTABELLE       (I*4)                            I
C I   LOCK SCHLOESSERTABELLE         (R*8)                            I
C I      L INDEX                     (I*4)                            I
C I    KEY SCHLUESSEL                (R*8)                            I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I      A AGGREGATIONSTABELLE       (I*4)                            I
C I      P POSITIONSTABELLE          (I*4)                            I
C I      U UNTERE TEILMATRIXINDIZES  (I*4)                            I
C I      O OBERE  TEILMATRIXINDIZES  (I*4)                            I
C I     UK UNTERE SCHLUESSELINDIZES  (I*4)                            I
C I     OK OBERE  SCHLUESSELINDIZES  (I*4)                            I
C I   KEYF TABELLE IN DATEI GEF.     (I*4)                            I
C I      K FUELLUNG VON KEYF         (I*4)                            I
C I        SCHLUESSEL                                                 I
C I    ERR FEHLER MIT GUELTIGEM      (I*4)                            I
C I        INDEXWERT                                                  I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C  -------------------------------------------------------------------
      CHARACTER*8 KEY,LOCK(*)
      INTEGER A(*),MU(*),P(*),U(*),O(*),UK(*),OK(*),ERR,KEYF(*)
C
C-IBM-VERSION
*     INCLUDE (AGGPOSM)
C-PC-VERSION
      INCLUDE 'AGGPOSM'
C      write(0,*) 'in aggpos',mg,'-',mu(1),'-',mu(mg)
C
C-----AGGREGATIONSSTUFENTABELLE---------------------------------------
C
      J=0
C
C-----SUCHEN DER SACHSCHLUESSEL IN DER EINGABETABELLE-----------------
C
      DO 10 JG=1,MG
c         write(0,*) 'jg',jg
         MX=MU(JG)
         DO 10 JU=1,MX
         J=J+1
C
         IF (LOCK(J).EQ.KEY) GO TO 20
C
   10 CONTINUE
c      write(0,*) 'nach 10'
C
C-----FEHLER FALLS NICHT GEFUNDEN--------------------------------------
C
      ERR=L
c      write(0,4010)
      WRITE (3,4010)
      RETURN
C
C-----ABSPEICHERN INDEX IN VERWEISTABELLE------------------------------
C
   20 K=K+1
c      write(0,*) 'nach 20'
      KEYF(K)=J
C
C-----FALLS DIMENSION SCHON ANGEFANGEN VERGLEICHEN AGGREGATIONSSTUFE----
C-----DES GEFUNDENEN SCHLUESSELS MIT VORHERIGEM WERT.  FALLS------------
C-----UNGLEICH, NEUES TABELLENFELD MIT NEUEM WERT AUFBAUEN--------------
C
      IF (IND) 30,40,30
   30 IF (A(IND)-JG) 40,120,40
   40 IND=IND+1
      A(IND)=JG
C
C-----SETZEN ANFANGSWERTE FUER------------------------------------------
C-----POSITIONSINDEX-LISTEN, UNTERE UND OBERE TEILMATRIXINDEX-LISTEN----
C-----BELEGTE SCHLUESSELFELDER IN VERWEISTABELLE------------------------
C
      U(IND)=L
      O(IND)=U(IND)
      UK(IND)=K
      OK(IND)=K
      J=0
C
C-----SETZEN OBERE TEILMATRIXINDICES UND OBERE VERWEISINDICES FUER------
C-----VORHERIGE AGGREGATIONSSTUFE (ABSCHLUSS)---------------------------
C
      LP=IND-1
      IF (LP) 50,120,50
   50 O(LP)=L-1
      OK(LP)=K-1
C
C-----SETZEN POSITIONSINDICES FUER LAUFENDE AGGREGATION-----------------
C
      DO 110 I=1,LP
         L1=IND-I
         IF (A(L1)-A(IND)) 100,60,90
   60    IF (J) 80,80,70
   70    P(IND)=P(L1)+1
         RETURN
   80    P(IND)=P(L1)
         RETURN
   90    J=1
  100    P(IND)=P(L1)
  110 CONTINUE
  120 return
      END

      SUBROUTINE AGTEST (M, A, ERR)
C  -------------------------------------------------------------------
C I                                                                   I
C I   A G T E S T                                                     I
C I                                                                   I
C I   SUBROUTINE ZUM UEBERPRUEFEN DES GUELTIGEN AUFBAUES DER          I
C I   EINGABEDATEI HINSICHTLICH DER AGGREGATION                       I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   SUBROUTINE AGTEST(M,A,ERR)                                      I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I      M LAENGE DER AGGREGATIONS-  (I*4)                            I
C I        TABELLE                                                    I
C I      A AGGREGATIONSTABELLE       (I*4)                            I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I    ERR FEHLER MIT GUELTIGEM      (I*4)                            I
C I        INDEXWERT                                                  I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C  -------------------------------------------------------------------
      INTEGER A(*),ERR
C
C-IBM-VERSION
*     INCLUDE (AGTESTM)
C-PC-VERSION
      INCLUDE 'AGTESTM'
C
C-----AUFSUCHEN EINER ISOLIERTEN AGGREGATIONSSTUFE---------------------
C
C
C-----EXISTENZ HOEHERER AGGREGATIONSSTUFEN (SONST FEHLER ERR=-1)-------
C
      DO 10 J=2,M
         IF (A(1)-A(J)) 20,10,10
   10 CONTINUE
C
      WRITE (3,4010)
      ERR=-1
      RETURN
C
C-----LETZTER INDEX IN DER ZEILE HAT UM 1 HOEHERE AGGREGATIONSSTUFE-----
C-----SONST FEHLER ERR=-M (M LETZTE SPALTE)-----------------------------
C
   20 IF (A(M)-1-A(M-1)) 30,40,30
   30 WRITE (3,4020)
      ERR=-M
      RETURN
C
C-----ALLE WEITEREN INDIZES 1 < J < M HABEN HOECHSTENS UM 1 HOEHERE-----
C-----AGGREGATION.------------------------------------------------------
C-----SONST FEHLER INDEX J WIRD UEBERGEBEN------------------------------
C
   40 M1=M-1
      DO 80 J=2,M1
         IF (A(J)-1-A(J-1)) 50,80,70
   50    J1=J+1
C
C-----BEI ALLEN WEITEREN IST DIE AGGREGATION MINDESTENS 1 MAL HOEHER----
C
         DO 60 L=J1,M
            IF (A(L)-A(J)) 60,60,80
   60    CONTINUE
   70    ERR=-J
         WRITE (3,4030)
         RETURN
   80 CONTINUE
      RETURN
      END

      SUBROUTINE TABELL (LG, TABI, IL, NDIM, IA, IP, IU, IO, IDDD,
     #NZ, ISZ, itab)
C  -------------------------------------------------------------------
C I                                                                   I
C I   T A B E L L                                                     I
C I                                                                   I
C I   SUBROUTINE ZUM AUFBAU UND ZUR AUSGABE DER (RELATIVEN) INDIZES   I
C I   ALLER ARBEITSTABELLEN FUER DAS SPERRPROGRAMM AUF ARBEITSDATEI   I
C I   (WEGEN DER ITERATIVEN ABARBEITUNG) IN BINAERER FORM.            I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I   NDIM ZAHL DER DIMENSIONEN      (I*4)                            I
C I   TABI REL. UNTERTAB.-INDICES    (I*4)                            I
C I     IA AGGREGATIONSTABELLE       (I*4)                            I
C I     LG MAXLAENGE EINER UNTERTAB. (I*4)                            I
C I     IU UNTERE TEILMATRIXINDICES  (I*4)                            I
C I     IO OBERE  TEILMATRIXINDICES  (I*4)                            I
C I     IP POSITIONSTABELLE          (I*4)                            I
C I     IL ANZAHL DER SCHLOESSER     (I*4)                            I
C I   IDDD SUMME DER DIM.-LAENGE     (I*4)                            I
C I     NZ ANZAHL DER UNTERTABELLEN  (I*4)                            I
C I   itab ANZAHL DER Einzeltabellen (I*4)                            I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I    ISZ ZAHL DER EING.-SAETZE     (I*4)                            I
C I                                                                   I
C I                                                                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   FETCH                                                           I
C I                                                                   I
C I   SUBROUTINE TABELL(LG,TABI,IL,NDIM,IA,IP,IU,IO,IDDD,NZ,ISZ       I
C  -------------------------------------------------------------------
      REAL*8 PRL
      INTEGER TABI(LG),IA(IL,NDIM),IP(IL,NDIM),IU(IL,NDIM),IMDI(7),
     #IO(IL,NDIM),MA(7),MP(7),II(7),MS(7),MPP(7),LI(7),TAB(3257)
      COMMON /INDEX/ II,LI,MS,MA,MP
      COMMON /MERK/ MERK,ILD,TAB
      EQUIVALENCE (IMDI(1),ID1),(IMDI(2),ID2),(IMDI(3),ID3),(IMDI(4),
     #ID4),(IMDI(5),ID5),(IMDI(6),ID6),(IMDI(7),ID7)
      EQUIVALENCE (MS(1),MS1),(MS(2),MS2),(MS(3),MS3),(MS(4),MS4),(MS(5)
     #,MS5),(MS(6),MS6),(MS(7),MS7)
      EQUIVALENCE (MP(1),II1),(MP(2),II2),(MP(3),II3),(MP(4),II4),(MP(5)
     #,II5),(MP(6),II6),(MP(7),II7)
      EQUIVALENCE (MPP(1),MP1),(MPP(2),MP2),(MPP(3),MP3),(MPP(4),MP4),
     #(MPP(5),MP5),(MPP(6),MP6),(MPP(7),MP7)
      MERK=1
      ILD=0
C-IBM-VERSION
*     INCLUDE (TABELLM)
C-PC-VERSION
      INCLUDE 'TABELLM'
C
      READ (UNIT=59+itab,REC=MERK) (TAB(ILL),ILL=1,3257)
C
      DO 10 I=NDIM+1,7
   10    IMDI(I)=1
      NZ=0
      LM0=0
      LMS=0
      IDD0=0
      DO 20 II0=1,7
   20    MPP(II0)=1
C
C-----ABARBEITUNG ALLER UNTERTABELLEN DER BASISTABELLE------------------
C
      DO 240 I7=1,MS7
         MA(7)=MS7-I7+1
         DO 230 I6=1,MS6
            MA(6)=MS6-I6+1
            DO 220 I5=1,MS5
               MA(5)=MS5-I5+1
               DO 210 I4=1,MS4
                  MA(4)=MS4-I4+1
                  DO 200 I3=1,MS3
                     MA(3)=MS3-I3+1
                     DO 190 I2=1,MS2
                        MA(2)=MS2-I2+1
                        DO 180 I1=1,MS1
                           MA(1)=MS1-I1+1
                           DO 40 II0=1,NDIM
                              III1=II(II0)
                              MPP(II0)=IP(1,II0)
C-----AENDERUNG 17.7.85 : VERARBEITUNG VON DATENSTRUKTUREN
C-----                    MIT UNVOLLSTAENDIGER HOECHSTER AGGREGATION
C-----                    (HUCKEPACK-VERFAHREN MIT UNTERSCHIEDLICHER
C-----                    HOECHSTER AGGREGATIONSSTUFE IN EINEM ORDN.
C-----                    KRITERIUM)
                              IMP=MPP(II0)
                              DO 30 II1=1,III1
C-----AENDERUNG 17.7.85 : GLEICHER GRUND
                                 IF (IA(II1,II0).GT.MA(II0)) IMP=
     #                            MPP(II0)
                                 IF (IA(II1,II0).NE.MA(II0)) GO TO 30
                                 MPP(II0)=IP(II1,II0)
   30                         CONTINUE
C-----AENDERUNG 17.7.85 : GLEICHER GRUND
                              MPP(II0)=IMP
   40                      CONTINUE
                           DO 170 II7=1,MP7
                              DO 160 II6=1,MP6
                                 DO 150 II5=1,MP5
                                    DO 140 II4=1,MP4
                                       DO 130 II3=1,MP3
                                          DO 120 II2=1,MP2
                                             DO 110 II1=1,MP1
                                                LM=1
                                                IDD=0
                                                DO 90 IIU=1,NDIM
                                                   IIIW=II(IIU)
                                                   IMDI(IIU)=1
                                                   DO 70 IIV=1,IIIW
                                                      IF ((IA(IIV,IIU)
     #                                                 .NE.MA(IIU))
     #                                                 .OR.(IP(IIV,IIU)
     #                                                 .NE.MP(IIU))) GO
     #                                                 TO 60
                                                      IMDI(IIU)=
     #                                                 IMDI(IIU)+IO(IIV,
     #                                                 IIU)-IU(IIV,IIU)+
     #                                                 1
   60                                                 CONTINUE
   70                                              CONTINUE
C-----------------------------------------------------------------------
                                                   IDD=IDD+IMDI(IIU)
                                                   LM=LM*IMDI(IIU)
   90                                           CONTINUE
C
C-----ABBRUCH BEI ZU KLEINER LAUFENDER UNTERTABELLE ODER ZU KLEINER-----
C-----ARBEITSTABELLE FUER SPERRUNG MIT LAENGE DER RANDSUMME DER---------
C-----LAUFENDEN UNTERTABELLE--------------------------------------------
C
                                                IF ((LM.LE.LG)
     #                                           .AND.(IDD.LE.IDDD)) GO
     #                                           TO 100
                                                WRITE (3,4010) LM,LG
                                                WRITE (7,4020) IDD,IDDD
                                                STOP 3017
  100                                           LMS=LMS+LM
                                                IF (LM.GT.LM0) LM0=LM
                                                IF (IDD.GT.IDD0) IDD0=
     #                                           IDD
C
C-----ERSTELLUNG DER (REL.) INDICES DER LAUFENDEN UNTERTABELLE----------
C
                                                CALL FETCH ( IL,
     #                                           NDIM, IA, IP, IU, IO,
     #                                           LM, TABI, itab)

                                                WRITE (39+itab) MA,MP,
     #                                           IMDI,LM,(TABI(II0),II0=
     #                                           1,LM)

C  200 FORMAT(20A4)
                                                NZ=NZ+1
  110                                        CONTINUE
  120                                     CONTINUE
  130                                  CONTINUE
  140                               CONTINUE
  150                            CONTINUE
  160                         CONTINUE
  170                      CONTINUE
  180                   CONTINUE
  190                CONTINUE
  200             CONTINUE
  210          CONTINUE
  220       CONTINUE
  230    CONTINUE
  240 CONTINUE
C
C-----STATISTIK (AUSGABE DER MAXIMALWERTE)------------------------------
C
      WRITE (3,4030) LM0,LG,IDD0,IDDD

      PRL=DFLOAT(LMS)*100.0/DFLOAT(ISZ)
      WRITE (3,4040) LMS,PRL

      RETURN
      END

      SUBROUTINE FETCH ( IL, NDIM, IA, IP, IU, IO, LM, TABI, itab)
C  -------------------------------------------------------------------
C I                                                                   I
C I   F E T C H                                                       I
C I                                                                   I
C I   SUBROUTINE ZUM AUFSUCHEN DER (RELATIVEN) INDIZES EINER UNTER-   I
C I   TABELLE IN EINER (TEMPORAEREN) ARBEITSDATEI .                   I
C I   (AUS PERFORMANCE-GRUENDEN WURDE EIN GEBLOCKTER RRDS-DIREKT-     I
C I   ZUGRIFF MIT VOLLSPURBLOCKUNG GEWAEHLT.)                         I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I     IL ANZ. DER MOEGL. AGGR./POSI(I*4)                            I
C I   NDIM ZAHL DER DIMENSIONEN      (I*4)                            I
C I     IA AGGREGATIONSINDICES JE DIM(I*4)                            I
C I     IP POSITIONSINDICES JE DIM.  (I*4)                            I
C I     IU UNTERE TEILMATRIXINDICES  (I*4)                            I
C I     IO OBERE  TEILMATRIXINDICES  (I*4)                            I
C I     LM INDEX VON TABI            (I*4)                            I
C I   TABI RELATIVE UNTERTABELLENIND.(I*4)                            I
C I   itab ANZAHL DER Einzeltabellen (I*4)                            I
C I                                                                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C I   SUBROUTINE FETCH(JJ,IL,NDIM,IA,IP,IU,IO,LM,TABI)                I
C  -------------------------------------------------------------------
      INTEGER TABI(LM),IA(IL,NDIM),IP(IL,NDIM),II(7),LI(7),IU(IL,NDIM),
     #IO(IL,NDIM),MA(7),MP(7),I0,ID(15),MS(7),TAB(3257)
      COMMON /INDEX/ II,LI,MS,MA,MP
      COMMON /MERK/ MERK,ILD,TAB
      EQUIVALENCE (II(1),II1),(II(2),II2),(II(3),II3),(II(4),II4),(II(5)
     #,II5),(II(6),II6),(II(7),II7),(LI(1),IK1),(LI(2),IK2),(LI(3),IK3),
     #(LI(4),IK4),(LI(5),IK5),(LI(6),IK6),(LI(7),IK7),(ID(1),IU1),(ID(2)
     #,IU2),(ID(3),IU3),(ID(4),IU4),(ID(5),IU5),(ID(6),IU6),(ID(7),IU7),
     #(ID(8),IO1),(ID(9),IO2),(ID(10),IO3),(ID(11),IO4),(ID(12),IO5),
     #(ID(13),IO6),(ID(14),IO7),(ID(15),IS)

      DO 10 I=1,15
   10    ID(I)=0
C
C-----NACH INITIALISIERUNG VON IUI,IOI,IS  WIRD DIEJENIGE UNTERTABELLE--
C-----AUSGEWAEHLT , DIE (MA(I),I=1(1)7) ALS AGGREGATIONSINDIZES UND-----
C-----(MP(I),I=1(1)7) ALS POSITIONSINDIZES ENTHAELT---------------------
C-----DIE SCHLEIFEN WURDEN AUS PERFORMANCE-GRUENDEN AUSGESCHRIEBEN.-----
C
      DO 370 I7=1,II7
         IF (I7-II7) 30,20,20
   20    IO7=IO7+1
         IU7=IO7
         GO TO 40
   30    IF (IA(I7,7).NE.MA(7).OR.IP(I7,7).NE.MP(7)) GO TO 370
         IU7=IU(I7,7)
         IO7=IO(I7,7)
   40    DO 360 I07=IU7,IO7
            IJ7=(I07-1)*IK6-1
            DO 350 I6=1,II6
               IF (I6-II6) 60,50,50
   50          IO6=IO6+1
               IU6=IO6
               GO TO 70
   60          IF (IA(I6,6).NE.MA(6).OR.IP(I6,6).NE.MP(6)) GO TO 350
               IU6=IU(I6,6)
               IO6=IO(I6,6)
   70          DO 340 I06=IU6,IO6
                  IJ6=(IJ7+I06)*IK5-1
                  DO 330 I5=1,II5
                     IF (I5-II5) 90,80,80
   80                IO5=IO5+1
                     IU5=IO5
                     GO TO 100
   90                IF (IA(I5,5).NE.MA(5).OR.IP(I5,5).NE.MP(5)) GO TO
     #                330
                     IU5=IU(I5,5)
                     IO5=IO(I5,5)
  100                DO 320 I05=IU5,IO5
                        IJ5=(IJ6+I05)*IK4-1
                        DO 310 I4=1,II4
                           IF (I4-II4) 120,110,110
  110                      IO4=IO4+1
                           IU4=IO4
                           GO TO 130
  120                      IF (IA(I4,4).NE.MA(4).OR.IP(I4,4).NE.MP(4))
     #                      GO TO 310
                           IU4=IU(I4,4)
                           IO4=IO(I4,4)
  130                      DO 300 I04=IU4,IO4
                              IJ4=(IJ5+I04)*IK3-1
                              DO 290 I3=1,II3
                                 IF (I3-II3) 150,140,140
  140                            IO3=IO3+1
                                 IU3=IO3
                                 GO TO 160
  150                            IF (IA(I3,3).NE.MA(3).OR.IP(I3,3)
     #                            .NE.MP(3)) GO TO 290
                                 IU3=IU(I3,3)
                                 IO3=IO(I3,3)
  160                            DO 280 I03=IU3,IO3
                                    IJ3=(IJ4+I03)*IK2-1
                                    DO 270 I2=1,II2
                                       IF (I2-II2) 180,170,170
  170                                  IO2=IO2+1
                                       IU2=IO2
                                       GO TO 190
  180                                  IF (IA(I2,2).NE.MA(2).OR.IP(I2,2)
     #                                  .NE.MP(2)) GO TO 270
                                       IU2=IU(I2,2)
                                       IO2=IO(I2,2)
  190                                  DO 260 I02=IU2,IO2
                                          IJ2=(IJ3+I02)*IK1
                                          DO 250 I1=1,II1
                                             IF (I1-II1) 210,200,200
  200                                        IO1=IO1+1
                                             IU1=IO1
                                             GO TO 220
  210                                        IF (IA(I1,1).NE.MA(1)
     #                                        .OR.IP(I1,1).NE.MP(1)) GO
     #                                        TO 250
                                             IU1=IU(I1,1)
                                             IO1=IO(I1,1)
  220                                        DO 240 I0=IU1,IO1
C
C-----BESTIMMUNG DES LINEAREN INDEXWERTES IND---------------------------
C
                                                IND=I0+IJ2-ILD
C
C-----ERMITTLUNG DES JEWEILIGEN GEBLOCKTEN SATZES MERK DER--------------
C-----BESETZUNGSTABELLE. EINLESEN DES REL. (SATZNUMMER) ADDRESSIERTEN---
C-----SATZES TAB (BLOCK) UND ENTNAHME DES JEWEILIGEN SATZES TAB(IND)----
C-----FUER ZUGRIFFSTABELLE TABI DAZU MUSS IND (SATZNUMMER) DER----------
C-----BESETZUNGSDATEI BESTIMMT WERDEN-----------------------------------
C
                                                IF ((IND.GT.0)
     #                                           .AND.(IND.LT.3258)) GO
     #                                           TO 230
                                                MERK=(I0+IJ2+3256)/3257
C
                                                READ (UNIT=59+itab,
     #                                            REC=MERK)
     #                                            (TAB(ILL),ILL=1,3257)
C
                                                ILD=(MERK-1)*3257
                                                IND=I0+IJ2-ILD
  230                                           IS=IS+1
                                                TABI(IS)=TAB(IND)
  240                                        CONTINUE
  250                                     CONTINUE
  260                                  CONTINUE
  270                               CONTINUE
  280                            CONTINUE
  290                         CONTINUE
  300                      CONTINUE
  310                   CONTINUE
  320                CONTINUE
  330             CONTINUE
  340          CONTINUE
  350       CONTINUE
  360    CONTINUE
  370 CONTINUE
      RETURN
      END

      SUBROUTINE ARBEIT (TAB, TABR, ID1, ID2, ID3, ID4, ID5, ID6, ID7,
     #NDIM, RSCHS, RSCHE, RSCHG, NNNN, itab)
C ------------------------------------------------------------------
CI--ABARBEITUNG EINER UNTERTABELLE----------------------------------I
C ------------------------------------------------------------------
CI ABARBEITUNG DER UNTERTABELLE                                     I
CI JEDES GEHEIME FELD (PIVOTFELD) WIRD AUF SICHERUNGSBEDUERFTIGKEIT I
CI UEBERPRUEFT UND GEGEBENENFALLS GESICHERT                         I
C ------------------------------------------------------------------
CI TAB    -  TABELLE DER WERTE (7-D)                                I
CI TABR   -  PARALLELE ARBEITSTABELLE ZU TAB                        I
CI ID1-7  -  DIMENSIONEN                                            I
CI NDIM   -  ANZAHL DER DIMENSIONEN                                 I
CI IGEI   -         ''         DES EINZELBERICHTSPFLICHTIGEN        I
CI RSCHS  -  KLASSIERUNGSWERT FUER SEKUND. GESPERRTE FELDER IN TABR I
CI RSCHE  -  KLASSIERUNGSWERT FUER EINZELBERICHTSPFL. IN TABR       I
CI RSCHG  -  ALLE FREIEN WERTE IM RAND -DER NICHT GESAMTRAND IST-   I
CI           SIND GROESSER GLEICH RSCHG.                            I
CI           ALLE SONSTIGEN FELDER SIND KLEINER ALS RSCHG           I
CI ISPSQ -   SPERRSCHLUESSEL  FUER QUADERSPERRUNGEN                 I
CI NNNN  -   ANZAHL DER BEREITS. DURCHGEFÜHRTEN ITERATIONEN         I
CI itab  -   ANZAHL DER Einzeltabellen                              I
C ------------------------------------------------------------------
C  IGG       VEKTOR DER KOORDINATEN DES PIVOTFELDES
C
C -----------------------------------------------------------------
CI UNTERPROGRAMME                                                  I
CI    PRUEF,SPERR                                                  I
C -----------------------------------------------------------------
C
      LOGICAL RF
      CHARACTER*4 TEST
*     INTEGER III(7),ZZ/0/,IDD(7),RAND
      INTEGER III(7),IDD(7),RAND
      INTEGER IIZW,IIZW3,IDRZW,tabanz
      INTEGER RFLAG(7)
      INTEGER KWERT
c----changes giessing 09/02-a
      INTEGER EXACT
c----changes giessing 09/02-e
c----changes giessing 03/03-a
      INTEGER NREDU(11)
c----changes giessing 03/03-e
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 FMIN,MAXF
c     changes drumm 08/02
      real*4 wh
	logical open_l
c     changes drumm 08/02-e
      REAL*4 ROZW,RUZW
*     REAL*8 ROZW,RUZW
      COMPLEX*16 TAB(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
      COMPLEX*16 TABR(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
*     SAVE  ZZ
c----changes giessing 09/02-a
      COMMON /EXACT/EXACT
c----changes giessing 09/02-e
c----changes giessing 03/03-a
      COMMON /NREDU/NREDU
c----changes giessing 03/03-e
      COMMON IGG(7),IMDI(7),IU(7),IO(7)
      COMMON /TEST/ TEST
      COMMON /RFLAG/ RFLAG
      COMMON /KWERT/ KWERT
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMMON /SPMERK/ IGEI,ISPP,ISPS,ISPS1,IGEIT,ISPSQ,ISPSQ1,
     #                ISPSO1,ISPSM1
      COMMON /LAPP/ tabanz
      EQUIVALENCE (III(1),I1),(III(2),I2),(III(3),I3),(III(4),I4),
     #(III(5),I5),(III(6),I6),(III(7),I7)
C-IBM-VERSION
*     INCLUDE (ARBEITM)
C-PC-VERSION
      INCLUDE 'ARBEITM'
C
c      write(0,*) 'arbeittest',test
      IDD(1)=ID1
      IDD(2)=ID2
      IDD(3)=ID3
      IDD(4)=ID4
      IDD(5)=ID5
      IDD(6)=ID6
      IDD(7)=ID7
C
C-----SETZEN RANDSCHRANKE FUER ECHTE WERTEFELDER (GT 0) IM RAND---------
C
      DO 80 I7=1,ID7
         DO 70 I6=1,ID6
            DO 60 I5=1,ID5
               DO 50 I4=1,ID4
                  DO 40 I3=1,ID3
                     DO 30 I2=1,ID2
                        DO 20 I1=1,ID1
                           RF=.FALSE.
                           RAND=0
                           DO 10 II1=1,NDIM
                              IF (III(II1).EQ.IDD(II1)) THEN
                                 RF=.TRUE.
C--RANDSCHRANKE KANN BEI NEGATIVER AUSPRAEGUNG FUER BEVORZUGTE
C--SPERRUNGEN IN DEN RAND GENUTZT WERDEN
                                 IF (RFLAG(II1).NE.0)
     #                               RAND=RAND+RFLAG(II1)
                              END IF
   10                      CONTINUE
                           CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #                       TAB(I1,I2,I3,I4,I5,I6,I7))
C--- Grundeinstellung Markierungen
                           idrzw3=0
                           irizw3=1
                           IF (IIZW.GT.0) THEN
                              IIZW3=IIZW
                           ELSE IF (IIZW.LT.0) THEN
C--- KLASSIERUNG SPERRUNG FUER PARALLELE TABELLE
                              IIZW3=INT(RSCHS)
C--- HOECHSTWERT DER DOMINANZ NACH UNTERTABELLE AUF NULL/BEI SEKUNDAER
C--- SPERRUNGEN  DA DOMINANZ BEREITS BERUECKSICHTIGT
                              IF (IIZW/KWERT.LT.ISPP) IDRZW=0
                            if (tabanz.gt.1) then
C--- temp. Markierung der EB-s in Parallelltabelle uebernehmen
                              if (iizw/kwert.eq.igeit) then
                                 idrzw3=55555
C--- temp. Markierung der EB-s zuruecksetzen
                                 iizw=int(iizw+(igei-igeit)*kwert)
                              endif
C--- markieren fuer einfache Sicherung, kein doppelquader bei EB
                              if (iizw/kwert.eq.iSPSO1) then
                                irizw3=55555
C--- temp. Markierung der ALTSperrung mit Ueberpruefung
                                iizw=int(iizw+(iSPS-iSPSO1)*kwert)
                              elseif(iizw/kwert.eq.ispsm1) then
C--- temp. Markierung der ALTSperrung ohne Ueberpruefung
                                irizw3=55555
                                iizw=int(iizw+(iSPSQ-iSPSm1)*kwert)
*                             elseif(iizw/kwert.eq.isps.and.
                              endif
                            ENDIF
                           ELSE
                              IIZW3=0
                           END IF
                           IF (RF) THEN
                              IF (IIZW.GT.0) THEN
C--- RANDSCHRANKEN-ADDITION
                                 IIZW3=INT(REAL(IIZW)+
     #                                 (RSCHG*REAL(RAND)))
                              END IF
                           ELSE
                              IF (IIZW/KWERT.EQ.IGEI) THEN
C--- KLASSIERUNG EINZELBERICHTSPFLICHTIGE
                                 IIZW3=INT(RSCHE)
                              END IF
                           END IF
C--- UEBERNAHME IN PARALLELTABELLE FUER SONDERKLASSIERUNG
C--- DOMINANZ/EB IN ORIGINALTABELLE UPDATEN/ZURUECKSETZEN
                         CALL RCIN(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #                      TAB(I1,I2,I3,I4,I5,I6,I7))
                         CALL RCIN(IIZW3,IRIZW3,IDRZW3,ROZW,RUZW,
     #                      TABR(I1,I2,I3,I4,I5,I6,I7))
   20                   CONTINUE
   30                CONTINUE
   40             CONTINUE
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
   80 CONTINUE
C   SCAN DER TABELLE
      IFEHL=0
      DO 170 I7=1,ID7
         DO 160 I6=1,ID6
            DO 150 I5=1,ID5
               DO 140 I4=1,ID4
                  DO 130 I3=1,ID3
                     DO 120 I2=1,ID2
                        DO 110 I1=1,ID1
C GEHEIMES FELD ?, PIVOT BEREITS DURCH QUADERVERFAHREN GESICHERT ?
                           CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #                       TAB(I1,I2,I3,I4,I5,I6,I7))
                           IF (IIZW.GE.0) GO TO 110
                           IF (NNNN.EQ.1) THEN
C---- 1. ITER.: ALLE AUSSER FRISCH GESETZTE/bereits gepruefte
                              IF (IIZW/KWERT.EQ.ISPSQ.OR.
     #                            IIZW/KWERT.EQ.ISPSQ1) goto 110
C PRIMAERE EINZELBERICHTSPFL. IM RAND BEREITS DURCH INNEN GESPERRT
                              IF (IIZW/KWERT.EQ.IGEI) THEN
                                DO 2 II1=1,NDIM
                                   IF(III(II1).EQ.IDD(II1)) GOTO 110
    2                           CONTINUE
                              ENDIF
                           ELSE
C---- WEITERE ITER.: KEINE PRIM. GH, SEK. GH IM RAND, GH AUS AGG. WECH.
                              IF (IIZW/KWERT.GE.ISPP) GO TO 110
                              DO 90 II1=1,NDIM
                                 IF (III(II1).EQ.IDD(II1)) GO TO 100
   90                         CONTINUE
                              IF (IIZW/KWERT.NE.ISPS1) GO TO 110
                           END IF
C PIVOT GEFUNDEN
  100                      IGG(1)=I1
                           IGG(2)=I2
                           IGG(3)=I3
                           IGG(4)=I4
                           IGG(5)=I5
                           IGG(6)=I6
                           IGG(7)=I7
C--DICHTPRUEFUNG UND EINSPEICHERN DER INDEXLEISTEN----------------------
                           CALL PRUEF (TAB, TABR, ID1, ID2, ID3, ID4,
     #                      ID5, ID6, ID7, NDIM, RSCHE,
     #                      IPRUE)
                           IF (IPRUE.EQ.0) THEN
c                              write(0,*) 'nachpruef',test
c----changes giessing 08/02-a
C--BEI BEDARF: SCHRITTWEISE REDUKTION (FAKTOR 1/WH) DER QUADERQUOTE BIS AUF 0
                              WH=0.
                              QQUALT=QQUOTE
105                           CONTINUE
c----changes giessing 08/02-e
                              CALL SPERR (TAB, TABR, ID1, ID2, ID3, ID4,
     #                          ID5, ID6, ID7, NDIM, RSCHS, RSCHE,
     #                         IPRUE)
c                               write(0,*) 'nachsperr',test
c----changes giessing 08/02-a
                               IF (WH.GT.0.) then
                               QQUOTE=QQUALT
c    changes drumm 08/02-a
c    ausgabe in proto003 u. proto001 u. proto002   
      INQUIRE(UNIT=2,OPENED=open_l)
      IF(.NOT.open_l) CALL OpenFile ('PROTO003', 2, 'N', 'F', ' ', 0)
                               If (IPRUE.gt.0) then 
c----changes giessing 03/03-a
                                if(exact.ne.1) then
                                 if(wh.le.9.) then
                                  write(2,4030)  QQUOTE/(WH+1.)
                                  write(3,4030)  QQUOTE/(WH+1.)
                                  write(7,4030)  QQUOTE/(WH+1.)
                                 elseif(wh.le.10.) then
                                  write(2,4030)  QQUOTE/maxw
                                  write(3,4030)  QQUOTE/maxw
                                  write(7,4030)  QQUOTE/maxw
                                 elseif(wh.eq.11.) then
                                  write(2,4035)   
                                  write(3,4035)  
                                  write(7,4035)   
                                 endif
                                else
                                  nredu(int(wh))=nredu(int(wh))+1
                                endif
c----changes giessing 03/03-e
                               endif
	                          
c    changes drumm-e
                              endif
c----changes giessing 08/02-e
                              IF (IPRUE.EQ.0) THEN
c                                  write(0,*) 'nach2pruef',test
                                 if (wh.eq.0) IFEHL=IFEHL+1
C- Neu 12.11.01
                                 IF(NDIM.LE.4) then
c----changes giessing 03/03-a
                                   IF(((NDIM.LE.2).and.(exact.ne.1))
     #                                .OR.(TEST.EQ.'TEST')) 
     #                                 CALL MONL(NDIM,0)
                                 ENDIF
c----changes giessing 03/03-e
c----changes giessing 08/02-a
                                 WH=WH+1.
                                 IF(WH.LE.9.) THEN
                                  QQUOTE=QQUOTE/(WH+1.)
                                  GOTO 105
                                 ELSEIF (WH.le.10.) THEN
                                  QQUOTE=qquote/maxw
                                  GOTO 105
                                 ELSEIF (WH.gt.10.) THEN
                                   if(exact.eq.1) then
                                     IF (WH.le.11.) THEN
                                  
                                      QQUOTE=0.
                                      GOTO 105
                                     endif
                                    endif
                                 endif
                                 write(7,4040) 
                                 IF (((exact.eq.1).and.(WH.gt.11.))
     #                    .or.((exact.eq.0).and.(WH.gt.10.))) then
c                                  write(0,*) 'arbeitttest',test
c                                  write(0,*) 'Teststop',Test
                                  stop 4105
                                 endif
c    changes drumm 08/02
c      in proto002 schreiben    
                                                            
c----changes giessing 08/02-e
                                 
                              ELSEIF (TEST.EQ.'TEST') THEN
                                 IF(NDIM.LE.4) CALL MONL(NDIM,1)
C- Neu 12.11.01
                              ENDIF
                           END IF
  110                   CONTINUE
  120                CONTINUE
  130             CONTINUE
  140          CONTINUE
  150       CONTINUE
  160    CONTINUE
  170 CONTINUE
      IF ((IFEHL.GT.0).and.(exact.ne.1)) THEN
         WRITE (3,4010) IFEHL
         WRITE (7,4010) IFEHL
*        CALL TABIN (TAB, ID1, ID2, ID3, ID4, ID5, ID6, ID7, MA, MP,
*    #    NDIM, 6, IT, IGR, 1, )
      END IF
      RETURN
      END

      SUBROUTINE PRUEF (TAB, TABR, ID1, ID2, ID3, ID4, ID5, ID6, ID7,
     #NDIM, RSCHE, IPRUE)
C ------------------------------------------------------------------
CI--PRUEFUNG EINER UNTERTABELLE AUF KORREKTE GEHEIMHALTUNG----------I
C ------------------------------------------------------------------
CI TAB    -  TABELLE DER WERTE (7-D)                                I
CI IMDI   -  VEKTOR DER DIMENSIONEN  (ID1,ID2,....,ID7)             I
CI NDIM   -  ANZAHL DER DIMENSIONEN                                 I
CI IGEI   -         ''         DES EINZELBERICHTSPFLICHTIGEN        I
CI IPRUE  -  DICHTANZEIGE                                           I
CI           0 - KEINE DICHTIGKEIT ERMITTELT                        I
CI           1 - DICHT                                              I
CI           0    - DICHTPRUEFUNG UND SPERRUNG                      I
CI           SONST- NUR DICHTPRUEFUNG                               I
C ------------------------------------------------------------------
C  IGG1
C   ...      KOORDINATEN DES PIVOTFELDES
C   ...
C  IGG7
      COMMON IGG(7),IMDI(7),IU(7),IO(7)
      LOGICAL DBQU,SUCH,PRIM
      LOGICAL DBQUA
*09.10.2000
      LOGICAL seku
      INTEGER ABSBET,GEWRAN,SCHAEZ
      COMMON /ABSBET/ ABSBET,GEWRAN,SCHAEZ,DBQUA
C
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 FMIN,MAXF
C
      REAL*4 RANXO(0:1),RANXU(0:1)
      REAL*4 RUZW,ROZW,ROZW3,RUZW3,DOM
*     REAL*8 RUZW,ROZW,ROZW3,RUZW3
*     REAL*4 DOM
      INTEGER IIZW,IIZW3
      INTEGER TABD(4),GEWI,IDRZW,IDRZW3,IRIZW,IXZW
      INTEGER IDD(7),IGD(7),IGE(7)
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMPLEX*16 TAB(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
      COMPLEX*16 TABR(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
      INTEGER TMIN(0:1),WERT,PWERT,NTEIL,IMERK1(7)
      REAL*8 QWERT1(4),QWERT2(4)
      EQUIVALENCE (IGD(1),II1),(IGD(2),II2),(IGD(3),II3),(IGD(4),II4),
     #(IGD(5),II5),(IGD(6),II6),(IGD(7),II7)
      INTEGER KWERT
      COMMON /KWERT/ KWERT
      INTEGER P1,P2,P3
*     COMMON /PRINT/ P1,P2,P3
      COMMON /SPMERK/ IGEI,ISPP,ISPS,ISPS1,IGEIT,ISPSQ,ISPSQ1,
     #                ISPSO1,ISPSM1
C
      CALL RCOUT(PWERT,IRIZW,TABD(2),ROZW,RUZW,
     #   TAB(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
      CALL RCOUT(iizw3,IRIZW3,idrzw3,ROZW3,RUZW3,
     #   TABR(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
      TABD(1)=PWERT
      seku=.TRUE.
      IF (PWERT/KWERT.GE.ISPP) THEN
         PRIM=.TRUE.
      ELSE
*09.10.2000 Umsetzung der Markierungen Altsperrungen
         if(pwert/kwert.eq.isps.and.irizw3.gt.50) seku=.FALSE.
         PRIM=.FALSE.
      END IF
      PWERT=ABS(MOD(PWERT,KWERT))
      IF (PWERT.EQ.KWERT-1) PWERT=0
      DBQU=.FALSE.
      SUCH=.FALSE.
      IPRUE=0
      IDD(1)=ID1
      IDD(2)=ID2
      IDD(3)=ID3
      IDD(4)=ID4
      IDD(5)=ID5
      IDD(6)=ID6
      IDD(7)=ID7
      DO 10 III=1,7
         IMERK1(III)=1
   10 CONTINUE
   20 DO 190 II7=1,ID7
         IF (SUCH.AND.IMERK1(7).GT.II7) GO TO 190
         DO 180 II6=1,ID6
            IF (SUCH.AND.IMERK1(6).GT.II6) GO TO 180
            DO 170 II5=1,ID5
               IF (SUCH.AND.IMERK1(5).GT.II5) GO TO 170
               DO 160 II4=1,ID4
                  IF (SUCH.AND.IMERK1(4).GT.II4) GO TO 160
                  DO 150 II3=1,ID3
                     IF (SUCH.AND.IMERK1(3).GT.II3) GO TO 150
                     DO 140 II2=1,ID2
                        IF (SUCH.AND.IMERK1(2).GT.II2) GO TO 140
                        DO 130 II1=1,ID1
                           IF (SUCH) THEN
                              IF (IMERK1(1).EQ.II1) SUCH=.FALSE.
                              GO TO 130
                           END IF
                           INEB=0
C---- DIA MUSS GEHEIM SEIN, KEIN GESPERRTER EB
                           CALL RCOUT(WERT,IRIZW,IDRZW,ROZW,RUZW,
     #                       TAB(II1,II2,II3,II4,II5,II6,II7))
                           IF (WERT.GE.0) GO TO 130
*09.10.2000
                           IF (seku.and.WERT/KWERT.EQ.IGEIT) goto 130
*                          IF (WERT/KWERT.EQ.IGEIT) GO TO 130
                           CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #                       TABR(II1,II2,II3,II4,II5,II6,II7))
                           IF (REAL(IIZW3).EQ.RSCHE) INEB=INEB+1
C---- DIA-KOORDINATE DARF NICHT TEIL VON PIVOT-KOORDINATE SEIN
                           DO 30 III=1,NDIM
                              IF (IGD(III).EQ.IGG(III)) GO TO 130
   30                      CONTINUE
                           DO 40 III=NDIM+1,7
                              IGE(III)=1
   40                      CONTINUE
C---- SCHLEIFE UEBER ALLE ECKFELDER (NICHT DIA)
                           DO 60 III=1,2**NDIM-2
                              NTEIL=0
                              DO 50 III1=1,NDIM
                                 IF (BTEST(III,III1-1)) THEN
                                    IGE(III1)=IGD(III1)
                                    NTEIL=NTEIL+1
                                 ELSE
                                    IGE(III1)=IGG(III1)
                                 END IF
C--- AGGREGATIONSTUFE BERUECKSICHTIGEN
                                 IF (IGE(III1).EQ.IDD(III1))
     #                              NTEIL=NTEIL+1
   50                         CONTINUE
C---- NICHT GEHEIMES ECKFELD, KEIN GESPERRTER EB
                              CALL RCOUT(WERT,IRIZW,TABD(4),ROZW,RUZW,
     #                         TAB(IGE(1),IGE(2),IGE(3),IGE(4),
     #                         IGE(5),IGE(6),IGE(7)))
                              IF (WERT.GE.0) GO TO 130
*09.10.2000
                              IF (seku.and.WERT/KWERT.EQ.IGEIT) goto 130
*                             IF (WERT/KWERT.EQ.IGEIT) GO TO 130
C--- DOMINANZ UEBER WERT/HOECHST. EINZELWERT VON PIVOT UND AKTUELLEN
C--- FELD NUR FUER EB-S UND PRIMAER, DIREKTE NACHBARSCHAFT, GLEICHE
C--- AGGREGATIONSSTUFE
                              TABD(3)=WERT
                              DOM=0.0
                              IF (PRIM.AND.NTEIL.EQ.1)
     #                           CALL DOMTST(TABD,DOM)
                              IF (DOM.GT.0.0) GOTO 130
                             CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #                         TABR(IGE(1),IGE(2),IGE(3),IGE(4),
     #                         IGE(5),IGE(6),IGE(7)))
                              IF (REAL(IIZW3).EQ.RSCHE) INEB=INEB+1
   60                      CONTINUE
C--- BEHANDLUNG VON EB OHNE SPERRPARTNER EB (KEIN DOPPELQUADER)
C-----AUSSCHLIESSEN VON UEBERPRUEFUNG DER SEKUNDAERSPERRUNGEN
C-----FUER DOPPELQUADER
                         IF(.NOT.DBQUA.AND.INEB.GT.0.and.seku) GOTO 130
C---- TEST DER BEIDEN TEILGESAMTHEITEN
                           RANXO(0)=MAXW
                           RANXO(1)=MAXW
                           RANXU(0)=MAXW
                           RANXU(1)=MAXW
                           DO 80 III=0,2**NDIM-1
                              NTEIL=0
                              DO 70 III1=1,NDIM
                                 IF (BTEST(III,III1-1)) THEN
                                    IGE(III1)=IGD(III1)
                                    NTEIL=NTEIL+1
                                 ELSE
                                    IGE(III1)=IGG(III1)
                                 END IF
                                 IF (IGE(III1).EQ.IDD(III1)) NTEIL=
     #                            NTEIL+1
   70                         CONTINUE
                              NTEIL=MOD(NTEIL,2)
                              CALL RCOUT(WERT,GEWI,IDRZW,ROZW,RUZW,
     #                         TAB(IGE(1),IGE(2),IGE(3),IGE(4),
     #                         IGE(5),IGE(6),IGE(7)))
                              WERT=ABS(MOD(WERT,KWERT))
                              IF (WERT.EQ.KWERT-1) WERT=0
C--- ERMITTELN MINIMUM DER TEILGESAMTHEITEN
                              RANXO(NTEIL)=MIN(RANXO(NTEIL),ABS(ROZW))
                              RANXU(NTEIL)=MIN(RANXU(NTEIL),RUZW)
   80                      CONTINUE
C-----RANGE DARF NICHT 0 SEIN
C-----AUSNAHME: WENN EIN TEILQ. NICHT BESETZT (KOMMT NUR BEI ECKE VOR)
C-----DADURCH WERDEN NULL-TB NUR UEBER ECKE GESICHERT
                           IF (MIN(RANXO(0),RANXU(1)).EQ.0.0.AND.
     #                         MIN(RANXO(1),RANXU(0)).EQ.0.0) GOTO 130
C-----BERECHNEN DES RANGES UND DER QUOTE (WERTE IN ARRAY QWERT1(4))
*09.10.2000
*                          IF (PRIM) THEN
                           IF (seku.and.prim) THEN
C-----NICHT BESETZTE TEILQUADER BLEIBEN AUF HI-VALUE
                              CALL QURECH (PWERT,
     #                         QWERT1(1), QWERT1(2), QWERT1(3),
     #                         QWERT1(4), RANXO(0), RANXO(1),
     #                         RANXU(0), RANXU(1))
                              IF (QWERT1(4).LT.QQUOTE) GO TO 130
                           END IF
C-----EB IM 1. QUADER => SUCHE NACH 2. QUADER
C-----AUSSCHLIESSEN VON UEBERPRUEFUNG DER SEKUNDAERSPERRUNGEN
C- 09.10.2000
*                          IF (.NOT.DBQU.AND.INEB.GT.0.) THEN
                           IF (.NOT.DBQU.AND.INEB.GT.0.and.seku)
     #                        THEN
                              DBQU=.TRUE.
                              DO 100 III=1,2**NDIM-1
                                 DO 90 III1=1,NDIM
                                    IF (BTEST(III,III1-1)) THEN
                                       IGE(III1)=IGD(III1)
                                    ELSE
                                       IGE(III1)=IGG(III1)
                                    END IF
   90                            CONTINUE
                                 CALL RCOUT(IIZW3,IRIZW3,IDRZW3,
     #                            ROZW3,RUZW3,
     #                            TABR(IGE(1),IGE(2),IGE(3),IGE(4),
     #                            IGE(5),IGE(6),IGE(7)))
                                 IF (REAL(IIZW3).EQ.RSCHE) THEN
                                    CALL RCOUT(IIZW,IRIZW,IDRZW,
     #                               ROZW,RUZW,
     #                               TAB(IGE(1),IGE(2),IGE(3),IGE(4),
     #                               IGE(5),IGE(6),IGE(7)))
                                    IXZW=INT(IIZW+(IGEIT-IGEI)*KWERT)
                                    CALL RCIN(IXZW,IRIZW,IDRZW,
     #                               ROZW,RUZW,
     #                               TAB(IGE(1),IGE(2),IGE(3),IGE(4),
     #                               IGE(5),IGE(6),IGE(7)))
                                 END IF
  100                         CONTINUE
                              DO 110 III=1,7
                                 IMERK1(III)=IGD(III)
  110                         CONTINUE
                              DO 120 III=1,4
                                 QWERT2(III)=QWERT1(III)
  120                         CONTINUE
                              GO TO 130
                           END IF
C-----DURCH ALLE PRUEFUNGEN ==> QUADER GEFUNDEN, ENDE
C-----KONTROLLE WELCHE Q. GEFUNDEN WURDEN
*                          IF (BTEST(P3,1).AND.PRIM) THEN
*                             IF (DBQU) THEN
*                                WRITE (14,'(3(F16.0),F16.5)')
*    #                             (QWERT2(III),III=1,4)
*                                WRITE (14,'(3(F16.0),F16.5)')
*    #                             (QWERT1(III),III=1,4)
*                             ELSE
*                                WRITE (14,'(3(F16.0),F16.5)')
*    #                             (QWERT1(III),III=1,4)
*                             END IF
*                          END IF
*                          IF (BTEST(P3,0)) THEN
*                             WRITE (3,'(/A,7I3)') ' P PIV.:',(IGG(III),
*    #                         III=1,7)
*                             IF (DBQU) THEN
*                                IF (PRIM) THEN
*                                   WRITE (3,4010) ' P 1.Q.:',
*    #                               (IMERK1(III),III=1,7),'  P:',
*    #                               QWERT2(1),'  R0:',QWERT2(2),'  R1:'
*    #                               ,QWERT2(3),'  Q:',QWERT2(4)
*                                   WRITE (3,4010) ' P 2.Q.:',(IGD(III),
*    #                               III=1,7),'  P:',QWERT1(1),'  R0:',
*    #                               QWERT1(2),'  R1:',QWERT1(3),'  Q:',
*    #                               QWERT1(4)
*                                ELSE
*                                   WRITE (3,4010) ' P 1.Q.:',
*    #                               (IMERK1(III),III=1,7)
*                                   WRITE (3,4010) ' P 2.Q.:',(IGD(III),
*    #                               III=1,7)
*                                END IF
*                             ELSE
*                                IF (PRIM) THEN
*                                   WRITE (3,4010) ' P   Q.:',(IGD(III),
*    #                               III=1,7),'  P:',QWERT1(1),'  R0:',
*    #                               QWERT1(2),'  R1:',QWERT1(3),'  Q:',
*    #                               QWERT1(4)
*                                ELSE
*                                   WRITE (3,4010) ' P   Q.:',(IGD(III),
*    #                               III=1,7)
*                                END IF
*                             END IF
*4010 FORMAT (A,7I3,3X,1P,3(A,E8.1E2),A,0P,F8.3)
*                          END IF
C-----
                           IPRUE=1
                           GO TO 200
  130                   CONTINUE
  140                CONTINUE
  150             CONTINUE
  160          CONTINUE
  170       CONTINUE
  180    CONTINUE
  190 CONTINUE

  200 IF (DBQU) THEN
         DO 220 III=1,2**NDIM-1
            DO 210 III1=1,NDIM
               IF (BTEST(III,III1-1)) THEN
                  IGE(III1)=IMERK1(III1)
               ELSE
                  IGE(III1)=IGG(III1)
               END IF
  210       CONTINUE
            CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #       TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
            IF (IIZW/KWERT.EQ.IGEIT) THEN
               IXZW=INT(IIZW+(IGEI-IGEIT)*KWERT)
               CALL RCIN(IXZW,IRIZW,IDRZW,ROZW,RUZW,
     #          TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
            ENDIF
  220    CONTINUE
C---- WENN KEINEN 2. QUADER GEFUNDEN, DANN WEITER MIT NORMALSUCHE
         IF (IPRUE.EQ.0) THEN
            DBQU=.FALSE.
            SUCH=.TRUE.
            GO TO 20
         END IF
      END IF
      RETURN
      END

      SUBROUTINE SPERR (TAB, TABR, ID1, ID2, ID3, ID4, ID5, ID6, ID7,
     #NDIM, RSCHS, RSCHE, IPRUE)
C ---------------------------------------------------------------------
CI  SPERRUNG EINER UNTERTABELLE                                        I
C ---------------------------------------------------------------------
CI  KOMPRIMIERTE UNTERTABELLE WIRD UEBERGEBEN.                         I
CI  DER VEKTOR IG ENTHAELT DIE POSITION DES GEHEIMEN FELDES,DAS ZU     I
CI  SICHERN IST.                                                       I
CI  ES IST SICHERZUSTELLEN, DASS NICHTGEHEIME WERTE IN EINEM RAND-     I
CI  VEKTOR, DER NICHT RAND DER BASISTABELLE IST,NICHT ALS SPERRPARTNER I
CI  ZUR VERFUEGUNG STEHEN.                                             I
C ---------------------------------------------------------------------
CUPDATE-------------
C  TAB    - TABELLE  WIRD IM PROGRAMM MODIFIZIERT (SEKUNDAERSPERRUNG)
CEINGANG------------
C  ID1
C   .     - DIMENSIONEN VON TAB
C   .
C  ID7
C  NDIM   - ANZAHL DER DIMENSIONEN
C  ISPSQ - SPERRSCHLUESSEL FUER DIE SEKUNDAERSPERRUNG (QUADER)
C  RSCHG  - RANDSCHRANKE
C
      COMMON IGG(7),IMDI(7),IU(7),IO(7)
CCOMMON-------------
C  IGG    - VEKTOR DER KOORDINATEN DES PIVOTFELDES
C  IMDI   - VEKTOR DER DIMENSIONEN
C  IU     - VEKTOR DER ZEIGER AUF DAS ERSTE PARTNERFELD
C  IO     - VEKTOR DER ZEIGER AUF DAS LETZTE PARTNERFELD
C
      LOGICAL DBQU,PRIM,DBQUA
*09.10.2000
      LOGICAL seku
      INTEGER IDD(7),IMERK(7),IMERK1(7),IGD(7),IGE(7)
      CHARACTER*4 TEST
      COMMON /TEST/ TEST
      INTEGER WERT,PWERT,NTEIL,GTEIL,IIZW,IIZW3
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*4 ROZW,RUZW,ROZW3,RUZW3,DOM
      REAL*8 FMIN,MAXF
      COMPLEX*16 TAB(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
      COMPLEX*16 TABR(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
      INTEGER TABD(4),GEWI,IDRZW,IDRZW3,IRIZW,INEB1
      REAL*4 SWERT
      REAL*4 RANXO(0:1),RANXU(0:1)
      REAL*8 SUMME,MINSUM,QWERT(4),QWERT1(4),QWERT2(4)
      EQUIVALENCE (IGD(1),II1),(IGD(2),II2),(IGD(3),II3),(IGD(4),II4),
     #(IGD(5),II5),(IGD(6),II6),(IGD(7),II7)
      INTEGER KWERT,tabanz
      INTEGER P1,P2,P3
      REAL*8 POTG
      INTEGER ABSBET,GEWRAN,SCHAEZ,NWERT
C- Neu 12.11.01
*     REAL*4 QDA(2000,5,135)
      REAL*4 QDA(50000,5,21)
C- Neu 12.11.01
      INTEGER QUA,MON1
      COMMON /QUA/ QUA,QDA
      REAL*4 MUP
      REAL*8 POT(7),GEWMAX,GEWMIN
      COMMON /LAPP/ tabanz
      COMMON /INSTA/POT,GEWMAX,GEWMIN
      COMMON /ABSBET/ ABSBET,GEWRAN,SCHAEZ,DBQUA
      COMMON /KWERT/ KWERT
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMMON /SPMERK/ IGEI,ISPP,ISPS,ISPS1,IGEIT,ISPSQ,ISPSQ1,
     #                ISPSO1,ISPSM1
C-IBM-VERSION
*     INCLUDE (SPERRM)
C-PC-VERSION
      INCLUDE 'SPERRM'
C
c      write(0,*) 'insperr',test
      MINSUM=0
      INEB1=0
      POTG=0.0
      DO 34 I=1,NDIM
         POTG=POTG+ABS(POT(I))
   34 CONTINUE
      MON1=0
      QUA=0
      CALL RCOUT(PWERT,IRIZW,TABD(2),ROZW,RUZW,
     #           TAB(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
      CALL RCOUT(iizw3,IRIZW3,idrzw3,ROZW3,RUZW3,
     #           TABR(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
      TABD(1)=PWERT
      seku=.TRUE.
      IF (PWERT/KWERT.GE.ISPP) THEN
         PRIM=.TRUE.
      ELSE
*09.10.2000  Umsetzung Markierung der Altsperrungen
         if(pwert/kwert.eq.isps.and.irizw3.gt.50) seku=.FALSE.
         ppwert=pwert
         PRIM=.FALSE.
      END IF
      PWERT=ABS(MOD(PWERT,KWERT))
      IF (PWERT.EQ.KWERT-1) PWERT=0
      IDD(1)=ID1
      IDD(2)=ID2
      IDD(3)=ID3
      IDD(4)=ID4
      IDD(5)=ID5
      IDD(6)=ID6
      IDD(7)=ID7
      DBQU=.FALSE.
   10 IPRUE=0
      
      DO 160 II7=1,ID7
         DO 150 II6=1,ID6
            DO 140 II5=1,ID5
               DO 130 II4=1,ID4

                  DO 120 II3=1,ID3
                     DO 110 II2=1,ID2
                        DO 199 II1=1,ID1

                           INEB=0
C---- DIA DARF NICHT NULL SEIN, KEIN GESPERRTER EB
                           CALL RCOUT(WERT,IRIZW,IDRZW,ROZW,RUZW,
     #                          TAB(II1,II2,II3,II4,II5,II6,II7))
                           IF (WERT.EQ.0) GO TO 100
C- 09.10.2000
                           IF (seku.and.WERT/KWERT.EQ.IGEIT)
     #                         GO TO 100
                           CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #                          TABR(II1,II2,II3,II4,II5,II6,II7))
                           IF (REAL(IIZW3).EQ.RSCHE) INEB=INEB+1
C---- DIA-KOORDINATE DARF NICHT TEIL VON PIVOT-KOORDINATE SEIN
                           DO 20 III=1,NDIM
                              IF (IGD(III).EQ.IGG(III)) GO TO 100
   20                      CONTINUE
                           DO 30 III=NDIM+1,7
                              IGE(III)=1
   30                      CONTINUE

                           SUMME=0.0
                           MON1=1
                           QUA=QUA+1
C- Neu 12.11.01
                           IF(QUA.GT.50000) THEN
C- Neu 12.11.01
                               WRITE (7,1010)
                               STOP 6001
                           ENDIF
C- Neu 12.11.01
                           if(ndim.le.4) then
                             do 31 illi=1,5
                               qda(qua,2,2**ndim+illi)=0.0
31                             continue
                             QDA(QUA,2,MON1)=TABD(1)
                           endif
C- Neu 12.11.01
C----- SCHLEIFE UEBER ALLE ECKFELDER
                           DO 50 III=1,2**NDIM-1
                              MUP=0.0
                              NTEIL=0
                              DO 40 III1=1,NDIM
                                 IF (BTEST(III,III1-1)) THEN
                                    IGE(III1)=IGD(III1)
C---- FUER EINSTUFUNG IN GERADE ODER UNGERADE TEILGESAMTHEIT
                                    NTEIL=NTEIL+1
                                 ELSE
                                    IGE(III1)=IGG(III1)
                                 END IF
C---- AGGREGATIONSSTUFEN FUER EINSTUFUNG MIT BERUECKSICHTIGEN
                                 IF(IGE(III1).EQ.IDD(III1))
     #                              NTEIL=NTEIL+1
C---- KEIN ABSTAND (0), KEINE POTENZIERUNG
                                 IF(ABS(IGE(III1)-IGG(III1)).EQ.0)
     #                              GOTO 40
C- ABSTAENDE ZUM PIVOT UEBER INDIZES UND EXT. POTENTIALFAKTOR BERECHNEN -
                                 MUP=MUP+REAL(ABS(IGE(III1)-IGG(III1))
     #                           **POT(III1))
   40                         CONTINUE

C---- ECKFELD DARF NICHT NULL SEIN, KEIN GESPERRTER EB
                              CALL RCOUT(WERT,GEWI,TABD(4),ROZW,RUZW,
     #                         TAB(IGE(1),IGE(2),IGE(3),IGE(4),
     #                         IGE(5),IGE(6),IGE(7)))
C- Neu 12.11.01
                              if(ndim.le.4) then
                                MON1=MON1+1
                                QDA(QUA,2,MON1)=WERT
                              endif
C- Neu 12.11.01
                              IF (WERT.EQ.0) THEN
C- Neu 12.11.01
                                 if(ndim.le.4) QDA(QUA,2,2**NDIM+1)=1.0
C- Neu 12.11.01
                                 GOTO 100
                              ENDIF
*09.10.2000
                              IF (seku.and.WERT/KWERT.EQ.IGEIT) THEN
C- Neu 12.11.01
                                 if(ndim.le.4) QDA(QUA,2,2**NDIM+1)=2.0
C- Neu 12.11.01
                                 GOTO 100
                              ENDIF
                              TABD(3)=WERT
                              DOM=0.0
C---- NUR PRIMAERGESPERRTE UND NAECHSTER UMGEBUNG DOMINANZ
                              IF(PRIM.AND.NTEIL.EQ.1)
     #                         CALL DOMTST(TABD,DOM)


C- Neu 12.11.01
c                             if(ndim.le.4) QDA(QUA,1,mon1+2**NDIM-1)=dom


C- Neu 12.11.01
                              IF(DOM.GT.0.0) GOTO 100
C---- SUMMIERUNG (BEI DOPPELQUADER -> EB NIEDRIG)

                             CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #                         TABR(IGE(1),IGE(2),IGE(3),IGE(4),
     #                         IGE(5),IGE(6),IGE(7)))
                              SWERT=REAL(IIZW3)
                              IF (SWERT.EQ.RSCHE) INEB=INEB+1
C-----STEUERUNG OB EXTERNE RANGEBESTIMMUNG
C-----INTANTANE GEWICHTUNG NICHT AUF SPERRUNGEN
C- OHNE INSTANTANE GEWICHTUNG ERGIBT MUP=1 ALS STANDARD
                              IF(SWERT.LT.0.0.OR.POTG.EQ.0.0) THEN
                                    MUP=1
                              ENDIF
c*****changes giessing 02.01 a
c - GEWICHTUNG AUCH BEI GEWRAN=2
c                              IF (GEWRAN.NE.1) THEN
                              IF (GEWRAN.eq.0) THEN
c*****changes giessing 02.01 e
                                 SUMME=SUMME+SWERT*MUP
                              ELSE
C---EB UND SEK.GEHEIM MUESSEN NEGATIV BLEIBEN BEI GEWICHTUNG NEGATIV
C--- GEWICHTUNG ENT-KLASSIEREN
c*****changes giessing 10.02 a
c*****SONDERGEWICHTUNG: FALLS KLASSIERTER GEWICHTSWERT NE 1 -> 
C*****KOSTEN WIE PRIMAERSPERRUNGEN
                              if (gewran.ne.3) then
                                 SUMME=SUMME+(SWERT*MUP)*
     #                            (EXP(REAL(GEWI/TWERT))*MINW)
                              else
                                if (gewi.eq.1) then
                                  SUMME=SUMME+SWERT*MUP
                                else
                                  SUMME=SUMME+(SWERT+2*KWERT)*(-1)
                                endif
                              endif
c*****changes giessing 10.02 e
c                                  if((swert.gt.80730).and.
c     #                               (swert.lt.80740)) write(0,*)
c     #                           'SWERT,MUP,
c     #                            (EXP(REAL(GEWI/TWERT))*MINW),P
c     #                             ,gewi ',
c     #                            SWERT,MUP,
c     #                            (EXP(REAL(GEWI/TWERT))*MINW),
c     #                            (SWERT*MUP)*
c     #                            (EXP(REAL(GEWI/TWERT))*MINW),
c     #                             gewi
                              END IF
C- Neu 12.11.01
                             if(ndim.le.4) then
                              QDA(QUA,3,MON1)=SWERT
                              QDA(QUA,4,MON1)=SWERT*MUP
                              QDA(QUA,5,MON1)=SWERT*MUP*
     #                            (EXP(REAL(GEWI/TWERT))*MINW)
                             endif
C- Neu 12.11.01
   50                      CONTINUE

C--- BEHANDLUNG VON EB OHNE SPERRPARTNER EB (KEIN DOPPELQUADER)
C-----AUSSCHLIESSEN VON UEBERPRUEFUNG DER SEKUNDAERSPERRUNGEN
                           IF(.NOT.DBQUA.AND.INEB.GT.0.and.seku) THEN
C- Neu 12.11.01
                               if(ndim.le.4) QDA(QUA,2,2**NDIM+1)=3.0
C- Neu 12.11.01
                               GOTO 100
                           ENDIF
C---- TEST DER BEIDEN TEILGESAMTHEITEN + BESTIMMEN MAXIMAL-WERT
                           RANXO(0)=MAXW
                           RANXO(1)=MAXW
                           RANXU(0)=MAXW
                           RANXU(1)=MAXW
                           GTEIL=0
                           DO 70 III=0,2**NDIM-1
                              NTEIL=0
                              DO 60 III1=1,NDIM
                                 IF (BTEST(III,III1-1)) THEN
                                    IGE(III1)=IGD(III1)
                                    NTEIL=NTEIL+1
                                 ELSE
                                    IGE(III1)=IGG(III1)
                                 END IF
                                 IF (IGE(III1).EQ.IDD(III1)) NTEIL=
     #                            NTEIL+1
   60                         CONTINUE
C--- GESAMTANZAHL MERKEN; PIVOT GERADE WENN GESAMTANZAHL ALLER GERADE
                              GTEIL=GTEIL+NTEIL
C--- NTEIL GIBT AN OB GERADE (0) ODER UNGERADE (1) TEILGESAMTHEIT
                              NTEIL=MOD(NTEIL,2)
                              CALL RCOUT(WERT,GEWI,IDRZW,ROZW,RUZW,
     #                         TAB(IGE(1),IGE(2),IGE(3),IGE(4),
     #                         IGE(5),IGE(6),IGE(7)))
                              WERT=ABS(MOD(WERT,KWERT))
                              IF (WERT.EQ.KWERT-1) WERT=0
C--- MINIMUM DER OBEREN UND UNTEREN TEILGESAMTHEIT (GERADE/UNGERADE)
                              RANXO(NTEIL)=MIN(RANXO(NTEIL),ABS(ROZW))
                              RANXU(NTEIL)=MIN(RANXU(NTEIL),RUZW)
   70                      CONTINUE
C-----RANGE DARF NICHT 0 SEIN
C-----AUSNAHME: WENN EIN TEILQ. NICHT BESETZT (KOMMT NUR BEI ECKE VOR)
C-----DADURCH WERDEN NULL-TB NUR UEBER ECKE GESICHERT
                           IF (MIN(RANXO(0),RANXU(1)).EQ.0.0.AND.
     #                         MIN(RANXO(1),RANXU(0)).EQ.0.0) THEN
C- Neu 12.11.01
                               if(ndim.le.4) QDA(QUA,2,2**NDIM+1)=4.0
C- Neu 12.11.01
                               GOTO 100
                           ENDIF
C-----BERECHNEN DES RANGES UND DER QUOTE (WERTE IN ARRAY QWERT1(4))
C- 09.10.2000 Sekundaersperrungen fuer iterativ doch ausnehmen ?????
C-----AB SCHAETZFEHLERBERECHNUNG AUCH AUF SEKUNDAERSPERRUNGNEN
C- 09.10.2000
C-----NICHT BESETZTE TEILQUADER BLEIBEN AUF HI-VALUE
                           IF (seku) THEN
                              CALL QURECH (PWERT,
     #                         QWERT(1), QWERT(2), QWERT(3), QWERT(4),
     #                         RANXO(0), RANXO(1), RANXU(0), RANXU(1))
C- Neu 12.11.01
                             if(ndim.le.4) then
                              QDA(QUA,2,2**NDIM+2)=QWERT(2)
                              QDA(QUA,2,2**NDIM+3)=QWERT(3)
                              QDA(QUA,2,2**NDIM+4)=QWERT(4)
                             endif
C- Neu 12.11.01
C--- QUADERQUOTE UEBERPRUEFEN
                              IF (QWERT(4).LT.QQUOTE) GOTO 100
                           ENDIF
C- Neu 12.11.01
                           if(ndim.le.4) QDA(QUA,2,2**NDIM+5)=summe
C- Neu 12.11.01
C---- KLEINERE SUMME ?
                           IF (IPRUE.EQ.0.OR.SUMME.LT.MINSUM) THEN
                              IPRUE=1
                              MINSUM=SUMME
                              DO 80 III=1,7
                                 IMERK(III)=IGD(III)
   80                         CONTINUE
                              INEB1=INEB
                              DO 90 III=1,4
                                 QWERT1(III)=QWERT(III)
   90                         CONTINUE
                           END IF
C- Neu 12.11.01
                           if(ndim.le.4) QDA(QUA,2,2**NDIM+1)=0.0
C- Neu 12.11.01

  100                      CONTINUE
  199                   CONTINUE
  110                CONTINUE
  120             CONTINUE
  130          CONTINUE
  140       CONTINUE
  150    CONTINUE
  160 CONTINUE
c      write(0,*) 'sperrnach160',test
      IF (IPRUE.EQ.1) THEN
C-----SPERREN
         MON1=1
         QUA=QUA+1
         DO 180 III=1,2**NDIM-1
            NTEIL=0
            ISPVER=ISPSQ
            DO 170 III1=1,NDIM
               IF (BTEST(III,III1-1)) THEN
                  IGE(III1)=IMERK(III1)
                  NTEIL=NTEIL+1
               ELSE
                  IGE(III1)=IGG(III1)
               END IF
               NTEIL=MOD(NTEIL,2)
               IF (IGE(III1).EQ.IDD(III1)) ISPVER=ISPSQ1
  170       CONTINUE
C--- Markierung von Sperrungen die ein EB als Partner haben
C--- fuer die weitere Beurteilung bei der Ueberlappung
C--- !! IDRZW3 aus Pivot wird in Sperrung uebernommen !!
            IDRZW3=0
            IF(tabanz.gt.1) then
             CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #         TABR(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
             IF (idrzw3.NE.0.and.iizw3.ne.RSCHE) idrzw3=0
C--- Vorsperrungen auf Rand untersuchen
*            IF (.not.seku)
*    #         CALL RANDPR (NDIM, IGG(1), IGG(2), IGG(3), IGG(4), IGG(5)
*    #          , IGG(6), IGG(7))
            ENDIF
            ISPR=1
            CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #          TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
            IF(IIZW.GT.0) THEN
               CALL RCOUT(IIZW3,IRIZW3,IDRZW4,ROZW3,RUZW3,
     #          TABR(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
               IF (GEWRAN.EQ.1) IRIZW=INT(LOG(1.0/XMINW)*TWERT)+1
C--- AENDERUNGEN DER SCHAETZWERTFEHLER WENN NICHT NEGATIV UND ORIG.
C--- GROESSER ALS GEFUNDENER ODER GEFUNDENER GROESSER ALS TEMPORAER
C--- TEMPORAERER IN TABR, WIRD NACH ABARBEITUNG DER UNTERTABELLE ERST
C--- IN DIE ORIGINALTABELLE UEBERNOMMEN (TAB)
c----changes giessing 03/02-a
C----SCHAEZWERTFEHLER WERDEN NICHT UEBERNOMMEN
c               IF (ROZW.GT.0.AND.SCHAEZ.GT.0) THEN
c                  IF (ROZW.GT.REAL(QWERT1(NTEIL+2)).OR.
c     #              ABS(ROZW3).LT.REAL(QWERT1(NTEIL+2)))
c     #              ROZW3=(-1)*REAL(QWERT1(NTEIL+2))
c                  IF (RUZW.GT.REAL(QWERT1(3-NTEIL)).OR.
c     #              RUZW3.LT.REAL(QWERT1(3-NTEIL))) THEN
c                      RUZW3=REAL(QWERT1(3-NTEIL))
c                      IF (ROZW3.GT.0) ROZW3=ROZW*(-1)
c                  ENDIF
c               ENDIF
c----changes giessing 03/02-e
               ISPR=0
C--- FUER DIE SPERRUNG KLASSIEREN (TAB)
               IXZW=INT(IIZW*(-1)+ISPVER*KWERT)
               CALL RCIN(IXZW,IRIZW,IDRZW,ROZW,RUZW,
     #          TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
C--- FUER DIE SPERRUNG KLASSIEREN (TABR)
               IIZW3=INT(RSCHS)
               CALL RCIN(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #          TABR(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
               CALL RANDPR (NDIM, IGE(1), IGE(2), IGE(3), IGE(4), IGE(5)
     #          , IGE(6), IGE(7))
            END IF
            MON1=MON1+1
C- Neu 12.11.01
           if(ndim.le.4) then
            IF (ISPR.EQ.0) THEN
               QDA(QUA,2,MON1)=REAL(IXZW)
               QDA(QUA,3,MON1)=REAL(IIZW3)
            ELSE
               QDA(QUA,2,MON1)=REAL(IIZW)
            ENDIF
            QDA(QUA,3,2**NDIM+5)=9999999999.0
           endif
C- Neu 12.11.01
  180    CONTINUE
C--- AENDERUNG DER SCHAETZWERTFEHLER DES PIVOT (WIE OBEN)
         CALL RCOUT(NWERT,IRIZW,IDRZW,ROZW,RUZW,
     #    TAB(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
C- Neu 12.11.01
         if(ndim.le.4) QDA(QUA,2,1)=NWERT
C- Neu 12.11.01
         CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #    TABR(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
C- Neu 12.11.01
         if(ndim.le.4) QDA(QUA,3,1)=iizw3
C- Neu 12.11.01
c----changes giessing 03/02-a
C----SCHAEZWERTFEHLER WERDEN NICHT UEBERNOMMEN
c         IF (ROZW.GT.0.AND.SCHAEZ.GT.0) THEN
c            GTEIL=MOD(GTEIL,2)
c            IF (ROZW.GT.REAL(QWERT1(GTEIL+2)).OR.
c     #        ABS(ROZW3).LT.REAL(QWERT1(GTEIL+2)))
c     #        ROZW3=(-1)*REAL(QWERT1(GTEIL+2))
c            IF (RUZW.GT.REAL(QWERT1(3-GTEIL)).OR.
c     #        RUZW3.LT.REAL(QWERT1(3-GTEIL))) THEN
c                RUZW3=REAL(QWERT1(3-GTEIL))
c                IF(ROZW3.GT.0) ROZW3=ROZW*(-1)
c            ENDIF
c         ENDIF
c----changes giessing 03/02-e
         CALL RCIN(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #    TABR(IGG(1),IGG(2),IGG(3),IGG(4),IGG(5),IGG(6),IGG(7)))
C-----EB IM 1. QUADER => SUCHE NACH 2. QUADER
C-----AUSSCHLIESSEN VON UEBERPRUEFUNG DER SEKUNDAERSPERRUNGEN
*09.10.2000
         IF (.NOT.DBQU.AND.INEB1.GT.0.and.seku)
     #      THEN
*        IF (.NOT.DBQU.AND.INEB1.GT.0) THEN
            DBQU=.TRUE.
            DO 200 III=1,2**NDIM-1
               DO 190 III1=1,NDIM
                  IF (BTEST(III,III1-1)) THEN
                     IGE(III1)=IMERK(III1)
                  ELSE
                     IGE(III1)=IGG(III1)
                  END IF
  190          CONTINUE
               CALL RCOUT(IIZW3,IRIZW3,IDRZW3,ROZW3,RUZW3,
     #         TABR(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
               IF (REAL(IIZW3).EQ.RSCHE) THEN
                 CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #            TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
                 IXZW=INT(IIZW+(IGEIT-IGEI)*KWERT)
                 CALL RCIN(IXZW,IRIZW,IDRZW,ROZW,RUZW,
     #            TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
               END IF
  200       CONTINUE
            DO 210 III=1,7
               IMERK1(III)=IMERK(III)
  210       CONTINUE
            DO 220 III=1,4
               QWERT2(III)=QWERT1(III)
  220       CONTINUE
            GO TO 10
         END IF
      END IF
C-----KONTROLLE WELCHE Q. GEFUNDEN WURDEN
*     IF (BTEST(P3,1).AND.PRIM) THEN
*        IF (DBQU) THEN
*           WRITE (14,'(3(F16.0),F16.5)') (QWERT2(III),III=1,4)
*           WRITE (14,'(3(F16.0),F16.5)') (QWERT1(III),III=1,4)
*        ELSE
*           WRITE (14,'(3(F16.0),F16.5)') (QWERT1(III),III=1,4)
*        END IF
*     END IF
*     IF (BTEST(P3,0)) THEN
*        WRITE (3,'(/A,7I3)') ' S PIV.:',(IGG(III),III=1,7)
*        IF (DBQU) THEN
*           IF (PRIM) THEN
*              WRITE (3,4010) ' S 1.Q.:',(IMERK1(III),III=1,7),'  P:',
*    #          QWERT2(1),'  R0:',QWERT2(2),'  R1:',QWERT2(3),'  Q:',
*    #          QWERT2(4)
*              IF (IPRUE.EQ.1) WRITE (3,4010) ' S 2.Q.:',(IMERK(III),
*    #          III=1,7),'  P:',QWERT1(1),'  R0:',QWERT1(2),'  R1:',
*    #          QWERT1(3),'  Q:',QWERT1(4)
*           ELSE
*              WRITE (3,4010) ' S 1.Q.:',(IMERK1(III),III=1,7)
*              IF (IPRUE.EQ.1) WRITE (3,4010) ' S 2.Q.:',(IMERK(III),
*    #          III=1,7)
*           END IF
*        ELSE
*           IF (PRIM) THEN
*              WRITE (3,4010) ' S   Q.:',(IMERK(III),III=1,7),'  P:',
*    #          QWERT1(1),'  R0:',QWERT1(2),'  R1:',QWERT1(3),'  Q:',
*    #          QWERT1(4)
*           ELSE
*              WRITE (3,4010) ' S   Q.:',(IMERK(III),III=1,7)
*           END IF
*        END IF
*4010 FORMAT (A,7I3,3X,1P,3(A,E8.1E2),A,0P,F8.3)
*     END IF
C---- ZURUECKSETZEN EB BEI DB-Q.
      IF (DBQU) THEN
         IPRUE=IPRUE+2
         DO 240 III=1,2**NDIM-1
            DO 230 III1=1,NDIM
               IF (BTEST(III,III1-1)) THEN
                  IGE(III1)=IMERK1(III1)
               ELSE
                  IGE(III1)=IGG(III1)
               END IF
  230       CONTINUE
            CALL RCOUT(IIZW,IRIZW,IDRZW,ROZW,RUZW,
     #        TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
            IF (IIZW/KWERT.EQ.IGEIT) THEN
               IXZW=INT(IIZW+(IGEI-IGEIT)*KWERT)
               CALL RCIN(IXZW,IRIZW,IDRZW,ROZW,RUZW,
     #           TAB(IGE(1),IGE(2),IGE(3),IGE(4),IGE(5),IGE(6),IGE(7)))
            ENDIF
  240    CONTINUE
      END IF
c      write(0,*) 'endesperr',test
      RETURN
      END

      SUBROUTINE TABIN (TAB, ID1, ID2, ID3, ID4, ID5, ID6, ID7, MA, MP,
     # NDIM, IO, IK, PP)
C  -------------------------------------------------------------------
C I                                                                   I
C I   T A B I N                                                       I
C I                                                                   I
C I   SUBROUTINE ZUM PROTOKOLLIERUNG VON ARBEITSTABELLEN VOR ODER     I
C I   NACH DEM SPERRVORGANG.                                          I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   SUBROUTINE TABIN(TAB,ID1,ID2,ID3,ID4,ID5,ID6,ID7,MA,MP,NDIM,IO, I
C I  * IK,PP)                                                         I
C  -------------------------------------------------------------------
C  -------------------------------------------------------------------
C  -AUSGABE DER TABELLE . DIE INDIZES DER DIMENSION 2 UND FF. WERDEN--
C  -VORANGESTELLT. MAXIMAL 8-STELLIGE WERTE ZULAESSIG.----------------
C  -------------------------------------------------------------------
C I                                                                   I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I    TAB       LFD. UNTERTABELLE   (I*4)                            I
C I    ID1...ID7 LAENGE DER DIM.     (I*4)                            I
C I     MA       AGGREGATIONSINDICES (I*4)                            I
C I     MP       POSITIONSINDICES    (I*4)                            I
C I   NDIM       ANZAHL DER DIM.     (I*4)                            I
C I     IO       AUSGABEEINHEIT      (I*4)                            I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C  -------------------------------------------------------------------
      INTEGER IMDI(7),MA(7),MP(7),PP,WERT
      INTEGER KWERT
      COMMON /KWERT/ KWERT
      CHARACTER*1 KA(100),KZ(100)
      CHARACTER TABTEXT*20
      COMPLEX*16 TAB(ID1,ID2,ID3,ID4,ID5,ID6,ID7)
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 FMIN,MAXF
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      INTEGER IIZW(100),IDRZW,IRIZW(100)
      REAL*4 TABS(100),ROZW,RUZW
*+++++Changes jerskat 04.00

      IMDI(1)=ID1
      IMDI(2)=ID2
      IMDI(3)=ID3
      IMDI(4)=ID4
      IMDI(5)=ID5
      IMDI(6)=ID6
      IMDI(7)=ID7
C
C-----UEBERSCHRIFT (TESTTABELLE)----------------------------------------
C
C-IBM-VERSION
*     INCLUDE (TABINM)
C-PC-VERSION
      INCLUDE 'TABINM'
C
      WRITE (IO,4010) IK,TABTEXT
      WRITE (IO,4020) (MA(I),I=1,NDIM)
 4020 FORMAT (/1X,' MA(I),I=1(1)NDIM ',7I3)
      WRITE (IO,4030) (MP(I),I=1,NDIM)
 4030 FORMAT (1X,' MP(I),I=1(1)NDIM ',7I3)
      WRITE (IO,4050) NDIM,(IMDI(I),I=1,NDIM)
      WRITE (IO,4060) ' '
 4060 FORMAT (1X,A)
C
C-----SCHLEIFE UEBER ALLE DIMENSIONEN-----------------------------------
C
      DO 90 I7=1,ID7
         DO 80 I6=1,ID6
            DO 70 I5=1,ID5
               DO 60 I4=1,ID4
                  DO 50 I3=1,ID3
                     DO 40 I2=1,ID2
                        DO 10 I=1,100
                           TABS(I)=0
                           IIZW(I)=0
                           IRIZW(I)=0
   10                   CONTINUE
                        IF (PP.EQ.2) THEN
                           DO 20 I1=1,ID1
                              CALL RCOUT(IIZW(I1),IRIZW(I1),IDRZW,
     #                             ROZW,RUZW,TAB(I1,I2,I3,I4,I5,I6,I7))
                              WERT=ABS(MOD(IIZW(I1),KWERT))
                              IF (WERT.EQ.KWERT-1) THEN
                                 TABS(I1)=0.0
                              ELSE
                                 TABS(I1)=EXP(REAL(WERT)/TWERT)*MINW
                              END IF
   20                      CONTINUE
                           KA(I1)='('
                           KZ(I1)=')'
                           WRITE (IO,4090) (KA(I1),TABS(I1),IIZW(I1)/
     #                      KWERT,I1=1,ID1)
                           WRITE (IO,4080) (IRIZW(I1),KZ(I1),I1=1,ID1)
                        ELSE
                           DO 30 I1=1,ID1
                              KA(I1)='('
                              KZ(I1)=')'
                              CALL RCOUT(IIZW(I1),IRIZW(I1),IDRZW,
     #                             ROZW,RUZW,TAB(I1,I2,I3,I4,I5,I6,I7))
   30                      CONTINUE
                           WRITE (IO,4070) (KA(I1),MOD(IIZW(I1),KWERT),
     #                      IIZW(I1)/KWERT,I1=1,ID1)
                           WRITE (IO,4080) (IRIZW(I1),KZ(I1),I1=1,ID1)
                        END IF
 4070 FORMAT (1X,10(A1,I8,I2))
 4080 FORMAT (1X,10(1X,1PE8.1E2,A1))
 4090 FORMAT (1X,10(A1,1PE8.1E2,I2))
   40                CONTINUE
                     IF (NDIM.GT.2) WRITE (IO,4060) '--'
   50             CONTINUE
                  IF (NDIM.GT.3) WRITE (IO,4060) '---'
   60          CONTINUE
               IF (NDIM.GT.4) WRITE (IO,4060) '----'
   70       CONTINUE
            IF (NDIM.GT.5) WRITE (IO,4060) '-----'
   80    CONTINUE
         IF (NDIM.GT.6) WRITE (IO,4060) '------'
   90 CONTINUE
      RETURN
      END


c*****changes giessing 08.01 a
c*****changes giessing 08.01 a
c      SUBROUTINE UPDA (NGES, WGES, ISK, BS, LST, STACK, IJK, NDIM)
      SUBROUTINE UPDA (NGES, WGES, ISK, BS, LST, STACK, IJK, NDIM,mode)
c*****changes giessing 08.01 e
C  -------------------------------------------------------------------
C I                                                                   I
C I   U P D A                                                         I
C I                                                                   I
C I   SUBROUTINE ZUM RUECKSTELLEN DES WERTARTSCHLUESSEL BEI DEN ZU    I
C I   SPERRENDEN SAETZEN DER EINGABE.                                 I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I    NGES ANZAHL BES. FELDER       (I*4)                            I
C I    WGES WERTESUMME               (R*8)                            I
C I     ISK LAENGE DES SORTIERBER.   (I*4)                            I
C I      BS SORTIERBEREICH           (I*4)                            I
C I     LST LAENGE DER ARBEITST.     (I*4)                            I
C I   STACK ARBEITSTABELLE FUER SORT (I*4)                            I
C I     IJK ANZAHL ZU SORT. FELDER   (I*4)                            I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   ISORTS                                                          I
C I                                                                   I
C I   SUBROUTINE UPDA(NGES,WGES,ISK,BS,IBS,LST,STACK,IJK,NDIM)        I
C  -------------------------------------------------------------------
      IMPLICIT REAL*8 (W,P)
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 FMIN,MAXF
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      INTEGER BERPFL,BS(ISK),STACK(LST),IJK,NDIM
c*****changes giessing 08.01 a
      integer imodw,mode
c*****changes giessing 08.01 e
C
C-IBM-VERSION
*     CHARACTER*800 Eingabe
C-PC-VERSION
      CHARACTER*800 Eingabe
      INTEGER*2 anf,ende,WTART
C
      CHARACTER KEY(7)*8
      REAL*8 WERT,GEWI,FEHLO,FEHLU,HOEW
C-IBM-VERSION
*     INCLUDE (UPDAM)
C-PC-VERSION
      INCLUDE 'UPDAM'
C
C-----INITIALISIERUNG---------------------------------------------------
C
      IZK=0
      IZ=0
      IZ0=0
      IGS=0
      IGSN=0
      WERTZ=0.0
C
C-----SORTIERUNG DER ZU SPERRENDEN SAETZE (REL. SATZNUMMER BEZOGEN------
C-----AUF BESETZTE FELDER)----------------------------------------------
C
      REWIND  1
C-IBM-VERSION
      INQUIRE (1, RECL=irecl) 
	
      IF(irecl.gt.800) then
c        write(0,*) 'ircel',irecl
        STOP 8888
	endif
C
      LSTN=LST/2
      IF (IJK.GT.0) THEN
         CALL ISORTS (IJK, BS, LSTN, STACK)
      ELSE
         GO TO 90
      END IF
C
C-----VORBEIFUEHREN DER EINGABEDATEI AN ZU SPERRENDEN SATZNUMMERN-------
C
      IZK=IZK+1
      IF (IZK.GT.IJK) GO TO 80
      INUM=BS(IZK)
C-IBM-VERSION
*     READ (1,'(A)',END=70) Eingabe(1:irecl)
*     READ (Eingabe(1:irecl)) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,
*    *(Key(I),I=1,NDIM)
*     anf=0
*     ende=0
*     DO 15 i=1,irecl
*        if(eingabe(i:i).NE.' '.and.anf.eq.0) then
*         do 14 j=i,irecl
*           if (eingabe(j:j).EQ.' ') then
*              anf=j
*              goto 13
*           endif
*  14     continue
*        endif
*  15 continue
*  13 DO 16 i=irecl,1,-1
*       if(eingabe(i:i).NE.' '.and.ende.eq.0) then
*          ende=i
*          goto 17
*       endif
*  16 continue
C-PC-VERSION



c*****changes giessing 08.01 a

c   10 READ (1,*,END=70) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,
c     *(Key(I),I=1,NDIM)
  10  if(mode.eq.0) then
      READ (1,*,END=70) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,
     *(Key(I),I=1,NDIM)
      else if((mode.eq.2).or.(mode.eq.4)) then
      READ (1,*,END=70) WTART,BERPFL,WERT,GEWI,IMODW,
     #  HOEW,FEHLO,FEHLU,(Key(I),I=1,NDIM)
      endif
c*****changes giessing 08.01 e


C
   17 IZ0=IZ0+1
C
C-----BESETZUNG DER EINGABEDATEI PRUEFEN--------------------------------
C
      IF (WERT.NE.0.) GO TO 30
C
      IF (WTART.GT.127) GO TO 30
C
      IF (BERPFL.NE.0) GO TO 30
C
C-----NICHT GEHEIME FELDER MIT WERT = 0---------------------------------
C
C-IBM-VERSION
*  20 WRITE (8,*) WTART,Eingabe(anf:ende)
C-PC-VERSION


c*****changes giessing 08.01 a
c      20 WRITE (8,*) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,' ',(''''//
c     #Key(I)//''' ',I=1,NDIM)
  20  if(mode.eq.0) then
*      WRITE (8,*) WTART,BERPFL,WERT,GEWI,HOEW,
*    #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       WRITE (Eingabe,*) WTART,BERPFL,WERT,GEWI,HOEW,
     #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       do 771 i=800,1,-1
          if (Eingabe(i:i).ne.' ') goto 772
 771   continue
 772   write(8,*) eingabe(1:i)
      else if((mode.eq.2).or.(mode.eq.4)) then
*      WRITE (8,*) WTART,BERPFL,WERT,GEWI,imodw,HOEW,
*    #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       WRITE (Eingabe,*) WTART,BERPFL,WERT,GEWI,imodw,HOEW,
     #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       do 773 i=800,1,-1
          if (Eingabe(i:i).ne.' ') goto 774
 773   continue
 774   write(8,*) eingabe(1:i)
      endif
c*****changes giessing 08.01 e
C
      GO TO 10
C
C-----FELDER MIT WERT NE 0 ODER PRIMAER GEHEIM--------------------------
C
   30 IZ=IZ+1
   40 IF (IZ-INUM) 20,50,60
   50 WTART=WTART+1128
C
      IGS=IGS+1
      IF (WERT.EQ.NULW) THEN
         IGSN=IGSN+1
      ELSE
         WERTZ=WERTZ+WERT
      END IF
      GO TO 20
   60 IZK=IZK+1
      IF (IZK.GT.IJK) GO TO 80
      INUM=BS(IZK)
      GO TO 40
C
C-----EOF-EINGABEDATEI--------------------------------------------------
C
   70 IZK=IZK+1
      IF (IZK.GT.IJK) GO TO 100
      INUM=BS(IZK)
      WRITE (3,4010) IZ0,INUM
      GO TO 100
C
C-IBM-VERSION
*  80 WRITE (8,*) WTART,Eingabe(anf:ende)
C-PC-VERSION

c*****changes giessing 08.01 a
c   80 WRITE (8,*) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,' ',(''''//
c     #Key(I)//''' ',I=1,NDIM)
  80   if(mode.eq.0) then
*       WRITE (8,*) WTART,BERPFL,WERT,GEWI,HOEW,
*    #   FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       WRITE (Eingabe,*) WTART,BERPFL,WERT,GEWI,HOEW,
     #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       do 775 i=800,1,-1
          if (Eingabe(i:i).ne.' ') goto 776
 775   continue
 776   write(8,*) eingabe(1:i)
       else if((mode.eq.2).or.(mode.eq.4)) then
*       WRITE (8,*) WTART,BERPFL,WERT,GEWI,imodw,HOEW,
*    #   FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
c       WRITE (Eingabe,*) WTART,BERPFL,WERT,imodw,GEWI,HOEW,
c     #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
         WRITE (Eingabe,*) WTART,BERPFL,WERT,GEWI,imodw,HOEW,
     #FEHLO,FEHLU,' ',(''''//Key(I)//''' ',I=1,NDIM)
       do 777 i=800,1,-1
          if (Eingabe(i:i).ne.' ') goto 778
 777   continue
 778   write(8,*) eingabe(1:i)
       endif
c*****changes giessing 08.01 e


C
C-IBM-VERSION
*  90 INQUIRE (UNIT=1,RECL=irecl)
*     READ (1,'(A)',END=70) Eingabe(1:irecl)
*     READ (Eingabe(1:irecl),*) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,
*    *(Key(I),I=1,NDIM)
*     anf=0
*     ende=0
*     DO 95 i=1,irecl
*        if(eingabe(i:i).NE.' '.and.anf.eq.0) then
*         do 94 j=i,irecl
*           if (eingabe(j:j).EQ.' ') then
*              anf=j
*              goto 93
*           endif
*  94     continue
*        endif
*  95 continue
*  93 DO 96 i=irecl,1,-1
*       if(eingabe(i:i).NE.' '.and.ende.eq.0) then
*          ende=i
*          goto 97
*       endif
*  96 continue
C-PC-VERSION

c*****changes giessing 08.01 a

c      90 READ (1,*,END=70) WTART,BERPFL,WERT,GEWI,HOEW,FEHLO,FEHLU,
c     *(Key(I),I=1,NDIM)

  90   if(mode.eq.0) then
        READ (1,*,END=70) WTART,BERPFL,WERT,GEWI,HOEW,
     *FEHLO,FEHLU,(Key(I),I=1,NDIM)
       else if((mode.eq.2).or.(mode.eq.4)) then
        READ (1,*,END=70) WTART,BERPFL,WERT,GEWI,IMODW,HOEW,
     #  FEHLO,FEHLU,(Key(I),I=1,NDIM)
       endif
c*****changes giessing 08.01 e

   97 IZ0=IZ0+1
      GO TO 80
C
C-----ERSTELLUNG DER SPERRSTATISTIK-------------------------------------
C
  100 PRON=DFLOAT(IGS)*100.0/DFLOAT(NGES)
      IF (WGES.NE.0.0) THEN
         PROZ=WERTZ*100.0/WGES
      ELSE
         PROZ=0.0
      END IF
      WRITE (3,4020) IGS,PRON,WERTZ,PROZ

*     WRITE (7,'(I10,A)') IGS,' NSGH'
*     WRITE (7,'(I10,A)') IGSN,' NSGHN'
*     WRITE (7,'(F5.2,A)') PROZ,' WPR'
      RETURN
      END

      SUBROUTINE NSORT(ndim,t,dim,lstn,stack,anz_schl)
C  -------------------------------------------------------------------
C I                                                                   I
C I   N S O R T S                                                     I
C I                                                                   I
C I   SUBROUTINE ZUM SORTIEREN VON ERMITTELTEN UEBERLAPPUNGSINDICES   I
C I   ALLER VORHANDENEN DIMENSIONEN NACH SORT-KRITERIEN DER EINGABE-  I
C I   DATEIEN (PSEUDODIMENSIONEN NACH HINTEN)                         I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I       T WELCHE TABELLE           (I*4)                            I
C I    NDIM ORG. ANZ. DER DIMENS. der Tabelle (i*4)                   I
C I     DIM LAENGE VON erk_tab(1. DIM) (I*4) (Anz. aller Dim.)        I
C I    LSTN LAENGE DER 2. DIMENSION  (I*4)                            I
C I         VON STACK                                                 I
C I   STACK ARBEITSTABELLE FUER SORT (I*4)                            I
C IANZ_SCHL Anzahl der der gefundenen Schloesser je Dimension         I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I erg_tab ARRAY MIT SORT. FELD.  (I*2)                              I
C I    indx ARRAY MIT SORT. FELD.  (I*4)                              I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C  -------------------------------------------------------------------
      INTEGER*4 N1,STACK(2,LSTN),S,T,L,R,I,J,N2,dim,m,u,k
      INTEGER*4 neu_start
C- NEU 28.04.04
*      INTEGER*2 erk_tab(0:40,0:750000),int_tab(20,40,3500),x(40),w(40)
      INTEGER*2 erk_tab(0:40,0:750000),int_tab(20,40,12000),x(40),w(40)
C- NEU 28.04.04
      INTEGER pseu_stell(0:5,0:40,0:20),anz_dim(20),stell,ndim
      INTEGER anz_schl(0:40),max_schl,indx(750000)
      REAL*8 v,q,v_dim
      COMMON /PSEU/ anz_dim,pseu_stell,stell
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab
C-IBM-VERSION
*     INCLUDE (NSORTSM)
C-PC-VERSION
      INCLUDE 'NSORTSM'
C
C-----BESTIMMUNG DER GROESSE DES ARBEITSBEREICHS-----------------------
C
C Einordnungswert (groesste Schluesselanzahl fuer sort ermitteln)
      max_schl=0
      do 5 k =1,dim
         max_schl=max(anz_schl(k),max_schl)
    5 continue
      max_schl=max_schl+1+ndim
C- Teilkette vergroessert !!! (-80-)
      N2=INT(LOG(FLOAT(2*neu_start)))+80
      IF (N2.GT.LSTN) GO TO 110
C
C-----INITIALISIERUNG DES SORT-----------------------------------------
C
      S=1
      STACK(1,1)=1
      STACK(2,1)=neu_start
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN BEREICHE VON TEILKETTEN---------
C
   10 IF (S.EQ.0) GO TO 90
      L=STACK(1,S)
      R=STACK(2,S)
      S=S-1
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN TEILKETTEN EINES BEREICHS-------
C
   20 IF (L.GE.R) GO TO 10
      I=L
      J=R
      v_dim=dim
      v=0.0
      do 25 k = ndim,1,-1
         v=v+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),(L+R)/2))
         v_dim=v_dim-1
   25 CONTINUE
      do 26 k = dim,ndim+1,-1
         v=v+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),(L+R)/2))
         v_dim=v_dim-1
   26 CONTINUE
      GO TO 50
   30 I=I+1
   40 continue
      v_dim=dim
      q=0.0
      do 35 k = ndim,1,-1
         q=q+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),i))
         v_dim=v_dim-1
   35 CONTINUE
      do 36 k = dim,ndim+1,-1
         q=q+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),i))
         v_dim=v_dim-1
   36 CONTINUE
      IF (q.LT.v) GO TO 30
      GO TO 70
C
C-----SCHLEIFE FUER DIE SORTIERUNG EINER TEILKETTE----------------------
C
   50 IF (I.GT.J) GO TO 80
      GO TO 40
   60 J=J-1
   70 continue
      v_dim=dim
      q=0.0
C- Realer Schluessel-INDEX von hinten nach vorne
      do 45 k = ndim,1,-1
         q=q+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),j))
         v_dim=v_dim-1
   45 CONTINUE
C- Pseudo Schluessel-INDEX von hinten nach vorne
      do 46 k = dim,ndim+1,-1
         q=q+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),j))
         v_dim=v_dim-1
   46 CONTINUE
      IF (v.LT.q) GO TO 60
      IF (I.LE.J) THEN
         DO 78 m = 1,dim
            W(m)=erk_tab(pseu_stell(5,m,t),I)
            erk_tab(pseu_stell(5,m,t),I)=erk_tab(pseu_stell(5,m,t),J)
            erk_tab(pseu_stell(5,m,t),J)=W(m)
   78    CONTINUE
C- Tatsaechlicher Index auf die Schloesser mit sortieren
         u=indx(i)
         indx(I)=indx(J)
         indx(J)=u
         I=I+1
         J=J-1
      END IF
      GO TO 50
C
C-----AUSWAHL DER KUERZEREN TEILKETTE FUER WEITERE SORTIERUNG-----------
C
   80 IF (J-L.LT.R-I) THEN
         IF (I.LT.R) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (RECHTE TEILKETTE)---------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=I
            STACK(2,S)=R
         END IF
         R=J
      ELSE
         IF (L.LT.J) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (LINKE TEILKETTE)----------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=L
            STACK(2,S)=J
         END IF
         L=I
      END IF
      GO TO 20
C
   90 N1=neu_start-1
C
C-----KONTROLLE DER EXAKTEN SORTIERUNG----------------------------------
C
      q=0.0
      v=0.0
      DO 100 I=1,N1
         v_dim=dim
         do 95 k = ndim,1,-1
            q=q+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),i))
            v=v+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),i+1))
            v_dim=v_dim-1
   95    CONTINUE
         do 96 k = dim,ndim+1,-1
            q=q+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),i))
            v=v+max_schl**v_dim*real(erk_tab(pseu_stell(5,k,t),i+1))
            v_dim=v_dim-1
   96    CONTINUE
         IF (v-q.LT.0) THEN
            WRITE (7,4010)
            STOP 6500
         ENDIF
  100 CONTINUE
      GO TO 120
C
C-----ZU KLEINER SORTIERBEREICH-----------------------------------------
C
  110 WRITE (7,4020) N2,S,LSTN,neu_start
      STOP 6501
  120 continue
      RETURN
      END

      SUBROUTINE ISORTS (N, A, LSTN, STACK)
C  -------------------------------------------------------------------
C I                                                                   I
C I   I S O R T S                                                     I
C I                                                                   I
C I   SUBROUTINE ZUM SORTIEREN VON SEKUNDAER GESPERRTEN FELDERN NACH  I
C I   QUICKSORT-ALGORITHMUS.                                          I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I       N LAENGE VON A             (I*4)                            I
C I       A ARRAY MIT UNSORT. FELD.  (I*4)                            I
C I    LSTN LAENGE DER 2. DIMENSION  (I*4)                            I
C I         VON STACK                                                 I
C I   STACK ARBEITSTABELLE FUER SORT (I*4)                            I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I       A ARRAY MIT SORT. FELD.    (I*4)                            I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C I   SUBROUTINE ISORTS(N,A,LSTN,STACK)                               I
C  -------------------------------------------------------------------
      INTEGER*4 N,N1,A(N),STACK(2,LSTN),S,L,R,I,J,X,W,N2
C-IBM-VERSION
*     INCLUDE (ISORTSM)
C-PC-VERSION
      INCLUDE 'ISORTSM'
C
C-----BESTIMMUNG DER GROESSE DES ARBEITSBEREICHS-----------------------
C
      N2=INT(LOG(FLOAT(2*N)))+2
      IF (N2.GT.LSTN) GO TO 110
C
C-----INITIALISIERUNG DES SORT-----------------------------------------
C
      S=1
      STACK(1,1)=1
      STACK(2,1)=N
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN BEREICHE VON TEILKETTEN---------
C
   10 IF (S.EQ.0) GO TO 90
      L=STACK(1,S)
      R=STACK(2,S)
      S=S-1
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN TEILKETTEN EINES BEREICHS-------
C
   20 IF (L.GE.R) GO TO 10
      I=L
      J=R
      X=A((L+R)/2)
      GO TO 50
   30 I=I+1
   40 IF (A(I).LT.X) GO TO 30
      GO TO 70
C
C-----SCHLEIFE FUER DIE SORTIERUNG EINER TEILKETTE----------------------
C
   50 IF (I.GT.J) GO TO 80
      GO TO 40
   60 J=J-1
   70 IF (X.LT.A(J)) GO TO 60
      IF (I.LE.J) THEN
         W=A(I)
         A(I)=A(J)
         A(J)=W
         I=I+1
         J=J-1
      END IF
      GO TO 50
C
C-----AUSWAHL DER KUERZEREN TEILKETTE FUER WEITERE SORTIERUNG-----------
C
   80 IF (J-L.LT.R-I) THEN
         IF (I.LT.R) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (RECHTE TEILKETTE)---------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=I
            STACK(2,S)=R
         END IF
         R=J
      ELSE
         IF (L.LT.J) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (LINKE TEILKETTE)----------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=L
            STACK(2,S)=J
         END IF
         L=I
      END IF
      GO TO 20
C
   90 N1=N-1
C
C-----KONTROLLE DER EXAKTEN SORTIERUNG----------------------------------
C
      DO 100 I=1,N1
         IF (A(I+1)-A(I).LT.0) THEN
            WRITE (7,4010)
            STOP 6502
         END IF
  100 CONTINUE
      GO TO 120
C
C-----ZU KLEINER SORTIERBEREICH-----------------------------------------
C
  110 WRITE (7,4020) N2,S,LSTN,N
      STOP 6503
  120 RETURN
      END

      SUBROUTINE CSORTS (ss_stell,LSTN, STACK)
C  -------------------------------------------------------------------
C I                                                                   I
C I   C S O R T S                                                     I
C I                                                                   I
C I   SUBROUTINE ZUM SORTIEREN VON SCHLUESSELN (CHARAKTERFELD) NACH   I
C I   QUICKSORT-ALGORITHMUS. (Ermittlungstabelle fuer Ueberlappungen  I
C I   wird mitsortiert)                                               I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I    LSTN LAENGE DER 2. DIMENSION  (I*4)                            I
C I         VON STACK                                                 I
C I   STACK ARBEITSTABELLE FUER SORT (I*4)                            I
C ISS_STELL Anzahl der Dimensionen                                    I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I schl_nam Schloesser jeder Dimension (CH*8)                        I
C I  int_tab Ermittlungstabelle fuer Ueberlappungen                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C  -------------------------------------------------------------------
      INTEGER*4 N,N1,STACK(2,LSTN),S,L,R,I,J,N2,mm,nn,ss_stell
C- NEU 28.04.04
*      CHARACTER*8 x,w,schl_nam(0:40,0:3500),nam_dim(40)
      CHARACTER*8 x,w,schl_nam(0:40,0:12000),nam_dim(40)
C- NEU 28.04.04
      character*8 pseu_key(0:40,0:20)
      INTEGER i_dim,anz_schl(0:40)
C- NEU 28.04.04
*      INTEGER*2 int_tab(20,40,3500),y(20)
*      INTEGER*2 erk_tab(0:40,0:750000),erk2_tab(40,3500)
      INTEGER*2 int_tab(20,40,12000),y(20)
      INTEGER*2 erk_tab(0:40,0:750000),erk2_tab(40,12000)
C- NEU 28.04.04
      INTEGER indx(750000),unten,oben,tabanz
      INTEGER*2 SO(40)
      COMMON /AUFAB/SO
      COMMON /LAPP/ tabanz
      COMMON /SSLA/ anz_schl,nam_dim,pseu_key
      COMMON /SSNA/ schl_nam
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab
C-IBM-VERSION
*     INCLUDE (ISORTSM)
C-PC-VERSION
      INCLUDE 'ISORTSM'
C
C-----SORTIERUNG UEBER ALLE VORHANDENEN DIMENSIONEN ------------------
C
      DO 500 mm = 1, ss_stell
C
C-----BESTIMMUNG DER GROESSE DES ARBEITSBEREICHS-----------------------
C
      n=anz_schl(mm)
      N2=INT(LOG(FLOAT(2*N)))+2
      IF (N2.GT.LSTN) GO TO 110
C
C-----BESTIMMUNG OB AUFSTEIGEND ODER ABSTEIGEN ------------------------
C
      IF(SO(mm).eq.1) GOTO 200
C
C-----INITIALISIERUNG DES SORT AUFSTEIGEND ----------------------------
C
      S=1
      STACK(1,1)=1
      STACK(2,1)=n
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN BEREICHE VON TEILKETTEN---------
C
   10 IF (S.EQ.0) GO TO 90
      L=STACK(1,S)
      R=STACK(2,S)
      S=S-1
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN TEILKETTEN EINES BEREICHS-------
C
   20 IF (L.GE.R) GO TO 10
      I=L
      J=R
      X=schl_nam(mm,(L+R)/2)
      GO TO 50
   30 I=I+1
   40 IF (schl_nam(mm,I).LT.X) GO TO 30
      GO TO 70
C
C-----SCHLEIFE FUER DIE SORTIERUNG EINER TEILKETTE----------------------
C
   50 IF (I.GT.J) GO TO 80
      GO TO 40
   60 J=J-1
   70 IF (X.LT.schl_nam(mm,J)) GO TO 60
      IF (I.LE.J) THEN
         do 75 nn=1,tabanz
C- Ermittlungstabelle wird sortiert
            y(nn)=int_tab(nn,mm,i)
            int_tab(nn,mm,i)=int_tab(nn,mm,j)
            int_tab(nn,mm,j)=y(nn)
   75    continue
C- Schloessertabelle wird sortiert
         W=schl_nam(mm,I)
         schl_nam(mm,I)=schl_nam(mm,J)
         schl_nam(mm,J)=W
         I=I+1
         J=J-1
      END IF
      GO TO 50
C
C-----AUSWAHL DER KUERZEREN TEILKETTE FUER WEITERE SORTIERUNG-----------
C
   80 IF (J-L.LT.R-I) THEN
         IF (I.LT.R) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (RECHTE TEILKETTE)---------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=I
            STACK(2,S)=R
         END IF
         R=J
      ELSE
         IF (L.LT.J) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (LINKE TEILKETTE)----------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=L
            STACK(2,S)=J
         END IF
         L=I
      END IF
      GO TO 20
C
   90 N1=N-1
C
C-----KONTROLLE DER EXAKTEN SORTIERUNG----------------------------------
C
      DO 100 I=1,N1
         IF (schl_nam(mm,I).GT.schl_nam(mm,I+1)) THEN
            WRITE (7,4010)
            STOP 6504
         END IF
  100 CONTINUE
      DO 101 I=1,N
  101 CONTINUE
      GOTO 500
C
C-----INITIALISIERUNG DES SORT ABSTEIGEND ----------------------------
C
  200 S=1
      STACK(1,1)=1
      STACK(2,1)=n
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN BEREICHE VON TEILKETTEN---------
C
  210 IF (S.EQ.0) GO TO 290
      L=STACK(1,S)
      R=STACK(2,S)
      S=S-1
C
C-----SCHLEIFE UEBER ALLE GESPEICHERTEN TEILKETTEN EINES BEREICHS-------
C
  220 IF (L.GE.R) GO TO 210
      I=L
      J=R
      X=schl_nam(mm,(L+R)/2)
      GO TO 250
  230 I=I+1
  240 IF (schl_nam(mm,I).GT.X) GO TO 230
      GO TO 270
C
C-----SCHLEIFE FUER DIE SORTIERUNG EINER TEILKETTE----------------------
C
  250 IF (I.GT.J) GO TO 280
      GO TO 240
  260 J=J-1
  270 IF (X.GT.schl_nam(mm,J)) GO TO 260
      IF (I.LE.J) THEN
         do 275 nn=1,tabanz
C- Ermittlungstabelle wird sortiert
            y(nn)=int_tab(nn,mm,i)
            int_tab(nn,mm,i)=int_tab(nn,mm,j)
            int_tab(nn,mm,j)=y(nn)
  275    continue
C- Schloessertabelle wird sortiert
         W=schl_nam(mm,I)
         schl_nam(mm,I)=schl_nam(mm,J)
         schl_nam(mm,J)=W
         I=I+1
         J=J-1
      END IF
      GO TO 250
C
C-----AUSWAHL DER KUERZEREN TEILKETTE FUER WEITERE SORTIERUNG-----------
C
  280 IF (J-L.LT.R-I) THEN
         IF (I.LT.R) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (RECHTE TEILKETTE)---------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=I
            STACK(2,S)=R
         END IF
         R=J
      ELSE
         IF (L.LT.J) THEN
            S=S+1
C
C-----ABFRAGE AUF INDEXUEBERSCHREITUNG (LINKE TEILKETTE)----------------
C
            IF (S.GT.N2) GO TO 110
            STACK(1,S)=L
            STACK(2,S)=J
         END IF
         L=I
      END IF
      GO TO 220
C
  290 N1=N-1
C
C-----KONTROLLE DER EXAKTEN SORTIERUNG----------------------------------
C
      DO 300 I=1,N1
         IF (schl_nam(mm,I).LT.schl_nam(mm,I+1)) THEN
            WRITE (7,4010)
            STOP 6504
         END IF
  300 CONTINUE
      DO 301 I=1,N
  301 CONTINUE
  500 continue
      GO TO 120
C
C-----ZU KLEINER SORTIERBEREICH-----------------------------------------
C
  110 WRITE (7,4020) N2,S,LSTN,N
      STOP 6505
  120 RETURN
      END

      SUBROUTINE RANDPR (NDIM, IN1, IN2, IN3, IN4, IN5, IN6, IN7)
      INTEGER IRTAB(0:127),IN(7)
      COMMON IGG(7),IMDI(7),IUU(7),IOO(7)
      COMMON /RAND/ IRTAB

      IN(1)=IN1
      IN(2)=IN2
      IN(3)=IN3
      IN(4)=IN4
      IN(5)=IN5
      IN(6)=IN6
      IN(7)=IN7
      II=0
      DO 10 I=1,NDIM
         IF (IN(I).EQ.IMDI(I)) II=II+2**(I-1)
   10 CONTINUE
      IF (II.LT.0.OR.II.GT.127) STOP 9999
      IRTAB(II)=IRTAB(II)+1
      RETURN
      END

      SUBROUTINE QURECH (WERT, RWERT, RANGE0, RANGE1, QUOTE, RANXO0,
     #RANXO1, RANXU0, RANXU1)
      LOGICAL DBQUA
      INTEGER WERT
      REAL*8 RWERT,RANGE0,RANGE1,QUOTE
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*4 RANXO0,RANXO1,RANXU0,RANXU1
      REAL*8 FMIN,MAXF
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW

      IF (WERT.EQ.0) THEN
         RWERT=0.0
         QUOTE=1.0D75
      ELSE
         RWERT=EXP(REAL(WERT)/TWERT)*MINW
         RANGE0=MIN(DBLE(RANXO0),DBLE(RANXU1))
         RANGE1=MIN(DBLE(RANXO1),DBLE(RANXU0))
         QUOTE=DBLE(RANGE0+RANGE1)/RWERT
      END IF
      RETURN
      END

      SUBROUTINE OpenFile (FileNam, ID, Stat, Formt, Acc, Icl)
*********************************************************************
* SUBROUTINE OpenFile (FileNam,ID,Stat,Formt)                       *
*                                                                   *
* This sub opens a file with a set of given parameters. If the open *
* action fails a message will be given and the programm is          *
* terminated                                                        *
*                                                                   *
* Parameter: FileNam C8 Name of file to be open                     *
*            ID      I4 Logical file ID                             *
*            Stat    C1 Open status ( O := 'OLD'; N := 'NEW'        *
*                                     S := 'SCRATCH')               *
*            Formt   C1 Format status ( F := 'FORMATTED';           *
*                                       U := 'UNFORMATTED'          *
*            Acc     C1 Access status ( D := 'DIRECT'               *
*                                     (   :=  no entry              *
*            Icl     I4 Rec Length of file                          *
*********************************************************************
      LOGICAL exs
      INTEGER IOStatus,ID
      CHARACTER*8 FileNam,DatStat,Stat*1,formt*1,Datform*12,Acc*1

C-IBM-VERSION
*     INCLUDE (OPENFILM)
C-PC-VERSION
      INCLUDE 'OPENFILM'
C
      Datform='FORMATTED'
      IF ((Formt.EQ.'U').OR.(Formt.EQ.'u')) THEN
         Datform='UNFORMATTED'
      END IF

      IF ((Stat.EQ.'O').OR.(Stat.EQ.'o')) THEN
         DatStat='OLD'
      ELSE IF ((Stat.EQ.'N').OR.(Stat.EQ.'n')) THEN
         DatStat='NEW'
      ELSE IF ((Stat.EQ.'S').OR.(Stat.EQ.'s')) THEN
         DatStat='SCRATCH'
      ELSE
         WRITE (7,4010)
         STOP 0918
      END IF

      IF (DatStat.EQ.'SCRATCH') THEN

         IF (Acc.EQ.'D') THEN
C-PC-VERSION
            OPEN (UNIT=ID,FILE=FileNam,STATUS=DatStat,IOSTAT=IOStatus,
     #            ACCESS='DIRECT',RECL=Icl,FORM=Datform)
C-IBM-Version
*           OPEN (UNIT=ID,FILE=FileNam,IOSTAT=IOStatus,FORM=Datform,
*    #            ACCESS='DIRECT',RECL=Icl)
         ELSE
C-PC-VERSION
            OPEN (UNIT=ID,FILE=FileNam,STATUS=DatStat,IOSTAT=IOStatus,
     #            RECL=Icl,FORM=Datform)
C-IBM-Version
*           OPEN (UNIT=ID,FILE=FileNam,IOSTAT=IOStatus,FORM=Datform)
         END IF
      ELSE
         OPEN (UNIT=ID,FILE=FileNam,STATUS=DatStat,IOSTAT=IOStatus,FORM=
     #    Datform)
      END IF

      IF (IOStatus.NE.0) THEN

         IF (DatStat.EQ.'OLD') THEN
            WRITE (3,4020) FileNam
         ELSE
            WRITE (3,4030) FileNam
         END IF

         WRITE (7,4040)
         STOP 0920
      END IF
      RETURN
      END

      SUBROUTINE RCOUT(IIUP,IRRUP,IDRUP,ROUP,RUUP,LAB)
C--
C UEBERNAHME DES KLASSIERTEN WERTES(IIUP), DES KLASS. GEWICHTES(IRRUP),
C DES KLASSIERTEN HOECHSTEN EINZELWERT(IDRUP), DER OBEREN(ROUP)
C UND UNTEREN(RUUP) SCHAETZFEHLERGRENZE AUS DEM DOUBLE-COMPLEX-FELD(LAB)
C !!  KLASSIERTER HOECHSTER WERT -NUR- AUS TAB-TABELLE !!
C--
      INTEGER IIUP
C
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 RREALX,RIMAGX
      REAL*4 ROUP,RUUP,XRUP
      REAL*8 FMIN,MAXF
      REAL*4 KFEHL,FWERT
      REAL*8 DRUM
      COMPLEX*16 LAB
      INTEGER KWERT
      COMMON /KWERT/KWERT
      COMMON /MWX/MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      KFEHL=100000.0
      FWERT=LOG(MAXF/FMIN)
C
      RREALX=LAB
      RIMAGX=DIMAG(LAB)
C
C--KLASSIERTER WERT IM REALTEIL VOR DEM KOMA
      IIUP=INT(RREALX)
      XRUP=REAL(INT(RIMAGX))
C
C--GEWICHTUNG IM REALTEIL ERSTEN 6 STELLEN HINTER DEM KOMA
C--HOECHSTER WERT IM REALTEIL AB 7 STELLE HINTER DEM KOMA
C
      DRUM=ABS(RREALX-IIUP)*10.0**12
      IRRUP=INT(DRUM/10.0**6)
      IDRUP=INT(((DRUM/10.0**6)-REAL(IRRUP))*10.0**6)
C
C--NULLFAELLE ODER KEINE DOMINANZ KLASSIERT GROESSER KWERT(KWERT)
C
      IF(IDRUP.GE.KWERT) IDRUP=0
C
C--OB.  SCHAETZFEHLER IM IMAGETEIL VOR DEM KOMA(MIT VORZEICHENERHALTUNG)
C
      ROUP=EXP(ABS(XRUP)/(KFEHL/FWERT))*FMIN
      ROUP=SIGN(ROUP,XRUP)
C
C--UNT. SCHAETZFEHLER IM IMAGETEIL NACH DEM KOMA
C
      RUUP=EXP(REAL(ABS(RIMAGX)-DBLE(ABS(XRUP)))*FWERT)*FMIN
C
      IIUP=INT(RREALX)
      RETURN
      END

      SUBROUTINE RCIN(IIUP,IRRUP,IDRUP,ROUP,RUUP,LAB)
C--
C UEBERNAHME IN DAS DOUBLE-COMPLEX-FELD (LAB)
C DES KLASSIERTEN WERTES(IIUP), DES KLASSIERTEN GEWICHTES(IRRUP),
C DES KLASSIERTEN HOECHSTEN EINZELWERT(IDRUP), DER OBEREN(ROUP)
C UND UNTEREN(RUUP) SCHAETZFEHLERGRENZE
C !!  KLASSIERTER HOECHSTER WERT -NUR- AUS TAB-TABELLE !!
C--
      INTEGER IIUP,IDRUP,IRRUP
C
      INTEGER KWERT
      COMMON /KWERT/KWERT
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 RREALX,RIMAGX
      REAL*4 ROUP,RUUP,HRUP,VRUP,VIUP
      REAL*4 KFEHL,FWERT
      REAL*8 FMIN,MAXF
      REAL*8 DRUM,RIRRUP,RIDRUP,HIUP
      COMPLEX*16 LAB
      COMMON /MWX/MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      KFEHL=100000.0
      FWERT=LOG(MAXF/FMIN)
C
C--KLASS. WERT IM REALTEIL VOR, KL. GEWICHT ERSTEN 6 STELLEN NACH KOMA
C--KLASS. HOECHSTER WERT AB DER 7 STELLE
C
      RIRRUP=DBLE(IRRUP)
      IF(IDRUP.EQ.0) IDRUP=KWERT+100
      RIDRUP=DBLE(IDRUP)
      DRUM=RIRRUP*10.0**6+RIDRUP
      HIUP=DRUM/10.0**12
      VIUP=REAL(ABS(IIUP))
C--VORZEICHENERHALTUNG
      RREALX=DSIGN(DBLE(VIUP)+DBLE(HIUP),DBLE(IIUP))
C
C--OB. SCHAETZFEHLER IM IMAGETEIL VOR, UNT. SCHAETZFEHLER NACH DEM KOMA
C
      VRUP=LOG(ABS(ROUP)/FMIN)*KFEHL/FWERT
C
      HRUP=REAL(LOG(RUUP/FMIN)/FWERT)
C
      RIMAGX=DSIGN(DBLE(INT(VRUP))+DBLE(HRUP),DBLE(ROUP))
      LAB=DCMPLX(RREALX,RIMAGX)
      RETURN
      END

      SUBROUTINE DOMTST(TAB,DOM)
C--
C-- BERECHNEN DER DOMINANZQUOTE ANHAND DES
C--    TAB(1) -  PIVOTWERTES
C--    TAB(2) -  HOECHSTEN EINZELWERT DES PIVOT
C--    TAB(3) -  TATSAECHLICHER AKTUELLER WERT
C--    TAB(4) -  HOECHSTER EINZELWERT DES ALKTUELLEN WERT
C--
C-- DOM : DOMINANZQUOTE
C-- TAB : TABELLE DER OBEN BEZEICHNETEN WERTE
C--
      REAL*4 DOM,TABL(4)
      INTEGER TAB(4),KWERT
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      REAL*8 FMIN,MAXF
      COMMON /KWERT/KWERT
      COMMON /MWX/MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
C-- KLASSIERTE WERTE ZURUECKRECHNEN
      DO 100 I=1,4
         IDUMY=ABS(MOD(TAB(I),KWERT))
         IF(IDUMY.GE.KWERT-1) THEN
* ?????  IF(IDUMY.EQ.KWERT-1) THEN
            TABL(I)=0.0
         ELSE
            TABL(I)=EXP(REAL(IDUMY)/TWERT)*MINW
         ENDIF
  100 CONTINUE
C- DOMINANZQUOTE*(HOE-AKTUELLWERT+HOE-PIVOTWERT)-(PIVOTWERT+AKTUELLWERT)
      DOM=(DQUOTE*(TABL(4)+TABL(2)))-(TABL(1)+TABL(3))
      RETURN
      END

      SUBROUTINE MONL(NDIM,STAQUA)
C-- -----------------------------------------------------------------
C-- AUSGABE VON VERSCHIEDENEN SPERRPARAMETERN (WERT, KL. WERT,QUADER-
C-- SUMME, DOMINANZ USW. UM DIE SPERRUNGEN IM QUADER NACHZUVOLLZIEHEN)
C-- BEDARF DER ANGABE 'TEST' IN DER STEUERDATEI IN DER LETZTEN ZEILE
C-- -----------------------------------------------------------------
C-- NDIM   = Anzahl der Dimensionen
C-- STAQUA = 0=fehlerhafte Untertabelle, 1=Ausgabe zu Testzwecken
C-- -----------------------------------------------------------------
      LOGICAL open_l
C- Neu 12.11.01
*     REAL*4 QDA(2000,5,135)
      REAL*4 QDA(50000,5,21)
C- Neu 12.11.01
      INTEGER QUA
      COMMON /QUA/QUA,QDA
      INTEGER KWERT
      INTEGER I,NDIM,ANZ,J,N,STAQUA
      REAL*8 FMIN,MAXF
      REAL*8 OMINW
      REAL*8 MINW,MAXW,TWERT,NULW,QQUOTE,XMINW,DQUOTE
      CHARACTER*3 TITEL(5)
      CHARACTER*9 TABUE(9)
      CHARACTER*30 FORMA1
      CHARACTER*26 FORMA2
      CHARACTER*29 FORMA3
      CHARACTER*47 FORMA4
      CHARACTER*43 FORMA5
      COMMON /OSAV/OMINW
      COMMON /KWERT/KWERT
      COMMON /MWX/MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
C
C-IBM-VERSION
*     INCLUDE (MONLM)
C-PC-VERSION
      INCLUDE 'MONLM'
C- nur beim ersten mal PROTO003 oeffnen
      INQUIRE(UNIT=2,OPENED=open_l)
      IF(.NOT.open_l) CALL OpenFile ('PROTO003', 2, 'N', 'F', ' ', 0)
C
      FORMA5='(A3,000F10.0,1X,F2.0,2F9.0,F9.3,1X,1PE12.2)'
      FORMA4='(A3,000(1PE10.2),1X,F2.0,2F9.0,F9.3,1X,1PE12.2)'
      FORMA3='(A3,A10,000(I1,A9),1X,A2,4A9)'
      FORMA2='(A3,000F10.0,000(1X,F9.2))'
      FORMA1='(A3,A10,000(I1,A9),000(I1,A9))'
c             123456789012345678901234567890  
      TABUE(1)=' --'
      TABUE(2)='   PIVOT '
      TABUE(3)='.QUAW    '
      TABUE(4)='.DOMIN.  '
      TABUE(5)='AB'
      TABUE(6)=' 1 GRENZE'
      TABUE(7)=' 2 GRENZE'
      TABUE(8)='   QUOTE '
      TABUE(9)='   SUMME '
      TITEL(1)=' W:'
      TITEL(2)=' K:'
      TITEL(3)=' R:'
      TITEL(4)=' I:'
      TITEL(5)=' G:'
C- AUSGABEFORMATE VORBEREITEN
      ANZ=2**NDIM-1
      IF (ANZ.LT.10) THEN
         WRITE(FORMA1(11:11),'(I1)') ANZ
         WRITE(FORMA1(22:22),'(I1)') 2**(NDIM-1)
         WRITE(FORMA2(7:7),'(I1)') ANZ+1
         WRITE(FORMA2(16:16),'(I1)') 2**(NDIM-1)
         WRITE(FORMA3(11:11),'(I1)') ANZ
         WRITE(FORMA4(7:7),'(I1)') ANZ+1
         WRITE(FORMA5(7:7),'(I1)') ANZ+1
      ELSEIF(ANZ.LT.100) THEN
             WRITE(FORMA1(10:11),'(I2)') ANZ
             WRITE(FORMA1(21:22),'(I2)') 2**(NDIM-1)
             WRITE(FORMA2(6:7),'(I2)') ANZ+1
             WRITE(FORMA2(15:16),'(I2)') 2**(NDIM-1)
             WRITE(FORMA3(10:11),'(I2)') ANZ
             WRITE(FORMA4(6:7),'(I2)') ANZ+1
             WRITE(FORMA5(6:7),'(I2)') ANZ+1
          ELSE
             WRITE(FORMA1(09:11),'(I3)') ANZ
             WRITE(FORMA1(20:22),'(I3)') 2**(NDIM-1)
             WRITE(FORMA2(5:7),'(I3)') ANZ+1
             WRITE(FORMA2(14:16),'(I3)') 2**(NDIM-1)
             WRITE(FORMA3(09:11),'(I3)') ANZ
             WRITE(FORMA4(5:7),'(I3)') ANZ+1
             WRITE(FORMA5(5:7),'(I3)') ANZ+1
      ENDIF
C- IM FEHLERFALL TABELLE AUSGEBEN MIT UEBERSCHRIFT
      IF(STAQUA.EQ.0) THEN
         WRITE(2,1000)
         WRITE(2,2000)
      ENDIF
C- QUADERAUSGABE EINZELN ODER EN BLOCK BEI FEHLER
      DO 200 M=1,QUA
C- WERTE AUF TATSAECHLICHE WERTE UMRECHNEN
      DO 50 I=1,2**NDIM
         RDUM=ABS(INT(QDA(M,2,I)/KWERT)*KWERT)
         IF(OMINW.LT.0.0) THEN
           QDA(M,1,I)=(EXP(ABS(QDA(M,2,I)+RDUM)/TWERT)*MINW)+OMINW-XMINW
         ELSE
           QDA(M,1,I)=(EXP(ABS(QDA(M,2,I)+RDUM)/TWERT)*MINW)
         ENDIF
   50 CONTINUE
C- ORIGINALWERTE UND DOMINANZEN(NUR ANGRENZENDE)
      N=0

      WRITE(2,FORMA1) TABUE(1)(1:3),
     -             TABUE(2),
     -          (I,TABUE(3),I=1,ANZ),
     -          (N,TABUE(4),N=1,2**(NDIM-1),2**N)

      N=0
      WRITE(2,FORMA2) TITEL(1),(QDA(M,1,J),J=1,ANZ+1),
     -                (QDA(M,1,N+ANZ+1),N=1,2**(NDIM-1),2**N)

   75 CONTINUE
C- KLASS.-WERTE UND ERGEBNISSE
      WRITE(2,FORMA3) TABUE(1)(1:3),
     -             TABUE(2),
     -             (N,TABUE(3),N=1,ANZ),
     -             TABUE(5)(1:2),
     -             (TABUE(N),N=6,9)

      WRITE(2,FORMA5) TITEL(2),(QDA(M,2,J),J=1,ANZ+6)

      DO 100 I=3,5
         WRITE(2,FORMA4) TITEL(I),(QDA(M,I,J),J=1,ANZ+6)

  100 CONTINUE
  200 CONTINUE
      WRITE(2,'(A1)') ' '
      DO 350 M=1,QUA
       DO 250 J=1,5
C- Neu 12.11.01
        DO 225 I=1,21
C- Neu 12.11.01
          QDA(M,J,I)=0.0
  225   CONTINUE
  250  CONTINUE
  350 CONTINUE
      RETURN
      END

      SUBROUTINE BAULAP(ndim,iiab,ss_stell,ig_dim,ss)
C  -------------------------------------------------------------------
C I                                                                   I
C I   B A U L A P                                                     I
C I                                                                   I
C I Bildung von Ermittlungstabelle in SUCHSS-Modul die                I
C I Ueberlappenden Schluessel zu erkennen und eine Erkennungstabelle  I
C I mit den Ueberlappungsschluessel aufzubauen. Dabei wird fuer       I
C I eindeutige Erkennung der Schluessel umdimensioniert(Anhand der    I
C I Dimensionsbeschreibung).                                          I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I    ndim Anzahl der Dimensionen der Tabelle                        I
C I    iiab Nummerische Bezeichnung der zu bearbeitenden Tabelle      I
C I  ig_dim Gesamtanzahl von Schloessern je Dim. je Tabelle           I
C Iss_stell Anzahl der Dimensionen aller Tabellen                     I
C I      ss Schloesser aus den Steuerdateien                          I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I schl_nam Gesamt-Schloesser jeder Dimension (CH*8)                 I
C I  int_tab Ermittlungstabelle fuer Ueberlappungen                   I
C I anz_schl Gesamt-Anzahl von Schloessern in einer Dimension         I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C  -------------------------------------------------------------------
*     IMPLICIT LOGICAL (A-Z)
C- NEU 28.04.04
*      CHARACTER*8 ss(3500,7),pseu_key(0:40,0:20)
*      CHARACTER*8 schl_nam(0:40,0:3500)
      CHARACTER*8 ss(12000,7),pseu_key(0:40,0:20)
      CHARACTER*8 schl_nam(0:40,0:12000)
C- NEU 28.04.04
      CHARACTER*8 dimbez(20,7),nam_dim(40)
	character*8 hour
C-IBM-Version
*     CHARACTER*36 time
      INTEGER*2 erk_tab(0:40,0:750000)
      INTEGER pseu_stell(0:5,0:40,0:20),anz_dim(20),stell,ak_dim
      INTEGER ss_stell,ndim,ig_dim(7),tabanz,neu_start
      INTEGER i,j,n,anz_schl(0:40),iiab,ni
C- NEU 28.04.04
*      INTEGER*2 int_tab(20,40,3500)
      INTEGER*2 int_tab(20,40,12000)
C- NEU 28.04.04
      INTEGER indx(750000)
      INTEGER*2 SO(40)
      COMMON /AUFAB/SO
      COMMON /DIMBZ/DIMBEZ
      COMMON /LAPP/ tabanz
      COMMON /PSEU/ anz_dim,pseu_stell,stell
      COMMON /SSLA/ anz_schl,nam_dim,pseu_key
      COMMON /SSNA/ schl_nam
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab
C
C-IBM-Version
*     INCLUDE (BAULAPM)
C-PC-VERSION
      INCLUDE 'BAULAPM'
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1005) iiab,Time
C-PC-VERSION
c     Elke Drumm 15.11.01
c      call time(hour)
c      write(7,1005) iiab,hour
      WRITE(7,1005) iiab,CTime(Time())
c     end changes e.drumm 15.11.01
C
C Schleife ueber die Dimensionen der Einzeltabelle
C
      ss_stell=stell
      DO 20 i=1,ndim
C
C Schleife ueber die bisher gefundenen Dimensionsbeschreibungen
C
         DO 5 j=1,stell
            IF(nam_dim(j).EQ.dimbez(iiab,i)) then
              ak_dim=j
              GOTO 6
            ENDIF
    5    CONTINUE
         stell=stell+1
         IF(stell.GT.40) THEN
             WRITE(7,1010)
             STOP 2323
         ENDIF
C
C Dimensionen aller Tabellen umstellen und vereinheitlichen
C
         nam_dim(stell)=dimbez(iiab,i)
         ss_stell=stell
         ak_dim=stell
C
C Feststellen ob Dim. aufsteigend oder absteigen
C
C--change 18.01.2001 jerskat
*        if(ss(1,ak_dim).gt.ss(ig_dim(i),ak_dim)) SO(ak_dim)=1
    6    if(ss(1,i).gt.ss(ig_dim(i),i)) SO(ak_dim)=1
C
C NOCH ueberpruefen ob pseudo-dim in freien slot geschoben wird !!!
C
         DO 7 j=1,anz_dim(iiab)
*   6    DO 7 j=1,anz_dim(iiab)
C--change 18.01.2001 jerskat
            IF(ak_dim.EQ.pseu_stell(2,j,iiab)) THEN
               WRITE(7,1015)
               STOP 2324
            ENDIF
    7    CONTINUE
         pseu_stell(1,i,iiab)=ak_dim
C
C Fuellen der Ermittlungstabelle (1=vorhanden,0=nicht vorhanden)
C
         DO 17 j=1,ig_dim(i)
          DO 15 n=1,anz_schl(ak_dim)
            if(schl_nam(ak_dim,n).eq.ss(j,i)) THEN
               int_tab(iiab,ak_dim,n)=1
C
C ist der Schluessel in der Dimension je Tabelle vorhanden
C
               GOTO 17
            ENDIF
   15     CONTINUE
C
C Fuellen der Suchschluesseltabelle mit Schloesser (fuellen)
C
          ni=n
          schl_nam(ak_dim,ni)=ss(j,i)
C Setzen der Pseudo-Schluessel-erkennung in Ermittlungstabelle
          do 16 m = 1,tabanz
           IF(pseu_key(pseu_stell(4,ak_dim,m),m).EQ.schl_nam(ak_dim,ni))
     #      THEN
               int_tab(m,ak_dim,ni)=1
           ENDIF
   16     continue
          int_tab(iiab,ak_dim,ni)=1
          IF(ni.gt.anz_schl(ak_dim)) anz_schl(ak_dim)=ni
   17    CONTINUE
   20 CONTINUE
      RETURN
      END

      SUBROUTINE SUCHSS(ss_stell)
C  -------------------------------------------------------------------
C I                                                                   I
C I   S U C H S S                                                     I
C I                                                                   I
C I Durchsuchen der Ermittlungstabellen um eine                       I
C I Erkennungstabelle mit den Ueberlappungsschluessel aufzubauen      I
C I                                                                   I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C Iss_stell Anzahl der Dimensionen aller Tabellen                     I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I erg_tab verweis auf den index fuer die schl_nam                   I
C I    indx verweis auf die tataechlichen Reihenfolge der erg_tab     I
C I         um eindeutig auf die Ueberlappung zugreifen zu koennen    I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C  -------------------------------------------------------------------
*     IMPLICIT LOGICAL (A-Z)
C- NEU 28.04.04
*      CHARACTER*8 schl_nam(0:40,0:3500)
*      CHARACTER*8 ss(3500,7),pseu_key(0:40,0:20)
      CHARACTER*8 schl_nam(0:40,0:12000)
      CHARACTER*8 ss(12000,7),pseu_key(0:40,0:20)
C- NEU 28.04.04
      CHARACTER*8 nam_dim(40)
	character*8 hour
      CHARACTER*1 b(1)/' '/
C-IBM-Version
*     CHARACTER*36 time
      integer found_anz,found,dd,ss_stell,nr
      integer zmax,mm,m,wie,imax,n,nn,nnn,neu_start
      INTEGER jj,jjj,jjjj,ll,lll,ist,i_anz,tabanz,iibis
      INTEGER find_nam(40)
      INTEGER i_dim,anz_schl(0:40)
C- NEU 28.04.04
*      INTEGER*2 int_tab(20,40,3500)
*      INTEGER*2 erk_tab(0:40,0:750000),erk2_tab(40,3500)
      INTEGER*2 int_tab(20,40,12000)
      INTEGER*2 erk_tab(0:40,0:750000),erk2_tab(40,12000)
C- NEU 28.04.04
      INTEGER indx(750000)
      COMMON /LAPP/ tabanz
      COMMON /SSLA/ anz_schl,nam_dim,pseu_key
      COMMON /SSNA/ schl_nam
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab
C
C TESTAUSGABE
*     DO 2 jjj=1,ss_stell
*      Write(14,*) anz_schl(jjj)
*      DO 1 jjjj=1,anz_schl(jjj)
*       WRITE(14,*) schl_nam(jjj,jjjj),(int_tab(l,jjj,jjjj),l=1,tabanz)
*   1  CONTINUE
*   2 CONTINUE
*           CLOSE (UNIT=14)
C TESTAUSGABE
C-IBM-Version
*     INCLUDE (SUCHSSM)
C-PC-VERSION
      INCLUDE 'SUCHSSM'
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1005) Time
C-PC-VERSION
c     Elke Drumm
c      call time(hour)
c      write(7,1005) hour
      WRITE(7,1005) CTime(Time())
c     end changes e.drumm 15.11.01
C  Suchen nach Gesamtueberlappungen
C
      DO 20 jj=1,40
       find_nam(jj)=0
   20 CONTINUE
      dd=0
C- nur 1.Dimensionen durchgehen
C- Suchen nach zu pruefenden Paaerchen ueber alle Tabelle
         DO 879 ll=1,tabanz-1
            DO 878 lll=ll+1,tabanz
C- alle schluessel durchgehen
               nr=0
               imax=1
C- Suchen nach uebereinstimmenden Paaerchen anderer Dimensionen
               DO 177 jjj=1,ss_stell
                     found_anz=0
                     DO 176 jjjj=1,anz_schl(jjj)
C- nur Kombination 1-1 zulassen (paaerchen) mit Stellung von oben
                        ist=int_tab(ll,jjj,jjjj)+
     #                         int_tab(lll,jjj,jjjj)-2
                        IF(ist) 176,172,176
  172                   CONTINUE
                        found_anz=found_anz+1
                        find_nam(jjj)=find_nam(jjj)+1
                        erk2_tab(jjj,find_nam(jjj))=jjjj
  176                CONTINUE
C- kein paaerchen gefunden
                     IF(found_anz.EQ.0) goto 735
C wieviel schluessel in allen Dim. wurden gefunden(moegl. Kombinationen)
                     imax=find_nam(jjj)*imax
  177             CONTINUE
C-- Suchtabelle erstellen -Anfang- Spaltenweiser Aufbau
                     zmax=1
C-- Maximale groesse der Suchtabelle
                     IF(neu_start+1+imax.gt.750000) THEN
                         WRITE(7,2000)
                         STOP 2400
                     ENDIF
                     DO 333 nn=1,ss_stell
                        IF(find_nam(nn).EQ.1) THEN
                           wie=1
                           iibis=imax
                        ELSE
                           wie=zmax
                           iibis=imax/(find_nam(nn)*zmax)
                        ENDIF
                        mm=neu_start
                        DO 330 nnn=1,iibis
                           DO 320 m=1,find_nam(nn)
                             DO 310 n=1,wie
                                mm=mm+1
                                erk_tab(nn,mm)=erk2_tab(nn,m)
                                indx(mm)=mm
  310                        CONTINUE
  320                      CONTINUE
  330                   CONTINUE
                        zmax=find_nam(nn)*zmax
  333                CONTINUE
C
C  Aufbau einer kompletten Erkennungstabelle-Zeilenweise Ueberpruefung
C
                     IF(neu_start.EQ.0) THEN
                        neu_start=imax
                        dd=imax
                        GOTO 735
                     ENDIF
                     DO 733 m=neu_start+1,neu_start+imax
                        DO 731 mm=1,neu_start
                           DO 730 nn=1,ss_stell
                              wie=erk_tab(nn,mm)-erk_tab(nn,m)
                              IF(wie) 731,730,731
  730                      CONTINUE
                           GOTO 733
  731                   CONTINUE
                        dd=dd+1
                        indx(dd)=dd
                        DO 732 nn=1,ss_stell
                           erk_tab(nn,dd)=erk_tab(nn,m)
  732                   CONTINUE
  733                CONTINUE
                     neu_start=dd
C-- Suchtabelle erstellen -Ende-
  735                DO 736 nn=1,ss_stell
                        find_nam(nn)=0
  736                CONTINUE
                     imax=1
  878       CONTINUE
  879    CONTINUE
      DO 999 dd=1,neu_start
C-- Ausgabe der gefundenen Ueberlappungsschluessel (wird zuviel)
*        WRITE(7,1007) indx(dd),
*    #            (schl_nam(nn,erk_tab(nn,dd)),b(1),nn=1,ss_stell)
  999 CONTINUE
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1006) Time
C-PC-VERSION

c     Elke Drumm
c      call time(hour)
c       write(7,1006) hour
      WRITE(7,1006) CTime(Time())
c     end changes e.drumm
      RETURN
      END

c*****changes giessing 02.01 a
c      SUBROUTINE INIT(jj,imxdim)
      SUBROUTINE INIT(jj,imxdim,mode)
c*****changes giessing 02.01 e
C  -------------------------------------------------------------------
C I                                                                   I
C I   I N I T                                                         I
C I                                                                   I
C I   UEBERNAHME DER STEUERUNG FUER DIE UEBERLAPPENDEN TABELLEN       I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I      jj    Angegebener Speicherbereich                            I
C I  imxdim    maximale Anzahl von Dimensionen von allen Tabellen     I
C I pseu_stell Stellung des Pseudo-Schloss/normale Schloesser         I
C I            (Festlegung der Stellung der Dimensionen)              I
C I pseu_key   Pseudo-Schloss der Tabelle und Dimension               I
C I  tabanz    Anzahl der zu bearbeitenden Tabellen                   I
C I tab_nam    Name der Eingabedateien zu jeder Tabelle               I
C I tab_out    Name der Ausgabedateien zu jeder Tabelle               I
C I tab_ft04   Name der Steuerdateien zu jeder Tabelle                I
C I nam_dim    Bezeichnung der Dimension                              I
C I anz_dim    Anzahl von Pseudo-Dimensionen                          I
C I                                                                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C  -------------------------------------------------------------------
      LOGICAL DBQUA
      CHARACTER*8 tab_nam(20),tab_ft04(20),pseu_key(0:40,0:20)
	character*8 tab_out(20)
      CHARACTER*8 pseu_dim,nam_dim(40)
      REAL*8 MINW,MAXW,NULW,QQUOTE,DQUOTE,OMINW
      REAL*8 FMIN,MAXF,XMINW,TWERT
      INTEGER n,i,j,k,l,stell,jj,imxdim,absbet,gewran,schaez
      INTEGER tabanz,anz_dim(20),pseu_stell(0:5,0:40,0:20)
      INTEGER anz_schl(0:40),neu_start
      INTEGER indx(750000)
C- NEU 28.04.04
*      INTEGER*2 int_tab(20,40,3500)
      INTEGER*2 int_tab(20,40,12000)
C- NEU 28.04.04
      INTEGER*2 erk_tab(0:40,0:750000)
c*****changes giessing 02.01 a
      integer mode,print1,exact
c*****changes giessing 08.01 e

C
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMMON /OSAV/OMINW
      COMMON /ABSBET/ ABSBET,GEWRAN,SCHAEZ,DBQUA
      COMMON /DAT/ tab_ft04,tab_nam,tab_out
      COMMON /LAPP/ tabanz
      COMMON /PSEU/ anz_dim,pseu_stell,stell
      COMMON /SSLA/ anz_schl,nam_dim,pseu_key
      COMMON /SUCH/ neu_start,indx,int_tab,erk_tab
c*****changes giessing 02.01 a
      common/print1/print1
c*****changes giessing 08.01 e
c----changes giessing 09/02-a
      COMMON /EXACT/EXACT
c----changes giessing 09/02-e


C
C-IBM-VERSION
*     INCLUDE (INITM)
C-PC-VERSION
      INCLUDE 'INITM'
C

C INIT TABELLEN
      DO 993 i = 1,40
       DO 992 l = 1,20
C- NEU 28.04.04
*        DO 991 n = 1,3500
        DO 991 n = 1,12000
C- NEU 28.04.04
          int_tab(l,i,n)=0
  991   CONTINUE
  992  CONTINUE
  993 CONTINUE
      DO 994 l = 0,40
          anz_schl(l)=0
  994 CONTINUE
      DO 1 i = 1,40
        DO 2 j = 1,20
          DO 3 n = 1,5
            pseu_stell(n,i,j)=0
    3     CONTINUE
          pseu_key(i,j)='        '
          anz_dim(j)=0
          tab_nam(j)= '        '
          tab_out(j)= '        '
          tab_ft04(j)='        '
    2   CONTINUE
        nam_dim(i)=  '        '
    1 CONTINUE
      NULW=0.0
      ABSBET=0
      MINW=1.0
      MAXW=10.0**15
      tabanz=1
C--Speicheranforderung
      READ(9,*) jj
      WRITE(3,1040) jj

c*****changes giessing 02.01 a
C-------Einlesemodus einlesen: mode=1 => Einlesen ueber poolac, sequences und keys
c-------                mode=2 oder 4 => Lauf nach Poolac, Aufruf von weights
      READ (9,'(i1,1x,i1,1x,i1)') mode,print1,exact
      write(3,*) ' mode,print1,exact',mode,print1,exact
c*****changes giessing 02.01 e

C--ANFANGSWERT, SKALIERUNG
      READ (9,*) MINW,MAXW
      IF(MINW.EQ.0.0.OR.MINW.GE.MAXW) STOP 3000
      WRITE (3,4010) MINW,MAXW
      OMINW=MINW
C--Sperrbare Nullwerte
      READ (9,*) NULW
      IF (NULW.EQ.0.0) THEN
C--NULLWERT AUF MAXIMALEN ZAHLENWERT SETZEN
         NULW=REAL(7.2*(10.0**37))
*        NULW=REAL(7.2*(10.0**75))
         WRITE (3,4120)
C
      ELSE IF (NULW.LE.MAXW) THEN
         WRITE (3,4140) NULW
         WRITE (7,4130)
         STOP 3001
      ELSE
         WRITE (3,4140) NULW
      END IF
C
C--Pos. / Negative Tabelle
      READ (9,*) ABSBET
C
C- TABELLENBEARBEITUNGSFORM UEBERPRUEFEN
C- (NEGATIVE TABELLE, NUR ALS ABSOLUTER BETRAG BEARBEITEN
C- DADURCH WIRD SIE BEHANDELT WIE EINE POSITIVE TABELLE)
C
      IF(ABSBET.GT.3.OR.ABSBET.LT.0) ABSBET=0
      IF(ABSBET.EQ.0) THEN
         IF(MINW.LE.0.OR.MAXW.LE.0.OR.MAXW.LE.MINW) THEN
            WRITE(7,4051)
            STOP 3101
         ENDIF
      ENDIF
      IF(ABSBET.EQ.1) THEN
         IF(MINW.GE.0.0.OR.MAXW.LT.0.0.OR.MAXW.LE.MINW) THEN
            WRITE(7,4052)
            STOP 3102
         ENDIF
      ENDIF
      IF(ABSBET.EQ.2) THEN
         IF(MINW.LE.0.0.OR.MAXW.LT.0.0.OR.MAXW.LE.MINW) THEN
            WRITE(7,4053)
            STOP 3103
         ENDIF
      ENDIF
      IF(ABSBET.EQ.3) THEN
         IF(MINW.GE.0.0.OR.MAXW.GE.0.0.OR.MAXW.LT.MINW) THEN
            WRITE(7,4054)
            STOP 3104
         ENDIF
         MAXW=ABS(MINW)
         MINW=ABS(MAXW)
      ENDIF
C  Maximale Anzahl von Dimensionen in einer Tabelle
      READ(9,*) imxdim
      IF ((imxdim.lt.1).OR.(imxdim.GT.7)) THEN
C  Abbruch bei falscher Dimensionsangabe
         WRITE (7,1050) imxdim
C        STOP 1003
      ENDIF
      WRITE(3,1060) imxdim
C
C  Anzahl der zu bearbeitenden Tabellen
      READ(9,*,END=100) tabanz
      IF (tabanz.GT.20) THEN
         WRITE(7,1010)
         STOP  2322
      ENDIF
C
      WRITE(3,1020) tabanz
C  Dateinamen der einzelnen Tabellen
      DO 10 i = 1,tabanz
         READ(9,*,end=15) tab_nam(i)
         READ(9,*,end=15) tab_ft04(i)
         READ(9,*,end=15) tab_out(i)
         WRITE(3,1030) i,tab_nam(i),tab_ft04(i),tab_out(i)
   10 CONTINUE
C Einzeltabelle keine weitere Bearbeitung
   15 IF (tabanz.LT.2) GOTO 100
      stell=0
C  Informationen der einzelnen ueberlappenden Tabellen einholen
      DO 30 i = 1,tabanz
C  Welche Tabelle, wieviel Pseudo-Dimensionen
         READ(9,*,END=100) anz_dim(i)
         DO 20 j = 1,anz_dim(i)
C  Bezeichnung Pseudo-Dimensionen
            READ(9,*) pseu_dim
            READ(9,'(A8)') pseu_key(j,i)
C  Stellungen fuer jede bezeichnete Dimension festlegen
            DO 19 k = 1,stell+1
               IF(nam_dim(k).EQ.pseu_dim) THEN
                  pseu_stell(2,j,i)=k
                  pseu_stell(4,k,i)=j
                  WRITE(3,1070) i,pseu_stell(2,j,i),pseu_key(j,i),
     #                          pseu_stell(2,j,i)
                  GOTO 20
C  Fuellen der Suchtabelle da Suchschluessel schon in Dim. vorhanden
               ENDIF
   19       CONTINUE
            stell=stell+1
            IF(stell.GT.40) THEN
               WRITE(7,1080)
               STOP 2323
            ENDIF
C  Fuellen der Suchschluesseltabelle in leere Dim.
            nam_dim(stell)=pseu_dim
            pseu_stell(2,j,i)=stell
            pseu_stell(4,stell,i)=j
            WRITE(3,1070) i,j,pseu_key(j,i),pseu_stell(2,j,i)
   20    CONTINUE
   30 CONTINUE
  100 RETURN
      END

C-NEU
      SUBROUTINE INFO(IW,MUS,IMS,NDIM,MS,imxdim,ig_dim,iiab,ss)
C-NEU
C  -------------------------------------------------------------------
C I                                                                   I
C I   I N F O                                                         I
C I                                                                   I
C I   UEBERNAHME DER STEUERDATEI UND CHECK                            I
C  -------------------------------------------------------------------
C I                                                                   I
C I   PARAMETER:                                                      I
C I   EINGABE                                                         I
C I                                                                   I
C I   iiab     aktuelle Tabellennummer                                I
C I   imxdim   maximale Anzahl von Dimensionen                        I
C I   IMS      maximal moegliche Aggregationsstufen je Dimension      I
C I   IW       maximal moegliche Anzahl von Schloessern je Dimension  I
C I                                                                   I
C I   AUSGABE                                                         I
C I                                                                   I
C I   ig_dim   Gesamtanzahl von Schloessern je Dim. je Tabelle        I
C I   MUS      Anzahl der Schloesser der Dimension der akt. Tab.      I
C I   NDIM     Anzahl der Dimensionen der aktuellen Tabelle           I
C I   MS       ANZAHL DER AGGREGATIONSSTUFEN JE DIMENSION  akt. Tab.  I
C I   ss       Schloesser der aktuellen Tabelle                       I
C I                                                                   I
C I                                                                   I
C I                                                                   I
C I   UNTERPROGRAMME:                                                 I
C I   KEINE                                                           I
C I                                                                   I
C  -------------------------------------------------------------------
      LOGICAL DBQUA
      CHARACTER*4 TEST
C- NEU 28.04.04
*      CHARACTER*8 SS(3500,7)
      CHARACTER*8 SS(12000,7)
C- NEU 28.04.04
      CHARACTER*8 tab_nam(20),tab_ft04(20),tab_out(20)
      CHARACTER*72 EING,DIMBEZ(20,7)*8
      CHARACTER FORM1*72
	character*8 hour
C-IBM-Version
*     CHARACTER*36 time
      INTEGER IMS,IL,MUS(IMS,imxdim)
      INTEGER MS(7),RFLAG(7),NDIM,IG,IG1,IW,J,I,JJJ,INZ1,INZ0,INZ,LINZ
      INTEGER ABSBET,GEWRAN,SCHAEZ,IDBQUA
      INTEGER ig_dim(7),iiab
c*****changes giessing 08.01 a
      integer print1
c*****changes giessing 02.01 e

      REAL*8 POT(7),MINW,MAXW,NULW,QQUOTE,DQUOTE
      REAL*8 FMIN,MAXF
      REAL*8 GEWMAX,GEWMIN,XMINW,TWERT
      COMMON /DIMBZ/DIMBEZ
      COMMON /RFLAG/ RFLAG
      COMMON /MWX/ MINW,MAXW,TWERT,NULW,QQUOTE,FMIN,MAXF,DQUOTE,XMINW
      COMMON /ABSBET/ ABSBET,GEWRAN,SCHAEZ,DBQUA
      COMMON /INSTA/POT,GEWMAX,GEWMIN
      COMMON /TEST/TEST
      COMMON /DAT/ tab_ft04,tab_nam,tab_out
c*****changes giessing 08.01 a
      common /print1/print1
c*****changes giessing 08.01 e
C-IBM-VERSION
*     INCLUDE (INFOM)
C-PC-VERSION
      INCLUDE 'INFOM'
C
      IF(IIAB.EQ.0) IIAB=1
C-IBM-Version
*     CALL CTime(time)
*     WRITE(7,1005) tab_nam(iiab),Time
C-PC-VERSION
c     Elke Drumm
c       call time(hour)
c       write(7,1005) tab_nam(iiab),hour
      WRITE(7,1005) tab_nam(iiab),CTime(Time())
c     end changes e.drumm
C
C-----ANFANGSWERTE , SKALIERUNG-----------------------------------------
C
      SCHAEZ=0
      IDBQUA=0
      DQUOTE=0.0
      QQUOTE=0.0
      GEWRAN=0
      TEST='    '
      DO 15 I=1,imxdim
         POT(I)=0.0
         RFLAG(I)=1
   15 CONTINUE
C---
C1--UEBERSCHRIFT GESAMTLAUF HOLEN UND AUSGEBEN
C---
      READ (4,*) EING
      WRITE (3,4020) EING
*     WRITE (7,'(A72)') EING
C
C2----FORMATE DER SACHSCHLUESSEL----------------------------------------
C
      READ (4,*) FORM1
 4015 FORMAT (A72)
      WRITE (3,4025) FORM1
 4025 FORMAT (1X,A72)
C
C----ANZAHL DER DIMENSIONEN EINER TABELLE
C
      READ (4,*) NDIM
      IF(NDIM.gt.imxdim) THEN
         WRITE (7,4011) eing,NDIM,imxdim
         STOP 1004
      ENDIF
C
C-----ANZAHL DER AGGREGATIONSSTUFEN JE DIMENSION---
C
      READ (4,*) (MS(I),I=1,NDIM)
C
C-----RANDSCHRANKEN FUER JEDE DIMENSION---
C
      READ (4,*) (RFLAG(I),I=1,NDIM)
C
C-----INSTANTANE GEWICHTUNG FUER JEDE DIMENSION ---
C
      READ (4,*) (POT(I),I=1,NDIM)
C
C-----SCHLEIFE UEBER DIE DIMENSIONEN ---
C
      DO 10 I=1,NDIM
         IF (RFLAG(I).GT.1.OR.RFLAG(I).LT.(-1)) THEN
            WRITE (7,4012) I, RFLAG(I)
            STOP 2001
         ENDIF
         IG=MS(I)
         IF (IG.GT.IMS) THEN
            WRITE (7,4040) IG,IMS
            STOP 2002
         END IF
C
C-----KURZUEBERSCHRIFT JE DIMENSION-----
C
         READ (4,*) DIMBEZ(iiab,I)
C
C-----ANZAHL DER AUSPRAEGUNG JE AGGREGATIONSSTUFE--
C
         READ (4,*) (MUS(J,I),J=1,IG)
         IG1=0
         DO 20 J=1,IG
   20       IG1=IG1+MUS(J,I)
         IF (IG1.GT.IW) THEN
            WRITE (7,4050) IG1,IW
            STOP 2003
         END IF
C
C-----PROTOKOLLIERUNG DER GELESENEN WERTE-------------------------------
C
         WRITE (3,4060) I,DIMBEZ(iiab,I),POT(I),MS(I),(MUS(J,I),J=1,IG)
C
C-----EINGABE UND PROTOKOLLIERUNG DER SACHSCHLUESSEL-------------------
C
         READ (4,FORM1) (SS(JJJ,I),JJJ=1,IG1)
         ig_dim(i)=IG1
         INZ1=MS(I)
         WRITE (3,4070)
         INZ=0
         DO 25 LINZ=1,INZ1
            INZ0=INZ+1
            INZ=INZ+MUS(LINZ,I)
            WRITE (3,4080) LINZ,(SS(JJJ,I),JJJ=INZ0,INZ)
 4080 FORMAT (//5X,I2,7X,10(1X,A8)/(14X,10(1X,A8)))
   25    CONTINUE
   10 CONTINUE
C
C----RANGE(SCHAETZWERTABW.)/DOMINANZQUOTE
C
      READ (4,*,END=30) QQUOTE,DQUOTE
C
C----GEWICHTUNG - SCHAETZWERTFEHLER - DOPPELQUADER
C
      READ (4,*,END=30) GEWRAN,SCHAEZ,IDBQUA
C
C----MONITORING FUER TABELLE (SINNVOL BIS 3 DIMENSIONAL)
C
      READ (4,*,END=30) TEST
c      write(0,*) 'infotest',test
C


   30 CONTINUE
      IF(IDBQUA.EQ.1) THEN
         WRITE (3,4014)
         DBQUA=.FALSE.
      ELSE
         WRITE (3,4013)
         DBQUA=.TRUE.
      ENDIF
C
      WRITE (3,4150) QQUOTE
C
      IF (GEWRAN.EQ.1) THEN
         WRITE (3,4062)
      ELSE
c*****changes giessing 02.01 a
c*****changes giessing 10.02 a
c-----GEWICHTUNG VOM TYP GEWRAN=2 BZW 3 IST RANDSCHRANKENKONFORM;
C-----DAHER AUSSCHALTEN DER RANDSCHRANKE NICHT ERFORDERLICH
c         GEWRAN=0
         if((gewran.ne.2).and.(gewran.ne.3)) GEWRAN=0
c*****changes giessing 10.02 e
c*****changes giessing 02.01 e
      ENDIF
C
      WRITE (3,4063) DQUOTE
C---
C--- WENN DOMINANZ, DANN MUSS EINE QUADERQUOTE UEBER 1.0
C---
      IF (DQUOTE.GT.0.0.AND.QQUOTE.LE.1.0) THEN
         WRITE (7,4064)
         STOP 3002
      ENDIF
C
      IF (SCHAEZ.EQ.1) THEN
         WRITE (3,4160)
C- Kein Schaetzwertfehler = keine Quote bei positiv/negativ Tabellen
c*****changes giessing 02.01 a
C-----VERARBEITUNG SCHAEZ 2 HIER WIE SCHAEZ 1
c      ELSE
      ELSE if (schaez.gt.3) then
c*****changes giessing 02.01 e
         SCHAEZ=0
         IF (ABSBET.EQ.1.OR.ABSBET.EQ.2) THEN
            IF (DQUOTE.EQ.0.0.AND.QQUOTE.GT.0.0) THEN
               QQUOTE=0.0
               WRITE(3,4066)
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END
C
C-IBM-Version
*     SUBROUTINE CTime(time)
*     CHARACTER*36 time
*     INTEGER*4 d(8)
*     CALL DATIM(d(1),d(2),d(3),d(4),d(5),d(6),d(7),d(8))
*     WRITE(time,'(I2,A1,I2,A1,I4,A2,3(I2,A1),I3,A1)')
*    #                  d(6),'.',d(7),'.',d(8),'  ',
*    #                  d(5),':',d(4),':',d(3),':',
*    #                  d(2),'h'
*     RETURN
*     END

