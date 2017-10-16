      INTEGER UTMZ                                                      00010000
      REAL LAT,LONG                                                     00020000
      WRITE(6,600)                                                      00030000
      WRITE(6,610)                                                      00040000
   5  READ(5,500)IC                                                     00050000
      IF(IC.EQ.1 .OR. IC.EQ.2) GO TO 10                                 00060000
      WRITE(6,620)IC                                                    00070000
      GO TO 5                                                           00080000
   10 IF(IC.EQ.1)  THEN                                                 00090000
C*** CONVERSION FROM LAT LON TO UTM                                     00100000
        WRITE(6,630)                                                    00110000
   20   READ(5,*,END=999) LTD,LTM,LTS,LGD,LGM,LGS                       00120000
C  SUBROUTINE REQUIRES DECIMAL LAT LON                                  00130000
C  CONVERT D M S TO DEC LAT LON BEFORE SUB CALL                         00140000
        LAT =FLOAT(LTD)+(FLOAT(LTM)/60.)+(FLOAT(LTS)/3600.)             00150000
        LONG=FLOAT(LGD)+(FLOAT(LGM)/60.)+(FLOAT(LGS)/3600.)             00160000
        CALL UTMFL(UTMX,UTMY,UTMZ,LAT,LONG)                             00170000
        WRITE(6,680)UTMX,UTMY,UTMZ                                      00180000
          GO TO 20                                                      00190000
       ELSE                                                             00200000
C*** CONVERSION FROM UTM TO LAT LON                                     00210000
        WRITE(6,640)                                                    00220000
   30   READ(5,*,END=999)UTMX,UTMY,UTMZ                                 00230000
        CALL UTMFU(UTMX,UTMY,UTMZ,LAT,LONG)                             00240000
C  SUBROUTINE RETURNS DECIMAL LAT LON                                   00250000
C  CONVERT DECIMAL LAT LON TO D M S AFTER SUB CALL                      00260000
        LTD=LAT                                                         00270000
        RLTM=(LAT-FLOAT(LTD))*60.                                       00280000
        LTM=(LAT-FLOAT(LTD))*60.                                        00290000
        LTS =(RLTM-FLOAT(LTM))*60.                                      00300000
        LGD=LONG                                                        00310000
        RLGM=(LONG-FLOAT(LGD))*60.                                      00320000
        LGM =(LONG-FLOAT(LGD))*60.                                      00330000
        LGS =(RLGM-FLOAT(LGM))*60.                                      00340000
        WRITE(6,690)LTD,LTM,LTS,LGD,LGM,LGS                             00350000
          GO TO 30                                                      00360000
      ENDIF                                                             00370000
  500 FORMAT(I1)                                                        00380000
  600 FORMAT(15X,'***FOR OPT 1 - LL TO UTM CONVERSION ENTER "1"')       00390000
  610 FORMAT(15X,'***FOR OPT 2 - UTM TO LL CONVERSION ENTER "2"')       00400000
  620 FORMAT(10X,'***CONVERSION CODE ',I1,' IS INVALID, TRY AGAIN')     00410000
  630 FORMAT(10X,'***CONVERSION FROM LL TO UTM HAS BEEN SELECTED'/,     00420000
     *'      ***ENTER LL IN DEGS MINS AND SECS SEPARATED BY SPACES')    00430000
  640 FORMAT(10X,'***CONVERSION FROM UTM TO LL HAS BEEN SELECTED'/,     00440000
     *'      ***ENTER EASTING NORTHING AND ZONE SEPARATED BY SPACES')   00450000
  680 FORMAT(24X,'EASTING=',F7.3,'  NORTHING=',F8.3,'  ZONE=',I2)       00460000
  690 FORMAT(24X,'LAT(D M S)=',3(I2,1X),'  LON(D M S)=',I3,1X,2(I2,1X)) 00470000
  999 STOP                                                              00480000
      END                                                               00490000
      SUBROUTINE UTMF (ZONE)                                            00500000
C                                                                       00510000
C                                                                       00520000
C     THIS SUBROUTINE CONVERTS COORDINATES FROM UTM TO LAT-LONG AND     00530000
C          FROM LAT-LONG TO UTM.                                        00540000
C                                                                       00550000
C                                                                       00560000
C     A CALL TO THE SUBROUTINE BY NAME CONTROLS THE UTM ZONE IN WHICH   00570000
C          THE OUTPUT UTM COORDINATES ARE PLACED.  THE VARIABLE ZONE    00580000
C          STARTS WITH A VALUE OF ZERO AND CAN BE CHANGED TO THE VALUE  00590000
C          PASSED BY CALLING THE SUBROUTINE BY ITS NAME.  IF ZONE HAS A 00600000
C          VALUE OF ZERO, THEN THE UTM ZONE IS COMPUTED FROM THE        00610000
C          LONGITUDE.  IF ZONE IS NOT EQUAL TO ZERO ALL UTM COORDINATES 00620000
C          ARE FORCED TO THAT ZONE.                                     00630000
C                                                                       00640000
C     USE:                                                              00650000
C                                                                       00660000
C          IT IS NECESSARY FOR PLOTING TO HAVE ALL COORDINATES IN THE SA00670000
C          UTM ZONE.  PICK A ZONE (CENTROID?) AND THEN FORCE ALL COORDIN00680000
C          TO THAT ZONE.                                                00690000
C                                                                       00700000
C                                                                       00710000
C     ENTRY UTMFL (UTMX,UTMY,UTMZ,LAT,LONG)                             00720000
C     ENTRY UTMFU (UTMX,UTMY,UTMZ,LAT,LONG)                             00730000
C                                                                       00740000
C                                                                       00750000
C     ENTRY UTMFL CONVERTS FROM LATITUDE - LONGITUDE                    00760000
C                                                                       00770000
C     ENTRY UTMFU CONVERTS FROM UTM                                     00780000
C                                                                       00790000
C                                                                       00800000
C                                                                       00810000
      REAL DEGRAD                                                       00820000
C                                                                       00830000
C     DEGRAD IS THE CONVERSION FACTOR FROM DEGREES TO RADIANS.          00840000
C                                                                       00850000
C                                                                       00860000
      REAL DLONG,DLONGP,DLONGX                                          00870000
C                                                                       00880000
C     WORK VARIABLES                                                    00890000
C                                                                       00900000
C                                                                       00910000
      REAL LAT                                                          00920000
C                                                                       00930000
C     CONTAINS THE LATITUDE IN DECIMAL DEGREES, INPUT OR OUTPUT.        00940000
C                                                                       00950000
C                                                                       00960000
      REAL LATX                                                         00970000
C                                                                       00980000
C     WORK VARIABLE                                                     00990000
C                                                                       01000000
C                                                                       01010000
      REAL LONG                                                         01020000
C                                                                       01030000
C     CONTAINS THE LONGITUDE IN DECIMAL DEGREES, INPUT OR OUTPUT.       01040000
C                                                                       01050000
C                                                                       01060000
      REAL UTMX, UTMY                                                   01070000
C                                                                       01080000
C     THE UTM COORDINATES IN DECIMAL, INPUT OR OUTPUT.                  01090000
C                                                                       01100000
C                                                                       01110000
      REAL UTMYM                                                        01120000
C                                                                       01130000
C     WORK VARIABLE.                                                    01140000
C                                                                       01150000
C                                                                       01160000
      INTEGER UTMZ                                                      01170000
C                                                                       01180000
C     THE UTM ZONE, INPUT OR OUTPUT.                                    01190000
C                                                                       01200000
C                                                                       01210000
      INTEGER ZONE                                                      01220000
C                                                                       01230000
C     CONTROL VARIABLE, USED AS DESCRIBED ABOVE.                        01240000
C                                                                       01250000
C                                                                       01260000
      INTEGER ZONEH                                                     01270000
C                                                                       01280000
C     CONTAINS THE CURRENT VALUE OF THE CONTROL VARIABLE ZONE.          01290000
C                                                                       01300000
C                                                                       01310000
C     SAVE THE VALUE OF ZONE.                                           01320000
      DATA DEGRAD/0.017453/                                             01330000
      DATA ZONEH/0/                                                     01340000
      ZONEH = ZONE                                                      01350000
      RETURN                                                            01360000
C                                                                       01370000
C     END OF CODE FROM SUBROUTINE CALL BY NAME.                         01380000
C                                                                       01390000
C                                                                       01400000
C                                                                       01410000
      ENTRY UTMFL (UTMX,UTMY,UTMZ,LAT,LONG)                             01420000
C                                                                       01430000
C     THIS ENTRY POINT CONVERTS FROM LATITUDE - LONGITUDE.              01440000
C                                                                       01450000
C                                                                       01460000
      UTMYM = 2.41 + 110.268 * LAT + 0.00903 * LAT * LAT                01470000
C**   UTMZ = IABS ((180. - LONG) / 6.) + 1  **(IBM DID NOT LIKE THIS)** 01480000
      RUTMZ = ABS ((180. - LONG) / 6.)                                  01490000
      UTMZ = INT(RUTMZ) + 1                                             01500000
      IF (ZONEH .NE. 0) UTMZ = ZONEH                                    01510000
      DLONG = 180. - 6. * UTMZ + 3. - LONG                              01520000
      UTMY = 3187 * SIN (DEGRAD * 2.0 * LAT) *                          01530000
     $ (1.0 - COS (DEGRAD * DLONG)) + UTMYM                             01540000
      UTMX = (111.226 + 0.0053 * LAT) * COS (DEGRAD * LAT) * DLONG +    01550000
     $ 500.                                                             01560000
      RETURN                                                            01570000
C                                                                       01580000
C     END OF CODE FOR CONVERTING FROM LATITUDE - LONGITUDE.             01590000
C                                                                       01600000
C                                                                       01610000
C                                                                       01620000
      ENTRY UTMFU (UTMX,UTMY,UTMZ,LAT,LONG)                             01630000
C                                                                       01640000
C                                                                       01650000
C     THIS ENTRY POINT CONVERTS FROM UTM COORDINATES.                   01660000
C                                                                       01670000
C                                                                       01680000
      LATX = UTMY / 111.                                                01690000
      DLONGX = (UTMX - 500.) / ((111.226 + 0.0053 * LATX) *             01700000
     C (COS (DEGRAD * LATX)))                                           01710000
      UTMYM = UTMY - (3187. * SIN (2. * DEGRAD * LATX) *                01720000
     C (1. -COS (DEGRAD * DLONGX)))                                     01730000
      LAT = (UTMYM - 2.41 - 0.00903 * LATX * LATX) / 110.270            01740000
      DLONGP = (UTMX - 500.) / ((111.226 + 0.0053 * LAT) *              01750000
     C  (COS (DEGRAD * LAT)))                                           01760000
      LONG = 180 - (6 * UTMZ) + 3 - DLONGP                              01770000
C                                                                       01780000
      RETURN                                                            01790000
C                                                                       01800000
C                                                                       01810000
C     END OF CODE FOR CONVERTING FROM UTM COORDINATES.                  01820000
C                                                                       01830000
C                                                                       01840000
C                                                                       01850000
      END                                                               01860000
