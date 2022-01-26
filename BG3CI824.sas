data salida3;
set;

      *---------------*                                                 
       LEE-PARAMETROS.                                                  
      *---------------*                                                 
           IF LK-FECHA IS NUMERIC AND LK-FECHA NOT = ZEROS              
              CONTINUE                                                  
           ELSE                                                         
              ACCEPT LK-FECHA FROM DATE                                 
           END-IF.                                                      
           MOVE LK-DIA        TO WS-FECPROC(1:2).                       
           MOVE LK-MES        TO WS-FECPROC(4:2).                       
           MOVE LK-ANO        TO WS-FECPROC(9:2).                       
           MOVE '20'          TO WS-FECPROC(7:2).                       
           MOVE '-'           TO WS-FECPROC(3:1) WS-FECPROC(6:1).       
           ACCEPT WS-HORA FROM TIME.                                    
           MOVE WS-HORA (1:2) TO WS-HORA-EDIT (1:2).                    
           MOVE ':'           TO WS-HORA-EDIT (3:1) WS-HORA-EDIT (6:1). 
           MOVE WS-HORA (3:2) TO WS-HORA-EDIT (4:2).                   
           MOVE WS-HORA (5:2) TO WS-HORA-EDIT (7:2).                    
           MOVE WS-FECPROC    TO TIT-FECHA.                             
           MOVE LK-PERIODO    TO TIT-PERIODO.                           
                                                                        
           DISPLAY '*----  PARAMETROS DEL PROCESO  ----*'.              
           DISPLAY '* MES   DE PROCESO      : ' LK-PERIODO.             
           DISPLAY '* FECHA DE PROCESO      : ' WS-FECPROC.             
           DISPLAY '* HORA DE INICIO PROCESO: ' WS-HORA-EDIT.           
      *--------------*                                                  
       CARGA-TIPODOC.                                                   
      *--------------*                                                  
           INITIALIZE WW-TABLA-TIPODOC.                                 
           INITIALIZE TCWC1000.                                         
           MOVE '2'            TO W100-CDOPCIO.                         
           MOVE '0113'         TO W100-CDTABLA.                         
           MOVE '0011'         TO W100-STBANCO.                         
           MOVE 'R'            TO W100-TCCIDIOM.                        
           MOVE SPACES         TO W100-CLAVTG.                          
           MOVE 1              TO W100-NUCLAVE.                         
           CALL 'TC9C1000' USING TCWC1000.;
           
           IF W100-CDRETORN = '00'  THEN
           DO; PERFORM GUARDA_TIPODOC VARYING II FROM 1 BY 1             
                                     UNTIL II > 20;
			END;
           ELSE     
           DO; *DISPLAY 'ERROR EN RUTINA TC9C1000 :' W100_CDRETORN      
              DISPLAY 'RUTINA TIPO DOCUMENTO.....'                    
              DISPLAY 'SE CANCELA EL JOB.........'                  
              RETURN_CODE = '16'                                  
              STOP RUN;                                                  
           END;
                                                      
*GUARDA-TIPODOC.;                                                
           IF W100-CLAVSAL (II) = '' OR LOW-VALUES THEN 
           //DO;CONTINUE;
           END;
           ELSE
           DO;
              WS-IDDOC = W100-CLAVSAL (II);                       
              WW-IDDOC   (II)  =   SubStr(WS-IDDOC,2,1);           
              T113-DATOS  = W100-CONTOCUR(II);                    
              WW-CONVER  (II) = T113-PETIPDOCITF;              
              WW-LONGDOC (II) = T113-PELONDOCITF;                
              WW-TCEDOC  (II) = T113-PETCEDOCITF ;                
           END;
                                                           
*ABRIR-ARCHIVOS.*                                                  
	*OPEN INPUT E1DQSEQ1. ;                                        
           IF FS1 ^= '00' AND ^= '97' THEN 
           DO;*DISPLAY '** ERROR EN OPEN E1DQSEQ1. FS=' FS1              
              *DISPLAY '* (OJO: SI FS=93 ARCHIVO ABIERTO) *'             
              *RETURN-CODE = '16';                                   
              *STOP RUN;                                                  
           END;                                                      
           
           *OPEN OUTPUT S1DQSEQ2 REPORTE.                                
      *INC414417-INI                                                    
           OPEN OUTPUT S1DQSEQ3.                                        
      *INC414417-FIN    
      
      
                                               
                                                     
*PROCESO_PRINCIPAL.; 
	*PERFORM LEE-ENTRADA UNTIL SW-FIN = 1.                        
	

*LEE-ENTRADA.                                                   
	  READ E1DQSEQ1 AT END MOVE 1 TO SW-FIN. ;
           
           IF SW-FIN = '0' THEN 
           DO; WS-LEIDOS = +1;
              *PERFORM PROCESA-REGISTRO ;                               
           END;  
           
*PROCESA-REGISTRO;
 	REG-SALIDA1 = REG-ENTRADA;                            
           *INSPECT S1-IDDOC  REPLACING ALL LOW-VALUES BY SPACES       
           INSPECT S1-NUMDOC REPLACING ALL LOW-VALUES BY SPACES;
           
      *INC414417-INI;                                                  
           S3-TIPDOC-PE = S1-IDDOC;                          
           S3-NRODOC-PE = S1-NUMDOC;                           
      *INC414417-FIN;
      
IF (S1-IDDOC   = '' AND S1-NUMDOC = '') AND          
              (S1-CODTRAN = 'CT06' OR 'CT08' OR 'CT09' OR               
                            'CT12' OR 'CT13')THEN 
DO;S1-IDDOC  = '01';                             
   S1-NUMDOC = '11111111';   
   END;
ELSE
DO; *PERFORM CONVIERTE-DOCUMENTO;                              
	END;                                                       	
                                                                        
      *200404195-INI                                                    
           PERFORM CONSISTENCIA-PAIS.                                   
      *200404195-FIN                                                    
      *INC414417-INI;                                                   
           S3-TIPDOC-SBS = S1-IDDOC;                           
           S3-NRODOC-SBS = S1-NUMDOC;                          
IF S1-IDDOC = '00' OR '04' OR '07' THEN
DO;*WRITE REG-SALIDA3                                         
              ADD 1 TO WS-GRABADOS3;                                 
	END;                                                       
      *INC414417-FIN                                                   
       *WRITE REG-SALIDA1.                                           
           *ADD 1 TO WS-GRABADOS1.;  
           


*CONVIERTE-DOCUMENTO;                                            
           SW-MAL= '0';                                            
IF S1-IDDOC = '' OR LOW-VALUES THEN
DO;SW-MAL = '1';                                
   LD-DESCRI = 'SIN TIPO DE DOCUMENTO';                
   *PERFORM IMPRIME-DETALLE;
   END;
   
      
      *200312064_INI;                                                  
               S1-IDDOC = '01';                               
               S1-NUMDOC = '11111111';                              
      *200312064_FIN;
      END;
ELSE 
DO; SW-OK  = '0';                                         
    *PERFORM BUSCA-IDDOC VARYING II FROM 1 BY 1                
	 UNTIL II > 20 OR SW-OK = 1; 
	 END;
	 
IF SW-OK = '0' OR (WS-CONVER = '') OR;                   
   *(WS-LONGDOC NE NUMERIC) OR WS-LONGDOC = ZEROS; THEN      
DO; SW-MAL = '1';                            
    LD-DESCRI = 'TIPO DOC. SIN DATOS EN TABLAS CORPORATIVAS';                        
	*PERFORM IMPRIME-DETALLE;
	END;
	
      
      *200312064-INI;                                                    
                 S1-IDDOC = '01';                           
                 S1-NUMDOC = '11111111';                          
      *200312064-FIN;
      END;
ELSE
DO;                                                      
     S1-IDDOC = WS-CONVER;
     END;
     
     
IF S1-NUMDOC = '' OR LOW-VALUES THEN
DO;SW-MAL ='1';                              
   LD-DESCRI = 'SIN NUMERO DE DOCUMENTO';        
	*PERFORM IMPRIME-DETALLE ;
	END;
	
      *200404195-INI                                                    
                    PERFORM COMPLETA-NUMDOC                             
      *200404195-FIN                                                    
                 ELSE                                                   
                    PERFORM CONSISTENCIA-NUMDOC                         
                 END-IF                                                 
              END-IF                                                    
           END-IF.                                                      
      *------------*                                                    
       BUSCA-IDDOC.                                                     
      *------------*                                                    
           IF WW-IDDOC (II) = S1-IDDOC                                  
              MOVE 1              TO SW-OK                              
              MOVE WW-CONVER (II) TO WS-CONVER                          
              MOVE WW-LONGDOC(II) TO WS-LONGDOC                         
              MOVE WW-TCEDOC (II) TO WS-TCEDOC                          
           END-IF.                                                      
      *--------------------*                                            
       CONSISTENCIA-NUMDOC.                                             
      *--------------------*                                            
           MOVE 0 TO WS-CONTADOR.                                       
           MOVE S1-NUMDOC TO WS-NUMDOC.                                 
           INSPECT WS-NUMDOC TALLYING WS-CONTADOR FOR ALL SPACES.       
           COMPUTE WS-CONTADOR = 15 - WS-CONTADOR.                      
      * 1:LONGITUD EXACTA  0:LONGITUD MAXIMA                            
           IF (WS-TCEDOC(3:1) = '1' AND WS-CONTADOR NOT = WS-LONGDOC) OR 
              (WS-TCEDOC(3:1) = '0' AND WS-CONTADOR >     WS-LONGDOC)   04560000
              MOVE 1 TO SW-MAL                                          04570000
           END-IF.                                                      04580000
      * 5:NUMERICO         0:ALFANUMERICO                               04590000
           IF (WS-TCEDOC(1:1) = '5') AND                                04600000
              (WS-NUMDOC(1:WS-CONTADOR) IS NOT NUMERIC OR               04610000
               WS-NUMDOC(1:WS-CONTADOR) = ZEROS)                        04620000
              MOVE 1 TO SW-MAL                                          04630000
           END-IF.                                                      04640000
                                                                        04650000
           IF S1-IDDOC = '01'  AND S1-NUMDOC (1:2) = '20' AND           04660000
              WS-CONTADOR = 11 AND S1-NUMDOC (1:11) IS NUMERIC          04670000
              MOVE ZEROS TO SW-MAL                                      04680000
              MOVE '06'  TO S1-IDDOC                                    04690000
              MOVE 'NUMERO DE RUC CON TIPO DOCUMENTO DNI' TO LD-DESCRI  04700000
              PERFORM IMPRIME-DETALLE                                   04710000
           ELSE                                                         04720000
              IF S1-IDDOC = '01' AND SW-MAL = 1                         04730000
                 MOVE '11111111' TO S1-NUMDOC                           04740000
                 MOVE 'NUMERO DOCUMENTO INCORRECTO' TO LD-DESCRI        04750000
                 PERFORM IMPRIME-DETALLE                                04760000
              ELSE                                                      04770000
                 IF SW-MAL = 1                                          04780000
                    MOVE 'NUMERO DOCUMENTO INCORRECTO' TO LD-DESCRI     04790000
                    PERFORM IMPRIME-DETALLE                             04800000
      *200404195-INI                                                    04810000
                    PERFORM COMPLETA-NUMDOC                             04820000
      *200404195-FIN                                                    04830000
                 END-IF                                                 04840000
              END-IF                                                    04850000
           END-IF.                                                      04860000
                                                                        04870000
      *200404195-2-INI                                                  04880000
           IF S1-IDDOC = '06' AND SW-MAL = 0                            04890000
              PERFORM CONSISTENCIA-RUC                                  04900000
           END-IF.                                                      04910000
      *200404195-2-FIN                                                  04920000
                                                                        04930000
      *200404195-INI             
      *----------------*                                                04950000
       COMPLETA-NUMDOC.                                                 04960000
      *----------------*                                                04970000
           MOVE SPACES TO WS-NUMDOC.                                    04980000
           MOVE '11111111111111111111' TO WS-NUMDOC(1:WS-LONGDOC)       04990000
           MOVE WS-NUMDOC TO S1-NUMDOC.                                 05000000
      *------------------*                                              05010000
       CONSISTENCIA-PAIS.                                               05020000
      *------------------*                                              05030000
           IF S1-IDDOC = '01' OR '02' OR '03' OR '06' OR '08' OR '11'   05040000
              MOVE 'PER' TO S1-PAIS                                     05050000
           END-IF.                                                      05060000
           IF S1-IDDOC = '00'                                           05070000
              MOVE 'PTN' TO S1-PAIS                                     05080000
           END-IF.                                                      05090000
           IF S1-PAIS = SPACES OR LOW-VALUES                            05100000
              MOVE 'PER' TO S1-PAIS                                     05110000
           END-IF.                                                      05120000
      *200404195-FIN                                                    05130000
                                                                        05140000
      *200404195-2-INI                                                  05150000
      *-----------------*                                               05160000
       CONSISTENCIA-RUC.                                                
      *-----------------*                                               
           MOVE S1-NUMDOC(1:11) TO WK-RUC-ARR-R.                        
           COMPUTE WK-SUMA =  (WK-RUC1-ARR  *  5)  +                    
                              (WK-RUC2-ARR  *  4)  +                    
                              (WK-RUC3-ARR  *  3)  +                   
                              (WK-RUC4-ARR  *  2)  +                    
                              (WK-RUC5-ARR  *  7)  +                    
                              (WK-RUC6-ARR  *  6)  +                    
                              (WK-RUC7-ARR  *  5)  +                    05260000
                              (WK-RUC8-ARR  *  4)  +                    05270000
                              (WK-RUC9-ARR  *  3)  +                    05280000
                              (WK-RUC10-ARR *  2).                      05290000
           COMPUTE WK-CHEQUEO = WK-SUMA /  11.                          05300000
           COMPUTE WK-CHEQUEO = WK-SUMA - (11 * WK-CHEQUEO).            05310000
           COMPUTE WK-CHEQUEO = 11 - WK-CHEQUEO.                        05320000
           IF WK-CHEQUEO = 10                                           05330000
              MOVE ZEROS TO WK-CHEQUEO                                  05340000
           END-IF.                                                      05350000
           MOVE WK-CHEQUEO TO WK-MODULO11.                              05360000
           IF WK-MODULO11 NOT = WK-RUC11-ARR                            05370000
              MOVE 1 TO SW-MAL                                          05380000
              MOVE 'NUMERO DE RUC NO CUMPLE MODULO 11' TO LD-DESCRI     05390000
              PERFORM IMPRIME-DETALLE                                   05400000
              MOVE '20131312955    ' TO S1-NUMDOC                       05410000
           END-IF.                                                      05420000
      *200404195-2-FIN                                                  05430000
                                                                        05440000
      *----------------*                                                05450000
       IMPRIME-DETALLE.                                                 05460000
      *----------------*                                                05470000
           IF CONT-LIN > 59                                             05480000
              PERFORM IMPRIME-TITULOS                                   05490000
           END-IF.                                                      05500000
           MOVE E1-IDDOC   TO LD-IDDOC.                                 05510000
           MOVE E1-NUMDOC  TO LD-NUMDOC.                                05520000
           MOVE E1-CUENTA  TO LD-CUENTA.                                05530000
           MOVE E1-CODTRAN TO LD-CODTRAN.                               05540000
           WRITE LISTADO FROM LINEA-DETALLE AFTER 1.                    05550000
           ADD 1 TO CONT-LIN.                                           05560000
           ADD 1 TO WS-ERRADOS.                                         05570000
      *--------------*                                                  05580000
       PROCESO-FINAL.                                                   05590000
      *--------------*                                                  05600000
           ACCEPT WS-HORA FROM TIME.                                    05610000
           MOVE WS-HORA (1:2) TO WS-HORA-EDIT (1:2).                    05620000
           MOVE ':'           TO WS-HORA-EDIT (3:1) WS-HORA-EDIT (6:1). 05630000
           MOVE WS-HORA (3:2) TO WS-HORA-EDIT (4:2).                    05640000
           MOVE WS-HORA (5:2) TO WS-HORA-EDIT (7:2).                    05650000
                                                                        05660000
           DISPLAY '  '.                                                05670000
           DISPLAY '* HORA DE FIN DE PROCESO: ' WS-HORA-EDIT.           
           DISPLAY '  '.                                                
           DISPLAY 'REGISTROS LEIDOS       : ' WS-LEIDOS.               
           DISPLAY 'REGISTROS GRABADOS     : ' WS-GRABADOS1.            
      *INC414417-INI                                                    
           DISPLAY 'REGISTROS GRABADOS 2   : ' WS-GRABADOS3.            
      *INC414417-FIN                                                    
           DISPLAY 'CON ERROR EN DOCUMENTO : ' WS-ERRADOS.              
           DISPLAY '*------------------------------------*'.            
           DISPLAY '***  FINAL  PROGRAMA BG3CI824     **** '.           
           DISPLAY '*------------------------------------*'.            
           CLOSE E1DQSEQ1 S1DQSEQ2 REPORTE.                             
      *INC414417-INI                                                    
           CLOSE S1DQSEQ3.                                              
      *INC414417-FIN                                                    
      *----------------*                                                 
       IMPRIME-TITULOS.                                                 
      *----------------*                                                
           ADD  1 TO CONT-PAG.                                          
           MOVE CONT-PAG   TO TIT-PAGINAS.                              
           WRITE LISTADO FROM TITULO-01   AFTER PAGE.                   
           WRITE LISTADO FROM TITULO-02.                                
           WRITE LISTADO FROM TITULO-RAYA AFTER 1.                      
           WRITE LISTADO FROM TITULO-03.                                
           WRITE LISTADO FROM TITULO-RAYA AFTER 1.                      
           MOVE 5 TO CONT-LIN.                                          
      *-----------------------------*                                   
      *    FIN-PROGRAMA-BG3CI824    *                                   
      *-----------------------------*                                   
