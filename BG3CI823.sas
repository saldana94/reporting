DATA SALDA2;
SET ;
*PROCESO PRINCIPAL;
	*PERFORM LEE-MOVIMIENTOS.
           PERFORM LEE-PERSONAS.
            IF LK-OPCION = 'CT'
               PERFORM CRUCE-CUENTAS
           ELSE
              PERFORM CRUCE-DOCUMENTOS
           END-IF.;
*CRUCE CUENTAS;
IF E1_CUENTA = '' THEN
  DO; REG_SALIDA = REG_ENTRADA;
	*PERFORM GRABA-SALIDA
	 PERFORM LEE-MOVIMIETO;
	 END;
ELSE
IF E1_CUENTA = E2_CUENTA THEN
  DO; REG_SALIDA = REG_ENTRADA;
	S1_IDDOC = E2_IDDOC;
	S1_NUMDOC = E2_NUMDOC;
	S1_PAIS = E2_PAIS;
	ENCONTRADOS = +1;
	*PERFORM GRABA_SALIDA
	 PERFORM LEE_MOVIMIETO;
	END;
ELSE
IF E1_CUENTA < E2_CUENTA THEN
  DO; REG_SALIDA = REG_ENTRADA;
  	*PERFORM GRABA_SALIDA
	 PERFORM LEE_MOVIMIETO;
	 END;
ELSE 
  DO; *PERFORM LEE_PERSONAS;
  	 END;

*CRUCE DOCUMENTOS;
	*PERFORM UNTIL SW_FIN1=1;
IF E1_IDDOC = '' AND E1_NUMDOC = '' OR
   E1_PAIS NE '' THEN
  DO; REG_SALIDA = REG_ENTRADA;
    *PERFORM GRABA-SALIDA
     PERFORM LEE-MOVIMIENTOS;
     END;
ELSE 
IF E1-DOCUMENTO = E2-DOCUMENTO THEN 
  DO; ENCONTRADOS = +1;
      REG-SALIDA = REG-ENTRADA;
      S1-PAIS = E2-PAIS;
	*PERFORM GRABA-SALIDA
	 PERFORM LEE-MOVIMIENTOS;
	 END;
ELSE 
IF E1-DOCUMENTO < E2-DOCUMENTO THEN
  DO; REG-SALIDA = REG-ENTRADA;
	*PERFORM GRABA-SALIDA
	 PERFORM LEE-MOVIMIENTOS;
ELSE
  DO; *PERFORM LEE-PERSONAS;
	 END;
	 
*LEE MOVIMIENTOS;
	*READ E1DQSEQ1 AT END MOVE 1 TO SW-FIN1.;
IF SW-FIN1 = '0' THEN
  DO; LEIDOS1 = +1;
	END;
ELSE
  DO; E1-CUENTA E1-DOCUMENTO = HIGH-VALUES;
	END;
           
*LEE PERSONAS*;
	*READ E2DQSEQ2 AT END MOVE 1 TO SW-FIN2.;
IF SW-FIN2 = '0' THEN 
  DO; WS-LEIDOS2 = +1;
	END;
ELSE
  DO; E2-CUENTA E2-DOCUMENTO = HIGH-VALUES;
	END;
          
*GRABA-SALIDA*;
IF S1-IDDOC = '' OR 
   S1-NUMDOC = '' THEN 
  DO; WS-SIN-DOC = +1;
	END;
ELSE
  DO; WS-CON-DOC = +1;
	END;       
IF S1-PAIS = '' THEN 
  DO; WS-SIN-PAIS = +1;
	END;
ELSE
  DO; WS-CON-PAIS = +1;
	END;
           *WRITE REG-SALIDA.
            WS-GRABADOS = +1;
           
    
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           